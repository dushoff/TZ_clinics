library(survival)
library(ggplot2)
library(dplyr)

### Linked and Alive----

Linked <- survfit(
  Surv(followTime, LTFU_status) ~ 1
  , data=survTable
)

datLA <- with(Linked, data.frame(
  time = time,
  nrisk = n.risk,
  nrisk2 = nrow(survTable),
  events = n.event
))

# I'll make a function for this later (just use cumsum and do it in one step)
for(i in 2:nrow(datLA)){
  datLA$nrisk2[i] <- datLA$nrisk2[i-1] - datLA$events[i-1]
}

datLA <- datLA %>% mutate(
  surv = cumprod((nrisk-events)/nrisk),
  surv2 = cumprod((nrisk2 - events)/nrisk2),
  followUp = time/year
)

# Do this with rbind
dat2LA <- (data.frame(time = c(datLA$followUp,datLA$followUp),
                       surv = c(datLA$surv,datLA$surv2),
                       censoring = c(rep("Yes",nrow(datLA)),rep("No",nrow(datLA)))
))

print (ggplot(dat2LA, aes(time,surv,colour=censoring))
	+ geom_line() 
	+ ggtitle("Proportion linked")
	+ ylab("S(t)")
	+ theme_bw()
)

# Eligible for ART (Yes or No) ----

ELISurv <- survfit(
  Surv(eligibleTime, eligible_ever, type= "right") ~ 1
  , data=survTable
)

datELI <- data.frame(
  time = ELISurv$time,
  nrisk = ELISurv$n.risk,
  nrisk2 = nrow(survTable),
  events = ELISurv$n.event
)

# I'll make a function for this later
for(i in 2:nrow(datELI)){
  datELI$nrisk2[i] <- datELI$nrisk2[i-1] - datELI$events[i-1]
}

datELI <- datELI %>% mutate(
  surv = cumprod((nrisk-events)/nrisk),
  surv2 = cumprod((nrisk2 - events)/nrisk2),
  cumprob = 1-surv,
  cumprob2 = 1- surv2,
  followUp = time/year
)

dat2ELI <- (data.frame(time = c(datELI$followUp,datELI$followUp),
                        cumprob = c(datELI$cumprob,datELI$cumprob2),
                        censoring = c(rep("Yes",nrow(datELI)),rep("No",nrow(datELI)))
))

print(ggplot(dat2ELI, aes(time,cumprob,colour=censoring))
      + geom_line() 
      + ggtitle("Eligibility Through Time")
      + ylab("1 - S(t)")
      + theme_bw()
)

## ART (Yes or No) ----

ARTSurv <- survfit(
  Surv(arvFollowTime, arv_ever, type= "right") ~ 1
  , data=survTable
)

datART <- data.frame(
  time = ARTSurv$time,
  nrisk = ARTSurv$n.risk,
  nrisk2 = nrow(survTable),
  events = ARTSurv$n.event
)

# I'll make a function for this later
for(i in 2:nrow(datART)){
  datART$nrisk2[i] <- datART$nrisk2[i-1] - datART$events[i-1]
}

datART <- datART %>% mutate(
  surv = cumprod((nrisk-events)/nrisk),
  surv2 = cumprod((nrisk2 - events)/nrisk2),
  cumprob = 1-surv,
  cumprob2 = 1- surv2,
  followUp = time/year
)

dat2ART <- (data.frame(time = c(datART$followUp,datART$followUp),
                       cumprob = c(datART$cumprob,datART$cumprob2),
                       censoring = c(rep("Yes",nrow(datART)),rep("No",nrow(datART)))
))

ARTplot<- (ggplot(dat2ART, aes(time,cumprob,colour=censoring))
           + geom_line() 
           + ggtitle("ART Through Time")
           + ylab("1 - S(t)")
           + xlab("Time in Years")
           + theme_bw()
)

print(ARTplot)

## LA coxph semi-full model ----

LAcoxfull <- (coxph(
  Surv(followTime, LTFU_status) ~ factor(sex_baseline) 
                                + as.numeric(age_baseline) 
                     #           + cd4_baseline
                                + enrolYear
                                + eligible_ever
                                + arv_ever
  , data=survTable
))

print(LAcoxfull)

## ELI coxph semi-full model ----

ELIcoxfull <- (coxph(
  Surv(eligibleTime, eligible_ever) ~ factor(sex_baseline) 
  + as.numeric(age_baseline) 
  #           + cd4_baseline
  + enrolYear
  + arv_ever
  , data=survTable
))

print(ELIcoxfull)

## ART coxph semi-full model ----

ARTcoxfull <- (coxph(
  Surv(arvFollowTime, arv_ever) ~ factor(sex_baseline) 
  + as.numeric(age_baseline) 
  #           + cd4_baseline
  + enrolYear
  + eligible_ever
  , data=survTable
))

print(ARTcoxfull)


## Linked by Gender ----
LAsexSurv <- update(Linked,.~sex_baseline)

datLAsex <- data.frame(
  time = summary(LAsexSurv)$time,
  nrisk = summary(LAsexSurv)$n.risk,
  events = summary(LAsexSurv)$n.event,
  gender = summary(LAsexSurv)$strata
)

datLAsex <- (datLAsex 
            %>% group_by(gender)
            %>% mutate(
              surv = cumprod((nrisk-events)/nrisk),
              followUp = time/year
            )
)

#datLAsex <- datLAsex %>% mutate(sex = ifelse(gender == 1, "Male","Female"))


LAsexplot<- (ggplot(datLAsex, aes(followUp,surv,colour=gender))
          + geom_line() 
          + ggtitle("LTFU Through Time by gender")
          + ylab("S(t)")
          + xlab("Year Lag")
          + theme_bw()
)

print(LAsexplot)


## ART by Gender ----
ARTsexSurv <- update(ARTSurv,.~sex_baseline)

datARTsex <- data.frame(
  time = summary(ARTsexSurv)$time,
  nrisk = summary(ARTsexSurv)$n.risk,
  events = summary(ARTsexSurv)$n.event,
  gender = summary(ARTsexSurv)$strata
)


datARTsex <- (datARTsex 
             %>% group_by(gender)
             %>% mutate(
               surv = cumprod((nrisk-events)/nrisk),
               cumprob = 1 - surv,
               followUp = time/year
             )
)

ARTsexplot<- (ggplot(datARTsex, aes(followUp,cumprob,colour=gender))
             + geom_line() 
             + ggtitle("ART Through Time by gender")
             + ylab("1-S(t)")
             + xlab("Year Lag")
             + theme_bw()
)

print(ARTsexplot)

## ART by Eligibility ----
ARTeliSurv <- update(ARTSurv,.~eligible_ever)

datARTeli <- data.frame(
  time = summary(ARTeliSurv)$time,
  nrisk = summary(ARTeliSurv)$n.risk,
  events = summary(ARTeliSurv)$n.event,
  Eligible = summary(ARTeliSurv)$strata
)


datARTeli <- (datARTeli 
              %>% group_by(Eligible)
              %>% mutate(
                surv = cumprod((nrisk-events)/nrisk),
                cumprob = 1 - surv,
                followUp = time/year
              )
)


ARTeliplot<- (ggplot(datARTeli, aes(followUp,cumprob,colour=Eligible))
              + geom_line() 
              + ggtitle("ART Through Time by Eligibility")
              + ylab("1-S(t)")
              + xlab("Year Lag")
              + theme_bw()
)

print(ARTeliplot)

## ART by Enrolment year ----
ARTyearSurv <- update(ARTSurv,.~enrolYear)

datARTyear <- data.frame(
  time = summary(ARTyearSurv)$time,
  nrisk = summary(ARTyearSurv)$n.risk,
  events = summary(ARTyearSurv)$n.event,
  Enrolment_Year = summary(ARTyearSurv)$strata
)


datARTyear <- (datARTyear 
              %>% group_by(Enrolment_Year)
              %>% mutate(
                surv = cumprod((nrisk-events)/nrisk),
                cumprob = 1 - surv,
                followUp = time/year
              )
)

ARTyearplot<- (ggplot(datARTyear, aes(followUp,cumprob,colour=Enrolment_Year))
              + geom_line() 
              + ggtitle("ART Through Time by Enrolment Year")
              + ylab("1-S(t)")
              + xlab("Year Lag")
              + theme_bw()
)

print(ARTyearplot)

