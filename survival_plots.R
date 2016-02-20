library(survival)
library(ggplot2)
library(dplyr)

censoringDAT <- function(x){
  tempdf <- with(x, data.frame(
    time = time,
    events = n.event,
    nrisk = n.risk,
    nrisk2 = nrow(survTable)+first(n.event[n.event>0])-cumsum(n.event)
  ))
  
  cendf <- tempdf %>% transmute(
    time = time/year,
    surv = cumprod((nrisk-events)/nrisk),
    cumprob = 1-surv,
    censoring = "Yes"
  )
  
  uncendf <- tempdf %>% transmute(
    time = time/year,
    surv = cumprod((nrisk2-events)/nrisk2),
    cumprob = 1-surv,
    censoring = "No"
  )
  return(rbind(cendf,uncendf))
}

### Linked and Alive----

Linked <- survfit(
  Surv(followTime, LTFU_status) ~ 1
  , data=survTable
)

LinkedDF <- censoringDAT(Linked)

print (ggplot(LinkedDF, aes(time,surv,colour=censoring))
	+ geom_line() 
	+ ggtitle("Proportion linked")
	+ ylab("Probability")
	+ theme_bw()
)

# Eligible for ART (Yes or No) ----

Eligible <- survfit(
  Surv(eligibleTime, eligible_ever) ~ 1
  , data=survTable
)

EligibleDF <- censoringDAT(Eligible)

print(ggplot(EligibleDF, aes(time,cumprob,colour=censoring))
      + geom_line() 
      + ggtitle("Eligibility Through Time")
      + ylab("Probability")
      + theme_bw()
)

## ART (Yes or No) ----

ART <- survfit(
  Surv(arvFollowTime, arv_ever, type= "right") ~ 1
  , data=survTable
)

ARTDF <- censoringDAT(ART)

print(ggplot(ARTDF, aes(time,cumprob,colour=censoring))
           + geom_line() 
           + ggtitle("ART Through Time")
           + ylab("Probability")
           + xlab("Time in Years")
           + theme_bw()
)


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

