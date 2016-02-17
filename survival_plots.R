library(survival)
library(ggplot2)
library(dplyr)

### Linked and Alive... for this section we don't even care about ART, simply coming to get checkup----

laSurv <- survfit(
  Surv(followTime, LTFU_status) ~ 1
  , data=survTable
)

linked <- data.frame(
  time=laSurv$time
  , nrisk= laSurv$n.risk
  , linkedcount= laSurv$n.event
)

linked <- linked %>% mutate(
  surv = cumprod((nrisk-linkedcount)/nrisk)
  , cumprob = 1 - surv
  , followUp = time/year
)

print(ggplot(linked, aes(followUp, surv))
      + xlab("Lags (years)")
      + ylab("Survival Probaiblity")
      + geom_line()
      + ggtitle('Lost to Follow Up Through Time')
      + ylim(c(0, 1))
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

ELIplot<- (ggplot(dat2ELI, aes(time,cumprob,colour=censoring))
      + geom_line() 
      + ggtitle("I don't know what to name this")
      + ylab("1 - S(t)")
      + theme_bw()
)

print(ELIplot)
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
           + ggtitle("I don't know what to name this")
           + ylab("1 - S(t)")
           + theme_bw()
)

print(ARTplot)

quit()
### By enrolYear need to look at summary strata ARV(Yes or NO) -----

arvYearSurv <- update(arvSurv, .~enrolYear)
summary(arvYearSurv)
print(arvYearSurv)


arvpsm <- psm(
  Surv(arvFollowTime, arv_ever) ~ 1
  , data=survTable
)

print(arvpsm)

arvYearpsm <- psm(
  Surv(arvFollowTime, arv_ever) ~ enrolYear
  , data=survTable
)

print(arvYearpsm)
# plot(arvYearPH)
summary(arvYearPH)
print(arvYearPH)
anova(arvYearPH, arvPH)

arvYear <- with(arvYearSurv,data.frame(
  time=time, 
  nrisk = nstrata(n.risk,strata) , 
  yr = catstrata(strata),
  arvcount= n.event
))

arvYear <- arvYear %>% 
  group_by(yr) %>%  
  mutate(
    surv = cumprod((nrisk-arvcount)/nrisk),
    cumprob = 1-surv,
    followUp = time/year
  )
yr <- unique(arvYear$yr)
print(plot(arvYearSurv,mark.time = FALSE,main='Survival by Year: Getting ARV and delay',xlab="day lag",col=yr))

print(ggplot(arvYear, aes(followUp,cumprob,col=factor(yr),group=yr))
      + geom_line() 
      + ggtitle('Cumulative Probability of getting ART (POPULATION) by Year')
)

# still in the program (coming to check up, by year) ----

linkedyearSurv <- update(laSurv,.~enrolYear)
#plot(linkedyearSurv, mark.time=FALSE)


linkedyear <- with(linkedyearSurv,data.frame(
  time=time, 
  nrisk = nstrata(n.risk,strata),
  yr = catstrata(strata),
  linkedcount= n.event
))

linkedyear <- linkedyear %>% 
  group_by(yr) %>%  
  mutate(
    surv = cumprod((nrisk-linkedcount)/nrisk),
    cumprob = 1-surv,
    followUp = time/year
  )

plot(linkedyearSurv, mark.time=FALSE,main="Survival Linked by Year: Enrolling in program (getting check up)",col=yr)


print(ggplot(linkedyear, aes(followUp,cumprob,col=factor(yr),group=yr))
      + geom_line() 
      + ggtitle('Cumulative Probability of population still in the program by Year')
)

# stuff we didn't do yet ------
quit()

arvYearsum <- summary(arvYearSurv)

arvYear <- data.frame(time=arvYearsum$time,
                      Year= arvYearsum$strata,
                      yearcount = arvYearsum$n.event,
                      n = sum(arvYearSurv$n),
                      nart = sum(arvYearSurv$n.event))

arvYear <- arvYear %>% group_by(Year) %>%	mutate(probpop = yearcount/n,
                                                 probart = yearcount/nart,
                                                 cumprobpop = cumsum(probpop),
                                                 cumprobart = cumsum(probart),
                                                 followUp = time/year)

ggplot(arvYear, aes(followUp, cumprobpop, group=Year, colour=Year )) + geom_line() +
  ggtitle('Cumulative Probability of ART Start by Enrolment Year (POPULATION)') + xlab("Follow-up in Years") + ylab("Probability")

ggplot(arvYear, aes(followUp, cumprobart, group=Year, colour=Year )) + geom_line() + 
  ggtitle('Cumulative Probability of ART Start by Enrolment Year (ART)') + xlab("Follow-up in Years") + ylab("Probability")


linkedyearSurv <- update(laSurv,.~enrolYear)
linkedyearsum <- summary(linkedyearSurv)

linkedyear <- data.frame(time=linkedyearsum$time
                         , Year= linkedyearsum$strata
                         , yearcount = linkedyearsum$n.event
                         , n = sum(linkedyearSurv$n)
                         , nlinked = sum(linkedyearSurv$n.event)
)

linkedyear <- linkedyear %>% group_by(Year) %>%	mutate(probpop = yearcount/n,
                                                       problinked = yearcount/nlinked,
                                                       cumprobpop = cumsum(probpop),
                                                       cumproblinked = cumsum(problinked),
                                                       followUp = time/year)

ggplot(linkedyear,
       aes(followUp, cumprobpop, group=Year, colour=Year ))
+ geom_line()
+ ggtitle('Cumulative Probability of Linked by Enrolment Year (POPULATION)')
+ xlab("Follow-up in Years") + ylab("Probability")

ggplot(linkedyear, aes(followUp, cumproblinked, group=Year, colour=Year )) + geom_line() + 
  ggtitle('Cumulative Probability of Linked by Enrolment Year (Linked)') + xlab("Follow-up in Years") + ylab("Probability")

# 
# testing <- merge(arv,arvYear, by="time")
# head(testing)
# testing <- testing %>% group_by(year) %>% 
#	 mutate(ratio = event.per.year/event,
#					cumratio = cumsum(event.per.year)/cumsum(event))
# 
# head(testing)
# 
# ggplot(testing, aes(time, cumratio, group=year,colour=year)) + geom_line() + 
# ##Linked and Alive
# 
# SurvEnrol <- survfit(Surv(templast - as.numeric(startdate), templast >= endDate) ~ 1,data=survTable)
# plot(SurvEnrol, conf.int=FALSE, xlab = "Time", ylab = "Survival Probability", main = "Linked and Alive")
# 
# arvYearmod
