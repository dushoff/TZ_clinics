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

print(plot(laSurv,mark.time=TRUE,main='Linked Survival: Enrolling in program (getting check up)',xlab='Day Lag'))

print(plot(laSurv,mark.time=TRUE
           , main='Linked Survival: Enrolling in program (getting check up)'
           , xlab='Day Lag'
           , fun = function(x){1-x}
           )
      )


print(ggplot(linked, aes(followUp, cumprob))
      + xlab("Follow-up years")
      + ylab("Survival Probaiblity")
      + geom_line()
      + ggtitle('Survival while linked to care')
      + ylim(c(0, 1))
      + theme_bw()
)



# ARV treatment (Yes or No) ----

##testing first 30 patients 
aa <- head(survTable,30)
aaa <- aa %>% select(arvFollowTime,arv_ever)

arvSurv <- survfit(
  Surv(arvFollowTime, arv_ever, type= "right") ~ 1
 , data=survTable
   # ,data=aaa
)

print(plot(arvSurv
           , mark.time=TRUE
           , main='"Survival" until ART'
           , xlab='Time since enrolment (days)'
           )
)


print(plot(arvSurv
           , mark.time=TRUE
           , main='"Survival" until ART'
           , xlab='Time since enrolment (days)'
           , fun = function(x){1-x}
           )
)


# The censering graph is weird, I don't like it because the censered data changes nrisk
# creating weird jumps.


arv <- data.frame(
  time=arvSurv$time, 
  nrisk = arvSurv$n.risk, 
  events <- arvSurv$n.event
)

## S(t) = PRODUCT ( (#risk - #event) / #risk )
## It is weird no matter how you look at it.

arv <- arv %>% mutate(
  surv = cumprod((nrisk-events)/nrisk),
  cumprob = 1-surv, 
  followUp = time/year
)

### This is the cumulative probability plot of Enrolling in ART out of the population	
print(ggplot(arv, aes(followUp,cumprob))
      + geom_line() 
      + ggtitle('Cumulative Probability of Getting ART (POPULATION)')
      + theme_bw()
)



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
