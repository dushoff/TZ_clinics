## Compare arv_ever to arvstatuscode_first!

###Survival Analysis
library(survival)
library(dplyr)
library(ggplot2)

survTable <- (patientTable %>% 
	mutate(e_diff= eligible_status_delay + 1
		, arv_ever = !is.na(arv_status_delay)
		, arv_diff = arv_status_delay + 1
		, lastdate = ifelse(LTFU_status, followUp, endDate)
		, arvFollowTime = ifelse(
			arv_ever, arv_diff, lastdate - as.numeric(minDate)
		)
		, laFollowTime = lastdate-as.numeric(minDate)
		, enrolYear = format(minDate, "%Y")
	)
)

## We want avoid using survival objects for now until we actually need the analysis (coxs ph)
## For now, only use it for collaping data and extracting info back to dataframe
arvSurv <- survfit(Surv(
	arvFollowTime, arv_ever) ~ 1
	, data=survTable
)

print(sum(arvSurv$n.event)/arvSurv$n)
print(plot(arvSurv,mark.time=FALSE,main='ARV Ever Survival',xlab='day_lag'))

arv <- data.frame(
	time=arvSurv$time, 
	n = arvSurv$n, 
	arvcount= arvSurv$n.event
)

arv <- arv %>% mutate(
	prob = arvcount/n,
	cumprob = cumsum(prob),
	followUp = time/year
)

### This is the cumulative probability plot of Enrolling in ART out of the population	
print(ggplot(arv, aes(followUp,cumprob))
	+ geom_line() 
	+ ggtitle('Cumulative Probability of Enrolling in ART Ever (POPULATION)')
)

### Linked and Alive... for this section we don't even care about ART

laSurv <- survfit(
	Surv(laFollowTime, LTFU_status) ~ 1
	, data=survTable
)

laSurv

print(sum(laSurv$n.event)/laSurv$n)

linked <- data.frame(
	time=laSurv$time
	, n= laSurv$n
	, linkedcount= laSurv$n.event
)

linked <- linked %>% mutate(
	prob = linkedcount/n
	, cumprob = cumsum(prob)
	, survprob = 1-cumsum(prob)
	, followUp = time/year
)

print(plot(laSurv,mark.time=FALSE,main='Linked',xlab='Day Lag'))


print(ggplot(linked, aes(followUp, survprob))
	+ xlab("Follow-up years")
	+ ylab("Survival Probaiblity")
	+ geom_line()
	+ ggtitle('Survival while linked to care')
	+ ylim(c(0, 1))
)

### By enrolYear

arvyearsur <- update(arvSurv, .~enrolYear)
#plot(arvyearsur, mark.time=FALSE)

##need to look at summary strata 

arvyear <- with(arvyearsur,data.frame(
  time=time, 
  n = c(rep(n[1],strata[1]),
        rep(n[2],strata[2]),
        rep(n[3],strata[3]),
        rep(n[4],strata[4]))
        , 
  yr = c(rep(2011,strata[1]),
           rep(2012,strata[2]),
           rep(2013,strata[3]),
           rep(2014,strata[4])),
  arvcount= n.event
))

arvyear <- arvyear %>% 
  group_by(yr) %>%  
    mutate(
      prob = arvcount/500,
      cumprob = cumsum(prob),
      followUp = time/year
)

print(plot(arvyearsur,mark.time = FALSE,main='ARV Ever Survival by Year',xlab="day lag"))

print(ggplot(arvyear, aes(followUp,cumprob,col=factor(yr),group=yr))
      + geom_line() 
      + ggtitle('Cumulative Probability of Enrolling in ART (POPULATION) by Year')
)


linkedyearsur <- update(laSurv,.~enrolYear)
plot(linkedyearsur, mark.time=FALSE)

quit()

arvyearsum <- summary(arvyearsur)

arvyear <- data.frame(time=arvyearsum$time,
											Year= arvyearsum$strata,
											yearcount = arvyearsum$n.event,
											n = sum(arvyearsur$n),
											nart = sum(arvyearsur$n.event))

arvyear <- arvyear %>% group_by(Year) %>%	mutate(probpop = yearcount/n,
																									probart = yearcount/nart,
															cumprobpop = cumsum(probpop),
															cumprobart = cumsum(probart),
															followUp = time/year)

ggplot(arvyear, aes(followUp, cumprobpop, group=Year, colour=Year )) + geom_line() +
	ggtitle('Cumulative Probability of ART Start by Enrolment Year (POPULATION)') + xlab("Follow-up in Years") + ylab("Probability")

ggplot(arvyear, aes(followUp, cumprobart, group=Year, colour=Year )) + geom_line() + 
	ggtitle('Cumulative Probability of ART Start by Enrolment Year (ART)') + xlab("Follow-up in Years") + ylab("Probability")


linkedyearsur <- update(laSurv,.~enrolYear)
linkedyearsum <- summary(linkedyearsur)

linkedyear <- data.frame(time=linkedyearsum$time,
											Year= linkedyearsum$strata,
											yearcount = linkedyearsum$n.event,
											n = sum(linkedyearsur$n),
											nlinked = sum(linkedyearsur$n.event))

linkedyear <- linkedyear %>% group_by(Year) %>%	mutate(probpop = yearcount/n,
																									problinked = yearcount/nlinked,
																									cumprobpop = cumsum(probpop),
																									cumproblinked = cumsum(problinked),
																									followUp = time/year)

ggplot(linkedyear, aes(followUp, cumprobpop, group=Year, colour=Year )) + geom_line() + 
	ggtitle('Cumulative Probability of Linked by Enrolment Year (POPULATION)') + xlab("Follow-up in Years") + ylab("Probability")

ggplot(linkedyear, aes(followUp, cumproblinked, group=Year, colour=Year )) + geom_line() + 
	ggtitle('Cumulative Probability of Linked by Enrolment Year (Linked)') + xlab("Follow-up in Years") + ylab("Probability")

# 
# testing <- merge(arv,arvyear, by="time")
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
# arvyearmod
