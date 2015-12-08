## Compare arv_ever to arvstatuscode_first!

###Survival Analysis
library(survival)
library(dplyr)
library(ggplot2)

# Functions to extract survival objects ----

nstrata <- function(n,strata){
  ntemp <- numeric(sum(strata))
  temp <- numeric(length(strata)+1)
  for(i in 1:length(strata)){
    temp[i+1] <- strata[[i]]
  }
  temp <- cumsum(temp)
  for(i in 1:length(strata)){
    ntemp[(temp[i]+1):temp[i+1]] <- rep(n[i],strata[[i]])
  }
  return(ntemp)
}
catstrata <- function(strata){
  temp <- numeric(length(strata)+1)
  for(i in 1:length(strata)){
    temp[i+1] <- strata[[i]]
  }
  ntemp <- numeric(sum(strata))
  for(i in 1:length(strata)){
    ntemp[(sum(temp[1:i])+1) : sum(temp[1:(i+1)])] <- 
      rep(names(strata[i]),strata[i])
  }
  return(ntemp)
}

# calculating day difference via dplyr for survival objects ----

# Clean up minDate
survTable <- (patientTable %>% 
	mutate(e_diff= eligible_status_delay + 1
		, arv_ever = !is.na(arv_status_delay) #arv treatment at all 
		, arv_diff = arv_status_delay + 1
		, lastdate = ifelse(LTFU_status, followUp, endDate)
		, arvFollowTime = ifelse(
			arv_ever, arv_diff, lastdate - as.numeric(minDate) #arv diff or lastdate if no arv
		)
		, followTime = lastdate-as.numeric(minDate) 
		, enrolTotal = endDate-as.numeric(minDate)
		, enrolYear = format(minDate, "%Y")
	)
)

##trying to figure out what survival is doing ----
total <- nrow(survTable)

## We want avoid using survival objects for now until we actually need the analysis (coxs ph)
## For now, only use it for collapsing data and extracting info back to dataframe 

# ARV treatment (Yes or No) ----
arvSurv <- survfit(Surv(
	arvFollowTime, arv_ever) ~ 1
	, data=survTable
)

print(plot(arvSurv
	, mark.time=TRUE
	, main='"Survival" until ART'
	, xlab='Time since enrolment (days)')
)

# I like mark.time because the censored patient is used in the 
# KM survival calcuation 

arv <- data.frame(
	time=arvSurv$time, 
	nrisk = arvSurv$n.risk, 
	events <- arvSurv$n.event
)

## S(t) = PRODUCT ( (#risk - #event) / #risk )
## That was why the numbers on my ggplot did not add up SORRY!!

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


print(ggplot(linked, aes(followUp, cumprob))
	+ xlab("Follow-up years")
	+ ylab("Survival Probaiblity")
	+ geom_line()
	+ ggtitle('Survival while linked to care')
	+ ylim(c(0, 1))
	+ theme_bw()
)


### By enrolYear need to look at summary strata ARV(Yes or NO) -----

arvyearsur <- update(arvSurv, .~enrolYear)
#plot(arvyearsur, mark.time=FALSE)

arvyear <- with(arvyearsur,data.frame(
  time=time, 
  nrisk = nstrata(n.risk,strata) , 
  yr = catstrata(strata),
  arvcount= n.event
))

arvyear <- arvyear %>% 
  group_by(yr) %>%  
    mutate(
      surv = cumprod((nrisk-arvcount)/nrisk),
      cumprob = 1-surv,
      followUp = time/year
)
yr <- unique(arvyear$yr)
print(plot(arvyearsur,mark.time = FALSE,main='Survival by Year: Getting ARV and delay',xlab="day lag",col=yr))

print(ggplot(arvyear, aes(followUp,cumprob,col=factor(yr),group=yr))
      + geom_line() 
      + ggtitle('Cumulative Probability of getting ART (POPULATION) by Year')
)

# still in the program (coming to check up, by year) ----

linkedyearsur <- update(laSurv,.~enrolYear)
#plot(linkedyearsur, mark.time=FALSE)


linkedyear <- with(linkedyearsur,data.frame(
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

plot(linkedyearsur, mark.time=FALSE,main="Survival Linked by Year: Enrolling in program (getting check up)",col=yr)


print(ggplot(linkedyear, aes(followUp,cumprob,col=factor(yr),group=yr))
      + geom_line() 
      + ggtitle('Cumulative Probability of population still in the program by Year')
)

# stuff we didn't do yet ------
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
