## Compare arv_ever to arvstatuscode_first!

###Survival Analysis
library(survival)
library(dplyr)
library(ggplot2)
#library(rms)

# calculating day difference via dplyr for survival objects ----

eliDate <- as.Date("2015-04-01")

catage1 <- function(age){
  if(age < floor(year)){return("0-1 years")}
  if(age %in% c(floor(year):floor(5*year)-1)){return("1-4 years")}
  if(age %in% c(floor(5*year):floor(10*year)-1)){return("5-9 years")}
  if(age >= floor(10*year)){return("10-14 years")}
}


catage2 <- function(age){
  if(age < floor(2*year)){return("0-1 year")}
  if(age %in% c(floor(2*year):(floor(6*year)-1))){return("2-5 years")}
  if(age %in% c(floor(6*year):(floor(10*year)-1))){return("6-9 years")}
  if(age >= floor(10*year)){return("10-14 years")}
}

catageE1 <- function(age){
  if(age < year){return("< 1 year")}
  if(age < 5*year){return("between 24-59 months")}
  if(age >= 5*year){return("above 5 years")}
}

catageE2 <- function(age){
  if(age < floor(year/2)){return("< 12 months")}
  if(age %in% c(floor(year/2):floor(1.5*year))){return("between 12-18 months")}
  if(age %in% c((floor(1.5*year)+1): floor(3*year))){return("between 19-36 months")}
  if(age %in% c((floor(3*year)+ 1):floor(5*year))){return("between 37-60 months")}
  if(age > floor(5*year)){return("above 5 years")}
}

# Clean up minDate
survTable <- (patientTable %>% 
  group_by(patientid) %>% 
	mutate(e_diff= eligible_status_delay2 + 1
	  , eligible_ever = !is.na(eligible_status_delay2)
	  , eligibleTime = ifelse(eligible_ever,e_diff,eliDate-as.numeric(minDate))
		, arv_ever = !is.na(arv_status_delay) #arv treatment at all 
		, arv_diff = arv_status_delay + 1
		, lastdate = ifelse(LTFU_status, followUp, endDate)
		, arvFollowTime = ifelse(
			arv_ever, arv_diff, lastdate - as.numeric(minDate) 
		)
		, followTime = lastdate-as.numeric(minDate) 
		, enrolTotal = endDate-as.numeric(minDate)
		, enrolYear = format(minDate, "%Y")
		, real_age = minDate-dateofbirth_first
		, agecatA = catage1(real_age)
		, agecatB = catage2(real_age)
 		, agecatEA = catageE1(real_age)
 		, agecatEB = catageE2(real_age)
		, death_ever = !is.na(dateofdeath_first)
		, death_delay = ifelse(death_ever,dateofdeath_first - minDate + 1,followTime)
	)
)
