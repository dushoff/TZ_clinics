## Compare arv_ever to arvstatuscode_first!

###Survival Analysis
library(survival)
library(dplyr)
library(ggplot2)

# Helper functions ----

eliDate <- as.Date("2015-04-01")

catage1 <- function(age){
  if(age < floor(year)){return("0-1 years")}
  if(age %in% c(floor(year):floor(5*year)-1)){return("1-4 years")}
  if(age %in% c(floor(5*year):floor(10*year)-1)){return("4-9 years")}
  if(age >= floor(10*year)){return("Greater than 9")}
}


catage2 <- function(age){
  if(age < floor(2*year)){return("0-2 year")}
  if(age %in% c(floor(2*year):(floor(6*year)-1))){return("2-6 years")}
  if(age %in% c(floor(6*year):(floor(10*year)-1))){return("6-9 years")}
  if(age >= floor(10*year)){return("Greater than 9")}
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
	mutate(lastdate = ifelse(LTFU_status, followUp, endDate)
	  , followTime = lastdate-as.numeric(minDate) 
	  , death_ever = !is.na(dateofdeath_first)
	  , death_delay = ifelse(death_ever,(dateofdeath_first - minDate + 1)/year,followTime/year)
	  , LA_status = ifelse(death_ever,TRUE,LTFU_status)
	  , LA_delay = ifelse(LA_status,death_delay,(endDate-minDate)/year)
	  , arv_ever = !is.na(arv_status_delay) #arv treatment at all 
	  , arv_diff = arv_status_delay + 1
	  , arv_delay = ifelse(arv_ever, arv_diff/year, (endDate-minDate)/year)
	  , e_diff= eligible_status_delay2 + 1
	  , eligible_ever = !is.na(eligible_status_delay2)
	  , eligibleTime = ifelse(eligible_ever,e_diff,eliDate-as.numeric(minDate))
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
		, lost_status = ifelse(death_ever,FALSE,LTFU_status)
		, lost_delay = ifelse(death_ever,death_delay, followTime)
		, No_ART_delay = ifelse(death_ever, death_delay, endDate-minDate)
		, ART_delay = ifelse(arv_ever,arv_diff,No_ART_delay)
	)
)
