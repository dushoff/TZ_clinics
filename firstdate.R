endDate <- as.Date("2014-12-31")
year <- 365.25

library(dplyr)

minDate <- function(dates){
	if(length(dates)==0) return ((NA))
	return(min(dates))
}

maxDate <- function(dates){
  if(length(dates)==0) return ((NA))
  return(max(dates))
}

baseline <- function(cat){
	obs <- subset(cat,!is.na(cat))
	if(length(obs)==0) return(NA)
	return(obs[[1]])
}

baseline_delay <- function(cat, visits){
	obsl <- !is.na(cat)
	if(length(obsl)==0) return(NA)
	return(visits[obsl][[1]] - visits[[1]])
}

first <- function(cat){
	return(cat[[1]])
}

followUp <- function(dates){
  if(length(dates)==0)return((NA))
  return(max(dates) + year/2)
}

dateYear <- function(date){
  if(length(date)==0) return((NA))
  return(as.numeric(format(date,"%Y")))
}

##summarise will not include the other columns in eligible.RData
##combining baselines 

Datetable <- (summarise(group_by(eligible, patientid)
	, first_age = first(age)
	, sex = baseline(sex)
	, firstVisit = minDate(visitdate)
	, lastVisit = maxDate(visitdate)
	, FUDate = followUp(visitdate)
	, LTFU_status = FUDate < endDate
	, death_date = minDate(dateofdeath)
	, death = baseline(death)
	, cd4_date = as.Date(minDate(subset(visitdate, !is.na(cd4))))
	, eligible_date = minDate(subset(visitdate,
		!is.na(eligible) & eligible))
	, arv_date = minDate(subset(visitdate,
		arvstatuscode == "Start ARV"  & !is.na(arvstatuscode)))
	, base_cd4 = baseline(subset(cd4,!is.na(cd4))), sex_delay=baseline_delay(sex, visitdate)
	, base_whostage = baseline(whostage)
	, base_eligible = eligible[1] ## First eligibility status
	, base_facility = baseline(hf_type)
	, base_referred = baseline(referredfromid)
	, base_TB = baseline(tbscreeningid)
	, base_Cotrimoxazole = baseline(medication2)
	, base_Malnourshed = baseline(nutritionalstatusid)
) %>% mutate(start_year = dateYear(firstVisit)
 , cd4_year = dateYear(cd4_date)
 , eligible_year = dateYear(eligible_date)
 , arv_year = dateYear(arv_date)
))

