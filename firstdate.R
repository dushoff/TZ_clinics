### Attach CD4 baseline and days since CD4 baseline for each patient
### Still under development

library(dplyr)

minDate <- function(dates){
	if(length(dates)==0) return ((NA))
	return(min(dates))
}

baseline <- function(cat){
  if(length(subset(cat,!is.na(cat))==0) return((NA))
  return(subset(cat,!is.na(cat))$[1])
}

##summarise will not include the other columns in eligible.RData
##combining baselines 

Datetable <- (summarise(group_by(eligible, patientid)
  , age = baseline(age)
  , sex = baseline(sex)
	, firstVisit = minDate(visitdate)
	, cd4_date = minDate(subset(visitdate,!is.na(cd4)))
	, eligible_date = minDate(subset(visitdate,
		!is.na(eligible) & eligible))
	, arv_date = minDate(subset(visitdate,
		arvstatuscode == "Start ARV"  & !is.na(arvstatuscode)))
	, base_cd4 = baseline(subset(cd4,!is.na(cd4)))
  , base_whostage = baseline(whostage)
  , base_facility = baseline(hf_type)
  , base_referred = baseline(referredfromid)
))

