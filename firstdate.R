### Attach CD4 baseline and days since CD4 baseline for each patient
### Still under development

library(dplyr)

minDate <- function(dates){
	if(length(dates)==0) return ((NA))
	return(min(dates))
}

baseline <- function(cat){
  if(length(subset(cat,!is.na(cat)))==0) return((NA))
  return(subset(cat, !is.na(cat))[1])
}

maxDate <- function(dates){
  if(length(dates)==0)return((NA))
  return(min(max(dates)+182.625,as.Date("2014-12-31")))
}

year <- function(date){
  if(length(date)==0) return((NA))
  return(as.numeric(format(date,"%Y")))
}

eligible$MED <- unclass(eligible$medication2)


##summarise will not include the other columns in eligible.RData
##combining baselines 

Datetable <- (summarise(group_by(eligible, patientid)
  , age = baseline(age)
  , sex = baseline(sex)
	, firstVisit = minDate(visitdate)
  , LTFU_date = maxDate(visitdate)
  , death_date = minDate(dateofdeath)
  , death = baseline(death)
	, cd4_date = as.Date(minDate(subset(visitdate, !is.na(cd4))))
	, eligible_date = minDate(subset(visitdate,
		!is.na(eligible) & eligible))
	, arv_date = minDate(subset(visitdate,
		arvstatuscode == "Start ARV"  & !is.na(arvstatuscode)))
	, base_cd4 = baseline(subset(cd4,!is.na(cd4)))
  , base_whostage = baseline(whostage)
  , base_eligible = eligible[1]
  , base_facility = baseline(hf_type)
  , base_referred = baseline(referredfromid)
  , base_TB = baseline(tbscreeningid)
  , base_Cotrimoxazole = baseline(medication2)
  , base_Malnourshed = baseline(nutritionalstatusid)
  ) %>% mutate(start_year = year(firstVisit)
    , cd4_year = year(cd4_date)
    , eligible_year = year(eligible_date)
    , arv_year = year(arv_date)
))

