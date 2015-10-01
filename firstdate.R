### Attach CD4 baseline and days since CD4 baseline for each patient
### Still under development

library(dplyr)

minDate <- function(dates){
	if(length(dates)==0) return ((NA))
	return(min(dates))
}

##summarise will not include the other columns in eligible.RData

Datetable <- (summarise(group_by(eligible, patientid)
	, firstVisit = minDate(visitdate)
	, cd4_date = minDate(subset(visitdate,!is.na(cd4)))
	, eligible_date = minDate(subset(visitdate,
		!is.na(eligible) & eligible))
	, arv_date = minDate(subset(visitdate,
		arvstatuscode == "Start ARV"  & !is.na(arvstatuscode)))
))

