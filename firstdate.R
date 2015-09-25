### Attach CD4 baseline and days since CD4 baseline for each patient
### Still under development

library(dplyr)

load("eligible.visits.RData") # No_R_pipe

cd4base <- function(cd4, visitdate){
	cd4v <- !is.na(cd4)
	if(sum(cd4v)==0){
		return(c(firstdate=NA, cd4delay=NA))
	} else {
		firstcd4date <- min(visitdate[cd4v])
		firstcd4 <- cd4[visitdate==firstcd4date]
		if (length(firstcd4) != 1)
			stop(paste("firstcd4 length is", length(firstcd4)))
		return(c(firstcd4=firstcd4
			, cd4delay= firstcd4date - min(visitdate)
		))
	}
}

c_patients <- summarise(group_by(c_visits, patientid) 
	, firstdate=min(visitdate)
	, lastdate=max(visitdate)
	, firstcd4 = cd4base(cd4, visitdate)[[1]]
	, cd4delay = cd4base(cd4, visitdate)[[2]]
)

summary(c_patients)

c_visits <- (c_visits
	%>% group_by(patientid) 
	%>% mutate(diffday = visitdate - min(visitdate))
	%>% filter(!is.na(cd4))
	%>% filter(diffday == min(diffday))
)

minDate <- function(dates){
	if(length(dates)==0) return ((NA))
	return(min(dates))
}

Datetable <- (summarise(group_by(eligible, patientid)
	, firstVisit = minDate(visitdate)
	, cd4_date = minDate(subset(visitdate,!is.na(cd4)))
	, eligible_date = minDate(subset(visitdate,
		!is.na(eligible) & eligible))
	, arv_date = minDate(subset(visitdate,
		arvstatuscode == "Start ARV"  & !is.na(arvstatuscode)))
))

