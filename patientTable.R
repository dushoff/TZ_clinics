## We need to onfirm that table is sorted by both visit number and visit date!

library(dplyr)

endDate <- as.Date("2014-12-31")
year <- 365.25

##Date functions

minDate <- function(dates){
	if(length(dates)==0) return ((NA))
	return(min(dates))
}

maxDate <- function(dates){
  if(length(dates)==0) return ((NA))
  return(max(dates))
}

followUp <- function(dates){
  if(length(dates)==0)return((NA))
  return(max(dates) + year/2)
}

dateYear <- function(date){
  if(length(date)==0) return((NA))
  return(as.numeric(format(date,"%Y")))
}

first <- function(cat){
  return(cat[[1]])
}

##baseline functions

baseline <- function(cat){
	obs <- subset(cat,!is.na(cat))
	if(length(obs)==0) return(NA)
	return(obs[[1]])
}

delay <- function(cat, visits,logic=TRUE){
	obsl <- !is.na(cat) & logic
	if(sum(obsl)==0) return(NA)
	return(visits[obsl][[1]] - visits[[1]])
}

##summarise will not include the other columns in eligible.RData
##Now we should create a Datetable AND a Baselinetable and merge at the end by patientid 

patientdat <- eligible %>% group_by(patientid)

Dates <- (patientdat %>%
	summarise_each(funs(
			minDate, maxDate, followUp, LTFU_status = followUp < endDate
	) , visitdate)
)

Vars <- (patientdat %>%
	summarise_each(funs(first, baseline, 
		delay(., visitdate)
	))
)

StatusDelay <- (patientdat  %>%
	summarise_each(funs(
		eligible_status_delay = delay(eligible,
			., eligible==TRUE),
		arv_status_delay = delay(arvstatuscode,
			., arvstatuscode == "Start ARV")
	), visitdate)
)

patientTable <- (Dates
	%>% full_join(Vars)
	%>% full_join(StatusDelay)
)

# rdsave(patientTable)

