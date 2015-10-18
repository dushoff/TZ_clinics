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

statusYear <- function(visits,delay){
  return(as.numeric(format(min(visits)+delay,"%Y")))
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

delay_first <- function(cat, visits,logic=TRUE){
	obsl <- !is.na(cat) & logic
	if(sum(obsl)==0) return(NA)
	return(visits[obsl][[1]] - visits[[1]])
}

delay_birth <- function(cat, visits,birth,logic=TRUE){
	obsl <- !is.na(cat) & logic
	if(sum(obsl)==0) return(NA)
	return(visits[obsl][[1]] - birth[[1]])
}


patientdat <- eligible %>% group_by(patientid)

Dates <- (patientdat %>%
	summarise_each(funs(
			minDate, maxDate, followUp, LTFU_status = followUp < endDate
	) , visitdate)
)

Vars <- (patientdat %>%
	summarise_each(funs(first, baseline, 
		delay_first(., visitdate),
                delay_birth(.,visitdate,dateofbirth)
	))
)

StatusDelay <- (patientdat  %>%
	summarise_each(funs(
		eligible_status_delay = delay_first(eligible,
			., eligible==TRUE),
		eligible_status_year = statusYear(.,eligible_status_delay),
		arv_status_delay = delay_first(arvstatuscode,
			., arvstatuscode == "Start ARV"),
		arv_status_year = statusYear(.,arv_status_delay),
    eligible_birth_delay = delay_birth(eligible,
      .,dateofbirth, eligible==TRUE),
    arv_birth_delay = delay_birth(arvstatuscode,
      .,dateofbirth, arvstatuscode == "Start ARV")
	), visitdate)
)

patientTable <- (Dates
	%>% full_join(Vars)
	%>% full_join(StatusDelay)
)

# rdsave(patientTable, endDate, year)

