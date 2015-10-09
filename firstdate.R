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

baseline_delay <- function(cat, visits,logic=NULL){
	obsl <- !is.na(cat)
  if(!is.null(logic))(obsl <- !is.na(cat) & logic)
	if(sum(obsl)==0) return(NA)
	return(visits[obsl][[1]] - visits[[1]])
}


baseline_delay <- function(cat, visits){
  obsl <- !is.na(cat)
  if(length(obsl)==0) return(NA)
  return(visits[obsl][[1]] - visits[[1]])
}


##summarise will not include the other columns in eligible.RData
##Now we should create a Datetable AND a Baselinetable and merge at the end by patientid 

patientdat <- eligible %>% group_by(patientid)

Date_table <- (patientdat %>%
                 summarise_each(funs(minDate,maxDate,followUp,
                                     LTFU_status = followUp < endDate),visitdate)
               )

First_table <- (patientdat %>%
                  summarise_each(funs(first), 
                                 c(age,sex,death,eligible,whostage)
                                 )
                )

Baseline_table <- (patientdat %>%
                     summarise_each(funs(baseline),
                      c(cd4,hf_type,referredfromid,tbscreeningid,nutritionalstatusid)
                     )
)
                
Baseline_delay <- patientdat  %>%
  summarise_each(funs(baseline_delay(eligible,visitdate,eligible==TRUE),
                      baseline_delay(cd4,visitdate,cd4>0),
                      baseline_delay(arvstatuscode, visitdate,arvstatuscode == "Start ARV"))
  )

