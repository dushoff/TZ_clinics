## Compare arv_ever to arvstatuscode_first!

###Survival Analysis
library(survival)
library(dplyr)
library(ggplot2)
#library(rms)

# Functions to extract survival objects ----

nstrata <- function(n,strata){
  ntemp <- numeric(sum(strata))
  temp <- numeric(length(strata)+1)
  for(i in 1:length(strata)){
    temp[i+1] <- strata[[i]]
  }
  temp <- cumsum(temp)
  for(i in 1:length(strata)){
    ntemp[(temp[i]+1):temp[i+1]] <- rep(n[i],strata[[i]])
  }
  return(ntemp)
}
catstrata <- function(strata){
  temp <- numeric(length(strata)+1)
  for(i in 1:length(strata)){
    temp[i+1] <- strata[[i]]
  }
  ntemp <- numeric(sum(strata))
  for(i in 1:length(strata)){
    ntemp[(sum(temp[1:i])+1) : sum(temp[1:(i+1)])] <- 
      rep(names(strata[i]),strata[i])
  }
  return(ntemp)
}

# calculating day difference via dplyr for survival objects ----

# Clean up minDate
survTable <- (patientTable %>% 
	mutate(e_diff= eligible_status_delay + 1
		, arv_ever = !is.na(arv_status_delay) #arv treatment at all 
		, arv_diff = arv_status_delay + 1
		, lastdate = ifelse(LTFU_status, followUp, endDate)
		, arvFollowTime = ifelse(
			arv_ever, arv_diff, lastdate - as.numeric(minDate) 
		)
		, followTime = lastdate-as.numeric(minDate) 
		, enrolTotal = endDate-as.numeric(minDate)
		, enrolYear = format(minDate, "%Y")
	)
)

##trying to figure out what survival is doing ----
total <- nrow(survTable)

## We want avoid using survival objects for now until we actually need the analysis (coxs ph)
## For now, only use it for collapsing data and extracting info back to dataframe 
