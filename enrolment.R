### Attach CD4 baseline and days since CD4 baseline for each patient
### Still under development

library(dplyr)

load("~/tz_pediatric_hiv/keep.sample.RData") # No_R_pipe

# Pick a patient (currently we're looking for one with a baseline reading from not the first visit")
pat0 <- "01-01-0100-001791"
pat1 <- "01-01-0100-001890"

# Get the record for pat0

c0 <- subset(c_visits, patientid == pat0)
c1 <- subset(c_visits, patientid == pat1)

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

# Joseph: can you figure out a better way to do this in dplyr (call cd4base only once)?

c_patients <- summarise(group_by(c_visits, patientid) 
	, firstdate=min(visitdate)
	, lastdate=max(visitdate)
	, firstcd4 = cd4base(cd4, visitdate)[[1]]
	, cd4delay = cd4base(cd4, visitdate)[[2]]
)

summary(c_patients)

### Mike's new dplyr code

newdday <- function(dat){
  newdat <- dat %>% group_by(patientid) %>% 
    mutate(diffday = visitdate - min(visitdate)) %>% 
    filter(!is.na(cd4)) %>% filter(diffday == min(diffday))
}



