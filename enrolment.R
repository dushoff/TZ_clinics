### Attach CD4 baseline and days since CD4 baseline for each patient
### Still under development

library(dplyr)

load("~/tz_pediatric_hiv/c_visits.RData") # should we move this file to github repo??




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

### Mike's new dplyr code as modified by JD

c_visits <- (c_visits
	%>% group_by(patientid) 
	%>% mutate(diffday = visitdate - min(visitdate))
	%>% filter(!is.na(cd4))
	%>% filter(diffday == min(diffday))
)

##### Testing ######

pat0 <- "01-01-0100-001791"
pat1 <- "01-01-0100-001890"

print(as.data.frame(subset(c_visits, 
	patientid==pat0
	| patientid==pat1
)))

### Preliminary plots with the CD4baseline data
library(ggplot2)

ggplot(c_visits, aes(x=as.numeric(diffday))) + geom_density() +
    xlab('Day Lag') + ggtitle('CD4 Baseline Day Lag Density') + theme_bw()

ggplot(c_visits, aes(x=age)) + geom_bar() + ggtitle('CD4 Baseline Age Groups') +
    theme_bw()

ggplot(c_visits, aes(x=age, y=weight, group=age)) + geom_boxplot() +
    ggtitle('CD4 Baseline Weight of Each Age group') + theme_bw()

### Look into the weights for quality control.

### Need to establish Whostage Baseline



