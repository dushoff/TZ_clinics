library(dplyr)

c_visits <- (c_visits
	%>% group_by(patientid) 
	%>% mutate(diffday = visitdate - min(visitdate))
	%>% filter(!is.na(cd4))
	%>% filter(diffday == min(diffday))
)

pat0 <- "01-01-0100-001791"
pat1 <- "01-01-0100-001890"

print(as.data.frame(subset(c_visits, 
	patientid==pat0
	| patientid==pat1
)))
