library(dplyr)

c_visits <- (c_visits
	%>% group_by(patientid) 
	%>% mutate(
		firstvisit=min(visitdate)
		, firstcd4date=min(c(visitdate[!is.na(cd4)], Inf))
	)
)
