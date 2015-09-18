library(dplyr)

### Eligible or not 

## Better to put the data here. Also the standard name is children_visits.RData; you can copy it from the wiki
## Please add No_R_pipe when you're doing stuff the changing directories or loading files (even local files should be loaded by make in the pipeline)
load("~/tz_pediatric_hiv/c_visits.RData") ## No_R_pipe

eligible <- (c_visits  %>%  rowwise()
	%>% mutate(eligible = 
		(cd4<351 & !is.na(cd4))
		| (((cd4percent<25 | cd4<750) & !is.na(cd4percent))
			& ((age>1) & (age<5) & !is.na(age)))
		| (age<2 & !is.na(age))
		| (whostage > 2 & !is.na(whostage))
	)
)

print(eligible %>%
	select(c(patientid,age,cd4,cd4percent,whostage,eligible))
)

## need to try with some test cases
