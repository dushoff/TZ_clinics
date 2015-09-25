library(dplyr)

### Eligible or not 

## Please add No_R_pipe when you're doing stuff the changing directories or loading files (even local files should be loaded by make in the pipeline)!!
## Also, why not use a better data file, like keep.visits or keep.sample?
load("~/tz_pediatric_hiv/c_visits.RData") ## No_R_pipe

combineInfo <- function(test1, test2){
	if(!is.na(test1)) return(test1)
	return(test2)
}

year <- 365.25

c_visits <- (c_visits
	%>% mutate(ageDays=as.numeric(visitdate - dateofbirth))
)

# We can do one-way age comparisons because criteria are nested
# (we are always more likely to treat younger children)
eligible <- (c_visits 
	%>%  rowwise()
	%>% mutate(eligible =
		visitdate < as.Date("2012-01-01") &(
			whostage >=3
			| ageDays <= 365
			| (ageDays <= round(1.5*year) & cd4percent<20)
			| (ageDays <= round(3*year) &
				combineInfo(cd4percent<20, cd4<750))
			| (ageDays <= round(5*year) &
				combineInfo(cd4percent<20, cd4<350))
			| combineInfo(cd4percent<15, cd4<200)
		)
		| (visitdate >= as.Date("2012-01-01")
			& (visitdate < as.Date("2015-04-01")) & (
				whostage >= 3
				| ageDays <= 365
				| (ageDays <= round(5*year) &
						 combineInfo(cd4percent<25,cd4<750))
				| cd4<350
			)        
		)
	)
	%>% ungroup 
)

print(eligible %>%
	select(c(patientid,age,visitdate,cd4,cd4percent,whostage,eligible))
)

test <- (eligible %>%
	select(c(patientid,age,visitdate,cd4,cd4percent,whostage,eligible))
)      

a <- sample(1:length(test$patientid),20)

print(test[a,])
