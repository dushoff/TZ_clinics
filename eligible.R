library(dplyr)

### Eligible or not 

## Better to put the data here. Also the standard name is children_visits.RData; you can copy it from the wiki
## Please add No_R_pipe when you're doing stuff the changing directories or loading files (even local files should be loaded by make in the pipeline)
load("~/tz_pediatric_hiv/c_visits.RData") ## No_R_pipe

eligible <- (c_visits  %>%  rowwise()
	%>% mutate(eligible =
                (visitdate > as.Date("2011-12-31") &        
                    (age<2 & !is.na(age)
                    | (((cd4percent<25 | cd4<750) & !is.na(cd4percent))
			    & ((age>1) & (age<5) & !is.na(age)))
		    | (whostage > 2 & !is.na(whostage))
                    | (cd4<351 & !is.na(cd4)))) 
 
                |(visitdate < as.Date("2012-01-01") &
                    (age<2 & !is.na(age)
                    |(as.numeric(visitdate - dateofbirth)<548  &
                     ((cd4percent<20 | cd4<750) & !is.na(cd4percent)))
                    |((as.numeric(visitdate - dateofbirth)>547 & (age < 3)) &
                     ((cd4percent<20 | cd4<351) & !is.na(cd4percent)))
                    |((age > 4) &
                     ((cd4percent<15 | cd4<200) & !is.na(cd4percent)))
		    | (whostage > 2 & !is.na(whostage)))
)))
                   

print(eligible %>%
	select(c(patientid,age,visitdate,cd4,cd4percent,whostage,eligible))
)


test <- (eligible %>%
	select(c(patientid,age,visitdate,cd4,cd4percent,whostage,eligible))

)      

a <- sample(1:10000,10)

print(test[a,])


sample(unique(eligible$patientid),1)
## need to try with some test cases
