library(dplyr)

### Eligible or not 

## Better to put the data here. Also the standard name is children_visits.RData; you can copy it from the wiki
## Please add No_R_pipe when you're doing stuff the changing directories or loading files (even local files should be loaded by make in the pipeline)
load("~/tz_pediatric_hiv/c_visits.RData") ## No_R_pipe

eligible <- (c_visits  %>%  rowwise()
	%>% mutate(eligible =
                (visitdate > as.Date("2011-12-31") &        
                    (age<2
                    | (((cd4percent<25 & !is.na(cd4percent)) | (cd4<750 & !is.na(cd4)))
			    & ((age>1) & (age<5)))
		    | (whostage > 2)
                    | (cd4<351)))
 
                |(visitdate < as.Date("2012-01-01") &
                    (age<2
                    |(as.numeric(visitdate - dateofbirth)<548  &
                     ((cd4percent<20)))
                    |((as.numeric(visitdate - dateofbirth)>547 & (age < 3)) &
                     (((cd4percent<20 & !is.na(cd4percent)) | (cd4<750 & !is.na(cd4)))))
                    |(((age>2) & (age < 5)) &
                     (((cd4percent<20 & !is.na(cd4percent)) | (cd4<350 & !is.na(cd4)))))
                    |((age > 4) &
                     (((cd4percent<15 & !is.na(cd4percent)) | (cd4<200 & !is.na(cd4)))))
		    | (whostage > 2))
)))
                   

print(eligible %>%
	select(c(patientid,age,visitdate,cd4,cd4percent,whostage,eligible))
)


test <- (eligible %>%
	select(c(patientid,age,visitdate,cd4,cd4percent,whostage,eligible))

)      

a <- sample(1:length(test$patientid),10)

print(test[a,])

### bad test cases .. when we have " logical" or "na" it will be NA, we want logical instead  

bad <- "02-02-0101-000241"  ### problem (I think it is a NA logical problem)

print(subset(test,test$patientid == bad))

print(test %>% filter(!is.na(cd4percent)) %>% filter((age > 2) & (age < 5)) %>% filter(cd4percent<25))
