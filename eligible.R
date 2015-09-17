library(dplyr)

### Eligible or not 
load("~/tz_pediatric_hiv/c_visits.RData") ## diddo using data from a different repo 

eligible <- c_visits  %>%  rowwise() %>%
    mutate(eligible = ifelse((cd4<351 & !is.na(cd4)) |
                             ((cd4percent<25 & !is.na(cd4percent)) & ((age>1) & (age<5) & !is.na(age))) |
                             ((cd4<750 & !is.na(cd4)) & ((age>1) & (age<5) & !is.na(age))) |
                             (age<2 & !is.na(age)) |
                             (whostage > 2 & !is.na(whostage)),1,0))

eligible %>% select(c(patientid,age,cd4,cd4percent,whostage,eligible))

## need to try with some test cases
