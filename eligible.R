library(dplyr)

### Eligible or not 
### binary check for each visit 
### 0.5 if we need multiple conditions
### sum of all binary checks greater than 0.5 implies Eligible
load("~/tz_pediatric_hiv/c_visits.RData") ## diddo using data from a different repo 

eligible <- c_visits  %>% select(c(cd4,patientid,age,cd4percent,whostage)) %>% rowwise() %>% mutate(cd4test = ifelse((cd4<351 & !is.na(cd4)),1,0),
                          cd4percenttest = ifelse(cd4percent<25 & !is.na(cd4percent),0.5,0),
                          cd4test2 = ifelse(cd4<750 & !is.na(cd4) ,0.5,0),
                          agetest1 = ifelse(age<2 & !is.na(age) ,1,0),
                          agetest2 = ifelse((age>1) & (age<5) & !is.na(age) , 0.5,0),
                          whtest1 = ifelse(whostage > 2 & !is.na(whostage) , 1,0),
                          eligible = ifelse(sum(cd4test,cd4percenttest,cd4test2,agetest1,agetest2,whtest1)>0.5,1,0))

eligible2 <- c_visits  %>% select(c(cd4,patientid,age,cd4percent,whostage)) %>% rowwise() %>%
    mutate(eligible = ifelse((cd4<351 & !is.na(cd4)) |
                             ((cd4percent<25 & !is.na(cd4percent)) & ((age>1) & (age<5) & !is.na(age))) |
                             ((cd4<750 & !is.na(cd4)) & ((age>1) & (age<5) & !is.na(age))) |
                             (age<2 & !is.na(age)) |
                             (whostage > 2 & !is.na(whostage)),1,0))

all.equal(eligible$eligible,eligible2$eligible)
##ok the two vectors are not the same
##see where they are different
diff <- which(eligible$eligible != eligible2$eligible)
eligible[diff,]
eligible2[diff,]

## ok these 2 rows have BOTH CD4 and CD4percent ... I thought that is not true ?!?
## once we can confirm this, we can delete the first code


