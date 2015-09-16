library(dplyr)

### Eligible or not 
### binary check for each visit 
### 0.5 if we need multiple conditions
### sum of all binary checks greater than 0.5 implies Eligible

eligible <- c_visits %>% select(c(cd4,patientid,age,cd4percent,whostage)) %>% rowwise() %>% mutate(cd4test = ifelse((cd4<351 & !is.na(cd4)),1,0),
                          cd4percenttest = ifelse(cd4percent<25 & !is.na(cd4percent),0.5,0),
                          cd4test2 = ifelse(cd4<750 & !is.na(cd4) ,0.5,0),
                          agetest1 = ifelse(age<2 & !is.na(age) ,1,0),
                          agetest2 = ifelse((age>1) & (age<5) & !is.na(age) , 0.5,0),
                          whtest1 = ifelse(whostage > 2 & !is.na(whostage) , 1,0),
                          eligible = ifelse(sum(cd4test,cd4percenttest,cd4test2,agetest1,agetest2,whtest1)>0.5,1,0))


