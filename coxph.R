library(dplyr)
library(survival)

## Linked baseline coxph ----

LinkedFirstcph <- coxph(
  Surv(followTime, LTFU_status)~factor(sex_first)
  + factor(enrolYear)
  , data=survTable
)

print(summary(LinkedFirstcph))
print(anova(LinkedFirstcph))
print(drop1(LinkedFirstcph,test="Chisq"))


LinkedAgeA <- update(LinkedFirstcph,.~.+agecatA)
print(summary(LinkedAgeA))
print(anova(LinkedAgeA))
print(drop1(LinkedAgeA,test="Chisq"))


LinkedAgeB <- update(LinkedFirstcph,.~.+agecatB)
print(summary(LinkedAgeB))
print(anova(LinkedAgeB))
print(drop1(LinkedAgeB,test="Chisq"))

##note: HF and cd4 will have missing values

## Linked BL + HF ----
survTableHF <- survTable %>% filter(!is.na(hf_type_first))

LinkedAgeAHF <- update(LinkedAgeA,.~.+hf_type_first,data=survTableHF)
LinkedAgeBHF <- update(LinkedAgeB,.~.+hf_type_first,data=survTableHF)

print(summary(LinkedAgeAHF))
print(anova(LinkedAgeAHF))
print(drop1(LinkedAgeAHF,test = "Chisq"))


print(summary(LinkedAgeBHF))
print(anova(LinkedAgeBHF))
print(drop1(LinkedAgeBHF,test = "Chisq"))

## Linked cd4 (note:,adding cd4 will remove kids with no cd4 data)----
survTableCD4 <- survTable %>% 
  filter(!is.na(hf_type_first)) %>%  
  filter(!is.na(cd4_baseline))

LinkedAgeAHFcd4 <- update(LinkedAgeAHF,.~.+cd4_baseline,data=survTableCD4)
LinkedAgeBHFcd4 <- update(LinkedAgeBHF,.~.+cd4_baseline,data=survTableCD4)


print(summary(LinkedAgeAHFcd4))
print(anova(LinkedAgeAHFcd4))
print(drop1(LinkedAgeAHFcd4,test = "Chisq"))

print(summary(LinkedAgeBHFcd4))
print(anova(LinkedAgeBHFcd4))
print(drop1(LinkedAgeBHFcd4,test = "Chisq"))

##ART 

ARTFirstcph <- coxph(
  Surv(arvFollowTime, arv_ever)~factor(sex_first)
  + factor(enrolYear)
  , data=survTable
)

print(summary(ARTFirstcph))
print(anova(ARTFirstcph))
print(drop1(ARTFirstcph,test="Chisq"))

ARTAgeA <- update(ARTFirstcph,.~.+agecatA)
print(summary(ARTAgeA))
print(anova(ARTAgeA))
print(drop1(ARTAgeA,test="Chisq"))


ARTAgeB <- update(ARTFirstcph,.~.+agecatB)
print(summary(ARTAgeB))
print(anova(ARTAgeB))
print(drop1(ARTAgeB,test="Chisq"))


## ART BL + HF ----
survTableHF <- survTable %>% filter(!is.na(hf_type_first))

ARTAgeAHF <- update(ARTAgeA,.~.+hf_type_first,data=survTableHF)
ARTAgeBHF <- update(ARTAgeB,.~.+hf_type_first,data=survTableHF)

print(summary(ARTAgeAHF))
print(anova(ARTAgeAHF))
print(drop1(ARTAgeAHF,test = "Chisq"))


print(summary(ARTAgeBHF))
print(anova(ARTAgeBHF))
print(drop1(ARTAgeBHF,test = "Chisq"))

## ART cd4 (note:,adding cd4 will remove kids with no cd4 data)----
survTableCD4 <- survTable %>% 
  filter(!is.na(hf_type_first)) %>%  
  filter(!is.na(cd4_baseline))

ARTAgeAHFcd4 <- update(ARTAgeAHF,.~.+cd4_baseline,data=survTableCD4)
ARTAgeBHFcd4 <- update(ARTAgeBHF,.~.+cd4_baseline,data=survTableCD4)


print(summary(ARTAgeAHFcd4))
print(anova(ARTAgeAHFcd4))
print(drop1(ARTAgeAHFcd4,test = "Chisq"))

print(summary(ARTAgeBHFcd4))
print(anova(ARTAgeBHFcd4))
print(drop1(ARTAgeBHFcd4,test = "Chisq"))

# ## coxph competing risk ---- 
# 
# DL_status <- with(survTable,ifelse(death_ever,2*as.numeric(death_ever),as.numeric(LTFU_status)))
# DL_delay <- with(survTable, ifelse(death_ever,death_delay,followTime))
# DL_status <- factor(DL_status,0:2,labels=c("censor","LTFU","death"))
# 
# survTable$DL_status <- DL_status
# survTable$DL_delay <- DL_delay
# 
# LDCRcph <- coxph(Surv(DL_delay,DL_status)~factor(sex_first)
#                  + factor(enrolYear)
#                  , data=survTable
# )
