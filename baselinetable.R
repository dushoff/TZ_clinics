###summary table

baselinecat <- function(cat, logic){
  data.frame(Factor = cat,
        "2011" = nrow(subset(Datetable,logic & start_year == 2011)),
        "2012" = nrow(subset(Datetable,logic & start_year == 2012)),
        "2013" = nrow(subset(Datetable,logic & start_year == 2013)),
        "2014" = nrow(subset(Datetable,logic & start_year == 2014)),
        Total = nrow(subset(Datetable,logic)))
}

##still need facility type ... not sure about the code assignment

baseline_enrollment <- (rbind(
   baselinecat("Male",Datetable$sex == "Male")
  ,baselinecat("Female",Datetable$sex == "Female")
  ,baselinecat("AgeCat1 : < 1",Datetable$age < 1)
  ,baselinecat("AgeCat1 : 1-4",Datetable$age >= 1 & Datetable$age <= 4)
  ,baselinecat("AgeCat1 : 5-9",Datetable$age >= 5 & Datetable$age <= 9)
  ,baselinecat("AgeCat1 : 10-14",Datetable$age >= 10 & Datetable$age <= 14)
  ,baselinecat("AgeCat2 : < 2",Datetable$age < 2)
  ,baselinecat("AgeCat2 : 2-5",Datetable$age >= 2 & Datetable$age <= 5)
  ,baselinecat("AgeCat2 : 6-9",Datetable$age >= 6 & Datetable$age <= 9)
  ,baselinecat("AgeCat2 : 10-14",Datetable$age >= 10 & Datetable$age <= 14)
  ,baselinecat("WHO stage : 1",Datetable$base_whostage == 1)
  ,baselinecat("WHO stage : 2",Datetable$base_whostage == 2)
  ,baselinecat("WHO stage : 3",Datetable$base_whostage == 3)
  ,baselinecat("WHO stage : 4",Datetable$base_whostage == 4)
  ,baselinecat("WHO stage : missing",is.na(Datetable$base_whostage))
  ,baselinecat("CD4 cell count: <50",Datetable$base_cd4 < 50)
  ,baselinecat("CD4 cell count: 50-199",Datetable$base_cd4 >= 50 & Datetable$base_cd4 <=199)
  ,baselinecat("CD4 cell count: >=200",Datetable$base_cd4 >= 200)
  ,baselinecat("CD4 cell count: missing",is.na(Datetable$base_cd4))
))

ARTeligi <- function(cat,logic){
  data.frame(Factor = cat,
             "<2" = nrow(subset(Datetable,logic & Datetable$age < 1)),
             "2-5" = nrow(subset(Datetable,logic & Datetable$age >= 2 & Datetable$age <= 5)),
             "6-9" = nrow(subset(Datetable,logic & Datetable$age >= 6 & Datetable$age <= 9)),
             "10-14" = nrow(subset(Datetable,logic & Datetable$age >= 10 & Datetable$age <= 14)),
             Total = nrow(subset(Datetable,logic)))
  
}

ARTEligibility <- (rbind(
  ARTeligi("Eligible", Datetable$base_eligible == TRUE)
  ,ARTeligi("Not Eligible", Datetable$base_eligible == FALSE)
  ,ARTeligi("Unknown", is.na(Datetable$base_eligible))
  ,ARTeligi("Total", !is.na(Datetable$patientid))
))

ARTEligibility
  
###Last table use survival data ... 