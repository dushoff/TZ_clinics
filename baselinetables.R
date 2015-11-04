###summary table

baselinecat <- function(cat, logic){
  data.frame(Factor = cat,
        "Enrolled_2011" = nrow(subset(patientTable,logic & enrolled_year == 2011)),
        "Enrolled_2012" = nrow(subset(patientTable,logic & enrolled_year == 2012)),
        "Enrolled_2013" = nrow(subset(patientTable,logic & enrolled_year == 2013)),
        "Enrolled_2014" = nrow(subset(patientTable,logic & enrolled_year == 2014)),
        Total = nrow(subset(patientTable,logic)))
}

##still need facility type ... not sure about the code assignment

baseline_enrollment <- (rbind(
   baselinecat("Male",patientTable$sex_first == "Male")
  ,baselinecat("Female",patientTable$sex_first == "Female")
  ,baselinecat("AgeCat1 : < 1",patientTable$age_first < 1)
  ,baselinecat("AgeCat1 : 1-4",patientTable$age_first >= 1 & patientTable$age_first <= 4)
  ,baselinecat("AgeCat1 : 5-9",patientTable$age_first >= 5 & patientTable$age_first <= 9)
  ,baselinecat("AgeCat1 : 10-14",patientTable$age_first >= 10 & patientTable$age_first <= 14)
  ,baselinecat("AgeCat2 : < 2",patientTable$age_first < 2)
  ,baselinecat("AgeCat2 : 2-5",patientTable$age_first >= 2 & patientTable$age_first <= 5)
  ,baselinecat("AgeCat2 : 6-9",patientTable$age_first >= 6 & patientTable$age_first <= 9)
  ,baselinecat("AgeCat2 : 10-14",patientTable$age_first >= 10 & patientTable$age_first <= 14)
  ,baselinecat("WHO stage : 1",patientTable$whostage_first == 1)
  ,baselinecat("WHO stage : 2",patientTable$whostage_first == 2)
  ,baselinecat("WHO stage : 3",patientTable$whostage_first == 3)
  ,baselinecat("WHO stage : 4",patientTable$whostage_first == 4)
  ,baselinecat("WHO stage : missing",is.na(patientTable$whostage_first))
  ,baselinecat("CD4 cell count: <50",patientTable$cd4_first < 50)
  ,baselinecat("CD4 cell count: 50-199",patientTable$cd4_first >= 50 & patientTable$cd4_first <=199)
  ,baselinecat("CD4 cell count: >=200",patientTable$cd4_first >= 200)
  ,baselinecat("CD4 cell count: missing",is.na(patientTable$cd4_first))
  ,baselinecat("Facility Type : Dispensary", patientTable$hf_type_first == "Dispensary")
  ,baselinecat("Facility Type : Health Centre", patientTable$hf_type_first == "Health Centre")
  ,baselinecat("Facility Type : Hospital", patientTable$hf_type_first == "Hospital")
  ,baselinecat("Facility Type : Other", patientTable$hf_type_first == "Other")
  ,baselinecat("Facility Type : Missing", is.na(patientTable$hf_type_first))
  ,baselinecat("Referred From : HBC", patientTable$referredfromid_first == "HBC")
  ,baselinecat("Referred From : PITC", patientTable$referredfromid_first == "PITC")
  ,baselinecat("Referred From : PMTCT", patientTable$referredfromid_first == "PMTCT")
  ,baselinecat("Referred From : VCT", patientTable$referredfromid_first == "VCT")
  ,baselinecat("Referred From : Missing", is.na(patientTable$referredfromid_first))
  ,baselinecat("Positive for TB : Yes ",patientTable$tbscreeningid_first == "CXR+" |
                                        patientTable$tbscreeningid_first == "TB Susp" |
                                        patientTable$tbscreeningid_first == "SS+" |
                                        patientTable$tbscreeningid_first == "Conf/Yes" |
                                        patientTable$tbscreeningid_first == "TB SUSP")
  ,baselinecat("Positive for TB : No ", patientTable$tbscreeningid_first == "CXR-" |
                                        patientTable$tbscreeningid_first == "SCR -ve" |
                                        patientTable$tbscreeningid_first == "SS-")
  ,baselinecat("Positive for TB : Missing", patientTable$tbscreeningid_first == "")
  ,baselinecat("Malnourished : No", patientTable$nutritionalstatusid_first == "OK")
  ,baselinecat("Malnourished : Mild", patientTable$nutritionalstatusid_first == "MLD")
  ,baselinecat("Malnourished : Moderate", patientTable$nutritionalstatusid_first == "MOD")
  ,baselinecat("Malnourished : Severe", patientTable$nutritionalstatusid_first == "SEV")
  ,baselinecat("Malnourished : Missing", patientTable$nutritionalstatusid_first == "")
))

print(baseline_enrollment)
write.csv(baseline_enrollment,file="first_enrollment.csv")

per <- function(x){
  r <- nrow(x)
  c <- ncol(x)
  for(j in 1:c){
    tempsum <- sum(x[,j])
    for(i in 1:r){
      x[i,j] <- x[i,j]/tempsum
    }
  }
  return(x)
}

percentagetable <- baseline_enrollment
percentagetable[1:2,2:6] <- per(baseline_enrollment[1:2,2:6])
percentagetable[3:6,2:6] <- per(baseline_enrollment[3:6,2:6])
percentagetable[7:10,2:6] <- per(baseline_enrollment[7:10,2:6])
percentagetable[11:15,2:6] <- per(baseline_enrollment[11:15,2:6])
percentagetable[16:19,2:6] <- per(baseline_enrollment[16:19,2:6])
percentagetable[20:24,2:6] <- per(baseline_enrollment[20:24,2:6])
percentagetable[25:29,2:6] <- per(baseline_enrollment[25:29,2:6])
percentagetable[30:32,2:6] <- per(baseline_enrollment[30:32,2:6])
percentagetable[33:37,2:6] <- per(baseline_enrollment[33:37,2:6])

print(percentagetable)
write.csv(percentagetable,file="first_enrollment_percentage.csv")

ARTeligi <- function(cat,logic){
  data.frame(Factor = cat,
             "age<2" = nrow(subset(patientTable,logic & patientTable$age_first < 1)),
             "age between 2-5" = nrow(subset(patientTable,logic & patientTable$age_first >= 2 & patientTable$age_first <= 5)),
             "age between 6-9" = nrow(subset(patientTable,logic & patientTable$age_first >= 6 & patientTable$age_first <= 9)),
             "age between 10-14" = nrow(subset(patientTable,logic & patientTable$age_first >= 10 & patientTable$age_first <= 14)),
             Total = nrow(subset(patientTable,logic)))
  
}



ARTEligibility <- with(patientTable,rbind(
  ARTeligi("Eligible", eligible_first == TRUE)
  ,ARTeligi("Not Eligible", eligible_first == FALSE)
  ,ARTeligi("Unknown", is.na(eligible_first))
  ,ARTeligi("Total", !is.na(patientid))
))

print(ARTEligibility)
write.csv(ARTEligibility,file="ARTEligibility.csv")
  

time_ARTenrolment <- with(patientTable,rbind(
  ARTeligi("Enrolment", !is.na(arv_status_delay))
  ,ARTeligi("Within 1 month",(arv_status_delay <= 30))
  ,ARTeligi("Within 2 month",(arv_status_delay <= 60))
  ,ARTeligi("Within 3 month",(arv_status_delay <= 90))
  ,ARTeligi("Within 6 month",(arv_status_delay <= 180))
  ,ARTeligi("Within 12 month",(arv_status_delay<= 365))
  ,ARTeligi("> 12 month",(arv_status_delay > 365))
  ,ARTeligi("Not yet started", is.na(arv_status_delay))
))

print(time_ARTenrolment)
write.csv(time_ARTenrolment,file="ARTEligibility_delay.csv")


