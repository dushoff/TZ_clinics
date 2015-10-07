###summary table
library(dplyr)

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
  ,baselinecat("Facility Type : Dispensary", Datetable$base_facility == 1)
  ,baselinecat("Facility Type : Health Centre", Datetable$base_facility == 2)
  ,baselinecat("Facility Type : Hospital", Datetable$base_facility == 3)
  ,baselinecat("Facility Type : Other", Datetable$base_facility == 4)
  ,baselinecat("Facility Type : Missing", is.na(Datetable$base_facility))
  ,baselinecat("Referred From : HBC", Datetable$base_referred == 1)
  ,baselinecat("Referred From : PITC", Datetable$base_referred == 12)
  ,baselinecat("Referred From : PMTCT", Datetable$base_referred == 14)
  ,baselinecat("Referred From : VCT", Datetable$base_referred == 25)
  ,baselinecat("Referred From : Missing", is.na(Datetable$base_referred))
  ,baselinecat("Positive for TB : Yes ",Datetable$base_TB == "CXR+" |
                                        Datetable$base_TB == "TB Susp" |
                                        Datetable$base_TB == "SS+" |
                                        Datetable$base_TB == "Conf/Yes" |
                                        Datetable$base_TB == "TB SUSP")
  ,baselinecat("Positive for TB : No ", Datetable$base_TB == "CXR-" |
                                        Datetable$base_TB == "SCR -ve" |
                                        Datetable$base_TB == "SS-")
  ,baselinecat("Positive for TB : Missing", Datetable$base_TB == "")
  ,baselinecat("Malnourished : No", Datetable$base_Malnourshed == "OK")
  ,baselinecat("Malnourished : Mild", Datetable$base_Malnourshed == "MLD")
  ,baselinecat("Malnourished : Moderate", Datetable$base_Malnourshed == "MOD")
  ,baselinecat("Malnourished : Severe", Datetable$base_Malnourshed == "SEV")
  ,baselinecat("Malnourished : Missing", Datetable$base_Malnourshed == "")
))

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