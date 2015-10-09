
keepvar <- c(
	"patientid", "visitnum",
	"dateofbirth", "sex", "age", "dateofdeath",
	"visitdate",
	"cd4", "cd4percent",
	"hf_type", 
	"whostage",
	"arvstatuscode",
	"tbstatuscode", "tbscreeningid",
	"nutritionalstatusid",
	"arts_first_date", "artstart", "age_artstart",
	"artyear",
	"referredfromid",
	"death",
	"status",
	"medication2"
)

c_visits <- c_visits[keepvar]
patients <- unique(c_visits$patientid)   

# rdsave(c_visits, patients)
