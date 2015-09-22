library(dplyr)

### Eligible or not 

## Please add No_R_pipe when you're doing stuff the changing directories or loading files (even local files should be loaded by make in the pipeline)!!
## Also, why not use a better data file, like keep.visits or keep.sample?
load("~/tz_pediatric_hiv/c_visits.RData") ## No_R_pipe

combineInfo <- function(test1, test2){
	if(!is.na(test1)) return(test1)
	return(test2)
}
