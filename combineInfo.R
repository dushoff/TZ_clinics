library(dplyr)

combineInfo <- function(test1, test2){
	if(!is.na(test1)) return(test1)
	return(test2)
}

eligible <- (c_visits 
	%>%  rowwise()
	%>% mutate(eligible =
		combineInfo(cd4percent<20, cd4<750)
	)
)
