###summary table

baselinecat <- function(cat, logic){
  data.frame(Factor = cat,
        "2011" = nrow(subset(Datetable,logic & start_year == 2011)),
        "2012" = nrow(subset(Datetable,logic & start_year == 2012)),
        "2013" = nrow(subset(Datetable,logic & start_year == 2013)),
        "2014" = nrow(subset(Datetable,logic & start_year == 2014)),
        Total = nrow(subset(Datetable,logic)))
}
