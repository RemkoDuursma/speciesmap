flog_to_time <- function(x){
  as.POSIXct(gsub("\\[|\\]","",str_extract(x, "\\[(.*?)\\]")), tz="UTC")
}
