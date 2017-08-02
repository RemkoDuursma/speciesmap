#'@importFrom stringr str_extract
flog_to_time <- function(x){
  as.POSIXct(gsub("\\[|\\]","",str_extract(x, "\\[(.*?)\\]")), tz="UTC")
}

fix_caps <- function (string) {
  string <- tolower(string)  # Only line that differs from Hmisc::capitalize
  capped <- grep("^[A-Z]", string, invert = TRUE)
  substr(string[capped], 1, 1) <- toupper(substr(string[capped],
                                                 1, 1))
  return(string)
}

assert_species_name <- function(x){
  x <- strsplit(x, " ")
  length(x)[[1]] > 1
}
