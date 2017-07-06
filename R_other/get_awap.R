library(stringr)
library(RCurl)
library(raster)



get_awap <- function(dates){

  dates <- as.Date(dates)
  if(all(is.na(dates)))stop("Please provide Date(s) or string(s) in YYYY-MM-DD")

  if(length(dates) == 1){
    return(get_awap_one(dates))
  } else {
    l <- lapply(dates, get_awap_one)
    return(l)
  }
}


get_awap_one <- function(date){

  baseurl <- "http://www.bom.gov.au/web03/ncc/www/awap/temperature/maxave/daily/grid/0.05/history/nat/"

  dat <- format(date, "%Y%m%d%Y%m%d")
  url <- paste0(baseurl, dat, ".grid.Z")

  download.file(url, "tmp.Z", mode="wb")

  if(.Platform$OS.type == "windows"){

    cmd <- sprintf("7z e tmp.Z -y -o%s/", getwd())
    res <- shell(cmd, intern=TRUE)
    if(!any(grepl("Everything is Ok", res))){
      stop("A problem occurred unzipping the file. Make sure 7zip is installed and it is available on the path.")
    }

  } else if(.Platform$OS.type == "unix"){

    system("uncompress tmp.Z")

  } else {
    stop("This works on Windows/Unix/Mac only.")
  }

  tmax <- raster("tmp")

  return(tmax)
}





# example
d <- get_awap("2015-6-1")

# gives a list
f <- get_awap(c("2015-6-1", "2016-6-1"))
length(f)
plot(f[[2]])



