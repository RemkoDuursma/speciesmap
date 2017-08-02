#' Extract PET from the CGIAR-CSI database
#' @description To allow extraction of PET from the CGIAR-CSI Global aridity and PET database (\url{http://www.cgiar-csi.org/data/global-aridity-and-pet-database}), you must manually download the file 'Global PET - Annual.zip' from the above url (follow 'Download from the HarvestChoice Dropbox'), and unzip it in some directory. Then set the path with options() as shown below in the examples.
#'@param data Dataframe with longitude, latitude
#'@export
#'@examples
#'\dontrun{
#' options(zomerpetpath="path/to/zomer")
#' dat <- data.frame(longitude=c(150,145,140), latitude=c(-33,-33,-33))
#' get_zomer_pet(dat)
#' }
#' @importFrom sp CRS
#' @importFrom sp SpatialPoints
get_zomer_pet <- function(data){

  if(all(is.na(data$longitude))){
    return(cbind(data, PET=NA))
  }

  petpath <- options()$zomerpetpath
  if(is.null(petpath)){
    stop("Set path to downloaded PET database first, e.g. options(zomerpetpath = 'c:/zomerpet')")
  }

  r <- raster(file.path(petpath, "PET_he_annual/pet_he_yr"))

  here <- data.frame(lon=data$longitude,lat=data$latitude)
  coordinates(here) <- c("lon", "lat")
  proj4string(here) <- CRS("+proj=longlat +datum=WGS84")
  coors <- SpatialPoints(here)

  pet <- extract(r, coors)

  flog.info("Extracted %s records from CGIAR-CSI rasters.", nrow(data))

return(cbind(data, PET=pet))
}



