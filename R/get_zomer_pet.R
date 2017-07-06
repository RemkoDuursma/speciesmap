#' Extract PET from the CGIAR-CSI database
#'@export
#'@examples
#'dat <- data.frame(longitude=c(150,145,140), latitude=c(-33,-33,-33))
#' get_zomer_pet(dat)
get_zomer_pet <- function(data){

  petpath <- options()$zomerpetpath
  if(is.null(petpath)){
    stop("Set path to downloaded PET database first, see ?set_zomerpet_path.")
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




