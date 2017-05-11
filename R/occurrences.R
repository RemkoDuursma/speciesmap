#' Obtain species occurrences from GBIF
#' @description Very simple wrapper to \code{\link{occ_search}}
#' @param
#' @param
#' @param
#' @author Remko Duursma
#' @export
#' @examples
get_occurrences_gbif <- function(species){

  species <- fix_caps(species)
  
  time1 <- system.time(spdat <- occ_search(scientificName=species,
                      limit=50000,
                      fields =c('name','decimalLatitude','decimalLongitude'),
                      hasGeospatialIssue=FALSE,
                      return="data"))

  # remove obs with no lats and longs
  spdat <- as.data.frame(na.omit(spdat))

  # rename
  names(spdat) <- c("species","longitude","latitude")

  flog.info("GBIF returned %s records for %s in %s sec.",
            nrow(spdat), species, round(time1[[3]],1))

  return(spdat)
}


#' Get species occurrences from the ALA
#' @description
#' @param
#' @param
#' @param
#' @author Remko Duursma
#' @export
#' @examples
#' @importFrom ALA4R occurrences
get_occurrences_ala <- function(species){

  species <- fix_caps(species)
  
  time1 <- system.time(spdat <- occurrences(taxon=species, download_reason_id=7))

  spdat <- subset(spdat$data,
                  !is.na(longitude),
                  select = c(species,longitude,latitude))

  # Returns other species as well, for some reason
  spdat <- spdat[spdat$species == species,]

  flog.info("ALA returned %s records for %s in %s sec.",
            nrow(spdat), species, round(time1[[3]],1))

  return(spdat)
}


#' Does
#' @description
#' @param
#' @param
#' @param
#' @author Remko Duursma
#' @export
#' @examples
rasterize_occurrences <- function(spdat, return_raster=FALSE){

  # make a new raster same size as worlclim but each gridcellhas ID number
  gridcellID <- raster(nrow=900,ncol=2160,extent(c(-180,180,-60,90)), crs="+proj=longlat +datum=WGS84")
  gridcellID[] <- 1:1944000

  # get centerpoint of gridcells where species occur, 1 observation for each gridcell
  spdat$GridID <- raster::extract(gridcellID,cbind(spdat$longitude,spdat$latitude),
                                  method='simple')

  # extract the gridCell ID for observations
  presence <- as.data.frame(cbind(1,unique(spdat$GridID)))

  # make dataframe of cells present in
  colnames(presence) <- c("val","gridID")

  # raster of presence
  presence_raster <- raster::subs(gridcellID,
                                  as.data.frame(table(presence$gridID)))

  if(return_raster)return(presence_raster)
  
  # convert lat and longs of gridcells where species is present
  pxy <- as.data.frame(rasterToPoints(presence_raster))
  pxy <- pxy[,-3]
  names(pxy) <- c("longitude","latitude")

  pxy <- cbind(data.frame(species=unique(spdat$species)), pxy)

  flog.info("Rasterized %s occurrences into %s cells", nrow(spdat), nrow(pxy))

  return(pxy)
}

