#' Obtain species occurrences from GBIF
#' @description Very simple wrapper to \code{\link{occ_search}}
#' @param
#' @param
#' @param
#' @author Remko Duursma
#' @export
#' @examples
get_occurrences_gbif <- function(species){

  spdat <- occ_search(scientificName=species,
                      limit=50000,
                      fields =c('name','decimalLatitude','decimalLongitude'),
                      hasGeospatialIssue=FALSE,
                      return="data")

  # remove obs with no lats and longs
  spdat <- as.data.frame(na.omit(spdat))

  # rename
  names(spdat) <- c("species","longitude","latitude")

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
get_occurrences_ala <- function(species){

  spdat <- occurrences(taxon=species, download_reason_id=7)

  spdat <- subset(spdat$data,
                  !is.na(longitude),
                  select = c(species,longitude,latitude))

  # Returns other species as well, for some reason
  spdat <- spdat[spdat$species == species,]

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
rasterize_occurrences <- function(spdat){

  # make a new raster same size as worlclim but each gridcellhas ID number
  gridcellID <- raster(nrow=900,ncol=2160,extent(c(-180,180,-60,90)))
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

  # convert lat and longs of gridcells where species is present
  pxy <- as.data.frame(rasterToPoints(presence_raster))
  pxy <- pxy[,-3]
  names(pxy) <- c("longitude","latitude")

  pxy <- cbind(data.frame(species=unique(spdat$species)), pxy)

  return(pxy)
}

