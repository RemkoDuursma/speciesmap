#' Get species occurrences from the ALA or GBIF
#' @description A simple wrapper to \code{\link{occurrences}} from \CRANpkg{ALA4R}. Performs a search for a \code{species} (e.g. 'Eucalyptus globulus'), sets the \code{download_reason} to 'testing' (see \code{?ala_reasons}), and returns only the species, the longitude, and the latitude of all observations.
#' @param species The full scientific species name, quoted.
#' @param ala_args Optionally, a list of arguments passed to \code{occurrences} (ALA4R package).
#' @param gbif_args Optionally, a list of arguments passed to \code{occ_search} (rgbif package).
#' @details Note that when passing additional arguments to \code{occ_search}, the following arguments are already set and can not be overriden : \code{hasGeospatialIssue} (FALSE), \code{limit} (50000), \code{fields} (species, lat and long), and of course \code{scientificName}. When passing additional arguments to \code{occurrences}, only \code{download_reason} (7) is already set.
#' @author Remko Duursma
#' @export
#' @seealso \code{\link{get_occurrences_gbif}}
#' @examples
#' \dontrun{
#' o <- get_occurrences_ala("Eucalyptus botryoides")
#'
#' # Should return a message like:
#' # INFO [2017-05-12 09:52:10] ALA returned 3274 records for Eucalyptus botryoides in 2.6 sec.
#' }
#' @importFrom ALA4R occurrences
#' @rdname get_occurrences
#' @importFrom futile.logger flog.info
#' @importFrom stats na.omit
#' @importFrom utils download.file
#' @importFrom sp coordinates
#' @importFrom sp proj4string
#' @importFrom sp 'proj4string<-'
#' @importFrom sp 'coordinates<-'
get_occurrences_ala <- function(species, ala_args=NULL){

  if(length(species) > 1){
    l <- lapply(species, get_occurrences_ala, ala_args=ala_args)
    return(do.call(rbind, l))
  }
  
  species <- fix_caps(species)
  empty <- data.frame(species=species, longitude=NA, latitude=NA)

  if(!assert_species_name(species)){
    flog.info("No search performed for genus only: %s", species)
    return(empty)
  }
  
  time1 <- system.time({
    spdat <- do.call(occurrences, c(list(taxon=species, download_reason_id=7), ala_args))
  })
  
  if(nrow(spdat$data) == 0 || all(spdat$data$longitude == "")){
    flog.info("ALA did not find data for %s", species)
    return(empty)
  }

  # Remove missing longitudes
  spdat <- spdat$data[!is.na(spdat$data$longitude), c("species","longitude","latitude")]

  # Returns other species as well, for some reason
  spdat <- spdat[spdat$species == species,]

  flog.info("ALA returned %s records for %s in %s sec.",
            nrow(spdat), species, round(time1[[3]],1))

  return(spdat)
}


#' @rdname get_occurrences
#' @importFrom rgbif occ_search
#' @export
get_occurrences_gbif <- function(species, gbif_args=NULL){

  if(length(species) > 1){
    l <- lapply(species, get_occurrences_gbif, gbif_args=gbif_args)
    return(do.call(rbind, l))
  }
  
  species <- fix_caps(species)
  empty <- data.frame(species=species, longitude=NA, latitude=NA)
  
  if(!assert_species_name(species)){
    flog.info("No search performed for genus only: %s", species)
    return(empty)
  }
  
  time1 <- system.time({
    spdat <- try(do.call(occ_search, c(list(scientificName=species,
                                           limit=50000,
                                           fields =c('name','decimalLatitude','decimalLongitude'),
                                           hasGeospatialIssue=FALSE,
                                           return="data"), gbif_args)))
  })

  if(inherits(spdat, "try-error")){
    flog.info("GBIF service failed for %s, please try again.", species)
    return(empty)
  }

  # If GBIF does not like the name, it returns the name it likes instead.
  if(identical(names(spdat), "name")){
    time1 <- system.time(spdat <- occ_search(scientificName=spdat$name[1],
                                             limit=50000,
                                             fields =c('name','decimalLatitude','decimalLongitude'),
                                             hasGeospatialIssue=FALSE,
                                             return="data"))
  }
  
  # either gbif says 'no data', or all records have no lat/long,
  # in which case it returns a dataframe with one column.
  if(grepl("no data found", spdat[1]) | isTRUE(ncol(spdat) == 1)){
    flog.info("GBIF did not find data for %s", species)
    return(empty)
  }
  
  # Sometimes GBIF returns the records for the new species name, silently switching species.
  if(unique(spdat$name) != species){
    flog.info("GBIF prefers new name %s for %s", unique(spdat$name), species)
    spdat$name <- species
  }

  # remove obs with no lats and longs
  spdat <- as.data.frame(na.omit(spdat))

  # rename
  names(spdat) <- c("species","longitude","latitude")

  flog.info("GBIF returned %s records for %s in %s sec.",
            nrow(spdat), species, round(time1[[3]],1))

  return(spdat)
}

get_occurrences_both <- function(species, ala_args=NULL, gbif_args=NULL){

  gbif <- get_occurrences_gbif(species, gbif_args=gbif_args)
  ala <- get_occurrences_ala(species, ala_args=ala_args)

return(rbind(gbif, ala))
}


#' Rasterize species occurrences
#' @description Takes output from \code{\link{get_occurrences_ala}} or \code{\link{get_occurrences_gbif}}, and rasterizes the occurrences into 10min (ca. 18km2). At the moment rasterizing uses only this fixed resolution, which is the coarsest resolution for WorldClim data (see \url{http://www.worldclim.org/version1}). The result is a dataframe with latitude and longitude of the midpoints of the raster cells where the species occurs at least once.
#' @param spdat A dataframe returned by \code{\link{get_occurrences_ala}} or \code{\link{get_occurrences_gbif}}, or simply a dataframe with 'species', 'longitude', and 'latitude' (species is required).
#' @param return_raster Logical (default FALSE). If TRUE, return the raster object, instead of the raster converted to a dataframe with midpoint coordinates.
#' @author Remko Duursma
#' @export
#' @examples
#' \dontrun{
#' o <- get_occurrences_ala("Eucalyptus botryoides")
#' r <- rasterize_occurrences(o)
#' }
#' @importFrom raster extract
#' @importFrom raster subs
#' @importFrom raster raster
#' @importFrom raster rasterToPoints
#' @importFrom raster extent
rasterize_occurrences <- function(spdat, return_raster=FALSE){

  if(all(is.na(spdat$longitude)))return(spdat)

  # make a new raster same size as worlclim but each gridcellhas ID number
  gridcellID <- raster(nrow=900,ncol=2160,extent(c(-180,180,-60,90)), crs="+proj=longlat +datum=WGS84")
  gridcellID[] <- 1:1944000

  # get centerpoint of gridcells where species occur, 1 observation for each gridcell
  spdat$GridID <- extract(gridcellID,spdat[,c("longitude","latitude")],method='simple')

  # extract the gridCell ID for observations
  presence <- as.data.frame(cbind(1,unique(spdat$GridID)))

  # make dataframe of cells present in
  colnames(presence) <- c("val","gridID")

  # raster of presence
  presence_raster <- subs(gridcellID,
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

