#' Download Worldclim rasters
#' @description Downloads 10min resolution Worldlclim rasters, stores them for reuse, and reads them into raster objects. Currently only downloads only tmean (monthly mean temperature) and prec (monthly total precipitation) layers.
#' @param topath Local path to store files. Can use \code{\link{tempdir()}} if files need not be kept.
#' @param clean Delete files after downloading (unimplemented)
#' @details Zip files are downloaded from \url{http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur}
#' @author Remko Duursma
#' @export
#' @examples
#' # Return worldclim tmean and prec rasters, as a list with those components
#' wc <- get_worldclim_rasters(topath=tempdir())
#' @importFrom raster raster
get_worldclim_rasters <- function(topath, clean=FALSE){

  download_worldclim <- function(basen, topath){

    wc_fn_full <- file.path(topath, basen)
    havewc <- file.exists(wc_fn_full)

    if(!havewc){
      download.file(file.path("http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur",basen),
                    wc_fn_full, mode="wb", quiet=TRUE)
      flog.info("WorldClim raster %s downloaded.", basen)
    }

    u <- suppressWarnings(unzip(wc_fn_full, exdir=topath, overwrite=FALSE))

    return(u)
  }

  download_worldclim("tmean_10m_esri.zip", topath)
  download_worldclim("prec_10m_esri.zip", topath)

  # Read the rasters into a list
  tmean_raster <- list()
  prec_raster <- list()
  for(i in 1:12){
    tmean_raster[[i]] <- raster(file.path(topath, sprintf("tmean/tmean_%s", i)))
    prec_raster[[i]] <- raster(file.path(topath, sprintf("prec/prec_%s", i)))
  }
  flog.info("WorldClim rasters read.")

  return(list(tmean_raster=tmean_raster, prec_raster=prec_raster))
}


#' Get worldclim observations for specific locations
#' @description Given a dataframe with longitude and latitude coordinates, extracts rainfall and temperature data from Worldclim. Rasters are downloaded if not provided.
#' @param data Dataframe with columns \code{latitude} and \code{longitude}.
#' @param topath Path where Worldclim rasters will be downloaded to (defaults to a temporary directory)
#' @param return If 'all' returns monthly tmean, precip, as well as MAT and MAP. If 'summary', returns quantiles and means of MAT and MAP across all locations in \code{data}.
#' @param worldclim Optionally, list of rasters returned by \code{\link{get_worldclim_rasters}}, to save time.
#' @author Remko Duursma
#' @export
#' @examples
get_worldclim_prectemp <- function(data, topath=tempdir(),
                                   return=c("all","summary"),
                                   worldclim=NULL){

  return <- match.arg(return)

  if(is.null(worldclim)){
    worldclim <- get_worldclim_rasters(topath)
  }
  tmean_raster <- worldclim$tmean_raster
  prec_raster <- worldclim$prec_raster

  #extract worldclim data; extract the gridCell ID for observations
  tmeanm <- precm <- matrix(ncol=12, nrow=nrow(data))

  for(i in 1:12){
    tmeanm[,i] <- 0.1 * extract(tmean_raster[[i]], cbind(data$longitude,data$latitude))
    precm[,i] <- extract(prec_raster[[i]], cbind(data$longitude,data$latitude))
  }
  colnames(tmeanm) <- paste0("tmean_",1:12)
  colnames(precm) <- paste0("prec_",1:12)
  flog.info("Extracted %s records from WorldClim rasters.", nrow(data))

  pxy <- cbind(data, as.data.frame(tmeanm), as.data.frame(precm))
  names(pxy)[2:3] <- c("longitude","latitude")

  pxy$MAT <- apply(pxy[,grep("tmean_",names(pxy))],1,mean)
  pxy$MAP <- apply(pxy[,grep("prec_",names(pxy))],1,sum)

  #
  if(return == "all")return(pxy)

  if(return == "summary"){

    dfr <- suppressWarnings(with(pxy, data.frame(species=unique(data$species),
                                                 n=nrow(data),
                                                 lat_mean=mean(latitude,na.rm=TRUE),
                                                 long_mean=mean(longitude,na.rm=TRUE),
                                                 MAT_mean=mean(MAT,na.rm=TRUE),
                                                 MAT_q05=quantile(MAT,0.05,na.rm=TRUE),
                                                 MAT_q95=quantile(MAT,0.95,na.rm=TRUE),
                                                 MAP_mean=mean(MAP,na.rm=TRUE),
                                                 MAP_q05=quantile(MAP,0.05,na.rm=TRUE),
                                                 MAP_q95=quantile(MAP,0.95,na.rm=TRUE))))
    rownames(dfr) <- NULL
    return(dfr)
  }
}


#' Long-term climate at species occurrences
#' @description Simple interface to download species occurrences, and extract climate observations from Worldclim for where the species is present.
#' @param species Latin binomial for species, can be a vector.
#' @param database Either 'ALA' or 'GBIF'
#' @param rasterize If TRUE, the default, resamples species occurrences into same spatial scale as Worldclim data.
#' @param topath Path to store Worldclim rasters (if present there, will not be re-downloaded)
#' @param return If summary (the default), returns summary variables of precip and temp, one row per species. For 'all' returns climate variables (and monthly ones) for all locations; result is a list if species is a vector.
#' @author Remko Duursma
#' @export
#' @examples
worldclim_presence <- function(species, database=c("ALA","GBIF"),
                               rasterize=TRUE,
                               topath=tempdir(),
                               return=c("summary","all")){

  return <- match.arg(return)
  database <- match.arg(database)

  worldcl <- get_worldclim_rasters(topath)

  l <- list()

  for(i in seq_along(species)){

    if(database == "GBIF"){
      spocc <- get_occurrences_gbif(species[i])
    } else if(database == "ALA"){
      spocc <- get_occurrences_ala(species[i])
    }

    if(rasterize)spocc <- rasterize_occurrences(spocc)

    l[[i]] <- get_worldclim_prectemp(spocc, topath=topath, return=return, worldclim=worldcl)
  }

  if(return == "all"){
    if(length(species) == 1)
      return(l[[1]])
    else{
      names(l) <- species
      return(l)
    }
  } else {
    return(do.call(rbind,l))
  }

}



