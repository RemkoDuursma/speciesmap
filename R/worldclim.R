#' Download Worldclim rasters
#' @description Downloads and unzips 10min resolution Worldlclim rasters, stores them for reuse.
#' @details Zip files are downloaded from \url{http://biogeo.ucdavis.edu/data/worldclim/v2.0/tif/base}
#' @author Remko Duursma
#' @export
#' @importFrom raster raster
#' @importFrom utils unzip
#' @examples
#' # Return worldclim tmean and prec rasters, as a list with those components
#' \dontrun{
#' wc <- get_worldclim_rasters(wc_vars=c("tavg","tmin"))
#' }
get_worldclim_rasters <- function(wc_vars="tavg"){

  download_worldclim <- function(wc_var, topath){

    fn <- wc_url(wc_var)
    fn_to <- file.path(topath, basename(fn))
    havewc <- file.exists(fn_to)

    if(!havewc){
      download.file(fn, fn_to, mode="wb", quiet=TRUE)
      u <- suppressWarnings(unzip(fn_to, exdir=topath, overwrite=FALSE))
      flog.info("WorldClim raster %s downloaded and unzipped.", basename(fn))
    } else {
      flog.info("Found WorldClim %s layer.", wc_var)
    }

  }

  wcpath <- get_wcpath()
  
  for(i in seq_along(wc_vars)){
    download_worldclim(vars[i], wcpath)
  }

  
}

wc_url <- function(wc_var){
  sprintf("http://biogeo.ucdavis.edu/data/worldclim/v2.0/tif/base/wc2.0_10m_%s.zip",wc_var)
}

wc_tif_fns <- function(wc_var, topath){
  if(wc_var == "bio"){
    # 19 bioclimatic variables
    file.path(topath, sprintf("wc2.0_%s_10m_%02d.tif", wc_var, 1:19))
  } else {
    # monthly variables
    file.path(topath, sprintf("wc2.0_10m_%s_%02d.tif", wc_var, 1:12))
  }
  
}


get_wcpath <- function(){
  wcpath <- options()$worldclimpath
  if(is.null(wcpath)){
    stop("Set path for WorldClim rasters first, e.g. options(worldclimpath = 'c:/worldclim')")
  }
  return(wcpath)
}


read_worldclim_raster <- function(wc_var){
  
  wcpath <- get_wcpath()
  
  wc_tifs <- wc_tif_fns(wc_var, wcpath)
  if(!all(file.exists(wc_tifs))){
    get_worldclim_rasters(wc_var)
  }
  
  # Read the rasters into a list
  ras <- lapply(wc_tifs, raster)
  flog.info(sprintf("WorldClim raster %s read.", wc_var))
  
return(ras)
}


#' @title Get worldclim observations for specific locations
#' @description Given a dataframe with longitude and latitude coordinates, extracts rainfall and temperature data from Worldclim. Rasters are downloaded if not provided.
#' @param data Dataframe with columns \code{latitude} and \code{longitude}.
#' @param vars Vector of climate variables to be extracted; see Details.
#' @param output Either 'monthly' (default), or 'annual', in which case the \code{annualize} function is executed on the result. Alternatively you can generate monthly output, and use \code{annualize_clim} yourself (see Examples).
#' @param cl Output of \code{get_climate_vars}.
#' @param funs For \code{aggregate_clim}, a character vector of functions to be applied to each column.
#' @param probs For \code{aggregate_clim}, if 'quantile' is one of the functions, the probabilities to be calculated (see \code{\link{quantile}})
#' @author Remko Duursma
#' @export
#' @details The climate variables are any of the Worldclim 2.0 variables, including "tmin","tmax","tavg","prec","srad","wind","vapr". Also allowed is 'pet', which will be extracted from the Zomer PET database (see \code{\link{get_zomer_pet}}).
#' @rdname get_climate_vars
#' @examples
#' \dontrun{
#' # First set the path to where WorldClim layers will be downloaded,
#' # and if extracting PET, where you downloaded the Zomer PET database
#' # see ?get_zomer_pet
#' options(worldclimpath="/path/to/worldclim")
#' res <- get_climate_vars(data=data.frame(longitude=150, latitude=-33), vars=c("tmin","bio"))
#'
#' # To extract PET, first set the path to where the layers were downloaded
#' options(zomerpetpath="c:/data/zomer")
#' res <- get_climate_vars(data=data.frame(longitude=150, latitude=-33), vars=c("pet","tavg"))
#' 
#' # By default all monthly values are returned, use annualize to get annual
#' # averages (or total in case of MAP):
#' annualize(res)
#' 
#' # ... or simply use output="annual"
#' res <- get_climate_vars(data=data.frame(longitude=150, latitude=-33), 
#' vars=c("pet","tavg"), output="annual")
#' }
#' @importFrom stats quantile
get_climate_vars <- function(data, 
                             vars=NULL,
                             output=c("monthly","annual")){

  output <- match.arg(output)
  
  if("pet" %in% vars){
    pet <- get_zomer_pet(data)
    vars <- vars[vars != "pet"]
    havepet <- TRUE
  } else {
    havepet <- FALSE
  }
  if(!length(vars)){
    return(pet)
  }
  
  ras <- lapply(vars, read_worldclim_raster)
  names(ras) <- vars
  
  extract_from_wcras <- function(r, wc_var, data){
  
    m <- matrix(ncol=length(r), nrow=nrow(data), 
                dimnames=list(NULL, paste(wc_var, seq_along(r), sep="_")))
  
    for(i in seq_along(r)){
      m[,i] <- extract(r[[i]], data[,c("longitude","latitude")])
    }
    
    flog.info("Extracted %s records from %s WorldClim raster.", nrow(data), wc_var)
  
  return(as.data.frame(m))
  }  
  
  l <- list()
  for(i in seq_along(vars)){
    l[[i]] <- extract_from_wcras(ras[[i]], vars[i], data)
  }
  l <- do.call(cbind, l)
  
  if(havepet){
    l <- cbind(l, PET=pet$PET)
  }

  d <- as.data.frame(cbind(data, l))
  class(d) <- c("climvardf","data.frame")
  
  if(output == "annual"){
    d <- annualize_clim(d)
  }
  
return(d)
}

#'@importFrom doBy summaryBy
#'@export
#' @rdname get_climate_vars
aggregate_clim <- function(cl, funs=c("mean", "quantile"), probs=c(0.05,0.95)){
  
  # Loop because quantile uses special argument:
  l <- list()
  for(i in seq_along(funs)){
    fun <- funs[i]
    if(fun == "quantile"){
      res <- summaryBy(. ~ species, data=cl, FUN=quantile, probs=probs, na.rm=TRUE)
      ind <- res[,1,drop=FALSE]
      res <- res[,6:ncol(res),drop=FALSE] # drop species, lat, long
      nm <- names(res)
      
      for(j in seq_along(probs)){
        nm <- gsub(sprintf("\\.%s%%",probs[j]*100), sprintf("_q%02d", 100*probs[j]), nm)
      }
      names(res) <- nm
      l[[i]] <- res
    } else {
      f <- get(fun)
      res <- summaryBy(. ~ species, data=cl, FUN=f, na.rm=TRUE, keep.names=TRUE)
      ind <- res[,1,drop=FALSE]
      res <- res[,4:ncol(res),drop=FALSE] # drop species, lat, long
      names(res) <- paste(names(res),fun, sep="_")
      l[[i]] <- res
    }
  }
  
  l <- do.call(cbind, l)
  l <- l[,order(names(l)),drop=FALSE]

return(cbind(ind, l))
}

#' @rdname get_climate_vars
#' @export
annualize_clim <- function(cl){
  
  apply_var <- function(var, data, fun, newname=NULL){
    d <- data.frame(apply(data[, grep(var, names(data))],1,fun))
    names(d) <- ifelse(is.null(newname), var, newname)
    return(d)
  }
  
  vars <- gsub("_1","",names(cl)[grep(".+_1$", names(cl))])
  if("PET" %in% names(cl))vars <- c(vars, "PET")
  
  l <- list()
  for(i in seq_along(vars)){
    if(vars[i] == "prec"){
      l[[i]] <- apply_var(vars[i], cl, sum, newname="MAP")
    } else if(vars[i] == "tavg"){
      l[[i]] <- apply_var(vars[i], cl, mean, newname="MAT")
    } else if(vars[i] == "bio"){
      l[[i]] <- cl[,grep("bio", names(cl))]
    } else if (vars[i] == "PET") {
      l[[i]] <- cl[,"PET",drop=FALSE]
    } else {
      l[[i]] <- apply_var(vars[i], cl, mean)
    }
  }
  l <- do.call(cbind, l)
  
  return(cbind(cl[,c("species","longitude","latitude")], l))
  
}

#' Long-term climate at species occurrences
#' @description Simple interface to download species occurrences, and extract climate observations from Worldclim for where the species is present.
#' @param species Latin binomial for species, can be a vector.
#' @param database Either 'ALA' or 'GBIF'
#' @param vars Vector of climate variables to be extracted; see Details.
#' @param rasterize If TRUE, the default, resamples species occurrences into same spatial scale as Worldclim data.
#' @param return If summary (the default), returns summary variables of precip and temp, one row per species. For 'all' returns climate variables (and monthly ones) for all locations; result is a list if species is a vector.
#' @details The climate variables are any of the Worldclim 2.0 variables, including "tmin","tmax","tavg","prec","srad","wind","vapr". Also allowed is 'pet', which will be extracted from the Zomer PET database (see \code{\link{get_zomer_pet}}).
#' @author Remko Duursma
#' @export
climate_presence <- function(species, database=c("ALA","GBIF", "both"),
                             vars=NULL,
                             output=c("monthly","annual"),
                             rasterize=TRUE,
                             ala_args=NULL,
                             gbif_args=NULL){

  database <- match.arg(database)
  
  l <- list()

  for(i in seq_along(species)){

    if(database == "GBIF"){
      spocc <- get_occurrences_gbif(species[i], gbif_args=gbif_args)
    } else if(database == "ALA"){
      spocc <- get_occurrences_ala(species[i], ala_args=ala_args)
    } else if(database == "both"){
      spocc <- get_occurrences_both(species[i], ala_args=ala_args, gbif_args=gbif_args)
    }

    if(rasterize)spocc <- rasterize_occurrences(spocc)

    l[[i]] <- get_climate_vars(spocc, vars=vars, output=output)
    flog.info("Species %s of %s completed", i, length(species))
  }

  return(do.call(rbind,l))

}



