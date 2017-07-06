#' Set the path where Zomer's PET database is downloaded
#' @description To allow extraction of PET from the CGIAR-CSI Global aridity and PET database (\url{http://www.cgiar-csi.org/data/global-aridity-and-pet-database}), you must manually download the file 'Global PET - Annual.zip' from the above url (follow 'Download from the HarvestChoice Dropbox'), and unzip it in some directory. When intending to use the database, set the path to the location with this function.
#'@examples
#'# Directory that contains unzipped annual PET data (this directory will contain subdirectory 'PET_he_annual')
#'\dontrun{
#'set_zomerpath("c:/data/zomerpet")
#'}
#'@export
set_zomerpet_path <- function(path){

  if(dir.exists(path) && "PET_he_annual" %in% dir(path)){
    options(zomerpetpath = path)
  } else {
    stop("Path does not exist or 'PET_he_annual' not in path.")
  }

}
