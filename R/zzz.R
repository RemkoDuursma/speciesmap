.onAttach <- function(libname, pkgname){

  msg <- c("The speciesmap package is an interface to the following services -",
           "please visit those sites for information and CITATIONS:",
           "Worldclim - www.worldclim.org",
           "ALA - www.ala.org.au",
           "GBIF - www.gbif.org",
           "CGIAR-CSI - http://www.cgiar-csi.org/data/global-aridity-and-pet-database")
  packageStartupMessage(paste(msg, collapse="\n"))


}
