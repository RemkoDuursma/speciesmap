
library(devtools)

# to install do
#install_github("remkoduursma/speciesmap")

library(speciesmap)

# set the location where worldlcim files will
# be downloaded into.
# you must make this directory first
options(worldclimpath="c:/worldclim")

mylocations <- data.frame(latitude=c(-33,-34),
                          longitude=c(150,150.1))

# contains monthly data
dat <- get_climate_vars(mylocations, vars=c("tmin","tavg","prec"))

# simple function to make annual variables
annualize_clim(dat)

