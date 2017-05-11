
library(mapview)
library(speciesmap)

tere <- get_occurrences_ala("Eucalyptus tereticornis")
grey <- get_occurrences_ala("Eucalyptus punctata")

r <- rasterize_occurrences(tere, return_raster=TRUE)
mapview(r)


spec <- rbind(tere, grey)



spec2 <- subset(spec, longitude > 130 & longitude < 160 & latitude < 0)

spec2$var1 <- ifelse(spec2$latitude > -20, 1, 0)

spec2$dist <- sqrt((spec2$longitude - 150)^2 + (spec2$latitude + 33)^2)

coordinates(spec2) <- ~longitude+latitude
proj4string(spec2) <- CRS("+proj=longlat +datum=WGS84")

mapview(spec2, cex=0.1, burst=TRUE, legend=TRUE, color=rainbow)
