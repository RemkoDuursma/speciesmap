pacman::p_load(oz)


sp1 <- get_occurrences_gbif("Eucalyptus fibrosa")
sp2 <- get_occurrences_ala("Eucalyptus fibrosa")


#o <- ALA4R::occurrences(taxon="Eucalyptus fibrosa", download_reason_id=7)

windows(9,5)
par(mar=c(0,0,0,0), mfrow=c(1,2))
oz(sections=3:5)
with(sp1, points(longitude, latitude, pch=16, cex=0.1, col="red"))
oz(sections=3:5)
with(sp2, points(longitude, latitude, pch=16, cex=0.1, col="red"))



r1 <- rasterize_occurrences(sp1)
r2 <- rasterize_occurrences(sp2)

windows(9,5)
par(mar=c(0,0,0,0), mfrow=c(1,2))
oz(sections=3:5)
with(r1, points(longitude, latitude, pch=16, cex=0.1, col="red"))
oz(sections=3:5)
with(r2, points(longitude, latitude, pch=16, cex=0.1, col="red"))



wc <- get_worldclim_rasters(topath="c:/cache")
get_worldclim_prectemp(data.frame(latitude=-35, longitude=150))

get_worldclim_prectemp(data.frame(latitude=-35, longitude=150),
                       worldclim=wc)

get_worldclim_prectemp(data.frame(species="Eucalyptus sp.",
                                  latitude=-35, longitude=150),
                       return="summary")


w1 <- worldclim_presence("Eucalyptus benthamii",
                         topath="c:/cache",
                         database="GBIF", rasterize=TRUE)
w2 <- worldclim_presence("Eucalyptus fibrosa",
                         database="ALA", rasterize=TRUE,
                         topath="c:/cache")

w3 <- worldclim_presence("Eucalyptus benthamii",
                         topath="c:/cache",
                         database="GBIF", rasterize=FALSE)
w4 <- worldclim_presence("Eucalyptus benthamii",
                         database="ALA", rasterize=FALSE,
                         topath="c:/cache")




