# speciesmap : climate observations at species occurrences

[![](http://www.r-pkg.org/badges/version/speciesmap)]()

This R package obtains species records from the Global Biodiversity Information Facility ([GBIF](http://www.gbif.org/)) or the Atlas of Living Australia ([ALA](ala.org.au)), and retrieves climate variables at species locations from [WorldClim](www.worldclim.org), and optionally, potential evapotranspiration (PET) from the [CGIAR-CSI aridity database](http://www.cgiar-csi.org/data/global-aridity-and-pet-database).
 
Use this package if you want to estimate broad climate occupancy by species, as defined by for example mean annual precipitation and mean annual temperature across the range where the species occurs.

The main use of the package is the function `climate_presence`, which takes the following steps. Step 3 is optional but was the main reason to write this package.

1. Given a list of species names, obtain occurrences from ALA ([Atlas of Living Australia](ala.org.au)) or [GBIF](http://www.gbif.org/).
2. Downloads WorldClim current climate rasters, unless already available locally.
3. Rasterizes species occurrences to the same resolution as WorldClim (only the coarsest resolution is supported at the moment; 10min or ca. 18km2). This step is taken to reduce sampling bias - avoids the overweighting of species occurrences where they are sampled more often. The idea is that, in order to estimate broad climate occupancy, we need to find areas where species can occur, and not weigh areas more where they occur in higher densities or have been sampled more intensively.
4. Return all climate variables, monthly or annually. It is also possible to aggregate the climate variables by species, applying a set of user-defined functions (for example, mean and quantiles of MAP across the species range).

