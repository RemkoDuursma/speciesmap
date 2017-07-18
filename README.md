# speciesmap : species occurrences and long-term climate

[![](http://www.r-pkg.org/badges/version/speciesmap)]()

This R package obtains species records from GBIF or ALA, and retrieves climate variables (MAP, MAT) at species locations from WorldClim, and optionally, potential evapotranspiration (PET) from CGIAR-CSI.

Use this package if you want to estimate broad climate occupancy by species, as defined by MAP and MAT across the range where the species occur. The package calculates mean and quantiles of MAT and MAP across the range, and reduces sampling bias by rasterizing the occurrences.

The approach is explained in a [blog post](http://www.remkoduursma.com/blog/2017/05/12/calculating-species-climate-envelopes-in-r/).

The main use of the package is the function `worldclim_presence`, which takes the following steps. Step 3 is optional but was the main reason to write this package.

1. Given a list of species names, obtain occurrences from ALA ([Atlas of Living Australia](ala.org.au)) or [GBIF](http://www.gbif.org/).
2. Downloads WorldClim current climate variables to a temporary directory (or specified path; does not re-download if it already is present)
3. Rasterizes species occurrences to the same resolution as WorldClim (only the coarsest resolution is supported at the moment; 10min or ca. 18km2). This step is taken to reduce sampling bias - avoids the overweighting of species occurrences where they are sampled more often. The idea is that, in order to estimate broad climate occupancy, we need to find areas where species can occur, and not weigh areas more where they occur in higher densities or have been sampled more intensively. Watch this space for a blog post that explains the process in more detail.
4. Return either a matrix with a row for every 10min cell where the species occurs, and monthly precip and temperature, or a summary table with mean MAP, MAT, and 5% and 95% quantiles of MAP and MAT across all cells where the species occurs.


```
> w <- worldclim_presence("Eucalyptus botryoides")
INFO [2017-05-12 10:19:10] WorldClim raster tmean_10m_esri.zip downloaded.
INFO [2017-05-12 10:19:12] WorldClim raster prec_10m_esri.zip downloaded.
INFO [2017-05-12 10:19:13] WorldClim rasters read.
INFO [2017-05-12 10:19:13] ALA returned 3274 records for Eucalyptus botryoides in 0.1 sec.
INFO [2017-05-12 10:19:18] Rasterized 3274 occurrences into 214 cells
INFO [2017-05-12 10:19:22] Extracted 214 records from WorldClim rasters.
```
