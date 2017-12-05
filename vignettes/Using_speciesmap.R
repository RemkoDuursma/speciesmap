## ---- eval=FALSE---------------------------------------------------------
#  devtools::install_github("remkoduursma/speciesmap")

## ------------------------------------------------------------------------
library(speciesmap)

## ----eval=FALSE----------------------------------------------------------
#  benthocc <- get_occurrences_ala("Eucalyptus benthamii")

## ----echo=FALSE----------------------------------------------------------
data(benthocc)

## ----echo=FALSE----------------------------------------------------------
knitr::kable(head(benthocc))

## ---- eval=FALSE---------------------------------------------------------
#  benthocc <- get_occurrences_ala(c("Eucalyptus benthamii","Eucalyptus globulus"))

## ---- eval=FALSE---------------------------------------------------------
#  benthoccras <- rasterize_occurrences(benthocc)

## ---- echo=FALSE---------------------------------------------------------
data(benthoccras)

## ----eval=FALSE----------------------------------------------------------
#  options(worldclimpath="c:/data/worldclim",
#          zomerpetpath="c:/data/zomer")
#  benthclim <- climate_presence("Eucalyptus benthamii", database="ALA",
#                                vars=c("pet","tavg","tmax"))

## ---- echo=FALSE---------------------------------------------------------
data(benthclim)


## ----echo=FALSE----------------------------------------------------------
knitr::kable(benthclim[1:6,1:8])

## ---- include=FALSE------------------------------------------------------
library(tidyr)
library(reshape2)

## ------------------------------------------------------------------------
library(tidyr)
library(reshape2)
benthlong <- benthclim %>%
  dplyr::select(-PET) %>%
  melt(id.vars=c("species","longitude","latitude")) %>%
  separate("variable", c("variable","month")) %>% 
  spread(variable, value)

head(benthlong)

## ---- include=FALSE------------------------------------------------------
library(ggplot2)

## ------------------------------------------------------------------------
library(ggplot2)
ggplot(benthlong, aes(x=as.numeric(as.character(month)), y=tavg, group=interaction(latitude, longitude))) +
  geom_line() + 
  theme_minimal() + 
  labs(x="Month", y="Average temperature (deg C)") + 
  scale_x_continuous(breaks=1:12)

## ---- include=FALSE------------------------------------------------------
library(magrittr)

## ---- eval=FALSE---------------------------------------------------------
#  # Only first 6 rows shown.
#  # annualize_clim renames tavg into MAT (mean annual temperature),
#  # and prec into MAP (mean annual precipitation)
#  library(magrittr)
#  benthclim %>% annualize_clim

## ---- echo=FALSE---------------------------------------------------------
library(magrittr)
benthclim %>% annualize_clim %>% head

## ---- eval=TRUE----------------------------------------------------------
benthclim %>% annualize_clim %>% aggregate_clim

## ---- eval=TRUE----------------------------------------------------------
benthclim %>% annualize_clim %>% aggregate_clim(., funs=c("min", "median", "max") )

