# to prepare the SSN files, I use the R package openSTARS, see 
# Kattwinkel M, Szöcs E, Peterson E, Schäfer RB (2020) Preparing GIS data for analysis of stream monitoring data: The R package openSTARS. PLoS ONE 15(9): e0239237. https://doi.org/10.1371/journal.pone.0239237

# to install the package, follow instructions at https://github.com/MiKatt/openSTARS
# the YouTube tutorial https://www.youtube.com/watch?v=eKFeO1Dhvt8&t=22s by Matthew Bayly also helped

# the lines of code below need to be executed from a RStudio session opened from GRASS, to do so
# type in 
# >> open /Users/oliviergimenez/Dropbox/OG/GITHUB/otter-crayfish-model/otter-crayfish-model.Rproj in the GRASS Terminal
# if needed, use sudo chmod -755 /Users/oliviergimenez/Dropbox/OG/GITHUB/otter-crayfish-model/otter-crayfish-model.Rproj


library(raster)
library(tidyverse)
library(sf)
library(openSTARS)

#----- 1. get DEM 

## Elevation (merci Maëlis !)

# Download BDalti at https://geoservices.ign.fr/bdalti
# Load BDalti for each department
# Elevation <- raster::raster("data/shp/BDALTI/BDALTIV2_2-0_25M_ASC_LAMB93-IGN69_D012_2022-09-29/BDALTIV2/1_DONNEES_LIVRAISON_2023-01-00224/BDALTIV2_MNT_25M_ASC_LAMB93_IGN69_D012/BDALTIV2_25M_FXX_0600_6375_MNT_LAMB93_IGN69.asc") %>%
#   
#   raster::merge(raster::raster("data/shp/BDALTI/BDALTIV2_2-0_25M_ASC_LAMB93-IGN69_D012_2022-09-29/BDALTIV2/1_DONNEES_LIVRAISON_2023-01-00224/BDALTIV2_MNT_25M_ASC_LAMB93_IGN69_D012/BDALTIV2_25M_FXX_0600_6400_MNT_LAMB93_IGN69.asc")) %>%
#   
#   raster::merge(raster::raster("data/shp/BDALTI/BDALTIV2_2-0_25M_ASC_LAMB93-IGN69_D012_2022-09-29/BDALTIV2/1_DONNEES_LIVRAISON_2023-01-00224/BDALTIV2_MNT_25M_ASC_LAMB93_IGN69_D012/BDALTIV2_25M_FXX_0625_6350_MNT_LAMB93_IGN69.asc")) %>%
#   
#   raster::merge(raster::raster("data/shp/BDALTI/BDALTIV2_2-0_25M_ASC_LAMB93-IGN69_D012_2022-09-29/BDALTIV2/1_DONNEES_LIVRAISON_2023-01-00224/BDALTIV2_MNT_25M_ASC_LAMB93_IGN69_D012/BDALTIV2_25M_FXX_0625_6375_MNT_LAMB93_IGN69.asc")) %>%
#   
#   raster::merge(raster::raster("data/shp/BDALTI/BDALTIV2_2-0_25M_ASC_LAMB93-IGN69_D012_2022-09-29/BDALTIV2/1_DONNEES_LIVRAISON_2023-01-00224/BDALTIV2_MNT_25M_ASC_LAMB93_IGN69_D012/BDALTIV2_25M_FXX_0625_6400_MNT_LAMB93_IGN69.asc")) %>%
#   
#   raster::merge(raster::raster("data/shp/BDALTI/BDALTIV2_2-0_25M_ASC_LAMB93-IGN69_D012_2022-09-29/BDALTIV2/1_DONNEES_LIVRAISON_2023-01-00224/BDALTIV2_MNT_25M_ASC_LAMB93_IGN69_D012/BDALTIV2_25M_FXX_0650_6300_MNT_LAMB93_IGN69.asc")) %>%
#   
#   raster::merge(raster::raster("data/shp/BDALTI/BDALTIV2_2-0_25M_ASC_LAMB93-IGN69_D012_2022-09-29/BDALTIV2/1_DONNEES_LIVRAISON_2023-01-00224/BDALTIV2_MNT_25M_ASC_LAMB93_IGN69_D012/BDALTIV2_25M_FXX_0650_6325_MNT_LAMB93_IGN69.asc")) %>%
#   
#   raster::merge(raster::raster("data/shp/BDALTI/BDALTIV2_2-0_25M_ASC_LAMB93-IGN69_D012_2022-09-29/BDALTIV2/1_DONNEES_LIVRAISON_2023-01-00224/BDALTIV2_MNT_25M_ASC_LAMB93_IGN69_D012/BDALTIV2_25M_FXX_0650_6350_MNT_LAMB93_IGN69.asc")) %>%
#   
#   raster::merge(raster::raster("data/shp/BDALTI/BDALTIV2_2-0_25M_ASC_LAMB93-IGN69_D012_2022-09-29/BDALTIV2/1_DONNEES_LIVRAISON_2023-01-00224/BDALTIV2_MNT_25M_ASC_LAMB93_IGN69_D012/BDALTIV2_25M_FXX_0650_6375_MNT_LAMB93_IGN69.asc")) %>%
#   
#   raster::merge(raster::raster("data/shp/BDALTI/BDALTIV2_2-0_25M_ASC_LAMB93-IGN69_D012_2022-09-29/BDALTIV2/1_DONNEES_LIVRAISON_2023-01-00224/BDALTIV2_MNT_25M_ASC_LAMB93_IGN69_D012/BDALTIV2_25M_FXX_0650_6400_MNT_LAMB93_IGN69.asc")) %>%
#   
#   raster::merge(raster::raster("data/shp/BDALTI/BDALTIV2_2-0_25M_ASC_LAMB93-IGN69_D012_2022-09-29/BDALTIV2/1_DONNEES_LIVRAISON_2023-01-00224/BDALTIV2_MNT_25M_ASC_LAMB93_IGN69_D012/BDALTIV2_25M_FXX_0650_6425_MNT_LAMB93_IGN69.asc")) %>%
#   
#   raster::merge(raster::raster("data/shp/BDALTI/BDALTIV2_2-0_25M_ASC_LAMB93-IGN69_D012_2022-09-29/BDALTIV2/1_DONNEES_LIVRAISON_2023-01-00224/BDALTIV2_MNT_25M_ASC_LAMB93_IGN69_D012/BDALTIV2_25M_FXX_0675_6300_MNT_LAMB93_IGN69.asc")) %>%
#   
#   raster::merge(raster::raster("data/shp/BDALTI/BDALTIV2_2-0_25M_ASC_LAMB93-IGN69_D012_2022-09-29/BDALTIV2/1_DONNEES_LIVRAISON_2023-01-00224/BDALTIV2_MNT_25M_ASC_LAMB93_IGN69_D012/BDALTIV2_25M_FXX_0675_6325_MNT_LAMB93_IGN69.asc")) %>%
#   
#   raster::merge(raster::raster("data/shp/BDALTI/BDALTIV2_2-0_25M_ASC_LAMB93-IGN69_D012_2022-09-29/BDALTIV2/1_DONNEES_LIVRAISON_2023-01-00224/BDALTIV2_MNT_25M_ASC_LAMB93_IGN69_D012/BDALTIV2_25M_FXX_0675_6350_MNT_LAMB93_IGN69.asc")) %>%
#   
#   raster::merge(raster::raster("data/shp/BDALTI/BDALTIV2_2-0_25M_ASC_LAMB93-IGN69_D012_2022-09-29/BDALTIV2/1_DONNEES_LIVRAISON_2023-01-00224/BDALTIV2_MNT_25M_ASC_LAMB93_IGN69_D012/BDALTIV2_25M_FXX_0675_6375_MNT_LAMB93_IGN69.asc")) %>%
#   
#   raster::merge(raster::raster("data/shp/BDALTI/BDALTIV2_2-0_25M_ASC_LAMB93-IGN69_D012_2022-09-29/BDALTIV2/1_DONNEES_LIVRAISON_2023-01-00224/BDALTIV2_MNT_25M_ASC_LAMB93_IGN69_D012/BDALTIV2_25M_FXX_0675_6400_MNT_LAMB93_IGN69.asc")) %>%
#   
#   raster::merge(raster::raster("data/shp/BDALTI/BDALTIV2_2-0_25M_ASC_LAMB93-IGN69_D012_2022-09-29/BDALTIV2/1_DONNEES_LIVRAISON_2023-01-00224/BDALTIV2_MNT_25M_ASC_LAMB93_IGN69_D012/BDALTIV2_25M_FXX_0675_6425_MNT_LAMB93_IGN69.asc")) %>%
#   
#   raster::merge(raster::raster("data/shp/BDALTI/BDALTIV2_2-0_25M_ASC_LAMB93-IGN69_D012_2022-09-29/BDALTIV2/1_DONNEES_LIVRAISON_2023-01-00224/BDALTIV2_MNT_25M_ASC_LAMB93_IGN69_D012/BDALTIV2_25M_FXX_0675_6450_MNT_LAMB93_IGN69.asc")) %>%
#   
#   raster::merge(raster::raster("data/shp/BDALTI/BDALTIV2_2-0_25M_ASC_LAMB93-IGN69_D012_2022-09-29/BDALTIV2/1_DONNEES_LIVRAISON_2023-01-00224/BDALTIV2_MNT_25M_ASC_LAMB93_IGN69_D012/BDALTIV2_25M_FXX_0700_6300_MNT_LAMB93_IGN69.asc")) %>%
#   
#   raster::merge(raster::raster("data/shp/BDALTI/BDALTIV2_2-0_25M_ASC_LAMB93-IGN69_D012_2022-09-29/BDALTIV2/1_DONNEES_LIVRAISON_2023-01-00224/BDALTIV2_MNT_25M_ASC_LAMB93_IGN69_D012/BDALTIV2_25M_FXX_0700_6325_MNT_LAMB93_IGN69.asc")) %>%
#   
#   raster::merge(raster::raster("data/shp/BDALTI/BDALTIV2_2-0_25M_ASC_LAMB93-IGN69_D012_2022-09-29/BDALTIV2/1_DONNEES_LIVRAISON_2023-01-00224/BDALTIV2_MNT_25M_ASC_LAMB93_IGN69_D012/BDALTIV2_25M_FXX_0700_6350_MNT_LAMB93_IGN69.asc")) %>%
#   
#   raster::merge(raster::raster("data/shp/BDALTI/BDALTIV2_2-0_25M_ASC_LAMB93-IGN69_D012_2022-09-29/BDALTIV2/1_DONNEES_LIVRAISON_2023-01-00224/BDALTIV2_MNT_25M_ASC_LAMB93_IGN69_D012/BDALTIV2_25M_FXX_0700_6375_MNT_LAMB93_IGN69.asc")) %>%
#   
#   raster::merge(raster::raster("data/shp/BDALTI/BDALTIV2_2-0_25M_ASC_LAMB93-IGN69_D012_2022-09-29/BDALTIV2/1_DONNEES_LIVRAISON_2023-01-00224/BDALTIV2_MNT_25M_ASC_LAMB93_IGN69_D012/BDALTIV2_25M_FXX_0700_6400_MNT_LAMB93_IGN69.asc")) %>%
#   
#   raster::merge(raster::raster("data/shp/BDALTI/BDALTIV2_2-0_25M_ASC_LAMB93-IGN69_D012_2022-09-29/BDALTIV2/1_DONNEES_LIVRAISON_2023-01-00224/BDALTIV2_MNT_25M_ASC_LAMB93_IGN69_D012/BDALTIV2_25M_FXX_0725_6325_MNT_LAMB93_IGN69.asc")) %>%
#   
#   raster::merge(raster::raster("data/shp/BDALTI/BDALTIV2_2-0_25M_ASC_LAMB93-IGN69_D012_2022-09-29/BDALTIV2/1_DONNEES_LIVRAISON_2023-01-00224/BDALTIV2_MNT_25M_ASC_LAMB93_IGN69_D012/BDALTIV2_25M_FXX_0725_6350_MNT_LAMB93_IGN69.asc")) %>%
#   
#   raster::merge(raster::raster("data/shp/BDALTI/BDALTIV2_2-0_25M_ASC_LAMB93-IGN69_D046_2019-12-10/BDALTIV2/1_DONNEES_LIVRAISON_2021-10-00008/BDALTIV2_MNT_25M_ASC_LAMB93_IGN69_D046/BDALTIV2_25M_FXX_0525_6375_MNT_LAMB93_IGN69.asc")) %>%
#   
#   raster::merge(raster::raster("data/shp/BDALTI/BDALTIV2_2-0_25M_ASC_LAMB93-IGN69_D046_2019-12-10/BDALTIV2/1_DONNEES_LIVRAISON_2021-10-00008/BDALTIV2_MNT_25M_ASC_LAMB93_IGN69_D046/BDALTIV2_25M_FXX_0525_6400_MNT_LAMB93_IGN69.asc")) %>%
#   
#   raster::merge(raster::raster("data/shp/BDALTI/BDALTIV2_2-0_25M_ASC_LAMB93-IGN69_D046_2019-12-10/BDALTIV2/1_DONNEES_LIVRAISON_2021-10-00008/BDALTIV2_MNT_25M_ASC_LAMB93_IGN69_D046/BDALTIV2_25M_FXX_0550_6350_MNT_LAMB93_IGN69.asc")) %>%
#   
#   raster::merge(raster::raster("data/shp/BDALTI/BDALTIV2_2-0_25M_ASC_LAMB93-IGN69_D046_2019-12-10/BDALTIV2/1_DONNEES_LIVRAISON_2021-10-00008/BDALTIV2_MNT_25M_ASC_LAMB93_IGN69_D046/BDALTIV2_25M_FXX_0550_6375_MNT_LAMB93_IGN69.asc")) %>%
#   
#   raster::merge(raster::raster("data/shp/BDALTI/BDALTIV2_2-0_25M_ASC_LAMB93-IGN69_D046_2019-12-10/BDALTIV2/1_DONNEES_LIVRAISON_2021-10-00008/BDALTIV2_MNT_25M_ASC_LAMB93_IGN69_D046/BDALTIV2_25M_FXX_0550_6400_MNT_LAMB93_IGN69.asc")) %>%
#   
#   raster::merge(raster::raster("data/shp/BDALTI/BDALTIV2_2-0_25M_ASC_LAMB93-IGN69_D046_2019-12-10/BDALTIV2/1_DONNEES_LIVRAISON_2021-10-00008/BDALTIV2_MNT_25M_ASC_LAMB93_IGN69_D046/BDALTIV2_25M_FXX_0550_6425_MNT_LAMB93_IGN69.asc")) %>%
#   
#   raster::merge(raster::raster("data/shp/BDALTI/BDALTIV2_2-0_25M_ASC_LAMB93-IGN69_D046_2019-12-10/BDALTIV2/1_DONNEES_LIVRAISON_2021-10-00008/BDALTIV2_MNT_25M_ASC_LAMB93_IGN69_D046/BDALTIV2_25M_FXX_0550_6450_MNT_LAMB93_IGN69.asc")) %>%
#   
#   raster::merge(raster::raster("data/shp/BDALTI/BDALTIV2_2-0_25M_ASC_LAMB93-IGN69_D046_2019-12-10/BDALTIV2/1_DONNEES_LIVRAISON_2021-10-00008/BDALTIV2_MNT_25M_ASC_LAMB93_IGN69_D046/BDALTIV2_25M_FXX_0575_6350_MNT_LAMB93_IGN69.asc")) %>%
#   
#   raster::merge(raster::raster("data/shp/BDALTI/BDALTIV2_2-0_25M_ASC_LAMB93-IGN69_D046_2019-12-10/BDALTIV2/1_DONNEES_LIVRAISON_2021-10-00008/BDALTIV2_MNT_25M_ASC_LAMB93_IGN69_D046/BDALTIV2_25M_FXX_0575_6375_MNT_LAMB93_IGN69.asc")) %>%
#   
#   raster::merge(raster::raster("data/shp/BDALTI/BDALTIV2_2-0_25M_ASC_LAMB93-IGN69_D046_2019-12-10/BDALTIV2/1_DONNEES_LIVRAISON_2021-10-00008/BDALTIV2_MNT_25M_ASC_LAMB93_IGN69_D046/BDALTIV2_25M_FXX_0575_6400_MNT_LAMB93_IGN69.asc")) %>%
#   
#   raster::merge(raster::raster("data/shp/BDALTI/BDALTIV2_2-0_25M_ASC_LAMB93-IGN69_D046_2019-12-10/BDALTIV2/1_DONNEES_LIVRAISON_2021-10-00008/BDALTIV2_MNT_25M_ASC_LAMB93_IGN69_D046/BDALTIV2_25M_FXX_0575_6425_MNT_LAMB93_IGN69.asc")) %>%
#   
#   raster::merge(raster::raster("data/shp/BDALTI/BDALTIV2_2-0_25M_ASC_LAMB93-IGN69_D046_2019-12-10/BDALTIV2/1_DONNEES_LIVRAISON_2021-10-00008/BDALTIV2_MNT_25M_ASC_LAMB93_IGN69_D046/BDALTIV2_25M_FXX_0575_6450_MNT_LAMB93_IGN69.asc")) %>%
#   
#   raster::merge(raster::raster("data/shp/BDALTI/BDALTIV2_2-0_25M_ASC_LAMB93-IGN69_D046_2019-12-10/BDALTIV2/1_DONNEES_LIVRAISON_2021-10-00008/BDALTIV2_MNT_25M_ASC_LAMB93_IGN69_D046/BDALTIV2_25M_FXX_0600_6425_MNT_LAMB93_IGN69.asc")) %>%
#   
#   raster::merge(raster::raster("data/shp/BDALTI/BDALTIV2_2-0_25M_ASC_LAMB93-IGN69_D046_2019-12-10/BDALTIV2/1_DONNEES_LIVRAISON_2021-10-00008/BDALTIV2_MNT_25M_ASC_LAMB93_IGN69_D046/BDALTIV2_25M_FXX_0600_6450_MNT_LAMB93_IGN69.asc")) %>%
#   
#   raster::merge(raster::raster("data/shp/BDALTI/BDALTIV2_2-0_25M_ASC_LAMB93-IGN69_D046_2019-12-10/BDALTIV2/1_DONNEES_LIVRAISON_2021-10-00008/BDALTIV2_MNT_25M_ASC_LAMB93_IGN69_D046/BDALTIV2_25M_FXX_0625_6425_MNT_LAMB93_IGN69.asc")) %>%
#   
#   raster::merge(raster::raster("data/shp/BDALTI/BDALTIV2_2-0_25M_ASC_LAMB93-IGN69_D046_2019-12-10/BDALTIV2/1_DONNEES_LIVRAISON_2021-10-00008/BDALTIV2_MNT_25M_ASC_LAMB93_IGN69_D046/BDALTIV2_25M_FXX_0625_6450_MNT_LAMB93_IGN69.asc")) %>%
#   
#   raster::merge(raster::raster("data/shp/BDALTI/BDALTIV2_2-0_25M_ASC_LAMB93-IGN69_D082_2021-02-11/BDALTIV2/1_DONNEES_LIVRAISON_2023-02-00010/BDALTIV2_MNT_25M_ASC_LAMB93_IGN69_D082/BDALTIV2_25M_FXX_0600_6350_MNT_LAMB93_IGN69.asc")) %>%
#   
#   raster::merge(raster::raster("data/shp/BDALTI/BDALTIV2_2-0_25M_ASC_LAMB93-IGN69_D081_2022-07-29/BDALTIV2/1_DONNEES_LIVRAISON_2022-08-00118/BDALTIV2_MNT_25M_ASC_LAMB93_IGN69_D081/BDALTIV2_25M_FXX_0625_6325_MNT_LAMB93_IGN69.asc"))
# 
# raster::crs(Elevation) <- sp::CRS("EPSG:2154")
# 
# save(Elevation, file = "data/ElevationMassifCentral.RData")

load('data/ElevationMassifCentral.RData')
Elevation 
plot(Elevation)

## reduce raster size
Elevation <-  aggregate(Elevation, fact = 3) 
Elevation
#dev.off()
plot(Elevation)

raster::writeRaster(x = Elevation,
                    filename = "data/elevation.tif",
                    overwrite = TRUE)

dem <- raster::raster("data/elevation.tif")
plot(dem)

#----- 2. get occupancy data 

data <- read.csv(file = "data/data8_XY_EPSG27572.csv", 
                 sep = ";", 
                 header = TRUE) 
y <- as.matrix(data[,2:9]) # subset presence absence data
obs <- data[,10:17] # subset the observer responsible of each monitoring /!\ this data can not be used to retrieve the effort
an <- data[,18:25] # subset 
siteCovs <- data[,26:103]

# Activity (visited or not) of each site at each secondary sampling occasion
sampling <- y 
sampling[which(y == 0)] <- 1 # NA the site is not visited / 1 the site was visited

# Create a table that store the number of visits and detection per primary occasion
sites <- data %>%
  dplyr::select(ID, xcoord, ycoord, DPTS, 
                obs_2003_p1,obs_2003_p2, obs_2003_p3, obs_2003_p4,
                obs_1_2011, obs_2_2011, obs_3_2011, obs_4_2011,
                P100ZTCUL,ZT200_popK,RANG_RGE, RANG_RGEc, DENS_reso,
                CONTACT_1_2003,CONTACT_2_2003,CONTACT_3_2003,CONTACT_4_2003,
                cont_1_2011,cont_2_2011,cont_3_2011,cont_4_2011,
                Reg_Hydro) %>%
  sf::st_as_sf(coords = c("xcoord", "ycoord"), crs = 27572) %>%
  #sf::st_transform(crs = 4326) %>% # transform crs to WGS84
  dplyr::mutate(nb_dets = rowSums(y, na.rm = TRUE)) %>% # count the number of detection in total
  dplyr::mutate(nb_dets_2003 = rowSums(y[,1:4], na.rm = TRUE)) %>% # count the number of detection in 2003
  dplyr::mutate(nb_dets_2011 = rowSums(y[,5:8], na.rm = TRUE)) %>% # count the number of detection in 2011
  dplyr::mutate(nb_visits = rowSums(sampling, na.rm = TRUE)) %>% # count the number of visits in total
  dplyr::mutate(nb_visits_2003 = rowSums(sampling[,1:4], na.rm = TRUE)) %>% # count the number of visits in 2003
  dplyr::mutate(nb_visits_2011 = rowSums(sampling[,5:8], na.rm = TRUE)) # count the number of visits in 2011

str(sites)
#plot(sites)
#mapview::mapview(sites, zcol = "nb_dets")

# Subset sites: Remove sites that were sampled in 2003 and 2011, or sites in the Pyrenees, like in Maelis paper
# could go for all 2003 sites but quite a big dataset w/ > 900 sites
subset <- which(sites$nb_visits_2003 > 0 & sites$nb_visits_2011 > 0 & sites$DPTS != 9 & sites$DPTS != 11 & sites$DPTS != 31 & sites$DPTS != 32 & sites$DPTS != 40 & sites$DPTS != 64 & sites$DPTS != 65)
sites_occ <- sites[subset,] # sites 

str(sites_occ)
dim(sites_occ)

y <- y[subset,] # capture history

# mapview::mapview(sites_occ, zcol = "nb_visits")
#mapview::mapview(sites_occ, zcol = "nb_visits_2003")
# mapview::mapview(sites_occ, zcol = "nb_visits_2011")
# 
# mapview::mapview(sites_occ, zcol = "nb_dets")
#mapview::mapview(sites_occ, zcol = "nb_dets_2003")
# mapview::mapview(sites_occ, zcol = "nb_dets_2011")

# Put NA to observers when the sites was not visited
for(i in 1:8){
  sites_occ[which(is.na(y[,i])), i+2] <- NA}

#sf::st_coordinates(sites_occ)

# # Look at the data 
# M <- nrow(y) # number of sites 
# J <- 4 # number of survey
# T <- 2 # number of season
# y1 <- array(y, dim = c(M,J,T))
# # number of sites surveyed
# dim(y1)
# 
# # Number of sites where otters were detected at least once
# dim(y1)[1] - which(apply(y1, 1, sum, na.rm=TRUE) == 0) %>% length()
# 
# # number of sites where otters was detected at least once for each sampling sessions
# dim(y1)[1] - which(apply(y1[,,1],1,sum, na.rm = TRUE) == 0) %>% length()
# dim(y1)[1] - which(apply(y1[,,2],1,sum, na.rm = TRUE) == 0) %>% length()
# 
# # data for 2003
# y1[,,1]

# are coordinates of DEM and occupancy in same system?
st_crs(dem) == st_crs(sites_occ)

# transform occupancy site coord into DEM coord
sites_occ <- sites_occ %>% st_transform(crs = st_crs(dem))
st_crs(dem) == st_crs(sites_occ)

# check that all points are in raster
# 1. extract raster values for the points
extracted_values <- raster::extract(dem, sites_occ)
# 2. check if all points have non-NA values (indicating they belong to the raster)
all_points_belong_to_raster <- all(!is.na(extracted_values))
all_points_belong_to_raster

# now plot all points on raster
plot(dem)
points(st_coordinates(sites_occ), col = "red")

st_write(sites_occ, "data/shp/sites.shp")

# yeah!!



# w/ unmarked
library(unmarked)
y2003 <- cbind(sites_occ$CONTACT_1_2003,
               sites_occ$CONTACT_2_2003,
               sites_occ$CONTACT_3_2003,
               sites_occ$CONTACT_4_2003)

# add some fake covariates for illustration
site.covs <- data.frame(P100ZTCUL = sites_occ$P100ZTCUL,
                        ZT200_popK = sites_occ$ZT200_popK,
                        RANG_RGE = sites_occ$RANG_RGE,
                        RANG_RGEq = sites_occ$RANG_RGEc,
                        DENS_reso = sites_occ$DENS_reso,
                        Reg_Hydro = sites_occ$Reg_Hydro)

# observation covariates are in site-major, observation-minor order
obs.covs <- list(obs = cbind(sites_occ$obs_2003_p1,
                             sites_occ$obs_2003_p2, 
                             sites_occ$obs_2003_p3, 
                             sites_occ$obs_2003_p4))
umf <- unmarkedFrameOccu(y=y2003, siteCovs=site.covs, 
                         obsCovs=obs.covs)   # organize data
umf                     # look at data
summary(umf)            # summarize      
plot(umf)

(fm <- occu(~ obs + RANG_RGE + RANG_RGEq ~ scale(P100ZTCUL) + RANG_RGE + scale(log(ZT200_popK + 1)), umf))

# 
# Occupancy:
#   Estimate    SE      z P(>|z|)
# (Intercept)                   0.846 0.712  1.188  0.2348
# scale(P100ZTCUL)             -0.929 0.298 -3.121  0.0018
# RANG_RGE                     -0.329 0.493 -0.668  0.5044
# scale(log(ZT200_popK + 1))   -0.181 0.268 -0.675  0.4999
# 
# Detection:
#   Estimate    SE      z  P(>|z|)
# (Intercept)    1.039 1.661  0.626 5.31e-01
# obsONCFS      -1.493 0.359 -4.163 3.15e-05
# obsPNR        -2.113 0.698 -3.028 2.46e-03
# RANG_RGE       0.350 2.127  0.164 8.69e-01
# RANG_RGEq     -0.256 0.542 -0.472 6.37e-01


#----- 3. get river streams

#Download BDtopo at https://geoservices.ign.fr/bdtopo
#Load BDTopo for each department
path <- '/Users/oliviergimenez/Dropbox/OG/GITHUB/otter-crayfish-model/'
Ariege <- sf::st_read(paste0(path,"data/shp/BDTOPO/BDTOPOARIEGE/1_DONNEES_LIVRAISON_2022-12-00159/BDT_3-3_SHP_LAMB93_D009-ED2022-12-15/HYDROGRAPHIE/COURS_D_EAU.shp"))
Aude <- sf::st_read(paste0(path,"data/shp/BDTOPO/BDTOPOAUDE/1_DONNEES_LIVRAISON_2022-12-00159/BDT_3-3_SHP_LAMB93_D011-ED2022-12-15/HYDROGRAPHIE/COURS_D_EAU.shp"))
Aveyron <- sf::st_read(paste0(path,"data/shp/BDTOPO/BDTOPOAVEYRON/1_DONNEES_LIVRAISON_2022-12-00159/BDT_3-3_SHP_LAMB93_D012-ED2022-12-15/HYDROGRAPHIE/COURS_D_EAU.shp"))
Cantal <-  sf::st_read(paste0(path,"data/shp/BDTOPO/BDTOPOCANTAL/1_DONNEES_LIVRAISON_2022-12-00159/BDT_3-3_SHP_LAMB93_D015-ED2022-12-15/HYDROGRAPHIE/COURS_D_EAU.shp"))
Correze <- sf::st_read(paste0(path,"data/shp/BDTOPO/BDTOPOCORREZE/1_DONNEES_LIVRAISON_2022-12-00159/BDT_3-3_SHP_LAMB93_D019-ED2022-12-15/HYDROGRAPHIE/COURS_D_EAU.shp"))
Dordogne <- sf::st_read(paste0(path,"data/shp/BDTOPO/BDTOPODORDOGNE/1_DONNEES_LIVRAISON_2022-12-00159/BDT_3-3_SHP_LAMB93_D024-ED2022-12-15/HYDROGRAPHIE/COURS_D_EAU.shp"))
Gard <- sf::st_read(paste0(path,"data/shp/BDTOPO/BDTOPOGARD/1_DONNEES_LIVRAISON_2022-12-00159/BDT_3-3_SHP_LAMB93_D030-ED2022-12-15/HYDROGRAPHIE/COURS_D_EAU.shp"))
Gers <- sf::st_read(paste0(path,"data/shp/BDTOPO/BDTOPOGERS/1_DONNEES_LIVRAISON_2022-12-00159/BDT_3-3_SHP_LAMB93_D032-ED2022-12-15/HYDROGRAPHIE/COURS_D_EAU.shp"))
HauteGaronne <- sf::st_read(paste0(path,"data/shp/BDTOPO/BDTOPOHAUTEGARONNE/1_DONNEES_LIVRAISON_2022-12-00159/BDT_3-3_SHP_LAMB93_D031-ED2022-12-15/HYDROGRAPHIE/COURS_D_EAU.shp"))
HautesPyrenees <- sf::st_read(paste0(path,"data/shp/BDTOPO/BDTOPOHAUTESPYRENEES/1_DONNEES_LIVRAISON_2022-12-00159/BDT_3-3_SHP_LAMB93_D065-ED2022-12-15/HYDROGRAPHIE/COURS_D_EAU.shp"))
Herault <- sf::st_read(paste0(path,"data/shp/BDTOPO/BDTOPOHERAULT/1_DONNEES_LIVRAISON_2022-12-00159/BDT_3-3_SHP_LAMB93_D034-ED2022-12-15/HYDROGRAPHIE/COURS_D_EAU.shp"))
Landes <- sf::st_read(paste0(path,"data/shp/BDTOPO/BDTOPOLANDES/1_DONNEES_LIVRAISON_2022-12-00159/BDT_3-3_SHP_LAMB93_D040-ED2022-12-15/HYDROGRAPHIE/COURS_D_EAU.shp"))
Lot <- sf::st_read(paste0(path,"data/shp/BDTOPO/BDTOPOLOT/1_DONNEES_LIVRAISON_2022-12-00159/BDT_3-3_SHP_LAMB93_D046-ED2022-12-15/HYDROGRAPHIE/COURS_D_EAU.shp"))
LotetGaronne <- sf::st_read(paste0(path,"data/shp/BDTOPO/BDTOPOLOTETGARONNE/1_DONNEES_LIVRAISON_2022-12-00159/BDT_3-3_SHP_LAMB93_D047-ED2022-12-15/HYDROGRAPHIE/COURS_D_EAU.shp"))
Lozere <- sf::st_read(paste0(path,"data/shp/BDTOPO/BDTOPOLOZERE/1_DONNEES_LIVRAISON_2022-12-00159/BDT_3-3_SHP_LAMB93_D048-ED2022-12-15/HYDROGRAPHIE/COURS_D_EAU.shp"))
PyrennesAtlantiques <- sf::st_read(paste0(path,"data/shp/BDTOPO/BDTOPOPA/1_DONNEES_LIVRAISON_2022-12-00159/BDT_3-3_SHP_LAMB93_D064-ED2022-12-15/HYDROGRAPHIE/COURS_D_EAU.shp"))
PyreneesOrientales <- sf::st_read(paste0(path,"data/shp/BDTOPO/BDTOPOPO/1_DONNEES_LIVRAISON_2022-12-00159/BDT_3-3_SHP_LAMB93_D066-ED2022-12-15/HYDROGRAPHIE/COURS_D_EAU.shp"))
Tarn <- sf::st_read(paste0(path,"data/shp/BDTOPO/BDTOPOTARN/1_DONNEES_LIVRAISON_2022-12-00159/BDT_3-3_SHP_LAMB93_D081-ED2022-12-15/HYDROGRAPHIE/COURS_D_EAU.shp"))
TarnetGaronne <- sf::st_read(paste0(path,"data/shp/BDTOPO/BDTOPOTARNETGARONNE/1_DONNEES_LIVRAISON_2022-12-00159/BDT_3-3_SHP_LAMB93_D082-ED2022-12-15/HYDROGRAPHIE/COURS_D_EAU.shp"))
Gironde <- sf::st_read(paste0(path,"data/shp/BDTOPO/BDTOPOGIRONDE/1_DONNEES_LIVRAISON_2022-12-00159/BDT_3-3_SHP_LAMB93_D033-ED2022-12-15/HYDROGRAPHIE/COURS_D_EAU.shp"))

# Bind river
River_lines <- Ariege %>%
  rbind(Aude) %>%
  rbind(Aveyron) %>%
  rbind(Cantal) %>%
  rbind(Correze) %>%
  rbind(Dordogne) %>%
  rbind(Gard) %>%
  rbind(Gers) %>%
  rbind(HauteGaronne) %>%
  rbind(HautesPyrenees) %>%
  rbind(Herault) %>%
  rbind(Landes) %>%
  rbind(Lot) %>%
  rbind(LotetGaronne) %>%
  rbind(Lozere) %>%
  rbind(PyrennesAtlantiques) %>%
  rbind(PyreneesOrientales) %>%
  rbind(Tarn) %>%
  rbind(TarnetGaronne) %>%
  rbind(Gironde) %>%
  sf::st_transform(crs = st_crs(dem))

st_crs(River_lines) == st_crs(sites_occ)

#mapview::mapview(River_lines) +
#  mapview::mapview(sites_occ)

st_write(River_lines, "data/shp/streams.shp")



#----- 4. go for openSTARS

# give paths to GRASS and where to store the GRASS data base
# Linux e.g.
grass_program_path <- "/Applications/GRASS-8.3.app/Contents/Resources"

working_dir <- file.path(tempdir(), "grass_workflow")
grass_db_path <- file.path(working_dir, "grassDB")
dir.create(working_dir)
setwd(tempdir())


# specify the path to the digital elevation model
dem_path <- "/Users/oliviergimenez/Dropbox/OG/GITHUB/otter-crayfish-model/data/elevation.tif"
setup_grass_environment(dem = dem_path, 
                        gisBase = grass_program_path,
                        gisDbase = grass_db_path,
                        #location = "nc_openSTARS", # use working_dir directory
                        remove_GISRC = TRUE,
                        override = TRUE
)

gmeta()

# observation sites
sites_path <- "/Users/oliviergimenez/Dropbox/OG/GITHUB/otter-crayfish-model/data/shp/sites.shp"

# existing stream network
streams_path <- "/Users/oliviergimenez/Dropbox/OG/GITHUB/otter-crayfish-model/data/shp/streams.shp"

import_data(dem = dem_path, sites = sites_path, streams = streams_path)


#The DEM is loaded into the GRASS database as a raster map named dem, 
#the sites as a vector map named sites_o and the (optional) stream 
#network as a vector map named streams_o. 

# Next, the streams must be derived from the DEM.
derive_streams()

# library(sp)
# dem <- readRAST("dem", ignore.stderr = TRUE)
# sites <- readVECT("sites_o", ignore.stderr = TRUE)
# streams <- readVECT("streams_v", ignore.stderr = TRUE)
# plot(dem, col = terrain.colors(20))
# lines(streams, col = "blue", lwd = 0.1)
# points(sites, pch = 16, col = "red")


# Check and correct the network
cp <- check_compl_confluences()
if (cp)
  correct_compl_confluences()

# Prepare edges
calc_edges()

# Prepare sites
calc_sites()

dem <- readRAST("dem", ignore.stderr = TRUE)
sites <- readVECT("sites", ignore.stderr = TRUE)
sites_orig <- readVECT("sites_o", ignore.stderr = TRUE)
edges <- readVECT("edges", ignore.stderr = TRUE)
plot(dem, col = terrain.colors(20))
lines(edges, col = "blue")
points(sites_orig, pch = 20, col = "black")
points(sites, pch = 21, cex=0.75, bg = "grey")
legend("topright", 
       y = par("usr")[3]*1.01, 
       col = 1, 
       pt.bg = "grey", 
       pch = c(21, 19), 
       legend = c("snapped sites", "original sites"), 
       ncol = 2)

# snapped sites and original sites are the same, cool, i can use covariates 
# calculated on original sites

dim(sites)
sites

head(edges@data)
tail(edges@data)


# Write all files to an ssn folder
ssn_dir <- file.path(tempdir(), 'nc.ssn')
export_ssn(ssn_dir)
list.files(ssn_dir)


# quit RStudio and start from scratch, and not from GRASS
# import
ssn_dir <- 'data/nc.ssn'
ssn_obj <- SSN::importSSN(ssn_dir, o.write = TRUE)

#mf04p <- SSN2::ssn_import(ssn_dir)
#SSN2::ssn_create_distmat(mf04p, overwrite = TRUE)

#mf04p <- SSN::additive.function(ssn_obj, "H2OArea", "areaAFV")

#dev.off()
plot(ssn_obj)

names(ssn_obj@data)
names(ssn_obj)

# compute weights based on reach contributing area
ssn_obj <- SSN::additive.function(ssn_obj, "rcaArea", "computed.afv")

# Create Distance Matrix
SSN::createDistMat(ssn_obj, predpts = NULL, o.write=TRUE)
dmats <- SSN::getStreamDistMat(ssn_obj)

str(dmats)

simDFobs <- SSN::getSSNdata.frame(ssn_obj, "Obs")
net_num <- 108
## create matrix of distances/weights
net <- net_num # network to use in the object
addfunccol <- 'computed.afv' # additive function column used for tail-up spatial weights
path <- ssn_dir

names(ssn_obj@data)
names(ssn_obj)


#mat_all <- SSNbayes::dist_weight_mat(path = path, net = net, addfunccol = "computed.afv")


# go through source code
library(tidyverse)
library(sf)
pid <- NULL
n <- SSN2::ssn_import(path, overwrite = TRUE)
obs_data <- SSN2::ssn_get_data(n, name = "obs")
if (is(n) == "SSN") {
  xy <- st_coordinates(obs_data)
  colnames(xy) <- c("NEAR_X", "NEAR_Y")
  obs_data <- cbind(obs_data, xy)
}
obs_data <- obs_data %>% as.data.frame()
net
list.files(paste0(path,"/distance/obs"))


# D <- readRDS(paste0(path, "/distance/obs/dist.net", 107, 
#                     ".RData"))
# row.names(D)

#  [1] "1"  "2"  "3"  "4"  "5"  "6"  "7"  "8"  "9"  "10" "11" "12" "13" "14" "15" "16" "17" "18" "19" "20" "21" "22" "23" "24"

D <- readRDS(paste0(path, "/distance/obs/dist.net", 108, 
                    ".RData"))
row.names(D)

#  [1] "25" "26" "27" "28" "29" "30" "31" "32" "33" "34" "35" "36" "37" "38" "39" "40" "41" "42" "43" "44" "45" "46" "47" "48" "49" "50" "51" "52" "53" "54" "55" "56" "57" "58" "59"
#[36] "60" "61" "62" "63" "64" "65" "66" "67" "68" "69" "70" "71" "73" "74" "75" "76" "77" "78" "79" "80" "81"

# D <- readRDS(paste0(path, "/distance/obs/dist.net", 3, 
#                     ".RData"))
# row.names(D)
# # attention, 72 is missing
# 
# D <- readRDS(paste0(path, "/distance/obs/dist.net", 85, 
#                     ".RData"))
# row.names(D)
# # "82"
# 
# D <- readRDS(paste0(path, "/distance/obs/dist.net", 91, 
#                     ".RData"))
# row.names(D)
# # "83"
# 
# D <- readRDS(paste0(path, "/distance/obs/dist.net", 92, 
#                     ".RData"))
# row.names(D)
# # "84" "85"
# D <- readRDS(paste0(path, "/distance/obs/dist.net", 105, 
#                     ".RData"))
# row.names(D)
# 
# #[1] "86"  "87"  "88"  "89"  "90"  "91"  "92"  "93"  "94"  "95"  "96"  "97"  "98"  "99"  "100" "101" "102" "103" "104" "105" "106" "107" "108" "109" "110" "111" "112" "113" "114"
# #[30] "115" "116" "117" "118" "119" "120" "121" "122" "123" "124" "125" "126" "127" "128" "129" "130" "131" "132" "133" "134"
# 
# D <- readRDS(paste0(path, "/distance/obs/dist.net", 81, 
#                     ".RData"))
# row.names(D)
# #  [1] "135" "136" "137" "138" "139" "140" "141" "142" "143" "144" "145" "146" "147" "148" "149" "150" "151" "152" "153" "154" "155" "156" "157" "158"
# 
# 
# sites <- sf::st_read("data/shp/sites.shp")
# dem <- raster::raster("data/elevation.tif")
# raster::plot(dem)
# points(sf::st_coordinates(sites),pch = 20, col = "black")
# points(sf::st_coordinates(sites)[1:24,],pch = 21, cex=0.75, bg = "grey")
# points(sf::st_coordinates(sites)[c(25:71,73:81),],pch = 21, cex=0.75, bg = "green")
# points(sf::st_coordinates(sites)[72,],pch = 21, cex=0.75, bg = "blue")
# points(sf::st_coordinates(sites)[82,],pch = 21, cex=0.75, bg = "red")
# points(sf::st_coordinates(sites)[83,],pch = 21, cex=0.75, bg = "yellow")
# points(sf::st_coordinates(sites)[84:85,],pch = 21, cex=0.75, bg = "pink")
# points(sf::st_coordinates(sites)[86:134,],pch = 21, cex=0.75, bg = "purple")
# points(sf::st_coordinates(sites)[135:158,],pch = 21, cex=0.75, bg = "orange")
# 
# length(c(25:71,73:81)) ############
# length(86:134)
# length(135:158)
# 
# library(openSTARS)
# rgrass7::use_sp()
# dem <- readRAST("dem", ignore.stderr = TRUE)
# sites <- readVECT("sites", ignore.stderr = TRUE)
# sites_orig <- readVECT("sites_o", ignore.stderr = TRUE)
# edges <- readVECT("edges", ignore.stderr = TRUE)
# plot(dem, col = terrain.colors(20))
# lines(edges, col = "blue")
# points(sites_orig, pch = 20, col = "black")
# points(sites, pch = 21, cex=0.75, bg = "grey")
# 
# 
# 
# ssn_obj <- SSN::additive.function(ssn_obj, "rcaArea", "computed.afv")


H <- D + base::t(D)
mask <- c(25:71,73:81)
obs_data <- obs_data[mask,]
obs_data$computed.afv <- ssn_obj@data$computed.afv[mask]
obs_data <- dplyr::filter(obs_data, pid %in% colnames(H)) %>% 
  as.data.frame()
afv <- obs_data[c("locID", addfunccol)] %>% distinct()
nsofar <- 0
dist.junc <- matrix(0, nrow = length(afv[, 1]), ncol = length(afv[, 
                                                                  1]))
distmat <- D
ni <- length(distmat[1, ])
ordpi <- order(as.numeric(rownames(distmat)))
dist.junc[(nsofar + 1):(nsofar + ni), (nsofar + 1):(nsofar + 
                                                      ni)] <- distmat[ordpi, ordpi, drop = FALSE]
b.mat <- pmin(dist.junc, base::t(dist.junc))
dist.hydro <- as.matrix(dist.junc + base::t(dist.junc))
flow.con.mat <- 1 - (b.mat > 0) * 1
n.all <- ni
w.matrix <- sqrt(pmin(outer(afv[, addfunccol], rep(1, times = n.all)), 
                      base::t(outer(afv[, addfunccol], rep(1, times = n.all))))/pmax(outer(afv[, 
                                                                                               addfunccol], rep(1, times = n.all)), base::t(outer(afv[, 
                                                                                                                                                      addfunccol], rep(1, times = n.all))))) * flow.con.mat
obs_data$point <- "Obs"
obs_data$coords.x1 <- obs_data$NEAR_X
obs_data$coords.x2 <- obs_data$NEAR_Y
coor <- obs_data[, c("NEAR_X", "NEAR_Y")]
e <- coor %>% dist(., method = "euclidean", diag = FALSE, 
                   upper = FALSE) %>% as.matrix()
mat_all <- list(e = e, D = D, H = H, w.matrix = w.matrix, flow.con.mat = flow.con.mat)

# data
(nsite <- nrow(obs_data))
(flow_con_mat <- mat_all$flow.con.mat) #flow connected matrix
(D <- mat_all$D) #downstream hydro distance matrix
(h <- mat_all$H) # total stream distance
(alpha_max <- 4 * max(mat_all$H))

save(mat_all, file = "data/mat_all.RData")




