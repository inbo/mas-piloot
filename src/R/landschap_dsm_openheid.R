library(tidyverse)
library(sf)
library(terra)
library(here)

beheergebieden <- read_sf("data/bo_vlm/Beheergebied.shp")
akkervogelgebieden <- read_sf("data/bo_vlm/akkervogelgebieden.shp")
bbox <- round(st_bbox(beheergebieden))
bbox <- bbox[c("xmin", "xmax", "ymin", "ymax")]

vlaanderen_dtm1 <- rast("S:/Vlaanderen/Hoogte/DHMVII/DHMVIIDTMRAS1m.tif")
vlaanderen_dtm5 <- aggregate(vlaanderen_dtm1, fact = 5, fun = "mean")
writeRaster(x = vlaanderen_dtm5, filename = "data/processed/vlaanderen_dtm5.tif")
vlaanderen_dsm5 <- rast("S:/Vlaanderen/Hoogte/DHMVII/DHMVIIDSMRAS5m.tif")
vlaanderen_chm5 <- vlaanderen_dsm5 - vlaanderen_dtm5
writeRaster(x = vlaanderen_chm5,
            filename = "data/dem/vlaanderen_chm5.tif")


# vlaanderen_chm25 <- aggregate(vlaanderen_chm5, fact = 5, fun = "mean")
# writeRaster(x = vlaanderen_chm25,
#             filename = "data/dem/vlaanderen_chm25.tif")
#
#
# circle1000 <- focalMat(vlaanderen_chm25,
#                        d = 1000,
#                        type = "circle",
#                        fillNA = TRUE)
#
# vlaanderen_chm25_c1000_mean <- terra::focal(
#   vlaanderen_chm25,
#   w = circle1000,
#   fun = "sum",
#   na.rm = TRUE,
#   filename = "data/dem/vlaanderen_chm25_c1000_mean.tif")
#
# circle1000[circle1000 > 0] <- 1
# vlaanderen_chm25_c1000_sd <- terra::focal(
#   vlaanderen_chm25,
#   w = circle1000,
#   fun = "sd",
#   na.rm = TRUE,
#   filename = "data/dem/vlaanderen_chm25_c1000_sd.tif")
#
#
# vlaanderen_chm25_c1000_sd <- rast("data/dem/vlaanderen_chm25_c1000_sd.tif")
# vlaanderen_chm25_c1000_mean <- rast("data/dem/vlaanderen_chm25_c1000_mean.tif")
#
# hist(vlaanderen_chm25_c1000_sd)
# hist(vlaanderen_chm25_c1000_mean)
#
# plot(vlaanderen_chm25_c1000_sd)
# plot(vlaanderen_chm25_c1000_mean)
#
# meanvalues <- values(vlaanderen_chm25_c1000_mean)
# meanvalues <- meanvalues[!is.na(meanvalues)]
#
# sdvalues <- values(vlaanderen_chm25_c1000_sd)
# sdvalues <- sdvalues[!is.na(meanvalues)]
#
# indices <- sample(1:length(meanvalues), size = 50000)
#
#
# ggplot() +
#   geom_point(aes(x = meanvalues[indices], y = sdvalues[indices]), alpha = 0.1)
#
#
# rclasmat <- c(0, 2, 1,
#               2, 6, 2,
#               6, +Inf, 3) %>%
#   matrix(ncol = 3, byrow = TRUE)
#
# vlaanderen_landschapstype <- terra::classify(
#   vlaanderen_chm25_c1000_sd,
#   rcl = rclasmat,
#   include.lowest = TRUE,
#   filename = "data/dem/vlaanderen_landschapstype.tif",
#   overwrite = TRUE)
#
# vlaanderen_landschapstype[vlaanderen_chm25_c1000_mean > 8] <- NA
#
#
# my_palette <- RColorBrewer::brewer.pal(n = 3, name = "Dark2")
# vlaanderen_landschapstype_raster <- raster::raster(vlaanderen_landschapstype)
# vlaanderen_landschapstype_raster <- raster::ratify(vlaanderen_landschapstype_raster)
# levels(vlaanderen_landschapstype_raster)[[1]]$LT <- c("OL", "HOL", "KL")
#
# vlaanderen_landschapstype_raster %>%
#   mapview::mapview(col.regions = my_palette, att = "LT", alpha = 0.3)
#
# vlaanderen_chm25_c1000_mean %>%
#   raster::raster() %>%
#   mapview::mapview(alpha = 0.3)
#

# skyview / topographic openness?
library(qgisprocess)
View(qgis_algorithms())

result <- qgis_run_algorithm(
  "saga:topographicopenness",
  distance_units = "meters",
  area_units = "m2",
  DEM = vlaanderen_dsm5,
  POS = here::here("data", "dem", "openness300m_dsm_res5_vlaanderen.tif"),
  RADIUS = 300,
  METHOD = 1,
  DLEVEL = 3,
  NDIRS = 8,
  UNIT = 0,
  NADIR = TRUE)

# If you chose positive openness, the angle is measured from zenith.
# Flat area will return 1.57 (radians) = 90°
# Smaller angles = less open
# Larger angles = elevated position in landscape
top_open_300m <- terra::rast(
  here::here("data", "dem", "openness300m_dsm_res5_vlaanderen.tif"))

# aggregate to 25 m resolution
top_open_300m_res25 <- aggregate(
  top_open_300m, fact = 5,
  filename = here::here("data", "dem", "openness300m_dsm_res25_vlaanderen.tif"))

top_open_300m_res25 <-  terra::rast(
  here::here("data", "dem", "openness300m_dsm_res25_vlaanderen.tif"))


# next calculated average openness in 1000m radius landscape
circle1000 <- focalMat(top_open_300m_res25,
                       d = 1000,
                       type = "circle",
                       fillNA = TRUE)


vlaanderen_top_open_300m_c1000_mean_res25 <- terra::focal(
  top_open_300m_res25,
  w = circle1000,
  fun = "sum",
  na.rm = TRUE,
  filename = "data/dem/openness300m_dsm_res25_c1000_mean_vlaanderen.tif",
  overwrite = TRUE)


rclasmat <- c(0, 1.3, 1,
              1.3, 1.45, 2,
              1.45, +Inf, 3) %>%
  matrix(ncol = 3, byrow = TRUE)

vlaanderen_openheid_landschap <- terra::classify(
  vlaanderen_top_open_300m_c1000_mean_res25,
  rcl = rclasmat,
  include.lowest = TRUE,
  filename = here(
    "data", "dem",
    "openness300m_dsm_res25_c1000_mean_vlaanderen_classified.tif"),
  overwrite = TRUE)

my_palette <- RColorBrewer::brewer.pal(n = 3, name = "Dark2")
vlaanderen_openheid_landschap_raster <- raster::raster(vlaanderen_openheid_landschap)
vlaanderen_openheid_landschap_raster <- raster::ratify(vlaanderen_openheid_landschap_raster)
levels(vlaanderen_openheid_landschap_raster)[[1]]$LT <- c("GL", "HOL", "OL")


vlaanderen_openheid_landschap_raster %>%
  mapview::mapview(alpha = 0.3)
