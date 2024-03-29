library(tidyverse)
library(sf)
library(terra)
library(here)
library(qgisprocess)

vlaanderen_chm5 <- rast(
  "data/dem/vlaanderen_chm5.tif")

vlaanderen_chm5[vlaanderen_chm5 < 0] <- 0

result <- qgis_run_algorithm(
  "saga:topographicopenness",
  DEM = vlaanderen_chm5,
  POS = here::here("data", "dem", "openness300m_chm_res5_vlaanderen.tif"),
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
  here::here("data", "dem", "openness300m_chm_res5_vlaanderen.tif"))

# aggregate to 25 m resolution
top_open_300m_res25 <- aggregate(
  top_open_300m, fact = 5,
  filename = here::here("data", "dem", "openness300m_chm_res25_vlaanderen.tif"))

top_open_300m_res25 <-  terra::rast(
  here::here("data", "dem", "openness300m_chm_res25_vlaanderen.tif"))


# # next calculated average openness in 1000m radius landscape
# circle1000 <- focalMat(top_open_300m_res25,
#                        d = 1000,
#                        type = "circle",
#                        fillNA = TRUE)
#
#
# openness300m_chm_res25_c1000_mean_vlaanderen <- terra::focal(
#   top_open_300m_res25,
#   w = circle1000,
#   fun = "sum",
#   na.rm = TRUE,
#   filename = here("data", "dem",
#                   "openness300m_chm_res25_c1000_mean_vlaanderen.tif"),
#   overwrite = FALSE)
#

# next calculated average openness in 300m radius landscape
circle300 <- focalMat(top_open_300m_res25,
                       d = 300,
                       type = "circle",
                       fillNA = TRUE)


openness300m_chm_res25_c300_mean_vlaanderen <- terra::focal(
  top_open_300m_res25,
  w = circle300,
  fun = "sum",
  na.rm = TRUE,
  filename = here("data", "dem",
                  "openness300m_chm_res25_c300_mean_vlaanderen.tif"),
  overwrite = FALSE)



openness300m_chm_res25_c300_mean_vlaanderen <-
  rast(
    here("data", "dem",
         "openness300m_chm_res25_c300_mean_vlaanderen.tif")
  )

rclasmat <- c(0, 1.25, 1,
              1.25, 1.35, 2,
              1.35, 1.51, 3,
              1.51, +Inf, 4) %>%
  matrix(ncol = 3, byrow = TRUE)

# vlaanderen_openheid_landschap <- terra::classify(
#   openness300m_chm_res25_c1000_mean_vlaanderen,
#   rcl = rclasmat,
#   include.lowest = TRUE,
#   filename = here("data", "dem",
#                   "openness300m_chm_res25_c1000_mean_vlaanderen_classified.tif"),
#   overwrite = TRUE)

vlaanderen_openheid_landschap <- terra::classify(
  openness300m_chm_res25_c300_mean_vlaanderen,
  rcl = rclasmat,
  include.lowest = TRUE,
  filename = here("data", "dem",
                  "openness300m_chm_res25_c300_mean_vlaanderen_classified.tif"),
  overwrite = TRUE)


my_palette <- RColorBrewer::brewer.pal(n = 4, name = "Dark2")
vlaanderen_openheid_landschap_raster <- raster::raster(
  vlaanderen_openheid_landschap)
vlaanderen_openheid_landschap_raster <- raster::ratify(
  vlaanderen_openheid_landschap_raster)
levels(vlaanderen_openheid_landschap_raster)[[1]]$LT <-
  c("GL", "HGL", "HOL", "OL")


vlaanderen_openheid_landschap_raster %>%
  mapview::mapview(alpha = 0.3)
