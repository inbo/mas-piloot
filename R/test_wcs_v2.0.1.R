#test wcs
source("R/wfs_wcs.R")
library(httr)
library(terra)
library(sf)

point <- st_as_sf(data.frame(x = 180000, y = 195000),
                  coords = c("x", "y"),
                  crs = 31370)
dist <- 300
bbox_buffer <- point %>% st_buffer(dist = dist + 10) %>% st_bbox()
bbox_buffer <- round(bbox_buffer[c("xmin", "xmax", "ymin", "ymax")])

v1 <- get_coverage_wcs(
  wcs = "dsm",
  bbox = bbox_buffer,
  layername = "EL.GridCoverage.DSM",
  resolution = 1,
  version = "1.0.0")
v1
plot(v1)

v2 <- get_coverage_wcs(
  wcs = "dsm",
  bbox = bbox_buffer,
  layername = "EL.GridCoverage.DSM",
  resolution = 1,
  version = "2.0.1")
v2
plot(v2)


