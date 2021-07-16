library(grtsdb)
library(ggplot2)
library(dplyr)
library(sf)

plot_sample <- function(sample) {
  sample %>%
    st_as_sf(coords = c('x1c', 'x2c'), crs = 31370) %>%
    ggplot() +
    geom_sf()}

samplesize  <- 100
bbox <- rbind(x = c(22000, 258880), y = c(153050, 244030))

test_10m <- extract_sample(
  grtsdb = connect_db(here::here("data/c-mon/grts.sqlite")),
  samplesize = samplesize,
  bbox = bbox,
  cellsize = 10)

plot_sample(test_10m) # looks OK

add_level(bbox = bbox,
          cellsize = 640,
          grtsdb = connect_db(here::here("data/c-mon/grts.sqlite"))
          )
# adds levels 14 (20 m), 13, ..., 9 (640 m) to the database

test_20m <- extract_sample(
  grtsdb = connect_db(here::here("data/c-mon/grts.sqlite")),
  samplesize = samplesize,
  bbox = bbox,
  cellsize = 20
)

plot_sample(test_20m) #not correct
head(test_20m)

test_40m <- extract_sample(
  grtsdb = connect_db(here::here("data/c-mon/grts.sqlite")),
  samplesize = samplesize,
  bbox = bbox,
  cellsize = 40
)

head(test_40m) # empty, also for all coarser resolutions
