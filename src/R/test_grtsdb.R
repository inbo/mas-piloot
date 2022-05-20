library(grtsdb)
library(ggplot2)
library(dplyr)
library(sf)

plot_sample <- function(sample) {
  sample %>%
    count(x1c, x2c) %>%
    st_as_sf(coords = c('x1c', 'x2c'), crs = 31370) %>%
    ggplot() +
    geom_sf(aes(size = n))}

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
          grtsdb = connect_db(here::here("data/c-mon/grts.sqlite")),
          level = 9
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

plot_sample(test_40m)
head(test_40m)


library(grtsdb)
samplesize  <- 100
bbox <- rbind(x = c(22000, 258880), y = c(153050, 244030))
db2 <- connect_db(file.path("~", "Downloads", "test.sqlite"))
test <- extract_sample(
  grtsdb = db2, samplesize = samplesize, bbox = bbox, cellsize = 500
)
plot(test[, 1:2], asp = 1)
test <- extract_sample(
  grtsdb = db2, samplesize = samplesize, bbox = bbox, cellsize = 2000
)
plot(test[, 1:2], asp = 1)
compact_db(db2)
test <- extract_sample(
  grtsdb = db2, samplesize = samplesize, bbox = bbox, cellsize = 500
)
plot(test[, 1:2], asp = 1)
test <- extract_sample(
  grtsdb = db2, samplesize = samplesize, bbox = bbox, cellsize = 2000
)
plot(test[, 1:2], asp = 1)
test <- extract_sample(
  grtsdb = db2, samplesize = samplesize, bbox = bbox, cellsize = 200
)
plot(test[, 1:2], asp = 1)

