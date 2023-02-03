steekproef_uitdunnen <- function(
  steekproef_sf) {
  te_verwijderen <- steekproef_sf %>%
    filter(!is.na(nn_index)) %>%
    arrange(nn_distance, batch, sample_order) %>%
    group_by(nn_distance) %>%
    slice_tail() %>%
    st_drop_geometry()

  out <- steekproef_sf %>% anti_join(te_verwijderen)
  return(out)
}

nn_steekproef <- function(sample,
                          max_dist) {
  nn <- nngeo::st_nn(sample, sample,
                     maxdist = max_dist,
                     sparse = TRUE,
                     k = 2,
                     returnDist = TRUE)
  index <- vapply(nn$nn,
                  FUN = function(x) ifelse(length(x) > 1, x[2], NA),
                  FUN.VALUE = c(1))
  distance <- vapply(nn$dist,
                     FUN = function(x) ifelse(length(x) > 1, x[2], NA),
                     FUN.VALUE = c(1))
  nn_result <- sample %>%
    mutate(nn_index = index,
           nn_distance = distance)

  uitdunnen <- steekproef_uitdunnen(nn_result)

  return(uitdunnen)
}


bereken_zichtbaarheid <- function(point,
                       dist = 300,
                       obs_height = 1.7,
                       resolution = 1) {

  bbox_buffer <- point %>% st_buffer(dist = dist + 20) %>% st_bbox()
  bbox_buffer <- bbox_buffer[c("xmin", "xmax", "ymin", "ymax")]

  dsm_r1 <- get_coverage_wcs(wcs = "dsm",
                             bbox = bbox_buffer,
                             layername = "EL.GridCoverage.DSM",
                             resolution = resolution)
  dtm_r1 <- get_coverage_wcs(wcs = "dtm",
                             bbox = bbox_buffer,
                             layername = "EL.GridCoverage.DTM",
                             resolution = resolution)

  vis_prop <- GVI::visibility_proportion(
    observer = point,
    max_distance = dist,
    dsm_rast = dsm_r1,
    dtm_rast = dtm_r1,
    observer_height = obs_height,
    raster_res = resolution)

  out <- point %>%
    mutate(zichtbaarheid = vis_prop)

  return(out)
}

filter_zichtbaarheid <- function(sample, min_cvvi) {
  sample %>%
    filter(cvvi >= min_cvvi)
}

output_finaal <- function(files, write_out) {
  if (write_out) {
    for (i in seq_along(files)) {
      name <- names(files)[i]
      object <- files[[i]]

      qs::qsave(object, file = paste0("output/", name))
    }
  }
  return(write_out)
}
