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

bereken_vvi <- function(point,
                        dist,
                        obs_height,
                        resolution,
                        spacing,
                        output_type) {

  bbox_buffer <- point %>% st_buffer(dist = dist + 10) %>% st_bbox()

  # use get_coverage_wcs() from inbospatial not from source wfs_scs.R!
  dsm_r1 <- inbospatial::get_coverage_wcs(wcs = "dsm",
                                          bbox = bbox_buffer,
                                          layername = "EL.GridCoverage.DSM",
                                          resolution = resolution)
  dtm_r1 <- inbospatial::get_coverage_wcs(wcs = "dtm",
                                          bbox = bbox_buffer,
                                          layername = "EL.GridCoverage.DTM",
                                          resolution = resolution)

  poly25 <- point %>% st_buffer(dist = 25)

  vvi_from_sf(
    observer = poly25,
    spacing = spacing,
    cores = 1,
    progress = TRUE,
    max_distance = dist,
    dsm_rast = dsm_r1,
    dtm_rast = dtm_r1,
    observer_height = obs_height,
    raster_res = resolution,
    output_type = output_type)
}

add_visibility_to_frame <- function(punten_sf,
                                    resolution,
                                    spacing,
                                    viewshed_dist = 300,
                                    observer_dist = 25,
                                    obs_height = 1.7) {

  filename_dsm <- "DHMVIIDSMRAS5m.tif"
  file_dsm <- file.path(mbag_dir, "data", "dem", filename_dsm)
  dsm <- terra::rast(file_dsm)
  crs(dsm) <- "epsg:31370"

  filename_dtm <- "DHMVIIDTMRAS5m.tif"
  file_dtm <- file.path(mbag_dir, "data", "dem", filename_dtm)
  dtm <- terra::rast(file_dtm)
  crs(dtm) <- "epsg:31370"

  vvi_dm <- vvi_from_sf(
    observer = punten_sf %>% st_buffer(dist = observer_dist),
    spacing = spacing,
    cores = 1,
    progress = TRUE,
    max_distance = viewshed_dist,
    dsm_rast = dsm,
    dtm_rast = dtm,
    observer_height = obs_height,
    raster_res = resolution,
    output_type = "cumulative",
    by_row = TRUE)

  punten_sf$cvvi <- vvi_dm$cvvi

  return(punten_sf)
}

filter_zichtbaarheid <- function(sample, min_cvvi,
                                 resolution,
                                 spacing,
                                 viewshed_dist = 300,
                                 observer_dist = 25,
                                 obs_height = 1.7) {

  plus_visibility  <- add_visibility_to_frame(
      punten_sf = sample,
      resolution = resolution,
      spacing = spacing,
      viewshed_dist = viewshed_dist,
      observer_dist = observer_dist,
      obs_height = obs_height)

  plus_visibility %>%
    filter(cvvi >= min_cvvi)
}

output_finaal <- function(files, write_out) {
  if (write_out) {
    fs::dir_create("output")
    for (i in seq_along(files)) {
      name <- names(files)[i]
      object <- files[[i]]

      qs::qsave(object, file = paste0("output/", name))
    }
  }
  return(write_out)
}
