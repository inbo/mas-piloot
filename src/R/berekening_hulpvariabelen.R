path_to_bo <- function(jaar = 2021) {
  file_name <- paste("BO", jaar, "VLM_EXTERN.shp", sep = "_")
  file.path(mbag_dir, "data", "bo_vlm", file_name)
}

path_to_bo2021 <- function() {
  file.path(mbag_dir, "data", "bo_vlm", "BO_2021_VLM_EXTERN.shp")
}

read_bo <- function(path) {
  aes <- sf::st_read(dsn = path) %>%
    st_transform(crs = 31370)
  return(aes)
}

add_bo2021_to_frame <- function(
  punten_df,
  path_bo
  ) {

  bo2021 <- read_bo(path = path_bo)

  points_bo2021 <- landusemetrics_grid_cell(
    grid_cell = punten_df %>%
      st_buffer(dist = 300),
    layer = bo2021,
    grid_group_by_col = "pointid",
    layer_group_by_col = "SRT_OBJECT")

  bo_maatregelen <- bo2021 %>%
    st_drop_geometry() %>%
    distinct(BH_DOELST, SRT_OBJECT, EENHEID)

  aandeel_sb <- points_bo2021 %>%
    select(pointid, SRT_OBJECT, area_prop) %>%
    left_join(bo_maatregelen, by = "SRT_OBJECT") %>%
    filter(BH_DOELST == "soortenbescherming (SB)") %>%
    group_by(pointid) %>%
    summarise(area_prop_sb = sum(area_prop))

  punten <- punten_df %>%
    left_join(aandeel_sb,
              by = "pointid") %>%
    mutate(area_prop_sb = ifelse(is.na(area_prop_sb), 0, area_prop_sb))

  return(punten)
}

add_bo_to_frame <- function(
    punten_df,
    path_bo
    ) {

  bo_layer <- read_bo(path = path_bo)

  points_bo <- landusemetrics_grid_cell(
    grid_cell = punten_df %>%
      st_buffer(dist = 300),
    layer = bo_layer,
    grid_group_by_col = "pointid",
    layer_group_by_col = "SRT_OBJECT")

  bo_maatregelen <- bo_layer %>%
    st_drop_geometry() %>%
    distinct(BH_DOELST, SRT_OBJECT, EENHEID)

  aandeel_sb <- points_bo %>%
    select(pointid, SRT_OBJECT, area_prop) %>%
    left_join(bo_maatregelen, by = "SRT_OBJECT") %>%
    filter(BH_DOELST == "soortenbescherming (SB)") %>%
    group_by(pointid) %>%
    summarise(area_prop_sb = sum(area_prop))

  punten <- punten_df %>%
    left_join(aandeel_sb,
              by = "pointid") %>%
    mutate(area_prop_sb = ifelse(is.na(area_prop_sb), 0, area_prop_sb))

  return(punten)
}


path_to_lbg <- function(jaar = 2020) {
  file.path(mbag_dir, "data", "landbouwgebruikspercelen", "parquet",
            paste0("lbgbrprc", jaar, ".parquet"))
}

calc_lbg <- function(path,
                     punten_sf) {
  lbg_binding <- arrow::open_dataset(path)

  points_lbg <- landusemetrics_grid_cell(
    grid_cell = punten_sf %>%
      st_buffer(dist = 300),
    layer = lbg_binding %>%
      select(LBLHFDTLT, geometry) %>%
      sfarrow::read_sf_dataset() %>%
      st_transform(31370),
    grid_group_by_col = "pointid",
    layer_group_by_col = "LBLHFDTLT")

  mapping <- lbg_binding %>%
    select(GEWASGROEP, LBLHFDTLT) %>%
    collect() %>%
    distinct()

  points_lbg <- points_lbg %>%
    left_join(mapping)

  return(points_lbg)
}


path_to_openheid_landschap <- function() {
  file.path(mbag_dir,
    "data", "dem",
    "openness300m_chm_res25_c300_mean_vlaanderen.tif")
}

add_openheid_landschap_to_frame <- function(
  path,
  punten_sf,
  gebied,
  cutlevels = c(1.25, 1.35, 1.51),
  class_labels = c("GL", "HGL", "HOL", "OL")) {

  openheid <- rast(path)
  openheid <- crop(openheid, gebied)

  matvec <- c(0, rep(cutlevels, each = 2), +Inf)
  nclass <- length(matvec) / 2

  rclasmat <- matvec %>%
    matrix(ncol = 2, byrow = TRUE)
  rclasmat <- cbind(rclasmat, 1:nclass)

  openheid_klassen <- terra::classify(
    openheid,
    rcl = rclasmat,
    include.lowest = TRUE)

  openheid_classes_points <- terra::extract(
    x = openheid_klassen,
    y = vect(punten_sf))

  openheid_values_points <- terra::extract(
    x = openheid,
    y = vect(punten_sf))

  punten_sf <- punten_sf %>%
    bind_cols(openheid_classes_points %>%
                dplyr::select(openheid_klasse = focal_sum),
              openheid_values_points %>%
                dplyr::select(openheid_waarde = focal_sum)
    ) %>%
    mutate(openheid_klasse = factor(openheid_klasse,
                                    levels = 1:nclass,
                                    labels = class_labels))
  return(punten_sf)
}


path_to_sbp_akkervogels <- function(file = "akkervogelgebieden.shp") {
  file.path(mbag_dir, "data", "bo_vlm", file)
}

read_sbp_akkervogels <- function(
  path,
  gebied) {
  if (gebied$Naam == "De Moeren") {
    out <- st_read(path) %>%
      st_transform(crs = 31370) %>%
      filter(OBJECTID == 44) %>%
      st_intersection(gebied)
  } else {
    out <- st_read(path) %>%
      st_transform(crs = 31370) %>%
      filter(Prioriteit != "Zoekzone") %>%
      st_intersection(gebied)
  }

  if (nrow(out) >= 1) {
    out <- out %>%
      st_union() %>%
      st_buffer(dist = 20) %>%
      st_simplify(dTolerance = 10) %>%
      st_remove_holes() %>%
      st_as_sf() %>%
      mutate(Naam = gebied$Naam) %>%
      rename(geometry = x)
  }
  return(out)
}

add_stratum_sbp <- function(punten_sf, sbp) {

  telpunten <- punten_sf %>%
    mutate(is_sbp = st_intersects(.,
                                  sbp,
                                  sparse = FALSE) %>%
             as.logical()
    )

  return(telpunten)
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
                                    dist = 300,
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
    observer = punten_sf %>% st_buffer(dist = 25),
    spacing = spacing,
    cores = 1,
    progress = TRUE,
    max_distance = dist,
    dsm_rast = dsm,
    dtm_rast = dtm,
    observer_height = obs_height,
    raster_res = resolution,
    output_type = "cumulative",
    by_row = TRUE)

  punten_sf$cvvi <- vvi_dm$cvvi

  return(punten_sf)
}

