st_bbox.SpatRaster = function(obj, ...) {
  bb = as.vector(terra::ext(obj))[c(1,3,2,4)]
  names(bb) = c("xmin", "ymin", "xmax", "ymax")
  st_bbox(bb, crs = st_crs(obj))
}

st_crs.SpatRaster = function(x, ...) {
  st_crs(crs(x))
}


path_to_perimeters <- function(file = "piloot_perimeters.gpkg") {
  file.path(mbag_dir, "data", "processed", file)
}

path_to_osm_download <- function() {
  file.path(osmextract::oe_download_directory(),
            "geofabrik_belgium-latest.osm.pbf")
}

path_to_lum <- function(jaar = 2019) {
  file.path(mbag_dir, "data", "landgebruik", "vito",
            paste0("lu_landgebruik_vlaa_", jaar, "_v2.tif")
            )
}

path_to_legend_lum <- function() {
  file.path(mbag_dir, "data", "landgebruik", "legende_landgebruik.csv")
}

read_legend_lum <- function(file) {
  read_csv2(
    file,
    col_types = cols(
      bron = col_character(),
      bestand_id = col_character(),
      value = col_double(),
      label = col_character(),
      kleur = col_character()
    ))
}

selectie_openheid <- function(gebied, ol_strata,
                              cutlevels = c(1.25, 1.35, 1.51),
                              class_labels = c("GL", "HGL", "HOL", "OL")) {
  # Lees openheid laag
  openheid <- rast(path_to_openheid_landschap()) %>%
    project("epsg:31370")

  openheid_gebied <- crop(openheid, st_buffer(gebied, 100))

  # Classify raster
  matvec <- c(0, rep(cutlevels, each = 2), +Inf)
  nclass <- length(matvec) / 2

  rclasmat <- matvec %>%
    matrix(ncol = 2, byrow = TRUE)
  rclasmat <- cbind(rclasmat, 1:nclass)

  openheid_gebied_klassen <- terra::classify(
    openheid_gebied,
    rcl = rclasmat,
    include.lowest = TRUE)
  levels(openheid_gebied_klassen) <- data.frame(1:4,
                                       openheid_klassen = class_labels)

  # Raster naar polygoon en selecteer openheid
  openheid_gebied_sf <- as.polygons(openheid_gebied_klassen) %>%
    st_as_sf() %>%
    filter(openheid_klassen %in% ol_strata)

  # Maak intersectie met perimeters
  openheid_gebied_intersect <- st_intersection(gebied, openheid_gebied_sf)
  out <- openheid_gebied_intersect %>%
    group_by(Naam, section) %>%
    summarise(.groups = "drop")

  return(out)
}

check_osm_data <- function(gebied, update_osm_layer) {
  # Download periferie van osm België
  provider_file <- file.path(osmextract::oe_download_directory(),
                             "belgium_periferie_osm.kml")

  if (!file.exists(provider_file) | update_osm_layer) {
    download.file("https://download.geofabrik.de/europe/belgium.kml",
                  provider_file)
  } else {
    message("The chosen file was already detected in the download directory. Skip downloading.")
  }
  provider_data <- st_read(provider_file, quiet = TRUE) %>%
    st_zm(drop = TRUE, what = "ZM")

  # Valt het gebied binnen de periferie van osm België?
  matched_zones = provider_data[st_transform(gebied,
   crs = sf::st_crs(provider_data)), op = sf::st_contains]
  if (nrow(matched_zones) != 0L) {
    osmextract::oe_download("https://download.geofabrik.de/europe/belgium-latest.osm.pbf",
                            force_download = update_osm_layer)
  } else {
    stop("Gebied valt buiten België!", call. = FALSE)
  }
}

exclusie_buffer_osm <- function(gebied, osmdata, buffer, layer, geom_type) {
  # which keys are present to exclude
  keys <- names(layer)
  selection <- paste(keys, collapse = ", ")

  # create list with exclusion strings per key
  exclusion_list <- lapply(seq_along(layer), function(i) {
      paste0(keys[[i]], " IN ('", paste(layer[[i]], collapse = "', '"), "')")
    })

  # collapse exclusion strings in list to a single where clause
  exclusion_str <- paste(unlist(exclusion_list), collapse = " OR ")

  buffer_exclusie_vectortranslate = c(
    "-t_srs", "EPSG:31370",
    "-select", selection,
    "-where", exclusion_str,
    "-nlt", "PROMOTE_TO_MULTI"
  )

  if (geom_type == "lines") {
    exclusie_landgebruik <- osmextract::oe_read(
      file_path = osmdata,
      layer = geom_type,
      download_directory = dirname(osmdata),
      vectortranslate_options = buffer_exclusie_vectortranslate,
      extra_tags = keys,
      boundary = gebied %>% st_buffer(buffer),
      boundary_type = "clipsrc") %>%
      st_buffer(buffer)
  }

  if (geom_type == "multipolygons") {
    exclusie_landgebruik <- osmextract::oe_read(
      file_path = osmdata,
      layer = geom_type,
      download_directory = dirname(osmdata),
      vectortranslate_options = buffer_exclusie_vectortranslate,
      boundary = gebied %>% st_buffer(buffer),
      boundary_type = "clipsrc") %>%
      st_buffer(buffer)
  }

  out <- exclusie_landgebruik %>%
    st_cast("MULTIPOLYGON") %>%
    st_cast("GEOMETRYCOLLECTION") %>%
    mutate(id = seq_len(nrow(.))) %>%
    st_collection_extract("POLYGON") %>%
    aggregate(list(.$id), first, do_union = FALSE) %>%
    select(-id, -Group.1) %>%
    as_tibble %>%
    st_as_sf() %>%
    st_union() %>%
    st_simplify(dTolerance = 10) %>%
    st_remove_holes() %>%
    st_as_sf() %>%
    mutate(Naam = gebied$Naam)

  return(out)
}

# Set landuse or leisure to NULL if you don't want to exclude from these
exclusie_landgebruik_osm <- function(gebied, osmdata,
   landuse = c('residential', 'military', 'industrial', 'cemetery'),
   leisure = c('park'),
   buffer_poly = NULL, layer_poly = NULL,
   buffer_line = NULL, layer_line = NULL,
   update_osm_layer) {

  # Controleer of gebied binnen osm België valt
  check_osm_data(gebied, update_osm_layer)

  # Create string to exclude landuse and leisure variables
  exclusion_landuse <- paste0("('", paste(landuse, collapse = "', '"), "')")
  exclusion_leisure <- paste0("('", paste(leisure, collapse = "', '"), "')")

  if (is.null(leisure)) {
    exclusion_str <- paste("landuse IN", exclusion_landuse, sep = " ")
  } else {
    exclusion_str <- paste("landuse IN", exclusion_landuse,
                           "OR leisure IN", exclusion_leisure, sep = " ")
  }

  landuse_exclusie_vectortranslate = c(
    "-t_srs", "EPSG:31370",
    "-select", "landuse",
    "-where", exclusion_str,
    "-nlt", "PROMOTE_TO_MULTI"
  )

  # Exclusie landgebruik
  exclusie_landgebruik <- osmextract::oe_read(
    file_path = osmdata,
    layer = "multipolygons",
    download_directory = dirname(osmdata),
    vectortranslate_options = landuse_exclusie_vectortranslate,
    boundary = gebied,
    boundary_type = "clipsrc")


  exclusie_landgebruik <- exclusie_landgebruik %>%
    st_cast("GEOMETRYCOLLECTION") %>%
    mutate(id = seq_len(nrow(.))) %>%
    st_collection_extract("POLYGON") %>%
    aggregate(list(.$id), first, do_union = FALSE) %>%
    select(-id, -Group.1) %>%
    as_tibble %>%
    st_as_sf() %>%
    st_union() %>%
    st_buffer(dist = 20) %>%
    st_simplify(dTolerance = 10) %>%
    st_remove_holes() %>%
    st_as_sf() %>%
    mutate(Naam = gebied$Naam)

  if (!is.null(layer_poly) & is.null(layer_line)) {
    exclude_buffer <- exclusie_buffer_osm(gebied, osmdata, buffer = buffer_poly,
      layer = layer_poly, geom_type = "multipolygons")

    out <- bind_rows(exclusie_landgebruik, exclude_buffer)
    out <- st_union(st_make_valid(out)) %>%
      st_as_sf() %>%
      select(x) %>%
      mutate(Naam = gebied$Naam)
  } else if (is.null(layer_poly) & !is.null(layer_line)) {
    exclude_buffer <- exclusie_buffer_osm(gebied, osmdata, buffer = buffer_line,
      layer = layer_line, geom_type = "lines")

    out <- bind_rows(exclusie_landgebruik, exclude_buffer)
    out <- st_union(st_make_valid(out)) %>%
      st_as_sf() %>%
      select(x) %>%
      mutate(Naam = gebied$Naam)
  } else if (!is.null(layer_poly) & !is.null(layer_line)) {
    exclude_buffer_poly <- exclusie_buffer_osm(gebied, osmdata,
      buffer = buffer_poly, layer = layer_poly, geom_type = "multipolygons")
    exclude_buffer_line <- exclusie_buffer_osm(gebied, osmdata,
      buffer = buffer_line, layer = layer_line, geom_type = "lines")
    exclude_buffer <- st_union(exclude_buffer_poly, exclude_buffer_line) %>%
      select(x) %>%
      mutate(Naam = gebied$Naam)

    out <- bind_rows(exclusie_landgebruik, exclude_buffer)
    out <- st_union(st_make_valid(out)) %>%
      st_as_sf() %>%
      select(x) %>%
      mutate(Naam = gebied$Naam)
  } else {
    out <- exclusie_landgebruik
  }

  return(out)
}

# Set waterway to NULL if you don't want to include any waterway
extract_osm_paden <- function(gebied, exclusie, osmdata,
  paths_include = c('track', 'footway', 'path', 'cycleway', 'bridleway',
                    'tertiary', 'tertiary_link', 'unclassified'),
  cutting_exclude = c('yes', 'both', 'hollow_way'),
  historic_exclude = c('hollow_way'),
  waterway = c('river', 'stream', 'tidal channel', 'canal', 'drain', 'ditch'),
  update_osm_layer) {

  # Controleer of gebied binnen osm België valt
  check_osm_data(gebied, update_osm_layer)

  # Create string include
  inclusion_paths <- paste0("('", paste(paths_include,
                                        collapse = "', '"), "')")
  exclusion_cutting <- paste0("('", paste(cutting_exclude,
                                        collapse = "', '"), "'))")
  exclusion_historic <- paste0("('", paste(historic_exclude,
                                        collapse = "', '"), "'))))")
  inclusion_waterway <- paste0("('", paste(waterway,
                                        collapse = "', '"), "'))")

  if (is.null(waterway) & is.null(historic_exclude) &
      is.null(cutting_exclude)) {

    inclusion_str <- paste("highway IN", inclusion_paths, sep = " ")

  } else if (is.null(waterway) & !is.null(historic_exclude) &
      !is.null(cutting_exclude)) {

    inclusion_str <- paste("(highway IN", inclusion_paths,
                           "AND NOT ((cutting IN ", exclusion_cutting,
                           "OR (historic IN", exclusion_historic,
                           sep = " ")

  } else if (!is.null(waterway) & is.null(historic_exclude) &
             is.null(cutting_exclude)) {

    inclusion_str <- paste("(highway IN", inclusion_paths,
                           "OR (waterway IN", inclusion_waterway, sep = " ")

  } else {
    inclusion_str <- paste("(highway IN", inclusion_paths,
                           "AND NOT ((cutting IN ", exclusion_cutting,
                           "OR (historic IN", exclusion_historic,
                           "OR (waterway IN", inclusion_waterway, sep = " ")
  }

  my_vectortranslate = c(
    "-t_srs", "EPSG:31370",
    "-select",
    "highway, waterway",
    "-where", inclusion_str,
    "-nlt", "PROMOTE_TO_MULTI"
  )

  # Read-in data
  paden <- osmextract::oe_read(
    file_path = osmdata,
    download_directory = dirname(osmdata),
    vectortranslate_options = my_vectortranslate,
    extra_tags = c("historic", "cutting"),
    boundary = gebied,
    boundary_type = "spat")

  paden <- paden %>%
    mutate(key = ifelse(is.na(highway), "waterway", "highway"),
           value = ifelse(is.na(highway), waterway, highway)) %>%
    select(-waterway, -highway)

  paden <- paden %>%
    st_difference(
      exclusie) %>%
    st_intersection(
      gebied %>% st_buffer(dist = 10))

  return(paden)
}


paden_naar_punten <- function(data_paden,
                              gebieden,
                              interpoint_distance = 50,
                              border_distance = 300) {

  data_paden <- data_paden %>%
    select(key, value, Naam) %>%
    st_cast("MULTILINESTRING") %>%
    st_cast("LINESTRING", warn = FALSE)

  st_drop_geometry(data_paden) %>%
    bind_cols(
      data_paden %>%
        st_line_sample(
          density = units::set_units(1000 / interpoint_distance, 1/km),
          type = "regular") %>%
        st_as_sf() %>%
        rename(geometry = x)
    ) %>%
    st_as_sf() -> punten

  punten %>%
    st_cast("POINT", warn = FALSE) %>%
    rownames_to_column(var = "pointid") -> punten

  # punten die alsnog te dicht bij elkaar liggen verwijderen
  clusters <- punten %>%
    st_buffer(dist = interpoint_distance / 2 - 5) %>%
    st_union() %>%
    st_cast("POLYGON")

  groups_too_close <- st_intersects(
    punten,
    clusters
  )

  punten %>%
    bind_cols(groups = unlist(groups_too_close)) %>%
    group_by(groups) %>%
    slice_head(n = 1) %>%
    select(-groups) -> punten

  # punten op minder dan ... van de grens liggen verwijderen
  punten <- punten %>%
    st_intersection(gebieden %>%
                    st_buffer(dist = -border_distance) %>%
                    st_geometry())

  # zorgen dat pointid zeker uniek is
  punten <- punten %>%
    mutate(pointid = paste(abbreviate(Naam, 2), pointid, sep = "_"))

  return(punten)
}

read_lum_rast <- function(
  file,
  legend,
  add_levels = TRUE,
  add_colours = TRUE) {

  tr <- terra::rast(file)
  # re-assign categories labels which got lost in translation
  # (ESRI -> geotiff -> terra::rast)
  if (add_levels) {
    levels_df <- legend %>%
      filter(bestand_id == "vito") %>%
      select(ID = value, land_use = label) %>%
      as.data.frame()

    levels(tr) <- levels_df
  }
  if (add_colours) {
    colours_df <- legend %>%
      filter(bestand_id == "vito") %>%
      select(colour = kleur) %>%
      mutate(as_tibble(t(col2rgb(colour, alpha = TRUE)))) %>%
      select(-colour) %>%
      as.data.frame()

    terra::coltab(tr) <- colours_df
  }
  return(tr)
}


punten_lum_buffer <- function(
  punten_sf,
  radius = 300,
  ...) {

  lum_rast <- read_lum_rast(...)

  out <- landusemetrics_grid_cell(
    grid_cell = punten_sf %>%
      st_buffer(dist = radius),
    layer = lum_rast,
    grid_group_by_col = "pointid")

  return(out)
}

punten_selectie_landgebruik <- function(
  lum_extract_result,
  legend_rast,
  max_prop_overige = 0.5,
  min_prop_akker = 0.3,
  min_prop_akker_grasland = 0.4) {

  legende_lumvito2019 <- legend_rast %>%
    filter(bestand_id == "vito") %>%
    select(value, label, kleur)

  out <- lum_extract_result %>%
    left_join(legende_lumvito2019) %>%
    # herbereken freq zodat noemer = som van niet NA deel
    filter(!is.na(label)) %>%
    group_by(pointid) %>%
    mutate(freq = freq / sum(freq)) %>%
    mutate(label2 = factor(label,
                           levels = c("Akker", "Grasland", "Bos", "Struikgewas")
    ),
    label2 = as.character(label2),
    label2 = ifelse(is.na(label2), "Overige", label2)) %>%
    group_by(pointid, label2) %>%
    summarise(freq = sum(freq), .groups = "drop") %>%
    pivot_wider(names_from = label2, values_from = freq, values_fill = 0) %>%
    mutate(selectie =
             (Overige < max_prop_overige) &
             (Akker > min_prop_akker)) %>%
    mutate(selectie2 = Akker + Grasland > min_prop_akker_grasland &
             (Overige < max_prop_overige))

  return(out)
}


selectie_landgebruik_vito <- function(
  punten_sf,
  selectie_df
) {
  punten_sf <- punten_sf %>%
    semi_join(selectie_df %>%
                filter(selectie2),
              by = "pointid")
  return(punten_sf)
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

filter_zichtbaarheid <- function(punten_sf, min_cvvi) {
  punten_sf %>%
    filter(cvvi >= min_cvvi)
}
