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

exclusie_buffer_osm <- function(gebied, osmdata, buffer, layer, geom_type) {
  keys <- names(layer)
  selection <- paste(keys, collapse = ", ")

  exclusion_list <- lapply(seq_along(layer), function(i) {
      paste0(keys[[i]], " IN ('", paste(layer[[i]], collapse = "', '"), "')")
    })

  exclusion_str <- paste(unlist(exclusion_list), collapse = " OR ")

  landuse_exclusie_vectortranslate = c(
    "-t_srs", "EPSG:31370",
    "-select", selection,
    "-where", exclusion_str,
    "-nlt", "PROMOTE_TO_MULTI"
  )

  if (geom_type == "lines") {
    out <- osmextract::oe_get(
      place = gebied %>% st_buffer(buffer),
      layer = geom_type,
      vectortranslate_options = landuse_exclusie_vectortranslate,
      boundary = gebied %>% st_buffer(buffer),
      boundary_type = "clipsrc",
      download_directory = dirname(osmdata)) %>%
      st_buffer(buffer)
  }

  if (geom_type == "multipolygons") {
    out <- osmextract::oe_get(
      place = gebied %>% st_buffer(buffer_poly),
      layer = geom_type,
      vectortranslate_options = poly_buff_exclusie_vectortranslate,
      boundary = gebied %>% st_buffer(buffer_poly),
      boundary_type = "clipsrc",
      download_directory = dirname(osmdata)) %>%
      st_buffer(buffer_poly)
  }

  return(out)
}

# Set landuse or leisure to NULL if you don't want to exclude from these
exclusie_landgebruik_osm <- function(gebied, osmdata,
   landuse = c('residential', 'military', 'industrial', 'cemetery'),
   leisure = c('park'),
   buffer_poly = NULL, layer_poly = NULL,
   buffer_line = NULL, layer_line = NULL) {

  # Create string to exclude landuse and leisure variables
  exclusion_landuse <- paste0("('", paste(landuse, collapse = "', '"), "')")
  exclusion_leisure <- paste0("('", paste(leisure, collapse = "', '"), "')")

  if (is.null(landuse)) {
    exclusion_str <- paste("leisure IN", exclusion_leisure, sep = " ")
  } else if (is.null(leisure)) {
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

  exclusie_landgebruik <- osmextract::oe_get(
    place = gebied,
    layer = "multipolygons",
    vectortranslate_options = landuse_exclusie_vectortranslate,
    boundary = gebied,
    boundary_type = "clipsrc",
    download_directory = dirname(osmdata))

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

  return(exclusie_landgebruik)
}

# Set waterway to NULL if you don't want to include any waterway
extract_osm_paden <- function(gebied, exclusie, osmdata,
  paths_include = c('track', 'footway', 'path', 'cycleway', 'bridleway',
                    'tertiary', 'tertiary_link', 'unclassified'),
  cutting_exclude = c('yes', 'both', 'hollow_way'),
  historic_exclude = c('hollow_way'),
  waterway = c('river', 'stream', 'tidal channel', 'canal', 'drain', 'ditch')) {

  # Create string include
  inclusion_paths <- paste0("('", paste(paths_include,
                                        collapse = "', '"), "')")
  exclusion_cutting <- paste0("('", paste(cutting_exclude,
                                        collapse = "', '"), "'))")
  exclusion_historic <- paste0("('", paste(historic_exclude,
                                        collapse = "', '"), "'))))")
  inclusion_waterway <- paste0("('", paste(waterway,
                                        collapse = "', '"), "'))")

  if (is.null(waterway)) {
    inclusion_str <- paste("(highway IN", inclusion_paths,
                           "AND NOT ((cutting IN ", exclusion_cutting,
                           "OR (historic IN", exclusion_historic,
                           sep = " ")
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
  paden <- oe_get(
    place = gebied,
    extra_tags = c("historic", "cutting"),
    vectortranslate_options = my_vectortranslate,
    boundary = gebied,
    boundary_type = "spat",
    download_directory = dirname(osmdata))

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

