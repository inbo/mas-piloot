# Proportie beheerovereenkomst per telcirkel per jaar
add_bo_by_year <- function(punten_df, path_bo, ...) {
  # Read in bo layer and calculate start and stop year
  bo_layer <- read_bo(path = path_bo)
  bo_layer2 <- bo_layer %>%
    mutate(startjaar = year(START),
           stopjaar = year(STOP))

  # Loop over years
  years <- unique(punten_df$jaar)
  out_list <- vector(mode = "list", length = length(years))

  for (i in seq_along(years)) {
    year <- years[i]

    # Filter by year
    punten_df_year <- punten_df %>% filter(jaar == year)
    bo_layer_year <- bo_layer2 %>%
      filter(startjaar <= year & stopjaar >= year)

    out_df_year <- add_bo_to_frame(punten_df = punten_df_year,
                                   bo_layer = bo_layer_year,
                                   ...)

    out_list[[i]] <- out_df_year
  }

  return(do.call(rbind.data.frame, out_list))
}

# Proportie hoofdteelten per telcirkel per jaar
calc_lbg_by_year <- function(punten_df) {
  # Loop over years
  years <- unique(punten_df$jaar)
  out_list <- vector(mode = "list", length = length(years))

  for (i in seq_along(years)) {
    year <- years[i]

    # Filter by year
    punten_df_year <- punten_df %>% filter(jaar == year)
    lbg_file_year <- path_to_lbg(jaar = year)

    out_df_year <- calc_lbg(path = lbg_file_year,
                            punten_sf = punten_df_year)

    out_list[[i]] <- out_df_year %>% ungroup() %>% mutate(jaar = year)
  }

  return(do.call(rbind.data.frame, out_list))
}

# Binnen buiten sbp per regio
add_sbp_per_regio <- function(punten_sf, perimeters) {

  out_list <- vector(mode = "list", length = length(perimeters$Naam))
  i <- 0

  for (r in perimeters$Naam) {
    i <- i + 1

    # Filter by region
    punten_df_regio <- punten_sf %>% filter(regio == r)
    sbp_akkervogels_regio <- read_sbp_akkervogels(
      path = path_to_sbp_akkervogels(),
      gebied = perimeters %>% filter(Naam == r))

    telpunten_2018_2022_regio <- add_stratum_sbp(
      punten_sf = punten_df_regio,
      sbp       = sbp_akkervogels_regio) %>%
      mutate(sbp = ifelse(is_sbp == TRUE, "binnen", "buiten"))

    out_list[[i]] <- telpunten_2018_2022_regio
  }

  return(do.call(rbind.data.frame, out_list))
}

# Perceelgroottes per telcirkel per jaar
calc_perceelsgrootte_by_year <- function(punten_df) {
  # Loop over years
  years <- unique(punten_df$jaar)
  out_list <- vector(mode = "list", length = length(years))

  for (i in seq_along(years)) {
    year <- years[i]
    number_string <- sub(".*([0-9]{2})$", "\\1", year)

    # Filter by year
    punten_df_year <- punten_df %>% filter(jaar == year)
    lbg_binding <- st_read(file.path("data", "landbouwgebruikspercelen",
                                     "Shapefile", paste0("Lbgbrprc", number_string,
                                                         ".shp"))) %>%
      st_transform(crs = 31370)

    # Intersection
    intersect <- st_intersection(punten_df_year %>% st_buffer(300), lbg_binding)

    out_df_year <- lbg_binding %>%
      filter(OIDN %in% intersect$OIDN) %>%       # Filter percelen
      full_join(st_drop_geometry(intersect),
                by = c("OIDN", "UIDN", "ALVID", "HFDTLT", "LBLHFDTLT",
                       "GEWASGROEP", "PM", "LBLPM", "LENGTE", "OPPERVL")) %>%
      filter(!is.na(LBLHFDTLT)) %>%

      # Join neighbouring polygons by main crop
      group_by(pointid, LBLHFDTLT) %>%
      summarise() %>%
      st_cast("MULTIPOLYGON") %>%
      st_cast("POLYGON") %>%
      ungroup() %>%

      # Calculate median perceel area by point
      mutate(area = st_area(geometry)) %>%
      st_drop_geometry() %>%
      group_by(pointid) %>%
      summarise(perceel_median_area = units::drop_units(median(area)),
                perceel_iqr_area = IQR(area),
                perceel_cv_area = perceel_iqr_area / perceel_median_area,
                .groups = "drop")

    out_list[[i]] <- out_df_year %>% full_join(punten_df_year)
  }

  return(do.call(rbind.data.frame, out_list))
}

# Voeg inertia parameter toe
add_bo_inertia <- function(punten_df, path_bo, bh_doel) {
  # Read in bo layer and calculate start and stop year
  bo_layer <- read_bo(path = path_bo)
  bo_layer2 <- bo_layer %>%
    mutate(startjaar = year(START),
           stopjaar = year(STOP))

  # Loop over years
  years <- unique(punten_df$jaar)
  out_list <- vector(mode = "list", length = length(years))

  for (i in seq_along(years)) {
    year <- years[i]

    # Filter by year
    punten_df_year <- punten_df %>% filter(jaar == year)
    bo_layer_year <- bo_layer2 %>%
      filter(startjaar <= year & stopjaar >= year)

    out_df_year <- st_intersection(st_buffer(punten_df_year, 300),
                                   bo_layer_year) %>%
      st_drop_geometry() %>%
      filter(BH_DOELST %in% bh_doel) %>%
      mutate(inertia_row = jaar - startjaar + 1) %>%
      group_by(pointid) %>%
      summarise(inertia = round(mean(inertia_row)), .groups = "drop")

    out_list[[i]] <- out_df_year %>%
      full_join(punten_df_year, by = c("pointid")) %>%
      replace(is.na(.), 0) %>%
      mutate(jaar = year)
  }

  return(do.call(rbind.data.frame, out_list))
}

# Selecteer (multi)polygonen binnen buffered perimeter (ev. per gebied)
buffer_layers_to_perimeter <- function(layer, buffer_km, group_var = NULL) {
  buffer_to_meters <- buffer_km * 1000

  if (is.null(group_var)) {
    layer <- layer %>%
      rownames_to_column("id")

    polygons <- perimeters %>%
      st_buffer(buffer_to_meters) %>%
      st_intersection(layer) %>%
      pull(id)

    out <- layer %>%
      filter(id %in% polygons) %>%
      select(-id)
  } else {
    polygons <- perimeters %>%
      st_buffer(buffer_to_meters) %>%
      st_intersection(layer) %>%
      pull(group_var)

    out <- layer %>%
      filter(!!sym(group_var) %in% polygons)
  }
  return(out)
}

# Opsplitsen in aparte polygonen (ev. mergen per gebied eerst)
splits_polys <- function(layer, id_name, group_var = NULL) {
  if (is.null(group_var)) {
    out <- layer %>%
      st_cast("POLYGON") %>%
      rownames_to_column("id") %>%
      mutate(id = paste(id_name, id, sep = "_")) %>%
      select(id)
  } else {
    out <- layer %>%
      group_by_at(group_var) %>%
      summarise() %>%
      st_cast("MULTIPOLYGON") %>%
      st_cast("POLYGON") %>%
      rownames_to_column("id") %>%
      mutate(id = paste(id_name, id, sep = "_")) %>%
      select(id)
  }
  return(out)
}
