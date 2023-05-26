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
