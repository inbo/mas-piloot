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
