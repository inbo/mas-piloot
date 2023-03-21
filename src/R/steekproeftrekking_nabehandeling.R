path_to_existing <- function(file) {
  file.path(mbag_dir, "data", "processed", file)
}

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

thin_sample <- function(sample, thin_dist) {
  # Order samples
  keep <- sample[order(sample$sample_order), ]

  # Remove samples too close to a sample with lower sample order
  i <- 1
  while (i <= nrow(keep)) {
    from <- keep[i, ]
    to <- keep[-i, ]
    distances <- st_distance(from, to) %>%
      units::drop_units() %>%
      as.vector()
    far_enough <- distances > thin_dist
    keep <- rbind(from, to[far_enough, ])
    i <- i + 1
  }

  # Reorder samples
  keep <- keep[order(keep$sample_order), ]
  keep$thin_dist <- thin_dist

  return(keep)
}


replace_by_existing <- function(sample,
                                existing_points,
                                overlap_prop = 0.5,
                                sbp_file) {
  # Recalculate sbp stratum existing points
  old_points <- existing_points %>%
    mutate(is_sbp = st_intersects(.,
                                  st_union(sbp_file),
                                  sparse = FALSE) %>%
             as.logical()
    ) %>%
    mutate(openheid_klasse = ifelse(grepl("HOL", stratum), "HOL", "OL")) %>%
    select(definitief_punt, openheid_klasse, is_sbp)

  # Add buffers
  old_points_buff <- old_points %>%
    st_buffer(300)

  sample_buff <- sample %>%
    select(pointid, openheid_klasse, is_sbp) %>%
    st_buffer(300)

  # Which points overlap?
  intersect <- st_intersection(old_points_buff, sample_buff) %>%
    mutate(intersect_area = st_area(.) %>% units::drop_units()) %>%
    filter(intersect_area >= overlap_prop * 300 * 300 * pi,
           openheid_klasse == openheid_klasse.1,
           is_sbp == is_sbp.1) %>%
    select(definitief_punt, pointid, intersect_area) %>%
    st_drop_geometry() %>%
    group_by(pointid) %>%
    filter(intersect_area == max(intersect_area)) %>%
    ungroup() %>%
    select(-intersect_area)

  # Replace id and geometry
  columns <- names(sample)[names(sample) != "pointid"]

  existing_overlap <- old_points %>%
    inner_join(intersect, by = "definitief_punt") %>%
    select(definitief_punt, pointid) %>%
    inner_join(sample %>% st_drop_geometry(), by = "pointid") %>%
    select(pointid = definitief_punt, all_of(columns))

  # Add to sample
  sample_out <- sample %>%
    filter(!pointid %in% intersect$pointid) %>%
    bind_rows(existing_overlap) %>%
    arrange(sample_order)

  return(sample_out)
}


output_finaal <- function(files, write_out) {
  if (write_out) {
    fs::dir_create("output")
    for (i in seq_along(files)) {
      name <- names(files)[i]

      object <- files[[i]] %>%
        mutate(X = st_coordinates(.data$geometry)[,1],
               Y = st_coordinates(.data$geometry)[,2]) %>%
        st_drop_geometry()

      if ("thin_dist" %in% names(object)) {
        object <- object %>%
          select(-thin_dist)
      }

      git2rdata::write_vc(object, file = paste0("output/", name),
                          sorting = "pointid")
    }
  }
  return(write_out)
}
