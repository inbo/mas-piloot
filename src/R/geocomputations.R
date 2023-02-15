#' Convert a single point location to a grid cell polygon
#'
#' @param xy an object of class POINT
#' @param cell_width_m cell width in meter, default 500
#' @param point_position default center of grid cell
#' @param crs default EPSG code 31370
#'
#' @return
#' @export
#'
#' @examples
point_to_gridcell <- function(
  xy,
  cell_width_m = 500,
  point_position = c("center", "lowerleft", "upperleft", "lowerright", "upperright"),
  crs = 31370) {
  point_position <- match.arg(point_position)

  if (point_position != "center") stop(point_position, " not yet implemented")

  stopifnot(sf::st_is(xy, "POINT"))
  xy_df <- sf::st_drop_geometry(xy)
  xy <- sf::st_geometry(xy)

  # buffer with 1 point per quandrant
  xy_buffer <- sf::st_buffer(x = xy,
                             dist = cell_width_m / 2,
                             nQuadSegs = 1)

  # rotate 45 degrees around centroid
  rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
  pl <- (xy_buffer - xy) * rot(pi/4) + xy
  pl <- sf::st_sf(data.frame(xy_df, pl), crs = crs)
  return(pl)
}


#' Calculation of land-use metrics within a grid cell
#'
#' @param grid_cell A polygon within which boundaries zonal statistics will be
#' calculated
#' @param layer A rasterlayer containing land use classes or a polygon layer (sf object)
#' @param grid_group_by_col A character vector of columns to group by for zones
#' @param layer_group_by_col A character vector of columns to group by for
#' layer
#'
#' @return
#' @export
#'
#' @examples
landusemetrics_grid_cell <- function(
  grid_cell,
  layer,
  grid_group_by_col = "POINT_ID",
  layer_group_by_col = "",
  progress = FALSE
) {
  require(duckdb)
  if (inherits(layer, "SpatRaster") | inherits(layer, "RasterLayer")) {
    crs_grid <- gsub("^((.*?),\\n\\s*?){2}", "", sf::st_crs(grid_cell)$wkt)
    crs_layer <- gsub("^((.*?),\\n\\s*?){2}", "", terra::crs(layer))
    assertthat::assert_that(crs_grid == crs_layer)

    landcoverfraction <- function(df) {
      df %>%
        mutate(frac_total = coverage_fraction / sum(coverage_fraction)) %>%
        group_by(!!!syms(grid_group_by_col), value) %>%
        summarize(freq = sum(frac_total), .groups = "drop_last")
    }

    res <- exactextractr::exact_extract(
      x = layer,
      y = grid_cell,
      fun = landcoverfraction,
      summarize_df = TRUE,
      include_cols = grid_group_by_col,
      progress = progress)

    return(res)

  }

  if (inherits(layer, "sf")) {
    assertthat::assert_that(sf::st_crs(grid_cell)$wkt == sf::st_crs(layer)$wkt)

    int <- st_intersection(layer, grid_cell)

    cell_areas <- grid_cell %>%
      select(!!!syms(grid_group_by_col)) %>%
      mutate(cell_area = sf::st_area(geometry)) %>%
      sf::st_drop_geometry()

    temparrow <- tempfile(fileext = ".parquet")

    int$area <- sf::st_area(int$geometry)
    int <- int %>%
      sf::st_drop_geometry() %>%
      inner_join(cell_areas, by = grid_group_by_col) %>%
      arrow::write_dataset(path = temparrow)

    int <- arrow::open_dataset(temparrow) %>%
      arrow::to_duckdb() %>%
      group_by(!!!syms(grid_group_by_col),
               !!!syms(layer_group_by_col),
               cell_area) %>%
      summarise(area_m2 = sum(area)) %>%
      mutate(area_prop = area_m2 / cell_area) %>%
      collect()

    return(int)
  }
}

