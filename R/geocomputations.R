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
#' @param rasterlayer A rasterlayer
#'
#' @return
#' @export
#'
#' @examples
landusemetrics_grid_cell <- function(
  grid_cell,
  rasterlayer,
  group_by_col = "POINT_ID"
) {

  landcoverfraction <- function(df) {
    df %>%
      mutate(frac_total = coverage_fraction / sum(coverage_fraction)) %>%
      group_by(!!sym(group_by_col), value) %>%
      summarize(freq = sum(frac_total), .groups = "drop_last")
  }

  exactextractr::exact_extract(
    x = rasterlayer,
    y = grid_cell,
    fun = landcoverfraction,
    summarize_df = TRUE,
    include_cols = group_by_col)

}

