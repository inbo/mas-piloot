#' Convert a single point location to a grid cell polygon
#'
#' @param point_xy a vector with x and y coordinates
#' @param cell_width_m cell width in meter, default 500
#' @param cell_height_m  cell height in meter, default equal to cell_width_m
#' @param point_position default center of grid cell
#' @param crs default EPSG code 31370
#'
#' @return
#' @export
#'
#' @examples
point_to_gridcell <- function(
  point_xy,
  cell_width_m = 500,
  cell_height_m = cell_width_m,
  point_position = c("center", "lowerleft", "upperleft", "lowerright", "upperright"),
  crs = 31370) {
  point_position <- match.arg(point_position)

  if (point_position != "center") stop(point_position, " not yet implemented")

  xy <- sf::st_point(point_xy)
  pl <- list(
    rbind(
      c(xy[1] - cell_width_m / 2, xy[2] - cell_height_m / 2),
      c(xy[1] - cell_width_m / 2, xy[2] + cell_height_m / 2),
      c(xy[1] + cell_width_m / 2, xy[2] + cell_height_m / 2),
      c(xy[1] + cell_width_m / 2, xy[2] - cell_height_m / 2),
      c(xy[1] - cell_width_m / 2, xy[2] - cell_height_m / 2)
    )
  )
  polygon <- sf::st_polygon(pl)
  polygon <- sf::st_sfc(polygon, crs = crs)
  polygon <- sf::st_sf(geometry = polygon)
  return(polygon)
}


landusemetrics_grid_cell <- function(
  grid_cell,
  rasterlayer

) {
  exactextractr::exact_extract(
    x = rasterlayer,
    y = grid_cell)

}

