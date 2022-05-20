#' Extract a sample from the C-mon grts.sqlite database
#'
#' The sample is _not_ restricted to cells within Flanders (only restricted
#' to bbox).
#' The first time you run this function on the database downloaded from
#' https://zenodo.org/record/2784012#.YOMk2OgzZPY, it will take an hour or so
#' to build a database index. After that, the function will execute
#' instantly. Beware: this is a large database (50 Gb after index has been
#' added).
#' The downloaded database https://zenodo.org/record/2784012#.YOMk2OgzZPY only
#' holds the grts sample at cellsize 10 m resolution.
#' To also add coarser resolutions, i.e. 20 m, 40 m, 80 m, ...
#' grtsdb::add_level() function is needed.
#' Using this function at coarser resolution will require index to be built for
#' the corresponding level and will further increase the size of the database.
#'
#' @param database path to the database
#' @param samplesize integer
#' @param bbox 2 x 2 matrix holding the x and y lowerleft and upperright corners
#' @param cellsize cellsize in meters
#'
#' @details note that the grtsdb.R script that can be downloaded from
#' https://zenodo.org/record/2784012#.YOMk2OgzZPY will not reproduce the same
#' grts.sqlite database because SQLite does not have the equivalent of a
#' set.seed argument.
#'
#' @return a data.frame containing the sample (EPSG 31370)
#' @export
#'
#' @examples
cmon_extract_sample <- function(
  database,
  samplesize = 100000,
  bbox = rbind(x = c(22000, 258880), y = c(153050, 244030)),
  cellsize = 10) {
  # https://zenodo.org/record/2784012#.YOMk2OgzZPY
  # https://zenodo.org/record/5833399#.YefuKv7MJPY
  # extract the sample from the database
  conn <- grtsdb::connect_db(database)
  sample <- grtsdb::extract_sample(
    samplesize = samplesize,
    bbox = bbox,
    cellsize = cellsize,
    grtsdb = conn)
  RSQLite::dbDisconnect(conn)
  return(sample)
}


# https://duckdb.org/2021/06/25/querying-parquet.html looks interesting maybe
