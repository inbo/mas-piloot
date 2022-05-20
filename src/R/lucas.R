#' Download LUCAS harmonised data
#'
#' https://www.nature.com/articles/s41597-020-00675-z
#' data will be downloaded from the figshare archive
#' The unzipped archive will contain subfolders with .zip files inside,
#' use unzip_lucas_microdata() to unzip specific files
#'
#' @param path path to the folder where data will be unzipped.
#' Default ./data
#'
#' @export
#'
#' @examples
lucas_download_microdata <- function(
  from = "https://ndownloader.figshare.com/files/24265361",
  to = "./data") {
  download_unzip(from = from, to = to)
  writeLines(text = "*\n!/.gitignore",
             con = file.path(path, "lucas_harmonised/.gitignore"))
}


#' Unzip specific nested zip files from the downloaded lucas harmonised
#' microdata archive
#'
#' @param all Default FALSE, whether or not to unzip all files
#' @param path Path to the lucas harmonised data folder.
#' Default ./data/lucas_harmonised.
#'
#' @export
#'
#' @examples
lucas_unzip_microdata <- function(
  all = FALSE,
  path = "./data/lucas_harmonised") {

  zip_files <- fs::dir_ls(path = path,
             type = "file",
             glob = "*.zip",
             recurse = TRUE)
  if (all) {
    purrr::walk(
      .x = zip_files,
      .f = function(x) utils::unzip(x, exdir = fs::path_dir(x))
      )
  } else {
    msg <- paste0("Which files do you want to unzip?\n",
                  paste(
                    paste(1:length(zip_files), zip_files, sep = " = "),
                    collapse = "\n")
                  )
    cat(msg)
    answer <- readline(prompt = "Enter one or more numbers: ")
    answer <- as.numeric(
      unlist(
        strsplit(answer, split = "\\s+")
      )
    )
    purrr::walk(
      .x = zip_files[answer],
      .f = function(x) utils::unzip(x, exdir = fs::path_dir(x))
    )
  }
}



#' Extract Flanders (NUTS level 1 = BE2) from LUCAS microdata
#'
#' @param parquet_hive path to the Dataset containing LUCAS microdata
#' @param path path incl filename to write the result as a parquet file
#'
#' @export
#'
#' @examples
lucas_extract_flanders <- function(
  parquet_hive,
  path) {
  require(dplyr)
  require(arrow)
  arrow::open_dataset(sources = parquet_hive) %>%
    dplyr::filter(nuts1 == "BE2") %>%
    dplyr::collect() %>%
    arrow::write_parquet(sink = path)
}


#' Create the arrow schema for LUCAS harmonized data
#'
#' @return An arrow::schema() object
#' @export
#'
#' @examples
lucas_schema <- function() {
  require(arrow)
  arrow::schema(
    id = int64(),
    point_id = int64(),
    year = int64(),
    nuts0 = string(),
    nuts1 = string(),
    nuts2 = string(),
    nuts3 = string(),
    th_lat = double(),
    th_long = double(),
    office_pi = string(),
    ex_ante = string(),
    survey_date = string(),
    car_latitude = double(),
    car_ew = string(),
    car_longitude = double(),
    gps_proj = string(),
    gps_prec = int64(),
    gps_altitude = int64(),
    gps_lat = double(),
    gps_ew = string(),
    gps_long = double(),
    obs_dist = double(),
    obs_direct = string(),
    obs_type = string(),
    obs_radius = string(),
    letter_group = string(),
    lc1 = string(),
    lc1_label = string(),
    lc1_spec = string(),
    lc1_spec_label = string(),
    lc1_perc = string(),
    lc2 = string(),
    lc2_label = string(),
    lc2_spec = string(),
    lc2_spec_label = string(),
    lc2_perc = string(),
    lu1 = string(),
    lu1_label = string(),
    lu1_type = string(),
    lu1_type_label = string(),
    lu1_perc = string(),
    lu2 = string(),
    lu2_label = string(),
    lu2_type = string(),
    lu2_type_label = string(),
    lu2_perc = string(),
    parcel_area_ha = string(),
    tree_height_maturity = string(),
    tree_height_survey = string(),
    feature_width = string(),
    lm_stone_walls = string(),
    crop_residues = string(),
    lm_grass_margins = string(),
    grazing = string(),
    special_status = string(),
    lc_lu_special_remark = string(),
    cprn_cando = string(),
    cprn_lc = string(),
    cprn_lc_label = string(),
    cprn_lc1n = int64(),
    cprnc_lc1e = int64(),
    cprnc_lc1s = int64(),
    cprnc_lc1w = int64(),
    cprn_lc1n_brdth = int64(),
    cprn_lc1e_brdth = int64(),
    cprn_lc1s_brdth = int64(),
    cprn_lc1w_brdth = int64(),
    cprn_lc1n_next = string(),
    cprn_lc1s_next = string(),
    cprn_lc1e_next = string(),
    cprn_lc1w_next = string(),
    cprn_urban = string(),
    cprn_impervious_perc = int64(),
    inspire_plcc1 = int64(),
    inspire_plcc2 = int64(),
    inspire_plcc3 = int64(),
    inspire_plcc4 = int64(),
    inspire_plcc5 = int64(),
    inspire_plcc6 = int64(),
    inspire_plcc7 = int64(),
    inspire_plcc8 = int64(),
    eunis_complex = string(),
    grassland_sample = string(),
    grass_cando = string(),
    wm = string(),
    wm_source = string(),
    wm_type = string(),
    wm_delivery = string(),
    erosion_cando = string(),
    soil_stones_perc = string(),
    bio_sample = string(),
    soil_bio_taken = string(),
    bulk0_10_sample = string(),
    soil_blk_0_10_taken = string(),
    bulk10_20_sample = string(),
    soil_blk_10_20_taken = string(),
    bulk20_30_sample = string(),
    soil_blk_20_30_taken = string(),
    standard_sample = string(),
    soil_std_taken = string(),
    organic_sample = string(),
    soil_org_depth_cando = string(),
    soil_taken = string(),
    soil_crop = string(),
    photo_point = string(),
    photo_north = string(),
    photo_south = string(),
    photo_east = string(),
    photo_west = string(),
    transect = string(),
    revisit = int64(),
    th_gps_dist = double(),
    file_path_gisco_north = string(),
    file_path_gisco_south = string(),
    file_path_gisco_east = string(),
    file_path_gisco_west = string(),
    file_path_gisco_point = string()
  )
}

#' Read shapefiles LUCAS grid
#'
#' @param layer one of gps (actual gps coordinates; default),
#' th (theoretical grid point)
#' or trans (transect lines: 250 m east looking lines)
#' @param path Default data/lucas_harmonised/2_geometry/. Path to where folder
#' containing the shapefiles.
#'
#' @return A sf object
#' @export
#'
#' @examples
lucas_grid <- function(
  layer = c("gps", "th", "trans"),
  path = "data/lucas_harmonised/2_geometry/") {
  layer <- match.arg(layer)
  filename <- paste0("LUCAS_", layer, "_geom.shp")
  sf::read_sf(file.path(path, filename))
}
