#' Get a layer from a web coverage service within a bounding box
#'
get_feature_wfs <- function(
  wfs,
  version = "2.0.0",
  layername,
  crs,
  bbox = NULL,
  filter = NULL,
  cql_filter = NULL,
  output_format = NULL,
  property_name = NULL,
  result_type = c("results", "hits")
  ) {
  require(httr)
  require(sf)
  result_type <- match.arg(result_type)
  url <- parse_url(wfs)
  if (grepl(pattern = "^2", x = version)) {
    url$query <- list(service = "wfs",
                      request = "GetFeature",
                      typeNames = layername,
                      srsName = crs,
                      bbox = bbox,
                      filter = filter,
                      cql_filter = cql_filter,
                      outputFormat = output_format,
                      propertyName = property_name,
                      resultType = result_type
    )
  }
  if (grepl(pattern = "^1", x = version)) {
    url$query <- list(service = "wfs",
                      request = "GetFeature",
                      typeName = layername,
                      srsName = crs,
                      bbox = bbox,
                      filter = filter,
                      cql_filter = cql_filter,
                      outputFormat = output_format,
                      propertyName = property_name,
                      resultType = NULL
    )
  }

  request <- build_url(url)

  if (result_type == "hits") {
    result <- GET(request)
    parsed <- xml2::as_list(content(result, "parsed", encoding = "UTF-8"))
    n_features <- attr(parsed$FeatureCollection, "numberMatched")
    return(n_features)
  } else {
    result <- read_sf(request)
    return(result)
  }
}





#' Get a layer from a web coverage service within a bounding box
#'
get_coverage_wcs <- function(wcs = c("dtm", "omz", "dsm"),
                          bbox, #xmin, xmax, ymin, ymax
                          layername,
                          resolution,
                          wcs_crs = "EPSG:4258",
                          output_crs = "EPSG:31370",
                          bbox_crs = "EPSG:31370",
                          version = c("1.0.0", "2.0.1")) {
  # prelim check
  version <- match.arg(version)
  wcs <- match.arg(wcs)
  wcs_crs <- match.arg(wcs_crs)
  bbox_crs <- match.arg(bbox_crs)

  # set url
  wcs <- switch(
    wcs,
    omz = "https://inspire.informatievlaanderen.be/overdrachtdiensten/oi-omz/wcs",
    dtm = "https://inspire.informatievlaanderen.be/overdrachtdiensten/el-dtm/wcs",
    dsm = "https://inspire.informatievlaanderen.be/overdrachtdiensten/el-dsm/wcs"
  )

  assertthat::assert_that(is.character(layername))
  assertthat::assert_that(is.character(output_crs))
  assertthat::assert_that(
    is.vector(bbox, mode = "numeric"),
    length(bbox) == 4)

  names(bbox) <- c("xmin", "xmax", "ymin", "ymax")

  assertthat::assert_that(is.numeric(resolution))

  # build url request
  url <- parse_url(wcs)

  if (version == "2.0.1") {
    epsg_code <- stringr::str_extract(wcs_crs, "\\d+")
    matrix(bbox, ncol = 2, byrow = FALSE) %>%
      as.data.frame() %>%
      st_as_sf(coords = c("V1", "V2"), crs = bbox_crs) %>%
      st_transform(crs = wcs_crs) %>%
      st_coordinates() %>%
      as.vector() -> bbox

    names(bbox) <- c("xmin", "xmax", "ymin", "ymax")

    url$query <- list(SERVICE = "WCS",
                      VERSION = version,
                      REQUEST = "GetCoverage",
                      COVERAGEID = layername,
                      CRS = wcs_crs,
                      SUBSET = paste0(
                        "x,http://www.opengis.net/def/crs/EPSG/0/",
                        epsg_code, "(",
                        bbox["xmin"],
                        ",",
                        bbox["xmax"],")"),
                      SUBSET = paste0(
                        "y,http://www.opengis.net/def/crs/EPSG/0/",
                        epsg_code,
                        "(",
                        bbox["ymin"],
                        ",",
                        bbox["ymax"],")"),
                      SCALEFACTOR = resolution,
                      FORMAT = "image/tiff",
                      RESPONSE_CRS = wcs_crs
    )
    request <- build_url(url)
    file <- tempfile(fileext = ".mht")
    GET(url = request,
        write_disk(file))
    #multipart file extract tif part
    unpack_mht(file)
    file <- stringr::str_replace(file, "mht", "tif")
  }

  if (version == "1.0.0") {
    matrix(bbox, ncol = 2, byrow = FALSE) %>%
      as.data.frame() %>%
      st_as_sf(coords = c("V1", "V2"), crs = bbox_crs) %>%
      st_transform(crs = wcs_crs) %>%
      st_coordinates() %>%
      as.vector() -> bbox

    names(bbox) <- c("xmin", "xmax", "ymin", "ymax")

    url$query <- list(SERVICE = "WCS",
                      VERSION = version,
                      REQUEST = "GetCoverage",
                      COVERAGE = layername,
                      CRS = wcs_crs,
                      BBOX = paste(
                        bbox["xmin"],
                        bbox["ymin"],
                        bbox["xmax"],
                        bbox["ymax"],
                        sep = ","),
                      RESX = resolution,
                      RESY = resolution,
                      FORMAT = "geoTIFF",
                      RESPONSE_CRS = wcs_crs
    )
    request <- build_url(url)
    file <- tempfile(fileext = ".tif")
    GET(url = request,
        write_disk(file))
  }

  raster <- terra::rast(file)
  template <- terra::project(raster, output_crs)
  res(template) <- resolution
  raster <- terra::project(raster, template)
  return(raster)
}


unpack_mht <- function(path) {

  # need three ways to read in the mht file to get the tif file out...
  # read_lines() cannot read all lines due to embedded nulls
  # therefore, also read_lines_raw() needed for positioning of tif part in file
  # write_lines() does not work correctly on lines_raw[start:end]
  # possibly a bug/edge case in write_lines()
  # therefore, also read_file_raw() needed to extract from the raw vector
  lines_raw <- readr::read_lines_raw(path)
  lines_char <- suppressWarnings(readr::read_lines(path))
  raw_vector <- readr::read_file_raw(path)

  assertthat::assert_that(any(stringr::str_detect(lines_char, "image/tiff")))
  start <- max(which(stringr::str_detect(lines_char, "Content-"))) + 1
  end <- length(lines_raw) - 1
  pos_start <- length(unlist(lines_raw[1:(start - 1)])) + start
  pos_end <- length(raw_vector) - (length(lines_raw[end + 1]) + 1)

  tif <- raw_vector[pos_start:pos_end]
  readr::write_file(tif,
                    stringr::str_replace(path, "mht", "tif"))
}
