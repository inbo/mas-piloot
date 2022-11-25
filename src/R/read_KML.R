# dsn = data source name
# layer = layer name as character string or layer number as number;
#         if no layer is given, the first layer is loaded

read_KML <- function(dsn, layer = NULL) {
  require(sf)
  require(tidyverse)
  
  # Error handling
  ## File needs to exist
  if (!file.exists(dsn)) {
    stop(paste0("Cannot open ", '"', as.character(dsn), '"',
                "; The file doesn't seem to exist."), 
         call. = FALSE)
  }
  ## File needs to be a KML file
  extension <- substr(dsn, nchar(dsn) - 2, nchar(dsn))
  if (!grepl("kml$", extension, ignore.case = TRUE)) {
    stop("File needs to be a KML file", call. = FALSE)
  }
  ## If layer name is given, it needs to be to be present in the file
  if (is.character(layer)) {
    if (!(layer %in% st_layers(dsn)$name)) {
      stop(paste0("Layer ", '"', layer, '"', " not available in file."), 
           call. = FALSE)
    }
  }
  ## If layer number is given, it needs to be to be present in the file
  if (is.numeric(layer)) {
    if (!(layer %in% seq_len(length(st_layers(dsn)$name)))) {
      stop("Not that many layers in file", call. = FALSE)
    }
  }
  
  # Read in file
  ## First layer if layer == NULL
  if (is.null(layer)) {
    layer_name <- st_layers(dsn)$name[1]
    ## If layer number is given
  } else if (is.numeric(layer)) {
    layer_name <- st_layers(dsn)$name[layer]
    ## If layer name is given
  } else {
    layer_name <- layer
  }
  kml_file_raw <- st_read(dsn, layer_name)
  
  # Extract variable names in 'Description'
  col_names <- str_match_all(kml_file_raw$Description, "<br>(.*?):")[[1]][, 2]
  
  # Parse to table
  kml_file <- kml_file_raw %>%
    mutate(
      Name = readr::parse_character(Name),
      Name = trimws(Name, whitespace = "[\\h\\v]"),
      Name = trimws(Name),
      Description = str_remove_all(Description, "^description:\\s.*?<br>")) %>%
    separate(col = Description, into = col_names, sep = "(<br>)+") %>%
    mutate(across(beschrijving:wie, ~str_extract(., "(?<=:\\s).+")))
  
  # Return
  return(kml_file)
}
