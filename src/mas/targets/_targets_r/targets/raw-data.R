list(
  tarchetypes::tar_file(
    perimeters_file,
    path_to_perimeters()
    ),
  tar_target(
    perimeters_data,
    read_sf(perimeters_file)
    ),
  tarchetypes::tar_file(
    osm_belgium,
    path_to_osm_download()
    ),
  tarchetypes::tar_file(
    vito_lum_2019,
    path_to_lum(jaar = 2019)
    ),
  tarchetypes::tar_file(
    legend_lum_file,
    path_to_legend_lum()
    ),
  tar_target(
    legend_lum,
    read_legend_lum(file = legend_lum_file)
  )
)
