list(
  tarchetypes::tar_file(
    perimeters_file,
    path_to_perimeters("zandleemstreek_perimeters.gpkg")
    ),
  tar_target(
    perimeters_data,
    st_read(perimeters_file) %>% 
      arrange(Naam)
    ),
  tarchetypes::tar_file(
    osm_belgium,
    path_to_osm_download()
    ),
  tarchetypes::tar_file(
    vito_lum_2019_file,
    path_to_lum(jaar = 2019)
    ),
  tarchetypes::tar_file(
    legend_lum_file,
    path_to_legend_lum()
    ),
  tar_target(
    legend_lum,
    read_legend_lum(file = legend_lum_file)
  ),
  tarchetypes::tar_file(
    openheid_landschap_file,
    path_to_openheid_landschap()
    ),
  tarchetypes::tar_file(
    bo_file,
    path_to_bo(jaar = 2022)
    ),
  tarchetypes::tar_file(
    lbg_file,
    path_to_lbg(jaar = 2022)
  ),
  tarchetypes::tar_file(
    sbp_akkervogels_file,
    path_to_sbp_akkervogels(file = "akkervogelgebieden2022.shp")
  ),
  tarchetypes::tar_file(
    sbp_overige_file,
    path_to_sbp_akkervogels(file = "sbp_overige_soorten.shp")
  ),
  tarchetypes::tar_file(
    existing_file,
    path_to_existing(file = "steekproef_piloot_avimap.geojson")
  ),
  tar_target(
    existing_data,
    st_read(existing_file)
  )
)
