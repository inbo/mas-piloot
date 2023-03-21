list(
  tar_target(
    name = selectie_openheid_klasses,
    command = selectie_openheid(
      gebied = perimeters_data,
      ol_strata = c("OL", "HOL")
      ),
    pattern = map(perimeters_data)
  ),
  tar_target(
    name = exclusie_osm_landgebruiken,
    command = exclusie_landgebruik_osm(
      gebied = selectie_openheid_klasses,
      osmdata = osm_belgium,
      landuse = c("residential", "military", "industrial", "cemetery", 
                  "railway", "commercial", "farmyard"),
      leisure = c("park"),
      buffer_poly = 0, 
      layer_poly = list(aeroway = c("aerodrome")),
      buffer_line = 100, 
      layer_line = list(highway = c("motorway", "motorway_link")),
      update_osm_layer = FALSE
      ),
    pattern = map(selectie_openheid_klasses)
  ),
  tar_target(
    name = paden,
    command = extract_osm_paden(
      gebied = selectie_openheid_klasses,
      exclusie = exclusie_osm_landgebruiken,
      osmdata = osm_belgium,
      paths_include = c("track", "tertiary", "tertiary_link", "unclassified"),
      cutting_exclude = NULL,
      historic_exclude = NULL,
      waterway = NULL,
      update_osm_layer = FALSE
      ),
    pattern = map(selectie_openheid_klasses, exclusie_osm_landgebruiken)
  ),
  tar_target(
    name = punten,
    command = paden_naar_punten(
      data_paden = paden,
      gebieden = perimeters_data,
      interpoint_distance = 50,
      border_distance = 300
      ),
    pattern = map(perimeters_data, paden)
  ) ,
  tar_target(
    name = telcirkels_landgebruik,
    command = punten_lum_buffer(
      punten_sf = punten,
      radius = 300,
      file = vito_lum_2019_file,
      legend = legend_lum
      ),
    pattern = map(punten)
  ),
  tar_target(
    name = telcirkels_selectie_landgebruik,
      command = punten_selectie_landgebruik(
        lum_extract_result = telcirkels_landgebruik,
        legend_rast = legend_lum,
        max_prop_overige = 0.5,
        min_prop_akker = 0.3,
        min_prop_akker_grasland = 0.4
        ),
    pattern = map(telcirkels_landgebruik)
  ),
  tar_target(
    name = selectie_landgebruik,
    command = selectie_landgebruik_vito(
      punten_sf = punten,
      selectie_df = telcirkels_selectie_landgebruik
      ),
    pattern = map(punten, telcirkels_selectie_landgebruik)
  ),
  tarchetypes::tar_group_size(
    name = selectie_landgebruik_per_size,
    command = selectie_landgebruik,
    size = 200
  ),
  tar_target(
    name = punten_zichtbaarheid,
    command = add_visibility_to_frame(
      punten_sf = selectie_landgebruik_per_size,
      resolution = 5,
      spacing = 10
    ),
    pattern = map(selectie_landgebruik_per_size)
  ),
  tarchetypes::tar_group_by(
    name = punten_zichtbaarheid_per_regio,
    command = punten_zichtbaarheid,
    Naam
  ),
  tar_target(
    name = punten_selectie_zichtbaarheid,
    command = filter_zichtbaarheid(
      punten_sf = punten_zichtbaarheid_per_regio,
      min_cvvi = 0.1
    ),
    pattern = map(punten_zichtbaarheid_per_regio)
  )
)
