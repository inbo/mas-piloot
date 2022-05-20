list(
  tar_target(name = exclusie_osm_landgebruiken,
             command = exclusie_landgebruik_osm(
               gebied = perimeters_data,
               osmdata = osm_belgium),
             pattern = map(perimeters_data)
  ),
  tar_target(name = paden,
             command = extract_osm_paden(
               gebied = perimeters_data,
               exclusie = exclusie_osm_landgebruiken,
               osmdata = osm_belgium
               ),
             pattern = map(perimeters_data, exclusie_osm_landgebruiken)
             ),
  tar_target(name = punten,
             command = paden_naar_punten(
               data_paden = paden,
               gebieden = perimeters_data,
               interpoint_distance = 50,
               border_distance = 300
               ),
             pattern = map(perimeters_data, paden)
             ) ,
  tar_target(name = telcirkels_landgebruik,
             command = punten_lum_buffer(
               punten_sf = punten,
               radius = 300,
               file = vito_lum_2019_file,
               legend = legend_lum
              ),
             pattern = map(punten)),
  tar_target(name = telcirkels_selectie_landgebruik,
             command = punten_selectie_landgebruik(
               lum_extract_result = telcirkels_landgebruik,
               legend_rast = legend_lum,
               max_prop_overige = 0.5,
               min_prop_akker = 0.3,
               min_prop_akker_grasland = 0.4
               ),
             pattern = map(telcirkels_landgebruik)
  ),
  tar_target(name = punten_steekproefkader,
             command = selectie_landgebruik_vito(
               punten_sf = punten,
               selectie_df = telcirkels_selectie_landgebruik
             ),
             pattern = map(punten, telcirkels_selectie_landgebruik))
)
