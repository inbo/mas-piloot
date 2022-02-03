#' Read ABV data UTM squares
#'
#' @return a sf object with UTM 1 km squares selected for ABV
#' @export
#'
#' @examples
abv_read_utm_squares <- function() {
  require(dplyr)
  abv_grid <- readr::read_csv("./data/abv/data-1626258505755.csv",
                              na = c("", "NA", "NULL"))
  utm1belgium <- read_utm_sf()
  abv_grid_sf <- utm1belgium %>%
    inner_join(abv_grid,
               by = c("TAG" = "hok")) %>%
    relocate(TAG, geometry, .after = recall) %>%
    rename(utm_hok = TAG) %>%
    select(-Shape_Area, -Shape_Area)
  return(abv_grid_sf)
}


#' Reconstructed sampling frame of the ABV monitoring scheme
#'
#' @param file relative path to the tsv file
#'
#'
#' @return a data.frame
#' @export
#'
#' @examples
abv_read_sampling_frame <- function(file = "data/abv/sampling_frame.tsv") {
  git2rdata::read_vc(file)
}

#' Get ABV Visits
#'
#' @param con a database connection object
#'
#' @importFrom inbodb connect_inbo_db dbDisconnect
#' @importFrom dplyr tbl sql collect
#'
#' @return a data.frame
#' @export
#'
#' @examples
abv_get_visits <- function(con) {

  sql_visits <- "SELECT v.id AS visit_id
	--, u.first_name + ' ' + u.last_name AS teller
	, l.name AS telhok
	, v.start_date AS datum
	, CASE
		WHEN v.status = -1 THEN 'weersomstandigheden waren ongunstig'
		WHEN v.status = -2 THEN 'telmethode uit handleiding niet gevolgd'
		WHEN v.status = -3 THEN 'geen veldwerk mogelijk - locatie ontoegankelijk'
		WHEN v.status = -4 THEN 'geen veldwerk mogelijk - locatie is ongeschikt voor de soort'
		ELSE 'veldwerk is zonder problemen verlopen'
	  END AS verloop
	, l.geom.MakeValid().STCentroid().STX lon_centroid_hok
	, l.geom.MakeValid().STCentroid().STY lat_centroid_hok
	, v.analysis AS analyse
	, v.year_target AS jaardoel
	, REPLACE(REPLACE(v.notes, CHAR(13), ''), CHAR(10), '') AS opmerking--remove carriage return/linefeed in comments to avoid problems in csv export
FROM staging_meetnetten.projects_project p
	INNER JOIN staging_meetnetten.fieldwork_visit v ON v.project_id = p.id
	INNER JOIN staging_meetnetten.locations_location l ON l.id = v.location_id
	--INNER JOIN staging_meetnetten.accounts_user u ON u.id = v.user_id
WHERE 1=1
	AND P.Id = 51 --ABV
	AND V.validation_status <> -1 --geen afgekeurde bezoeken
"

  visits <- tbl(con, sql(sql_visits)) %>%
    collect()
  return(visits)
}


#' Get ABV observations
#'
#'
#' @param con a database connection object
#' @importFrom inbodb connect_inbo_db dbDisconnect
#' @importFrom dplyr tbl sql collect
#'
#' @return a lazy tbl
#' @export
#'
#' @examples
abv_get_observations <- function(con) {

  sql_observations <- "SELECT v.id AS visit_id
	, v.start_date AS datum
	, s.id AS sample_id
	, ls.name AS telpunt
	, s.not_counted AS niet_geteld
	, o.id AS observation_id
	, COALESCE(o.geom.MakeValid().STX, ls.geom.MakeValid().STX) AS lon
	, COALESCE(o.geom.MakeValid().STY, ls.geom.MakeValid().STY) AS lat
	--, o.is_bound_to_location
	, CASE WHEN o.is_bound_to_location = 1 THEN 'telpunt' ELSE 'exact' END AS precisie
	, sp.name AS soort
	, o.number_min AS aantal
	--, CASE WHEN o.validation_status_id = -1	THEN 'accepted (open)'
	--	WHEN o.validation_status_id = 2	THEN 'request more information (in behandeling)'
	--	WHEN o.validation_status_id = 3	THEN 'information added (in behandeling)'
	--	WHEN o.validation_status_id = 4	THEN 'approved (goedgekeurd)'
	--	WHEN o.validation_status_id = 5	THEN 'rejected (afgekeurd)'
	--	WHEN o.validation_status_id = 6	THEN 'inconclusive (niet te beoordelen)'
	--	WHEN o.validation_status_id = 7	THEN 'system validated (gevalideerd via bezoek)'
	--	ELSE 'onbekend'
	--  END AS validatiestatus
	, o.source
FROM staging_meetnetten.projects_project p
	INNER JOIN staging_meetnetten.fieldwork_visit v ON v.project_id = p.id
	LEFT JOIN staging_meetnetten.fieldwork_sample s ON s.visit_id = v.id
	LEFT JOIN staging_meetnetten.locations_location ls ON ls.id = s.location_id
	LEFT JOIN staging_meetnetten.fieldwork_observation o ON o.sample_id = s.id
	LEFT JOIN staging_meetnetten.species_species sp ON sp.id = o.species_id
	LEFT JOIN staging_meetnetten.locations_location lo ON lo.id = o.location_id
WHERE 1=1
	AND p.Id = 51 --ABV
	AND v.validation_status <> -1 --geen afgekeurde bezoeken
	AND o.validation_status_id in (-1,4,7) --open/goedgekeurd/gevalideerd via bezoek
"


  observations <- dplyr::tbl(
    con,
    dplyr::sql(sql_observations))

  return(observations)
  }
