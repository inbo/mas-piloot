SELECT l.id AS location_id
	, reg.name AS regio
	, RIGHT(L.name, 6) AS hok
	, CASE WHEN pl.is_active IS TRUE THEN 'actief' ELSE 'niet actief' END AS status
	, CASE WHEN pl.user_id IS NULL THEN 0 ELSE 1 END AS gereserveerd
	, wp.jaar
	, lv.laatste_bezoek
	, wp2.jaar as recall
FROM public.projects_projectlocation pl
	INNER JOIN public.locations_location l ON l.id = pl.location_id
	LEFT OUTER JOIN 
		(
		SELECT MAX(V.start_date) AS laatste_bezoek
			, location_id
		FROM fieldwork_visit V 
		WHERE V.project_id = 51
		GROUP BY location_id)lv ON lv.location_id = L.id
	LEFT OUTER JOIN 
		(
		SELECT R.id
			, L.name 
		FROM projects_projectregion R
		INNER JOIN locations_location L ON L.id = R.location_id)reg ON reg.id = PL.projectregion_id
	LEFT OUTER JOIN
		(
		SELECT l.id
			, MAX(EXTRACT(YEAR FROM wp.start_date)) as jaar
		FROM projects_workpackage_locations wpl
			INNER JOIN projects_workpackage wp ON wp.id = wpl.workpackage_id
			INNER JOIN locations_location l ON l.id = wpl.location_id
		WHERE 1 = 1
			AND wp.project_id = 51
			AND EXTRACT(YEAR FROM wp.start_date) in (2019, 2020, 2021)
			AND wp.name <> 'Veldwerk 2021bis'
		GROUP BY l.id)wp ON wp.id = l.id
	LEFT OUTER JOIN
		(
		SELECT l.id
			, MAX(EXTRACT(YEAR FROM wp.start_date)) as jaar
		FROM projects_workpackage_locations wpl
			INNER JOIN projects_workpackage wp ON wp.id = wpl.workpackage_id
			INNER JOIN locations_location l ON l.id = wpl.location_id
		WHERE 1 = 1
			AND wp.project_id = 51
			AND wp.name = 'Veldwerk 2021bis'
		GROUP BY l.id)wp2 ON wp2.id = l.id
WHERE 1 = 1
	AND pl.project_id = 51
	AND l.parent_id IS NULL
ORDER BY reg.name, l.name
	
	
	