# MBAG

Monitoring biodiversiteit in het agrarisch gebied

## Doel van de repo

Deze repository bevat voorlopig enkel R functies om data die relevant kunnen zijn voor de ontwikkeling van een monitoringsysteem voor de opvolging van biodiversiteit in het agrarisch gebied samen te brengen (bv download-functies) en te bevragen.
Zo kunnen de data bijvoorbeeld nodig zijn om het steekproefkader te maken (bv landgebruiksdata).
Andere data handelen dan weer over bestaande of recent opgestarte meetnetten waarmee een synergie kan gezocht worden (bv C-mon, LUCAS, ABV).


## Repo structuur


```
.
+-- data
+-- LICENSE
+-- mbag.Rproj
+-- media
|   \-- cmon-sample-allocation.png
+-- R
|   +-- abv.R
|   +-- c_mon.R
|   +-- geocomputations.R
|   +-- landuse_maps.R
|   +-- lucas.R
|   +-- read_agri_use_parcels.R
|   +-- test_grtsdb.R
|   \-- utils.R
+-- README.md
\-- src
    +-- images
    |   \-- cmon-sample-allocation.png
    +-- mbag.json
    +-- verkenning_data.html
    \-- verkenning_data.Rmd
```


De meeste data onder de map `./data/` dienen te worden gedownload met behulp van de functies onder de map `./R/`.

Wanneer al deze data gedownload worden, zal de repo er lokaal ongeveer zo uitzien (de grote bestanden vallen niet onder het `git` versiebeheer):

```
.
\-- data
    +-- abv
    +-- c-mon
    +-- landbouwgebruikspercelen
    |   +-- parquet
    |   \-- Shapefile
    |       \-- extra
    +-- landgebruik
    |   +-- inbo
    |   \-- vito
    \-- lucas_harmonised
        +-- 1_table
        |   \-- parquet_hive
        |       +-- year=2006
        |       +-- year=2009
        |       +-- year=2012
        |       +-- year=2015
        |       \-- year=2018
        +-- 2_geometry
        +-- 3_supporting
        \-- 4_mappings
```



