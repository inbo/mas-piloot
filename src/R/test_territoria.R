library(territoria)
library(dplyr)
library(ggplot2)
library(sf)
library(here)
library(lubridate)

obs <- simulate_observations()
names(obs)
summary(obs$centroids)
summary(obs$observations)
as_tibble(obs$observations)
as_tibble(obs$centroids)

ggplot(data = obs$observations,
       aes(x = x, y = y)) +
  geom_point(aes(colour = observed)) +
  geom_point(
    data = obs$centroids,
    shape = "cross") +
  facet_wrap(~survey)

r1_start <- "04-01"
r1_stop <- "04-20"
r2_start <- "04-21"
r2_stop <- "05-10"
r3_start <- "05-11"
r3_stop <- "06-10"
r4_start <- "06-21"
r4_stop <- "07-15"
tellingen_avimap <- read_sf(
  here("data", "mas",
       "avimap_514_0_MAS_Werkgroep_Grauwe_Kiekendief_Belgi__bezoekstippen.shp")
) %>%
  mutate(
    datum = ymd(paste(jaar, maand, dag, sep = "-")),
    periode_in_jaar = case_when(
      datum %within% interval(
        ymd(paste(jaar, r1_start, sep = "-")),
        ymd(paste(jaar, r1_stop, sep = "-"))) ~ "R1",
      datum %within% interval(
        ymd(paste(jaar, r2_start, sep = "-")),
        ymd(paste(jaar, r2_stop, sep = "-"))) ~ "R2",
      datum %within% interval(
        ymd(paste(jaar, r3_start, sep = "-")),
        ymd(paste(jaar, r3_stop, sep = "-"))) ~ "R3",
      datum %within% interval(
        ymd(paste(jaar, r4_start, sep = "-")),
        ymd(paste(jaar, r4_stop, sep = "-"))) ~ "R4"
    ))

telpunten_avimap <- read_sf(
  here("data", "mas",
       "avimap_514_0_MAS_Werkgroep_Grauwe_Kiekendief_Belgi__telpunten_xy.shp")
) %>%
  rename(teller_2021 = teller) %>%
  mutate(
    regio = ifelse(is.na(regio), "Vlaanderen - Bilzen", regio),
    type_teller_2021 = case_when(
      teller_2021 %in% c("WVNT00", "JJNN16", "NOVN00") |
        regio == "Vlaanderen - Leefdaal" ~ "professioneel",
      is.na(teller_2021) &
        regio != "Vlaanderen - Leefdaal" ~ "niet geteld in 2021",
      TRUE ~ "vrijwilliger")) %>%
  select(-st_x, -st_y)

avimap <- vector("list", 2)
avimap <- setNames(avimap, c("observations", "centroids"))

telpunten_avimap %>%
  filter(regio == "Vlaanderen - Leemstreek",
         !is.na(teller_2021)) %>%
  select(naam) %>%
  bind_cols(
    st_coordinates(.)
    ) %>%
  st_drop_geometry() %>%
  as_tibble() %>%
  janitor::clean_names() -> avimap$centroids

tellingen_avimap %>%
  semi_join(avimap$centroids, by = c("plotnaam" = "naam")) %>%
  filter(jaar == 2021,
         naam == "Geelgors") %>%
  select(survey = periode_in_jaar, status = broedcode, clterr, id) %>%
  bind_cols(
    st_coordinates(.)
  ) %>%
  mutate(
    survey = as.integer(stringr::str_extract(survey, "\\d")),
    status = status + 1 #only for testing, strange only status = 0??
    ) %>%
  st_drop_geometry() %>%
  janitor::clean_names() %>%
  filter(!is.na(status)) -> avimap$observations


nndist <- avimap$observations %>%
  st_as_sf(coords = c("x", "y"), crs = 31370) %>%
  nngeo::st_nn(x = ., y = ., sparse = TRUE, k = 2, returnDist = TRUE,
               maxdist = 300)

purrr::map_dbl(nndist$dist, function(x) x[2]) %>%
  as_tibble() %>%
  ggplot() +
  geom_histogram(aes(x = value), binwidth = 5)

library(mclust)
distdata <- purrr::map_dbl(nndist$dist, function(x) x[2]) %>%
  as_tibble() %>%
  filter(!is.na(value))
mclust_bic <- distdata %>%
  mclust::mclustBIC()

mclust_result <- Mclust(distdata, x = mclust_bic)

mclust_result %>%
  plot(what = "BIC")

mclust_result %>%
  plot(what = "classification")

conn <- connect_db()
fuze_dist <- 175
import_observations(observations = avimap$observations,
                    conn = conn,
                    max_dist = fuze_dist)
result <- get_cluster(conn = conn)
nrow(result$observations) == nrow(result$cluster)

distance_matrix(conn = conn, max_dist = fuze_dist)

cluster_observation(conn = conn, status = 1, max_dist = fuze_dist)
result1 <- get_cluster(conn = conn)
nrow(result1$observations) > nrow(result1$cluster)

dbDisconnect(conn)

result1$cluster %>%
  as_tibble() %>%
  st_as_sf(coords = c("centroid_x", "centroid_y"), crs = 31370) %>%
  ggplot() +
  geom_sf(aes(colour = factor(n_obs)))

result1$observations %>%
  as_tibble() %>%
  st_as_sf(coords = c("x", "y"), crs = 31370) %>%
  ggplot() +
  geom_sf(aes(colour = factor(survey)))

p <- result1$cluster %>%
  full_join(result1$observations) %>%
  as_tibble() %>%
  ggplot() +
  geom_segment(aes(x = centroid_x, y = centroid_y, xend = x, yend = y,
                   group = cluster)) +
  coord_equal()

plotly::ggplotly(p, dynamicTicks = TRUE)

