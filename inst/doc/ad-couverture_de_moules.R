## ---- include = FALSE----------------------------------------------------
  knitr::opts_chunk$set(
  collapse = FALSE,
  comment = "#>",
  out.width = "90%",
  fig.width = 7,
  fig.height = 6,
  fig.align = "center",
  eval = TRUE
  )

## ---- message=FALSE------------------------------------------------------
library(dplyr)
library(ggplot2)
# devtools::install_github("r-spatial/sf")
library(sf)
library(deeptools)
library(lubridate)
library(thinkr)
library(readr)
library(tidyr)

## ------------------------------------------------------------------------
blue <- "#093564"
yellow <- "#efcc26"
grey <- "#675546"

## ------------------------------------------------------------------------
# load data
export_file <- system.file("data_orig/export_last.csv", package = "deeptools")
liste_photo <- system.file("data_orig/liste_photo.txt", package = "deeptools")

## ------------------------------------------------------------------------
mission2 <- read_csv(export_file) %>%
  dplyr::select(-comment) %>%
  tidyr::extract(name,
          into = "datetime", regex = "_([[:digit:]]+).",
          remove = FALSE
  ) %>%
  mutate(datetime = ymd_hms(datetime, tz = "UTC")) %>%
  # clean names of species
  mutate(name_fr_clean = clean_vec(name_fr, unique = FALSE)) %>% 
  group_by(username) %>%
  mutate(
    user_id = paste(username, as.character(as.numeric(as.factor(datDeb))), sep = "-")
  ) %>%
  ungroup()

# Separate observatory dataset
mission2_MAR <- mission2 %>% filter(obs_code == "MAR")
mission2_ONC <- mission2 %>% filter(obs_code == "JDF")

## ------------------------------------------------------------------------
# Filter on Buccinide only
MAR_mussel <- mission2_MAR %>% 
  filter(name_fr_clean == "couverture_de_moules")

# Filter and transform as spatial data
MAR_mussel_carto <- mission2_MAR %>%
  filter(name_fr_clean == "couverture_de_moules") %>% 
  to_carto(name_fr_clean, "couverture_de_moules")

## ------------------------------------------------------------------------
ggplot(MAR_mussel_carto) +
  geom_sf(aes(fill = as.character(image_id)), alpha = 0.1) +
  guides(fill = FALSE)

## ------------------------------------------------------------------------
MAR_mussel %>% 
  count(username) %>% 
  arrange(desc(n))

## ------------------------------------------------------------------------
MAR_mussel %>% 
  count(image_id) %>% 
  arrange(desc(n))

## ------------------------------------------------------------------------
MAR_mussel %>% 
  group_by(image_id) %>% 
  summarize(n_users = length(unique(user_id))) %>% 
  arrange(desc(n_users))

## ---- echo=FALSE---------------------------------------------------------
MAR_mussel %>% 
  filter(username == "grillus33") %>% 
  count(image_id) %>% 
  arrange(desc(n))

gg_users_image(x = MAR_mussel_carto,
               filter_col = username, filter_val = "grillus33",
               image_id = 14190)


## ------------------------------------------------------------------------
gg_users_image(x = MAR_mussel_carto,
               image_id = 14190)

## ------------------------------------------------------------------------
gg_users_image(x = MAR_mussel_carto,
               image_id = 14190, buffer = 5)

## ---- eval=FALSE---------------------------------------------------------
#  # Chunk not evaluated in Rmd as results are saved
#  MAR_mussel_carto_groups <- MAR_mussel_carto %>%
#    find_groups_in_all_images(.progress = TRUE, keep_list = FALSE,
#                              as_sf = FALSE, dist_buffer = 5)
#  
#  if (!dir.exists(here::here("inst/outputs"))) {
#    dir.create("inst/outputs", recursive = TRUE)
#  }
#  
#  readr::write_rds(
#    MAR_mussel_carto_groups,
#    here::here("inst/outputs", "MAR_mussel_carto_groups.rds"),
#    compress = "gz")
#  

## ------------------------------------------------------------------------
outwd <- system.file("outputs", package = "deeptools")
MAR_mussel_carto_groups <- readr::read_rds(file.path(outwd, "MAR_mussel_carto_groups.rds"))

## ------------------------------------------------------------------------
# Number of objects per image (already known before)
mussel_nobjects <- MAR_mussel_carto_groups %>% 
  count(image_id) %>% 
  rename(n_objects = n) %>% 
  count(n_objects) %>% 
  arrange(desc(n)) %>% 
  rename(n_images = n)

# Number of marked objects by images
mussel_nobjects

# Plot
ggplot(mussel_nobjects) +
  geom_col(aes(x = n_objects, y = n_images), width = 1, 
           fill = yellow, colour = grey) +
  ggtitle("Total number of objects identified in images") +
  xlab("Number of objects by image") +
  ylab("Number of images") +
  theme(panel.background = element_rect(fill = blue))

## ------------------------------------------------------------------------
MAR_mussel_carto_groups_area <- MAR_mussel_carto_groups %>% 
  mutate(area = MAR_mussel_carto_groups %>% 
           st_sf() %>% st_area()) %>% 
  group_by(image_id, group_kept) %>% 
  summarise(nb_polygons_in_group = n(),
            area_mean = mean(area),
            area_sd = sd(area),
            area_sd = if_else(is.na(area_sd), 0, area_sd),
            area_cv = area_sd / area_mean) %>% 
  arrange(desc(nb_polygons_in_group))

MAR_mussel_carto_groups_area

## ------------------------------------------------------------------------
MAR_mussel_carto_groups_area %>% 
  ggplot() +
  geom_histogram(aes(area_mean), bins = 50,
             fill = yellow, colour = grey) +
  ggtitle("Average areas of groups identified in images") +
  xlab("Area (log scale)") +
  ylab("Number of groups") +
  theme(panel.background = element_rect(fill = blue)) +
  scale_x_log10()

