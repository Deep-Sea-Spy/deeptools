## ----setup, include = FALSE----------------------------------------------
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
library(lubridate)
library(tidyr)
library(ggplot2)
# devtools::install_github("r-spatial/sf")
library(sf)
library(raster)
library(fasterize)
library(igraph)
library(rasterVis)
library(cowplot)
library(deeptools)
# devtools::document()
# devtools::document()
# devtools::load_all(here::here(""))

## ------------------------------------------------------------------------
blue <- "#093564"
yellow <- "#efcc26"
grey <- "#675546"

## ------------------------------------------------------------------------
# load data
export_file <- system.file("data_orig/export_last.csv", package = "deeptools")
liste_photo <- system.file("data_orig/liste_photo.txt", package = "deeptools")

## ------------------------------------------------------------------------
mission2 <- readr::read_csv(export_file) %>%
  dplyr::select(-comment) %>%
  tidyr::extract(name,
          into = "datetime", regex = "_([[:digit:]]+).",
          remove = FALSE
  ) %>%
  mutate(datetime = ymd_hms(datetime, tz = "UTC")) %>%
  # clean names of species
  mutate(name_fr_clean = thinkr::clean_vec(name_fr, unique = FALSE)) %>% 
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
ONC2_pyc <- mission2_ONC %>% 
  filter(name_fr_clean == "pycnogonide")

# Filter and transform as spatial data
ONC2_pyc_carto <- mission2_ONC %>%
  to_carto(name_fr_clean, "pycnogonide")

## ------------------------------------------------------------------------
ONC2_pyc %>% 
  count(username) %>% 
  arrange(desc(n))

## ------------------------------------------------------------------------
ONC2_pyc %>% 
  count(image_id) %>% 
  arrange(desc(n))

## ------------------------------------------------------------------------
ONC2_pyc %>% 
  group_by(image_id) %>% 
  summarize(n_users = length(unique(user_id))) %>% 
  arrange(desc(n_users))

## ---- echo=FALSE---------------------------------------------------------
ONC2_pyc %>% 
  filter(username == "grillus33") %>% 
  count(image_id) %>% 
  arrange(desc(n))

gg_users_image(x = ONC2_pyc_carto,
               filter_col = username, filter_val = "grillus33",
               image_id = 12738)


## ------------------------------------------------------------------------
gg_users_image(x = ONC2_pyc_carto,
               image_id = 11425)

## ------------------------------------------------------------------------
gg_users_image(x = ONC2_pyc_carto,
               image_id = 11425, buffer = 40)

## ---- eval=FALSE---------------------------------------------------------
#  # Chunk not evaluated in Rmd as results are saved
#  ONC2_pyc_carto_groups <- find_groups_in_all_images(ONC2_pyc_carto, .progress = TRUE, keep_list = FALSE, as_sf = FALSE, dist_buffer = 40)
#  
#  if (!dir.exists(here::here("inst/outputs"))) {
#    dir.create("inst/outputs", recursive = TRUE)
#  }
#  
#  readr::write_rds(
#    ONC2_pyc_carto_groups,
#    here::here("inst/outputs", "ONC2_pyc_carto_groups.rds"),
#    compress = "gz")
#  

## ------------------------------------------------------------------------
outwd <- system.file("outputs", package = "deeptools")
ONC2_pyc_carto_groups <- readr::read_rds(file.path(outwd, "ONC2_pyc_carto_groups.rds"))

## ------------------------------------------------------------------------
# Number of objects per image (already known before)
pyc_nobjects <- ONC2_pyc_carto_groups %>% 
  count(image_id) %>% 
  rename(n_objects = n) %>% 
  count(n_objects) %>% 
  arrange(desc(n)) %>% 
  rename(n_images = n)

# Number of marked objects by images
pyc_nobjects

# Plot
ggplot(pyc_nobjects) +
  geom_col(aes(x = n_objects, y = n_images), width = 1, 
           fill = yellow, colour = grey) +
  ggtitle("Total number of objects identified in images") +
  xlab("Number of objects by image") +
  ylab("Number of images") +
  theme(panel.background = element_rect(fill = blue))

## ------------------------------------------------------------------------
# Stats on groups
pyc_groups <- ONC2_pyc_carto_groups %>% 
  group_by(image_id, group_kept) %>% 
  summarise(
    n_users = n(),
    n_user_id = mean(n_user_id),
    proportion = n()/mean(n_user_id)
  ) %>% 
  ungroup()

# Number of individuals ~ proportion
pyc_groups

# Plot
pyc_groups %>% 
  ggplot() +
  geom_histogram(aes(proportion), bins = 10,
                 fill = yellow, colour = grey) +
  ggtitle("Number of individuals ~ proportion") +
  xlab("Proportion of users who identified an individual") +
  ylab("Number of individuals in this case") +
  theme(panel.background = element_rect(fill = blue))

## ------------------------------------------------------------------------
## Stats on nb groups by image
pyc_ngroups_count <- pyc_groups %>% 
  group_by(image_id) %>% 
  summarise(n_groups = n()) 

# Number of images
pyc_ngroups_count %>% count(n_groups)

# Plot
ggplot(pyc_ngroups_count) +
  geom_histogram(aes(x = n_groups), bins = 30,
                 fill = yellow, colour = grey) +
  ggtitle("Number of groups (~individuals) in images") +
  xlab("Number of groups by image") + ylab("Number of images") +
  theme(panel.background = element_rect(fill = blue))

## ------------------------------------------------------------------------
## Stats on nb groups by image
pyc_ngroups_count_thd <- pyc_groups %>% 
  filter(proportion >= 0.5) %>% 
  group_by(image_id) %>% 
  summarise(n_groups = n()) 

# Number of images
pyc_ngroups_count_thd %>% count(n_groups)

# Plot
ggplot(pyc_ngroups_count_thd) +
  geom_histogram(aes(x = n_groups), bins = 30,
                 fill = yellow, colour = grey) +
  ggtitle("Number of groups (~individuals) in images") +
  xlab("Groups with proportion >= 0.5 by image") + ylab("Number of images") +
  theme(panel.background = element_rect(fill = blue))

