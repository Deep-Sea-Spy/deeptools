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
part <- mission2 %>% 
  count(username) %>% 
  mutate(perc = n * 100 / sum(n)) %>% 
  rlang::set_names(c("UserID", "nb_annotations", "percentage")) %>% 
  arrange(desc(nb_annotations)) %>% 
  mutate(sumcum = cumsum(percentage))

# 463 participants
part

## ------------------------------------------------------------------------
nb_annot_part <- part %>% 
  group_by(nb_annotations) %>% 
  summarize(Nb_participants = n()) %>% 
  arrange(desc(nb_annotations)) %>% 
  mutate(sumcum = cumsum(Nb_participants))

nb_annot_part
# Without Chipiok (58552 annotations)
part2 <- part[-1, 1:3]
part2$sumcum <- cumsum(part2$percentage)

## ------------------------------------------------------------------------
## Number of images annotated by participant##
nb_image_part <- mission2 %>%
  dplyr::select(image_id, username) %>%
  distinct() %>%
  count(username) %>%
  arrange(desc(n))

nb_image_part

## ------------------------------------------------------------------------
## Number of time an image was annotated ----
nb_part_image <- mission2 %>%
  dplyr::select(image_id, username) %>%
  distinct() %>%
  dplyr::count(image_id) %>%
  arrange(desc(n))

freq_dis <- nb_part_image %>%
  count(n) %>% 
  rename(n_annotation = n,
         n_images = nn)

freq_dis

## ------------------------------------------------------------------------
# histogram
ggplot(nb_part_image, aes(n)) +
  geom_bar(fill = yellow, colour = grey) +
  theme_bw() +
  xlab("Number of times an image was annotated") +
  ylab("Number of images") +
  ggtitle("Image annotations") +
  theme(panel.background = element_rect(fill = blue))

## ------------------------------------------------------------------------
# Top animal
mission2_ONC %>% 
  group_by(name_fr_clean) %>% 
  summarise(n_images = length(unique(image_id))) %>% 
  arrange(desc(n_images))

# Top user
mission2_ONC %>% 
  group_by(username) %>% 
  summarise(n_images = length(unique(image_id))) %>% 
  arrange(desc(n_images))

# Top image
mission2_ONC %>% 
  group_by(image_id) %>% 
  summarise(n_users = length(unique(username))) %>% 
  arrange(desc(n_users))

## ------------------------------------------------------------------------
mission2_ONC %>% 
  filter(username == "chipiok") %>% 
  group_by(datDeb) %>% 
  summarize(userlevel = mean(userlevel)) %>% 
  ggplot() +
  geom_line(aes(datDeb, userlevel), colour = yellow, size = 2) +
  theme_bw() +
  theme(panel.background = element_rect(fill = blue)) +
  ggtitle("Evolution of level for user 'Chipiok'") +
  ylab("User level") + xlab(NULL)


