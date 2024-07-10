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
ONC2_bucc <- mission2_ONC %>% 
  filter(name_fr_clean == "escargot_buccinide")

# Filter and transform as spatial data
ONC2_bucc_carto <- mission2_ONC %>%
  to_carto(name_fr_clean, "escargot_buccinide")

## ------------------------------------------------------------------------
ONC2_bucc %>% 
  count(username) %>% 
  arrange(desc(n))

## ------------------------------------------------------------------------
ONC2_bucc %>% 
  count(image_id) %>% 
  arrange(desc(n))

## ---- echo=FALSE---------------------------------------------------------
ONC2_bucc %>% 
  filter(username == "Lawzama") %>% 
  count(image_id) %>% 
  arrange(desc(n))

gg_users_image(x = ONC2_bucc_carto,
               filter_col = username, filter_val = "Lawzama",
               image_id = 10681)


## ------------------------------------------------------------------------
gg_users_image(x = ONC2_bucc_carto,
               filter_col = username, filter_val = "Kazu",
               image_id = 10681)

## ------------------------------------------------------------------------
gg_users_image(x = ONC2_bucc_carto,
               filter_col = username, filter_val = c("Kazu", "Lawzama"),
               image_id = 10681)

## ------------------------------------------------------------------------
# Number of animals seen on the image by username
ONC2_bucc %>% 
  filter(image_id == 10681) %>% 
  count(user_id)

gg_users_image(x = ONC2_bucc_carto,
               image_id = 10681, buffer = 10)  


## ---- fig.width=10, fig.height=4, out.width='90%'------------------------
# Choose one image for one user
image_id <- "10681"
user_id <- "Kazu-100"

bucc_image_user <- ONC2_bucc_carto %>% 
  dplyr::filter(image_id == !!image_id,
                user_id == !!user_id)

r_image <- raster(bucc_image_user, res = 0.5)
dist_buffer <- 10

# Calculate voronoi for one user_id
image_user_intermediates <- voronoi_rasterize(
  x = bucc_image_user,
  dist_buffer = dist_buffer,
  r_image = r_image,
  keep_intermediate = TRUE)

# Show intermediate steps
p1 <- ggplot(image_user_intermediates$points_in_voronoi) +
  geom_sf(aes(fill = image_pol_id)) +
  geom_sf(data = st_cast(st_geometry(bucc_image_user), "POINT"), size = 0.25) +
  theme_images(x = image_user_intermediates$points_in_voronoi, fill = "c", color = NULL)
  
p2 <- ggplot(image_user_intermediates$points_in_voronoi_in_buffer) +
  geom_sf(aes(fill = image_pol_id)) +
    geom_sf(data = st_cast(st_geometry(bucc_image_user), "POINT"), size = 0.25) +
  theme_images(x = image_user_intermediates$points_in_voronoi, fill = "c", color = NULL)

p3 <- gplot(image_user_intermediates$voronoi_in_buffer_as_raster) +
  geom_tile(aes(fill = value)) +
  theme_images(x = image_user_intermediates$points_in_voronoi, fill = "c", color = NULL, na.value = NA)

cowplot::plot_grid(plotlist = list(p1, p2, p3), ncol = 3)


## ------------------------------------------------------------------------
image_id <- "10681"
bucc_image <- ONC2_bucc_carto %>% 
  dplyr::filter(image_id == !!image_id)

r_image <- raster(bucc_image, res = 0.5)
dist_buffer <- 15

bucc_voronoi_stack <- voronoi_stacker(x = bucc_image,
           dist_buffer = dist_buffer,
           r_image = r_image)
  
gplot(bucc_voronoi_stack) +
  geom_tile(aes(fill = value)) +
  facet_wrap(~variable) +
  theme_images(x = image_user_intermediates$points_in_voronoi,
               fill = "c", color = NULL, na.value = NA) +
  ggtitle("Pixels numbered according to marked individuals")

## ------------------------------------------------------------------------
# Combine layers and find groups of polygons
bucc_groups_count <- group_pixels_count(bucc_voronoi_stack)
bucc_groups_count

## ------------------------------------------------------------------------
#' Find in which groups are each polygons
all_image_pol_ids <- pull(bucc_image, image_pol_id) %>% unique()

bucc_groups_top <- find_top_groups(bucc_groups_count,
                                   all_image_pol_ids)
test_ids_in_group <- test_groups_kept(bucc_groups_top)

# Show in how many groups are individuals (Should be only one)
test_ids_in_group$ids_in_groups_count

## ------------------------------------------------------------------------
# If image_pol_ids are not unique reduce possibilities until it is good
if (test_ids_in_group$max_groups > 1) {
  # Run again group selection while removing groups with problems
  # _Find groups to remove
  test_ids_in_group2 <- test_ids_in_group
  bucc_groups_top2 <- bucc_groups_top
  group_remove2 <- NULL
  
  while (test_ids_in_group2$max_groups > 1) {

    group_remove <- test_ids_in_group2$ids_in_groups_count %>% 
      filter(n > 1) %>% 
      inner_join(test_ids_in_group2$ids_in_groups, by = "list_ids") %>% 
      inner_join(bucc_groups_top2, by = "group_kept") %>% 
      dplyr::select(-image_pol_id) %>% 
      distinct() %>% 
      group_by(list_ids) %>% 
      arrange(desc(n_pols), desc(n_pixels)) %>% 
      slice(-1) %>% 
      pull(group_kept) %>% 
      unique()
    
    group_remove2 <- unique(c(group_remove2, group_remove))
    
    bucc_groups_top2 <- bucc_groups_count %>% 
      filter(!grouped_ids %in% group_remove2) %>% 
      find_top_groups(all_ids = all_image_pol_ids)
    
    # bucc_groups_top2 %>% filter(grepl(7, group_kept))
    # bucc_groups_top2 %>% filter(grepl(32, group_kept))
    
    test_ids_in_group2 <- test_groups_kept(bucc_groups_top2)
    
  }
  # Retrieve success grouping
  bucc_groups_top <- bucc_groups_top2
  # Last verification
  test_ids_in_group <- test_groups_kept(bucc_groups_top)
}

# Show in how many groups are individuals (Should be only one)
test_ids_in_group$ids_in_groups_count

## ------------------------------------------------------------------------
# Add group names in image_sf
bucc_image_grouped <- ONC2_bucc_carto %>% 
  find_groups_in_image(image_id = "10681")

# Create specific image with group names
bucc_image_grouped_groups <- bucc_image_grouped %>% 
  group_by(group_kept) %>% 
  summarize() %>% 
  st_centroid() %>% 
  cbind(st_coordinates(.))

ggplot(bucc_image_grouped %>% 
         mutate(group_kept = 
         forcats::fct_reorder(group_kept, desc(n_pols)))
       ) +
    geom_sf(aes(color = group_kept),
      show.legend = "line",
      size = 2 #alpha = 0.1
    ) +
  ggrepel::geom_text_repel(
    data = bucc_image_grouped_groups,
    aes(x = X, y = Y, label = group_kept)) +
    theme_images(x = bucc_image_grouped, fill = NULL, color = "d", na.value = "grey20") +
  guides(color = FALSE) +
  ggtitle("Identification of groups of marked individuals")

## ---- eval=FALSE---------------------------------------------------------
#  # Chunk not evaluated in Rmd as results are saved
#  ONC2_bucc_carto_groups <- find_groups_in_all_images(ONC2_bucc_carto, .progress = TRUE, keep_list = FALSE, as_sf = FALSE)
#  
#  if (!dir.exists(here::here("inst/outputs"))) {
#   dir.create("inst/outputs", recursive = TRUE)
#  }
#  
#  readr::write_rds(
#    ONC2_bucc_carto_groups,
#    here::here("inst/outputs", "ONC2_bucc_carto_groups.rds"),
#    compress = "gz")
#  

## ------------------------------------------------------------------------
outwd <- system.file("outputs", package = "deeptools")
ONC2_bucc_carto_groups <- readr::read_rds(file.path(outwd, "ONC2_bucc_carto_groups.rds"))

## ------------------------------------------------------------------------
# ONC2_bucc_carto_groups 

# Number of objects per image (already known before)
bucc_nobjects <- ONC2_bucc_carto_groups %>% 
  count(image_id) %>% 
  rename(n_objects = n) %>% 
  count(n_objects) %>% 
  arrange(desc(n)) %>% 
  rename(n_images = n)

# Number of marked objects by images
bucc_nobjects

# Plot
ggplot(bucc_nobjects) +
  geom_col(aes(x = n_objects, y = n_images), width = 1, 
           fill = yellow, colour = grey) +
  ggtitle("Total number of objects identified in images") +
  xlab("Number of objects by image") +
  ylab("Number of images") +
  theme(panel.background = element_rect(fill = blue))

## ------------------------------------------------------------------------
# Stats on groups
bucc_groups <- ONC2_bucc_carto_groups %>% 
  group_by(image_id, group_kept) %>% 
  summarise(
    n_users = n(),
    n_user_id = mean(n_user_id),
    proportion = n()/mean(n_user_id)
  ) %>% 
  ungroup()

# Number of individuals ~ proportion
bucc_groups

# Plot
bucc_groups %>% 
  ggplot() +
  geom_histogram(aes(proportion), bins = 10,
                 fill = yellow, colour = grey) +
  ggtitle("Number of individuals ~ proportion") +
  xlab("Proportion of users who identified an individual") +
  ylab("Number of individuals in this case") +
  theme(panel.background = element_rect(fill = blue))

## ------------------------------------------------------------------------
## Stats on nb groups by image
bucc_ngroups_count <- bucc_groups %>% 
  group_by(image_id) %>% 
  summarise(n_groups = n()) 

# Number of images
bucc_ngroups_count %>% count(n_groups)

# Plot
ggplot(bucc_ngroups_count) +
  geom_histogram(aes(x = n_groups), bins = 30,
                 fill = yellow, colour = grey) +
  ggtitle("Number of groups (~individuals) in images") +
  xlab("Number of groups by image") + ylab("Number of images") +
  theme(panel.background = element_rect(fill = blue))

## ------------------------------------------------------------------------
## Stats on nb groups by image
bucc_ngroups_count_thd <- bucc_groups %>% 
  filter(proportion >= 0.5) %>% 
  group_by(image_id) %>% 
  summarise(n_groups = n()) 

# Number of images
bucc_ngroups_count_thd %>% count(n_groups)

# Plot
ggplot(bucc_ngroups_count_thd) +
  geom_histogram(aes(x = n_groups), bins = 30,
                 fill = yellow, colour = grey) +
  ggtitle("Number of groups (~individuals) in images") +
  xlab("Groups with proportion >= 0.5 by image") + ylab("Number of images") +
  theme(panel.background = element_rect(fill = blue))

## ------------------------------------------------------------------------
# Mean size of individuals
bucc_lengths <- ONC2_bucc_carto_groups %>% 
  left_join(bucc_groups %>% 
              dplyr::select(image_id, group_kept, proportion),
            by = c("image_id", "group_kept")) %>% 
  filter(proportion >= 0.5) %>% 
  group_by(image_id, group_kept) %>% 
  summarise(mean_length = mean(length))

ggplot(bucc_lengths) +
  geom_histogram(aes(mean_length), bins = 50,
                 fill = yellow, colour = grey) +
  scale_x_log10() +
  theme(panel.background = element_rect(fill = blue)) +
  ggtitle("Distribution of lengths of individuals") +
  xlab("Group mean length (log scale)") +
  ylab("Number of individuals")


