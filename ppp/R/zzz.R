utils::globalVariables(unique(c(
  # extract_polygon
  ".",
  # find_groups_in_all_images:
  "image_id",
  "image_pol_id",
  "list_ids",
  # find_groups_in_image:
  "desc",
  "n_pols",
  "n_pixels",
  "group_kept",
  "grouped_ids",
  ".",
  "user_id",
  # find_top_groups
  "grouped_ids",
  "list_ids",
  "image_pol_id",
  "n_pols",
  "n_pixels",
  "group_kept",
  # gg_users_image:
  "user_id",
  ".",
  "grouped_ids",
  "n_pixels",
  # test_groups_kept:
  "group_kept",
  "list_ids",
  # to_carto_grouped_sf:
  "image_id",
  "image_pol_id",
  "pol_id",
  "pos1y",
  # to_carto_point:
  "id_ind",
  "pos1x",
  # to_carto_polygon:
  "polygon_values",
  "geometry",
  "null_geom",
  "image_id",
  "image_pol_id",
  "pol_id",
  #to_carto_segment:
  "pos1y",
  "pos2y",
  "id_ind",
  "pos1x",
  "pos2x",
  # voronoi_rasterize:
  "image_pol_id",
  ".",
  "geometry",
  # voronoi_stacker :
  "user_id",
  "."
  )))
