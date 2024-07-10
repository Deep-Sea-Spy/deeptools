#' Find groups in all images
#'
#' @param keep_list Logical. (debug only) Keep results as list of outputs for all images separated.
#' @param as_sf Logical. Wether to return results as sf object or not.
#' @inheritParams find_groups_in_image
#' @inheritParams furrr::future_map
#'
#' @importFrom dplyr pull
#' @importFrom future plan multisession
#' @importFrom furrr future_map furrr_options
#' @importFrom purrr map_dfr map
#' @importFrom sf st_sf
#'
#' @export

find_groups_in_all_images <- function(x, dist_buffer = 12, .progress = FALSE, workers = future::availableCores(),
                                      keep_list = FALSE, as_sf = FALSE) {

  all_ids <- x %>% pull(image_id) %>% unique()

  # Run calculation in parallel for all images
  plan_og <- plan()
  plan(multisession, workers = workers)

  x_groups_list <- future_map(
    all_ids,
    ~{
      res <- NA
      try(res <- find_groups_in_image(x, image_id = .x,
                                      dist_buffer = dist_buffer))
      return(res)
    },
    .progress = .progress, .options = furrr::furrr_options(seed = 123))

  # Stop multisession  # former multiprocess
  # future::plan(plan_og)
  on.exit(plan(plan_og), add = TRUE)
  gc()

  test <- purrr::map_lgl(x_groups_list, ~(length(.x) == 1))
  w.test <- which(test)
  # .x <- all_ids[w.test[1]]
  # .x <- all_ids[1525]
  # find_groups_in_image(ONC2_bucc_carto, image_id = .x) %>%
  # as_tibble() %>% pull(group_kept) %>% unique() %>% length()

  if (length(w.test) != 0) {
    warning("Something went wrong for: ",
            paste(w.test, collapse = ", "),
            ".")
    if (isTRUE(keep_list)) {
      warning("These are removed from results as keep_list = TRUE")
      x_groups_list <- x_groups_list[-w.test]
    }
  }

  if (isTRUE(keep_list)) {
    if (!isTRUE(as_sf)) {
      x_groups_list <- x_groups_list %>% map(as_tibble)
    }
    return(x_groups_list)
  } else {
    x_groups <- x_groups_list %>%
      map_dfr(as_tibble)
    if (isTRUE(as_sf)) {
      x_groups <- x_groups %>%
        st_sf()
    }
    return(x_groups)
  }

}

#' find_groups_in_image
#'
#' @param x All image in a sf object
#' @param image_id image_id on which to find groups
#' @param dist_buffer Numeric. Distance of the buffering area. The smaller the better.
#'
#' @importFrom dplyr pull filter inner_join select distinct group_by arrange slice left_join mutate desc
#' @importFrom raster raster
#'
#' @export

find_groups_in_image <- function(x, image_id, dist_buffer = 12) {
# browser()
  if (missing(image_id)) {
    image_id <- unique(pull(x, image_id))[1]
  }

  x_image <- x %>%
    filter(image_id == !!image_id)

  all_image_pol_ids <- x_image %>% pull(image_pol_id) %>% unique()
  r_image <- raster(st_buffer(x_image, 1), res = 0.5)

  # TODO
  # Not sure rasterize is useful. Try with :
  # With st_intersection() et n.overlaps + origins
  # https://thinkr.fr/logo-thinkr-cree-avec-librairie-sf/
  # + st_area

  # Rasterize with buffer
  x_voronoi_stack <- voronoi_stacker(x = x_image,
                                     dist_buffer = dist_buffer,
                                     r_image = r_image)

  # Find groups
  x_groups_count <- group_pixels_count(x_voronoi_stack)
  x_groups_top <- find_top_groups(x_groups_count,
                                  all_image_pol_ids)
  test_ids_in_group <- test_groups_kept(x_groups_top)

  # Reduce possibilities until it is good if needed
  if (test_ids_in_group$max_groups > 1) {
    test_ids_in_group2 <- test_ids_in_group
    x_groups_top2 <- x_groups_top
    group_remove2 <- NULL

    while (test_ids_in_group2$max_groups > 1) {
      # Find groups to remove
      group_remove <- test_ids_in_group2$ids_in_groups_count %>%
        filter(n > 1) %>%
        inner_join(test_ids_in_group2$ids_in_groups, by = "list_ids") %>%
        inner_join(x_groups_top2, by = "group_kept") %>%
        dplyr::select(-image_pol_id) %>%
        distinct() %>%
        group_by(list_ids) %>%
        arrange(desc(n_pixels), desc(n_pols)) %>%
        slice(-1) %>%
        pull(group_kept) %>%
        unique()

      # Collect removed groups
      group_remove2 <- unique(c(group_remove2, group_remove))

      # Find best groups on filtered groups
      x_groups_top2 <- x_groups_count %>%
        filter(!grouped_ids %in% group_remove2) %>%
        find_top_groups(all_ids = all_image_pol_ids)

      # Test new grouping
      test_ids_in_group2 <- test_groups_kept(x_groups_top2)
    }
    # Retrieve success grouping
    x_groups_top <- x_groups_top2
  }

  # Add group names in image_sf
  x_image_grouped <- x_image %>%
    left_join(x_groups_top, by = "image_pol_id") %>%
    mutate(n_user_id = length(unique(pull(., user_id))))

  return(x_image_grouped)
}

#' Create raster Stack of voronoi around sf for each image
#'
#' @param x sf object for multiple users with a column named "user_id"
#' @param image_id value on which to filter x for a unique image_id
#' @inheritParams voronoi_rasterize
#'
#' @details If there is no column names "image_id",
#' this function assumes all features are from same image
#'
#' @importFrom raster raster stack
#' @importFrom purrr map
#' @importFrom dplyr mutate filter
#' @importFrom stats setNames
#'
#' @export
voronoi_stacker <- function(x,
                            dist_buffer = 12,
                            r_image,
                            image_id) {

  if (!"image_pol_id" %in% names(x)) {
    x <- x %>% mutate(image_pol_id = 1:n())
  }

  if (missing(r_image)) {
    r_image <- raster(x, res = 0.5)
  }
  # image_id column
  if ("image_id" %in% names(x)) {
    all_ids <- unique(x[["image_id"]])
  } else {
    all_ids <- NULL
  }

  if (is.null(all_ids)[1]) {
    warning("No column named 'image_id'. ",
            "Assumes all features are from same image.",
            "x won't be filtered.")
    image_id <- NULL
  } else if (missing(image_id)) {
    if (length(all_ids) == 1) {
      image_id <- all_ids
    } else {
      warning("Multiple levels in 'image_id' column, only the first is kept: ", all_ids[1], ".")
      image_id <- all_ids[1]
    }
  }
  if (!is.null(image_id)) {
    x <- x %>% filter(image_id == !!image_id)
  }

  # .x <- unique(x[["user_id"]])[4]
  res <- map(unique(x[["user_id"]]), function(.x) {
    voronoi_rasterize(x = x %>% filter(user_id == .x),
                      dist_buffer = dist_buffer,
                      r_image = r_image)
  }) %>%
    stack() %>%
    setNames(., unique(x[["user_id"]]))

  return(res)
}

#' Create raster Stack of voronoi and buffer around sf
#'
#' @param x sf object for a unique user
#' @param dist_buffer distance in pixel to crop voronoi around sf object
#' @param r_image raster object of the complete image to be the basis for rasterizing sf object
#' @param keep_intermediate Logical. If TRUE, return a list intermediate spatial objects
#'
#' @importFrom sf st_cast st_buffer st_union st_voronoi st_sf st_join st_intersection
#' @importFrom sf st_sample st_length st_is st_set_agr st_geometry
#' @importFrom dplyr mutate group_by summarize as_tibble select
#' @importFrom raster raster
#' @importFrom fasterize fasterize
#'
#' @export
voronoi_rasterize <- function(x, dist_buffer = 12, r_image,
                              keep_intermediate = FALSE) {

  if (!"image_pol_id" %in% names(x)) {
    x <- x %>% mutate(image_pol_id = 1:n())
  }

  if (missing(r_image)) {
    r_image <- raster(x, res = 0.5)
  }

  buffer <- x %>%
    st_buffer(dist = dist_buffer)

  # nb_point_by_geom in case of future polygons
  nb_point_by_geom <- x %>%
    st_cast("POINT", warn = FALSE) %>%
    count(image_pol_id) %>%
    pull(n) %>% max()

  if (!isTRUE(all(st_is(x, c("POLYGON", "MULTIPOLYGON"))))) {
    if (isTRUE(all(st_is(x, c("POINT", "MULTIPOINT"))))) {
      x_sample <- x
    } else if (isTRUE(all(st_is(x, c("LINESTRING", "MULTILINESTRING"))))) {
      x_sample <- x %>%
        st_cast("MULTILINESTRING") %>%
        st_sample(size = rep(2 * nb_point_by_geom, length(x)), type = "regular") %>%
        cbind(., as_tibble(x) %>% dplyr::select(-geometry)) %>%
        st_sf() %>%
        st_cast("POINT", warn = FALSE)
    } else {
      stop("Geometry is not POINT, LINESTRING or POLYGON")
    }

    # Transform as voronoi and intersect with buffer
    if (nrow(x_sample) == 1) {
      points_in_voronoi_in_buffer <- points_in_voronoi <- buffer %>%
        mutate(voronoi = 1:n()) %>%
        st_cast("MULTIPOLYGON")
    } else {
      points_in_voronoi <- x_sample %>%
        st_union() %>%
        st_voronoi() %>%
        st_cast() %>%
        st_sf() %>%
        mutate(voronoi = 1:n())  %>%
        st_join(x_sample) %>%
        group_by(image_pol_id) %>%
        summarize()

      st_set_agr(points_in_voronoi, "constant")
      points_in_voronoi_in_buffer <- points_in_voronoi %>%
        st_intersection(st_geometry(st_union(buffer))) %>%
        st_cast("MULTIPOLYGON")
    }

  } else {
    # For Polygons directly
    points_in_voronoi_in_buffer <- points_in_voronoi <- x
  }

  voronoi_in_buffer_as_raster <- fasterize(
    points_in_voronoi_in_buffer,
    r_image, field = "image_pol_id",
    fun = "last")

  if (isTRUE(keep_intermediate)) {
    return(list(
      points_in_voronoi = points_in_voronoi,
      points_in_voronoi_in_buffer = points_in_voronoi_in_buffer,
      voronoi_in_buffer_as_raster = voronoi_in_buffer_as_raster
    ))
  } else {
    return(voronoi_in_buffer_as_raster)
  }
}

#' Count number of pixels by groups of image_pol_id from stack
#'
#' @param x stack as issued from \code{\link{voronoi_stacker}}
#'
#' @importFrom raster as.matrix
#' @importFrom dplyr as_tibble filter_all group_by summarise arrange any_vars desc
#' @importFrom tidyr unite
#'
#' @export
group_pixels_count <- function(x) {

  x %>%
    # as.data.frame() %>%
    as.matrix() %>%
    as_tibble() %>%
    filter(if_any(everything(), ~ !is.na(.))) %>% 
    unite(col = grouped_ids, sep = "-", remove = FALSE) %>%
    group_by(grouped_ids) %>%
    summarise(n_pixels = n()) %>%
    arrange(desc(n_pixels))
}

#' Retrieve numeric values out of character
#'
#' @param x character
as_num_nona <- function(x) {
  x[(x != "NA" & nzchar(x))] %>% as.numeric()
}

#' Find best group for each image_pol_id independently
#'
#' @param x dataframe as issued from \code{\link{group_pixels_count}}
#' @param all_ids vector of all image_pol_id
#'
#' @importFrom dplyr mutate rename group_by top_n ungroup right_join tibble
#' @importFrom purrr map map_dbl
#' @importFrom stringr str_split
#' @importFrom tidyr unnest
#'
#' @details If an image_pol_id has no group left, it is included in a group alone
#'
#' @export

find_top_groups <- function(x, all_ids) {

  x %>%
    mutate(list_ids = map(
      grouped_ids, ~str_split(.x, pattern = "-") %>%
        unlist() %>%
        as_num_nona()
    )) %>%
    mutate(n_pols = map_dbl(list_ids, length)) %>%
    unnest(list_ids) %>%
    rename(image_pol_id = list_ids) %>%
    group_by(image_pol_id) %>%
    top_n(1, n_pols) %>%
    # slice_max(n = 1, order_by = n_pols) %>%
    top_n(1, n_pixels) %>%
    # slice_max(n = 1, order_by = n_pixels) %>%
    ungroup() %>%
    rename(group_kept = grouped_ids) %>%
    right_join(tibble(all_ids), by = c("image_pol_id" = "all_ids")) %>%
    mutate(
      n_pixels =
        if_else(is.na(group_kept), 1L, n_pixels),
      n_pols =
        if_else(is.na(group_kept), 1, n_pols),
      group_kept =
        if_else(is.na(group_kept), paste0("-", image_pol_id, "-"), group_kept)
    )
}

#' Test if some image_pol_id are found in multiple groups
#'
#' @param x dataframe as issued from \code{\link{find_top_groups}}
#'
#' @importFrom dplyr mutate select distinct count arrange pull desc
#' @importFrom purrr map
#' @importFrom stringr str_split
#' @importFrom tidyr unnest
#'
#' @export
test_groups_kept <- function(x) {

  ids_in_groups <- x %>%
    mutate(list_ids = map(
      group_kept, ~str_split(.x, pattern = "-") %>%
        unlist() %>%
        as_num_nona()
    )) %>%
    unnest(list_ids) %>%
    dplyr::select(group_kept, list_ids) %>%
    distinct()

  ids_in_groups_count <- ids_in_groups %>%
    count(list_ids) %>%
    arrange(desc(n))

  max_groups <- ids_in_groups_count %>%
    pull(n) %>% max()

  list(ids_in_groups = ids_in_groups,
       ids_in_groups_count = ids_in_groups_count,
       max_groups = max_groups)

}
