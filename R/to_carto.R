#' Convert data to spatial object with fake crs
#' 
#' @param x Table with at least pos1x, pos1y, pos2x, pos2y columns
#' @param filter_col optionnal. column on which to apply a filter
#' @param filter_val optionnal. value to filter inside filter_col
#' @param reverse_y Logical. Reverse y coordinates to be shown like original images
#'
#' @importFrom rlang enquo
#' @importFrom dplyr filter
#'
#' @export
to_carto <- function(x, filter_col, filter_val, reverse_y = TRUE) {

  # Filter
  if (!missing(filter_val) & !missing(filter_col)) {
    col <- enquo(filter_col)
    x_tmp <- x %>% filter(!!col == filter_val)
    if (nrow(x_tmp) == 0) {
      stop("There is no '", filter_val, "' in column: '", as.character(filter_col)[2], "'")
    }
  }
  else {
    x_tmp <- x
  }
  if ("polygon_values" %in% colnames(x_tmp)) {
    if (all(is.na(x_tmp$pos2x)) & all(x_tmp$polygon_values == 
                                      "NULL")) {
      res <- to_carto_point(x_tmp, reverse_y = reverse_y)
    }
    else if (!all(is.na(x_tmp$pos2x)) & all(x_tmp$polygon_values == 
                                            "NULL")) {
      res <- to_carto_segment(x_tmp, reverse_y = reverse_y)
    }
    else if (all(is.na(x_tmp$pos2x)) & !all(x_tmp$polygon_values == 
                                            "NULL")) {
      res <- to_carto_polygon(x_tmp, reverse_y = reverse_y)
    }
    else {
      stop("Format of data selected is not uniform among points, segment or polygons")
    }
  } else {
    if (all(is.na(x_tmp$pos2x))) {
      res <- to_carto_point(x_tmp, reverse_y = reverse_y)
    }
    else if (!all(is.na(x_tmp$pos2x))) {
      res <- to_carto_segment(x_tmp, reverse_y = reverse_y)
    }
    else if (all(is.na(x_tmp$pos2x))) {
      res <- to_carto_polygon(x_tmp, reverse_y = reverse_y)
    }
    else {
      stop("Format of data selected is not uniform among points, segment or polygons")
    }
  }
  
  return(res)
}

#' Convert lines with 4 coords to spatial object with fake crs
#'
#' @inheritParams to_carto
#'
#' @importFrom dplyr mutate group_by ungroup n if_else
#' @importFrom sf st_linestring
#'
#' @rdname to_carto
#'
#' @export
to_carto_segment <- function(x, reverse_y = TRUE) {

  x %>%
    mutate(id_ind = as.character(1:n()),
           pos1y = if_else(rep(isTRUE(reverse_y), n()), -pos1y, pos1y),
           pos2y = if_else(rep(isTRUE(reverse_y), n()), -pos2y, pos2y)) %>%
    # slice(1:10) %>%
    group_by(id_ind) %>%
    mutate(geometry = list(st_linestring(
      matrix(c(pos1x, pos1y, pos2x, pos2y),
             ncol = 2, byrow = TRUE)))) %>%
    ungroup() %>%
    to_carto_grouped_sf()
}

#' Convert points to spatial object with fake crs
#'
#' @inheritParams to_carto
#'
#' @importFrom dplyr mutate group_by ungroup n if_else
#' @importFrom sf st_point
#'
#' @rdname to_carto
#'
#' @export
to_carto_point <- function(x, reverse_y = TRUE) {

  x %>%
    mutate(id_ind = as.character(1:n()),
           pos1y = if_else(rep(isTRUE(reverse_y), n()), -pos1y, pos1y)) %>%
    group_by(id_ind) %>%
    mutate(geometry = list(st_point(
      matrix(c(pos1x, pos1y),
             ncol = 2, byrow = TRUE)))) %>%
    ungroup() %>%
    to_carto_grouped_sf()

}

#' Transform a character containing coordinates to sf
#'
#' @param x character with x/y coordinates
#'
#' @inheritParams to_carto
#'
#' @importFrom stringr str_extract_all
#' @importFrom sf st_polygon
#'
extract_polygon <- function(x, reverse_y = TRUE) {
  mat <- str_extract_all(x, "[0-9]+")[[1]] %>%
    as.numeric() %>%
    matrix(ncol = 2, byrow = TRUE) %>%
    rbind(., .[1,])

  if (isTRUE(reverse_y)) {
    mat[,2] <- -1 * mat[,2]
  }

  if (nrow(mat) < 4) {
    # mat %>%
    #   st_linestring()
    NULL
  } else {
    list(mat) %>%
      st_polygon()
  }
}

#' Convert polygons to spatial object with fake crs
#'
#' @inheritParams to_carto
#'
#' @importFrom dplyr mutate group_by ungroup select everything n if_else
#' @importFrom sf st_sf
#' @importFrom purrr map map_lgl
#'
#' @rdname to_carto
#'
#' @export
to_carto_polygon <- function(x, reverse_y = TRUE) {

  x_geom <- x %>%
    mutate(id_ind = as.character(1:n()),
           geometry = map(polygon_values, extract_polygon, reverse_y = reverse_y),
           null_geom = map_lgl(geometry, is.null))

  test_geom <- x_geom %>% filter(null_geom)
  if (nrow(test_geom) > 0) {
    warning("Some polygons had not enough points. There were removed. rows = ",
            paste(test_geom$id_ind, collapse = ", "))
  }

  x_geom %>%
    filter(!null_geom) %>%
    select(-null_geom) %>%
    st_sf(sf_column_name = "geometry", crs = 2154) %>%
    group_by(image_id) %>%
    mutate(image_pol_id = 1:n()) %>%
    mutate(pol_id = paste(image_id, image_pol_id, sep = "_")) %>%
    ungroup() %>%
    select(pol_id, image_id, image_pol_id, everything())
}

#' Transform as grouped sf
#'
#' @param x dataset with geometry column
#'
#' @importFrom dplyr mutate group_by ungroup select everything n
#' @importFrom sf st_sf
#'
to_carto_grouped_sf <- function(x) {
  x %>%
    group_by(image_id) %>%
    mutate(image_pol_id = 1:n()) %>%
    mutate(pol_id = paste(image_id, image_pol_id, sep = "_")) %>%
    ungroup() %>%
    select(pol_id, image_id, image_pol_id, everything()) %>%
    st_sf(sf_column_name = "geometry", crs = 2154)
}
