#' Special ggplot theme for pixel images
#'
#' @param x sf object to get crs from
#' @param fill "c" for viridis continuous scale, "d" for viridis discrete scale, NULL otherwise
#' @param color "c" for viridis continuous scale, "d" for viridis discrete scale, NULL otherwise
#' @param xmax size of the image in x (in pixels)
#' @param ymax size of the image in y (in pixels)
#' @param ... More options for theme()
#'
#' @inheritParams ggplot2::scale_colour_gradient
#' @inheritParams to_carto
#'
#' @importFrom ggplot2 theme_bw %+replace% theme scale_color_viridis_d scale_fill_viridis_d scale_color_viridis_c scale_fill_viridis_c coord_sf
#' @importFrom sf st_crs
#'
#' @export
theme_images <- function(x, fill = "d", color = "d",  na.value = "grey50" ,
                         xmax = 1920, ymax = 1080, reverse_y = TRUE, ...) {
  res <- list()
  if (!is.null(color)) {
    if (color == "d") {
      res[[1]] <- scale_color_viridis_d(na.value = na.value)
    } else if (color == "c") {
      res[[1]] <- scale_color_viridis_c(na.value = na.value)
    }
  }
  if (!is.null(fill)) {
    if (fill == "d") {
      res[[2]] <- scale_fill_viridis_d(na.value = na.value)
    } else if (fill == "c") {
      res[[2]] <- scale_fill_viridis_c(na.value = na.value)
    }
  }
  res[[3]] <- theme_bw() %+replace% theme(...)
  res[[4]] <- coord_sf(
    crs = st_crs(x), datum = st_crs(x),
    expand = FALSE,
    xlim = c(1, xmax),
    ylim = c(
      ifelse(isTRUE(reverse_y), -ymax, 1),
      ifelse(isTRUE(reverse_y), -1, ymax))
  )
  return(res)
}


#' Draw position of individuals seen by a user on an image
#'
#' @param x spatial dataframe as issued from \code{\link{to_carto_segment}}
#' @param image_id image_id
#' @param filter_col optionnal. column on which to realize a filter
#' @param filter_val optionnal. value for the filter.col. Can be a vector of values.
#' @param path optionnal. Path where to save output image if set.
#' @param img_name name used if path is not empty
#' @param buffer optionnal. Numeric. Size of the buffer area to show.
#'
#' @importFrom dplyr filter mutate n group_by ungroup
#' @importFrom ggplot2 ggplot aes geom_sf ggsave facet_wrap scale_color_viridis_d coord_sf theme_bw
#' @importFrom sf st_bbox
#'
#' @examples
#' \dontrun{
#' gg_users_image(x = ONC2_bucc_carto, image_id = 12937)
#' gg_users_image(x = ONC2_bucc_carto, filter_col = username, filter_val = "2olg", image_id = 12937)
#' }
#'
#' @export
gg_users_image <- function(x, image_id, filter_col, filter_val, path,
                           img_name, buffer, alpha = 0.7) {

  if (!missing(filter_col)) {
    filter_col <- enquo(filter_col)
    x_tmp <- x %>%
      filter(!!filter_col %in% filter_val)
  } else {
    x_tmp <- x
  }

  # user_id in original dataset
  x_plot <- x_tmp %>%
    filter(image_id == !!image_id)
  # group_by(username) %>%
  # mutate(
  #   id = as.character(as.numeric(as.factor(datDeb))),
  #   user_id = paste(username, id, sep = "-")
  # ) %>%
  # ungroup()

  if (!missing(buffer)) {
    x_plot <- st_buffer(x_plot, dist = buffer)
    size <- 0.1
    show.legend <- "polygon"
  } else {
    size <- 1
    show.legend <- "line"
  }

  g <- ggplot(x_plot) +
    geom_sf(aes(color = user_id, fill = user_id),
            show.legend = show.legend,
            size = size, alpha = alpha
    ) +
    theme_images(x_plot, fill = "d", color = "d",
                 reverse_y = (abs(st_bbox(x_plot)[2]) >= abs(st_bbox(x_plot)[4])))

    # scale_color_viridis_d() +
    # scale_fill_viridis_d() +
    # coord_sf(
    #   crs = st_crs(x_plot), datum = sf::st_crs(x_plot),
    #   expand = FALSE
    # ) +
    # theme_bw()

  if (!missing(path)) {
    if (missing(img_name)) {
      img_name <- paste0(image_id, ".png")
    }
    ggsave(file.path(path, img_name), plot = g)
  }

  g
}
