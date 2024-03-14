#' Catapult Colour Scales
#'
#' @param option The viridis scale to use (A-H)
#' @param begin The beginning of the colour scale
#' @param end The end of the colour scale
#' @param direction The direction of the colour scale
#' @param legend_size The size of the legend (discrete)
#' @param ... Additional arguments to pass to `scale_colour_viridis_d`
#'
#' @return A colour scale to be used in ggplot2
#' @export
#'
#' @importFrom ggplot2 scale_colour_viridis_d guides guide_legend
scale_colour_catapult_d <- function(option = "D", begin = 0, end = 0.85, direction = 1, legend_size = NULL, ...) {

  if (!is.null(legend_size)) {
    if (!is.numeric(legend_size)) {
      stop("legend_size must be numeric")
    }
  }

  output <- list(
    ggplot2::scale_colour_viridis_d(..., option = option, begin = begin, end = end, direction = direction)
  )

  if (!is.null(legend_size)) {
    output <- c(output, ggplot2::guides(
      colour = ggplot2::guide_legend(override.aes = list(size = legend_size)),
      shape = ggplot2::guide_legend(override.aes = list(size = legend_size))
    ))
  }

  output

}

#' Catapult Colour Scales
#'
#' @param option The viridis scale to use (A-H)
#' @param begin The beginning of the colour scale
#' @param end The end of the colour scale
#' @param direction The direction of the colour scale
#' @param legend_width The width of the legend (continuous)
#' @param legend_height The height of the legend (continuous)
#' @param ... Additional arguments to be passed to `scale_colour_viridis_c`
#'
#' @return A colour scale to be used in ggplot2
#' @export
#'
#' @importFrom ggplot2 scale_colour_viridis_c guides guide_colourbar
scale_colour_catapult_c <- function(option = "D", begin = 0, end = 0.85, direction = 1, legend_width = NULL, legend_height = NULL, ...) {

  output <- list(ggplot2::scale_colour_viridis_c(..., option = option, begin = begin, end = end, direction = direction))

  if (!is.null(legend_width) & !is.null(legend_height)) {
    output <- c(output, ggplot2::guides(
      colour = ggplot2::guide_colourbar(barwidth = legend_width, barheight = legend_height)
    ))
  } else if (!is.null(legend_width)) {
    output <- c(output, ggplot2::guides(
      colour = ggplot2::guide_colourbar(barwidth = legend_width)
    ))
  } else if (!is.null(legend_height)) {
    output <- c(output, ggplot2::guides(
      colour = ggplot2::guide_colourbar(barheight = legend_height)
    ))
  }

  output

}

#' Catapult Colour Scales
#'
#' @param option The viridis scale to use (A-H)
#' @param begin The beginning of the colour scale
#' @param end The end of the colour scale
#' @param direction The direction of the colour scale
#' @param legend_size The size of the legend (discrete)
#' @param ... Additional arguments to pass to `scale_fill_viridis_d`
#'
#' @return A colour scale to be used in ggplot2
#' @export
#'
#' @importFrom ggplot2 scale_fill_viridis_d guides guide_legend
scale_fill_catapult_d <- function(option = "D", begin = 0, end = 0.85, direction = 1, legend_size = NULL, ...) {

  if (!is.null(legend_size)) {
    if (!is.numeric(legend_size)) {
      stop("legend_size must be numeric")
    }
  }

  output <- list(
    ggplot2::scale_fill_viridis_d(..., option = option, begin = begin, end = end, direction = direction)
  )

  if (!is.null(legend_size)) {
    output <- c(output, ggplot2::guides(
      fill = ggplot2::guide_legend(override.aes = list(size = legend_size))
    ))
  }

  output

}

#' Catapult Colour Scales
#'
#' @param option The viridis scale to use (A-H)
#' @param begin The beginning of the colour scale
#' @param end The end of the colour scale
#' @param direction The direction of the colour scale
#' @param legend_width The width of the legend (continuous)
#' @param legend_height The height of the legend (continuous)
#' @param ... Additional arguments to be passed to `scale_fill_viridis_c`
#'
#' @return A colour scale to be used in ggplot2
#' @export
#'
#' @importFrom ggplot2 scale_fill_viridis_c guides guide_colourbar
scale_fill_catapult_c <- function(option = "D", begin = 0, end = 0.85, direction = 1, legend_width = NULL, legend_height = NULL, ...) {

  output <- list(ggplot2::scale_fill_viridis_c(..., option = option, begin = begin, end = end, direction = direction))

  if (!is.null(legend_width) & !is.null(legend_height)) {
    output <- c(output, ggplot2::guides(
      fill = ggplot2::guide_colourbar(barwidth = legend_width, barheight = legend_height)
    ))
  } else if (!is.null(legend_width)) {
    output <- c(output, ggplot2::guides(
      fill = ggplot2::guide_colourbar(barwidth = legend_width)
    ))
  } else if (!is.null(legend_height)) {
    output <- c(output, ggplot2::guides(
      fill = ggplot2::guide_colourbar(barheight = legend_height)
    ))
  }

  output

}


#' Catapult Theme
#'
#' @param ... Additional theme elements
#' @param angle_x The angle of the x-axis text
#' @param angle_y The angle of the y-axis text
#' @param legend.position The position of the legend
#' @param text_size_offset The offset for the text size
#' @param show_gridlines Whether to show gridlines or not
#'
#' @return A theme to be used in ggplot2
#' @export
#'
#' @importFrom ggplot2 theme_linedraw theme element_text element_line element_blank
theme_catapult <- function(..., angle_x = 0, angle_y = 0, legend.position = "bottom", text_size_offset = 0, show_gridlines = TRUE) {

  if (!is.numeric(angle_x) | !is.numeric(angle_y)) {
    stop("angle_x and angle_y must be numeric")
  }

  if (angle_x < 0 | angle_x > 90 | angle_y < 0 | angle_y > 90) {
    stop("angle_x and angle_y must be between 0 and 90")
  }

  if (!is.numeric(text_size_offset)) {
    stop("text_size_offset must be numeric")
  }

  if (!is.logical(show_gridlines)) {
    stop("show_gridlines must be TRUE or FALSE")
  }

  if (!legend.position %in% c("none", "left", "right", "top", "bottom")) {
    stop("legend.position must be one of 'none', 'left', 'right', 'top', or 'bottom'")
  }

  hjust_x <- NULL
  vjust_x <- NULL
  hjust_y <- NULL
  vjust_y <- NULL

  if (angle_x == 0) {
    hjust_x <- 0.5
    vjust_x <- 1
  } else if (angle_x <= 45) {
    hjust_x <- rescale(c(0, angle_x, 45), a = 0.5, b = 1)[2]   # linear interpolation from 0.5-1
    vjust_x <- 1
  } else if (angle_x <= 90) {
    hjust_x <- 1
    vjust_x <- rescale(c(45, angle_x, 90), a = 1, b = 0.5)[2]     # linear interpolation from 1-0.5
  }

  if (angle_y == 0) {
    hjust_y <- 1
    vjust_y <- 0.5
  } else if (angle_y <= 45) {
    hjust_y <- 1
    vjust_y <- 0.5
  } else if (angle_y <= 90) {
    hjust_y <- rescale(c(45, angle_y, 90), a = 1, b = 0.5)[2]   # linear interpolation from 1-0.5
    vjust_y <- 0.5
  }

  output <- ggplot2::theme_linedraw() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 13 + text_size_offset, color = "black"),
      axis.title = ggplot2::element_text(size = 14 + text_size_offset, color = "black", face = "bold"),
      legend.title = ggplot2::element_text(size = 14 + text_size_offset, face = "bold", color = "black", hjust = 0.5, vjust = 1),
      legend.text = ggplot2::element_text(size = 13 + text_size_offset),
      strip.text = ggplot2::element_text(size = 14 + text_size_offset, face = "bold"),
      panel.grid.major = ggplot2::element_line(size = 1/3, color = "#DADADA", linetype = "dashed"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = legend.position,
      plot.title = ggplot2::element_text(size = 16 + text_size_offset, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 13 + text_size_offset, face = "italic"),
      axis.text.x = ggplot2::element_text(angle = angle_x, hjust = hjust_x, vjust = vjust_x),
      axis.text.y = ggplot2::element_text(angle = angle_y, hjust = hjust_y, vjust = vjust_y)
    ) +
    ggplot2::theme(
      ...
    )

  if (!show_gridlines) {
    output <- output +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank()
      )
  }

  output

}
