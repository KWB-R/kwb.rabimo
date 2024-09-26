# plot_triangle_of_fractions ---------------------------------------------------

#' Plot Triangle of Three Fractions
#'
#' @param fractions numeric vector with three values having a sum of one. The
#'   names of the vector elements are used as labels
#' @param cols vector of length three giving the colour names
#' @export
#' @examples
#' # blue, red, green | left, right, bottom
#' components <- c(runoff = 200, infiltration = 50, evaporation = 100)
#' fractions <- components / sum(components)
#' triangle_of_fractions(fractions)
triangle_of_fractions <- function(fractions, cols = c("blue", "red", "green"))
{
  stopifnot(sum(fractions) == 1)

  grad_to_rad <- function(x) x/180 * pi

  new_point <- function(x, y) {
    data.frame(x = x, y = y)
  }

  new_line <- function(slope, point) {
    list(slope = slope, intersept = point$y - slope * point$x)
  }

  new_axis <- function(origin, phi) {
    function(x) {
      # Convert Polar coordinates to Cartesian coordinates
      data.frame(x = origin$x + x * cos(phi), y = origin$y + x * sin(phi))
    }
  }

  line_crossing <- function(line_1, line_2) {
    x <- (line_2$intersept - line_1$intersept) / (line_1$slope - line_2$slope)
    new_point(x, line_1$slope * x + line_1$intersept)
  }

  arrow_path <- function(data, colour = "black") {
    ggplot2::geom_path(
      data = data,
      colour = colour,
      linewidth = 1,
      arrow = ggplot2::arrow(length = ggplot2::unit(0.3, "cm"))
    )
  }

  tick_path <- function(data, colour = "black") {
    ggplot2::geom_path(data = data, colour = colour)
  }

  rad_60 <- grad_to_rad(60)
  rad_150 <- grad_to_rad(150)
  rad_180 <- grad_to_rad(180)

  axis_1 <- new_axis(new_point(0, 0), phi = rad_60)
  axis_2 <- new_axis(axis_1(1), phi = -rad_60)
  axis_3 <- new_axis(new_point(1, 0), phi = rad_180)

  centre <- line_crossing(
    line_1 = new_line(slope = 0, point = axis_1(fractions[1L])),
    line_2 = new_line(slope = tan(rad_60), point = axis_2(fractions[2L]))
  )

  shift_ortho <- function(points, slope, by) {
    if (slope == 0) {
      dx <- 0
      dy <- by
    } else {
      ortho_slope <- -1/slope
      dx <- by / sqrt(1 + ortho_slope * ortho_slope)
      dy <- ortho_slope * dx
    }
    new_point(points$x + dx, points$y + dy)
  }

  annotate_axis <- function(points, i, angle = 0, size = 4) {
    ggplot2::annotate(
      "text",
      size = size,
      x = points$x,
      y = points$y,
      label = c("0", names(fractions)[i], "1"),
      colour = cols[i],
      angle = angle
    )
  }

  tick_pos <- seq(0, 1, 0.1)
  tick_begs_1 <- axis_1(tick_pos)
  tick_begs_2 <- axis_2(tick_pos)
  tick_begs_3 <- axis_3(tick_pos)

  by <- -0.02
  tick_ends_1 <- shift_ortho(tick_begs_1, tan(rad_60), by)
  tick_ends_2 <- shift_ortho(tick_begs_2, -tan(rad_60), -by)
  tick_ends_3 <- shift_ortho(tick_begs_3, 0, by)

  tick_data <- rbind(
    cbind(tick_begs_1, tick_ends_1),
    cbind(tick_begs_2, tick_ends_2),
    cbind(tick_begs_3, tick_ends_3)
  )

  names(tick_data) <- c("x", "y", "xend", "yend")

  blank <- ggplot2::element_blank()
  blank_theme <- ggplot2::theme(
    panel.grid.major = blank,
    panel.grid.minor = blank,
    axis.text = blank,
    axis.ticks = blank,
    panel.border = blank
  )

  by <- -0.06
  pos <- c(0, 0.5, 1)
  label_points_1 <- shift_ortho(axis_1(pos), tan(rad_60), by)
  label_points_2 <- shift_ortho(axis_2(pos), -tan(rad_60), -by)
  label_points_3 <- shift_ortho(axis_3(pos), 0, by)

  ggplot2::ggplot(mapping = ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_path(
      data = rbind(axis_1(1), axis_2(1), axis_3(1), axis_1(1))
    ) +
    arrow_path(rbind(axis_1(fractions[1L]), centre), col = cols[1L]) +
    arrow_path(rbind(axis_2(fractions[2L]), centre), col = cols[2L]) +
    arrow_path(rbind(axis_3(fractions[3L]), centre), col = cols[3L]) +
    ggplot2::coord_fixed() +
    ggplot2::labs(x = "", y = "") +
    ggplot2::theme_bw() +
    blank_theme +
    ggplot2::geom_segment(data = tick_data, mapping = ggplot2::aes(
      x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend
    )) +
    annotate_axis(label_points_1, 1, angle = 60) +
    annotate_axis(label_points_2, 2, angle = -60) +
    annotate_axis(label_points_3, 3)
}
