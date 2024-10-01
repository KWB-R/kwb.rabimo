# plot_triangle_of_fractions ---------------------------------------------------

#' Plot Triangle of Three Fractions
#'
#' @param fractions numeric vector with three values having a sum of one. The
#'   names of the vector elements are used as labels
#' @param fractions_2 optional. Similar to \code{fractions}. If given, these
#'   fractions are shown as dashed lines in the plot and the "deltas" between
#'   \code{fractions} and \code{fractions_2} are shown as horizontally
#'   stacked bars below the triangle.
#' @param cols vector of length three giving the colour names
#' @export
#' @examples
#' # blue, red, green | left, right, bottom
#' components <- c(runoff = 200, infiltration = 50, evaporation = 100)
#' fractions <- components / sum(components)
#' triangle_of_fractions(fractions)
#' triangle_of_fractions(fractions, fractions_2 = c(0.1, 0.3, 0.6))
triangle_of_fractions <- function(
    fractions, fractions_2 = NULL, cols = c("blue", "red", "darkgreen")
)
{
  stopifnot(sum(fractions) == 1)

  rad <- function(x) x/180 * pi
  ortho <- function(phi) phi + pi/2

  new_point <- function(x, y) {
    data.frame(x = x, y = y)
  }

  new_line <- function(slope, point) {
    list(slope = slope, intersept = point$y - slope * point$x)
  }

  new_axis <- function(origin, phi) {
    # Function to convert Polar coordinates to Cartesian coordinates
    function(x) {
      data.frame(x = origin$x + x * cos(phi), y = origin$y + x * sin(phi))
    }
  }

  line_crossing <- function(line_1, line_2) {
    x <- (line_2$intersept - line_1$intersept) / (line_1$slope - line_2$slope)
    new_point(x, line_1$slope * x + line_1$intersept)
  }

  arrow_path <- function(
    data, colour = "black", arrow_length_cm = 0.3, linetype = "solid"
  ) {
    ggplot2::geom_path(
      data = data,
      colour = colour,
      linewidth = 0.8,
      linetype = linetype,
      arrow = grid::arrow(length = ggplot2::unit(arrow_length_cm, "cm")),
      alpha = 0.8
    )
  }

  tick_path <- function(data, colour = "black") {
    ggplot2::geom_path(data = data, colour = colour, alpha = 0.8)
  }

  rad_1 <- rad(60)
  rad_2 <- rad(-60)
  rad_3 <- rad(180)

  axis_1 <- new_axis(new_point(0, 0), phi = rad_1)
  axis_2 <- new_axis(axis_1(1), phi = rad_2)
  axis_3 <- new_axis(new_point(1, 0), phi = rad_3)

  get_crossing <- function(f) {
    line_crossing(
      line_1 = new_line(slope = 0, point = axis_1(f[1L])),
      line_2 = new_line(slope = tan(rad_1), point = axis_2(f[2L]))
    )
  }

  shift_along_angle <- function(points, phi, by) {
    slope <- tan(phi)
    dx <- by / sqrt(1 + slope * slope)
    dy <- slope * dx
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

  # Calculate begin and end positions of axis ticks
  get_tick_data <- function(tick_pos = seq(0, 1, 0.1), tick_len = 0.02) {
    tick_begs_1 <- axis_1(tick_pos)
    tick_begs_2 <- axis_2(tick_pos)
    tick_begs_3 <- axis_3(tick_pos)
    tick_data <- rbind(
      cbind(tick_begs_1, shift_along_angle(tick_begs_1, ortho(rad_1), by = -tick_len)),
      cbind(tick_begs_2, shift_along_angle(tick_begs_2, ortho(rad_2), by = tick_len)),
      cbind(tick_begs_3, shift_along_angle(tick_begs_3, ortho(rad_3), by = -tick_len))
    )
    names(tick_data) <- c("x", "y", "xend", "yend")
    tick_data
  }

  my_theme <- function(x_axis = FALSE) {
    blank <- ggplot2::element_blank()
    theme <- ggplot2::theme(
      panel.grid.major = blank,
      panel.grid.minor = blank,
      axis.text.y = blank,
      axis.ticks.y = blank
    )
    if (isTRUE(x_axis)) {
      return(theme)
    }
    theme + ggplot2::theme(
      axis.text.x = blank,
      axis.ticks.x = blank,
      panel.border = blank
    )
  }

  by <- -0.06
  pos <- c(0, 0.5, 1)
  label_points_1 <- shift_along_angle(axis_1(pos), ortho(rad_1), by)
  label_points_2 <- shift_along_angle(axis_2(pos), ortho(rad_2), -by)
  label_points_3 <- shift_along_angle(axis_3(pos), ortho(rad_3), by)

  p <- ggplot2::ggplot(mapping = ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_path(
      data = rbind(axis_1(1), axis_2(1), axis_3(1), axis_1(1))
    )

  crossing <- get_crossing(f = fractions)
  p <- p +
    arrow_path(rbind(axis_1(fractions[1L]), crossing), col = cols[1L]) +
    arrow_path(rbind(axis_2(fractions[2L]), crossing), col = cols[2L]) +
    arrow_path(rbind(axis_3(fractions[3L]), crossing), col = cols[3L])

  if (!is.null(fractions_2)) {

    crossing <- get_crossing(f = fractions_2)

    p <- p +
      arrow_path(rbind(axis_1(fractions_2[1L]), crossing), col = cols[1L], linetype = "dashed") +
      arrow_path(rbind(axis_2(fractions_2[2L]), crossing), col = cols[2L], linetype = "dashed") +
      arrow_path(rbind(axis_3(fractions_2[3L]), crossing), col = cols[3L], linetype = "dashed")

    thick_line <- function(data, colour, linewidth = 1.5) {
      ggplot2::geom_line(data = data, colour = colour, linewidth = linewidth)
    }

    fracs <- rbind(fractions, fractions_2)

    line_1 <- axis_1(fracs[, 1L])
    line_2 <- axis_2(fracs[, 2L])
    line_3 <- axis_3(fracs[, 3L])

    p <- p +
      thick_line(line_1, cols[1L]) +
      thick_line(line_2, cols[2L]) +
      thick_line(line_3, cols[3L])

    delta_axis <- new_axis(origin = new_point(0, -0.3), phi = 0)
    abs_diffs <- 0.5 * abs(fracs[1L, ] - fracs[2L, ])
    to_values <- cumsum(abs_diffs)
    from_points <- delta_axis(c(0, to_values[-length(to_values)]))
    to_points <- delta_axis(to_values)

    # Delta-W
    p <- p +
      thick_line(rbind(from_points[1L, ], to_points[1L, ]), cols[1L], 5) +
      thick_line(rbind(from_points[2L, ], to_points[2L, ]), cols[2L], 5) +
      thick_line(rbind(from_points[3L, ], to_points[3L, ]), cols[3L], 5) +
      ggplot2::annotate(
        geom = "text",
        x = 0,
        y = -0.2,
        label = sprintf("Wasserhaushaltsdifferenz = %0.2f", sum(abs_diffs)),
        hjust = 0
      )
  }

  p + ggplot2::coord_fixed() +
    ggplot2::labs(x = "", y = "") +
    ggplot2::theme_bw() +
    my_theme(x_axis = !is.null(fractions_2)) +
    ggplot2::geom_segment(data = get_tick_data(), mapping = ggplot2::aes(
      x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend
    )) +
    annotate_axis(label_points_1, 1, angle = 60) +
    annotate_axis(label_points_2, 2, angle = -60) +
    annotate_axis(label_points_3, 3)
}
