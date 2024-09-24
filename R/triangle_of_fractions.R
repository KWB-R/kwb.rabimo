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

  point <- function(x, y) data.frame(x = x, y = y)
  grad_to_rad <- function(x) x/180 * pi
  sin_grad <- function(x) sin(grad_to_rad(x))
  cos_grad <- function(x) cos(grad_to_rad(x))
  tan_grad <- function(x) tan(grad_to_rad(x))
  linear_n <- function(x, y, m) y - m * x

  y_fun <- function(m, n) function(x) {m * x + n}
  x_fun <- function(m, n) function(y) {stopifnot(m != 0);(y - n) / m}

  shift_x <- function(dist = 0.04) dist * sin_grad(60)
  shift_y <- function(dist = 0.04) dist * cos_grad(60)

  ytop <- 0.5 * sqrt(3)

  triangle <- rbind(
    point(-0.5,    0),
    point(   0, ytop),
    point(+0.5,    0),
    point(-0.5,    0)
  )

  # Share 1 (starting from left leg of triangle)
  dx_1 <- fractions[1L] * cos_grad(60)
  dy_1 <- fractions[1L] * sin_grad(60)

  # Share 2 (starting from right leg of triangle)
  dx_2 <- fractions[2L] * sin_grad(30)
  dy_2 <- fractions[2L] * cos_grad(30)
  slope_2 <- tan_grad(60)
  intersept_2 <- linear_n(x = dx_2, y = ytop - dy_2, m = slope_2)
  x_fun_2 <- x_fun(slope_2, intersept_2)
  y_fun_2 <- y_fun(slope_2, intersept_2)

  # Share 3 (starting from bottom leg of triangle)
  slope_3 <- -tan_grad(60)
  intersept_3 <- linear_n(x = x_fun_2(dy_1), y = dy_1, m = slope_3)
  x_fun_3 <- x_fun(slope_3, intersept_3)
  centre <- point(x_fun_2(dy_1), dy_1)

  my_path <- function(data, colour) ggplot2::geom_path(
    data = data,
    colour = colour,
    linewidth = 1,
    arrow = ggplot2::arrow(length = ggplot2::unit(0.3, "cm"))
  )

  my_annotate <- function(...) ggplot2::annotate("text", ...)

  ggplot2::ggplot(mapping = ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_path(data = triangle) +
    my_path(rbind(point(-0.5 + dx_1, dy_1), centre), cols[1L]) +
    my_path(rbind(point(dx_2, y_fun_2(dx_2)), centre), cols[2L]) +
    my_path(rbind(point(x_fun_3(0), 0), centre), cols[3L]) +
    my_annotate(
      x = c(-0.5, 0.0, 0.5),
      y = -0.05,
      label = c("1", names(fractions)[3L], "0"),
      colour = cols[3L]
    ) +
    my_annotate(
      x = c(-0.5 -shift_x(), -0.3, -shift_x()),
      y = c(0 + shift_y(), ytop/2, ytop + shift_y()),
      label = c("0", names(fractions)[1L], "1"),
      colour = cols[1L],
      angle = 60,
      hjust = 0.5,
      vjust = 0.5
    ) +
    my_annotate(
      x = c(0 + shift_x(), 0.3, 0.5 + shift_x()),
      y = c(ytop + shift_y(), ytop/2, 0 + shift_y()),
      label = c("0", names(fractions)[2L], "1"),
      colour = cols[2L],
      angle = -60,
      hjust = 0.5,
      vjust = 0.5
    ) +
    ggplot2::coord_fixed() +
    ggplot2::labs(x = "", y = "") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank()
    )
}
