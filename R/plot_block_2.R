#kwb.utils::assignPackageObjects("kwb.rabimo")
#block <- kwb.rabimo::prepare_input_data(kwb.abimo::abimo_input_2019)[1, ]

# plot_block_2 -----------------------------------------------------------------

#' Plot Area Fractions of one Block as Part of a Square
#'
#' @param block data frame with one row and columns as available
#'   in the data frame that is returned by \code{\link{prepare_input_data}}
#' @param mar margin vector being passed to \code{\link{par}}
#' @param density_sealed density of shading lines indicating sealed areas
#' @param density_connected density of shading lines indicating connected areas
#' @param col_main colour to be given to main area
#' @param col_road colour to be given to road area
#' @export
#'
plot_block_2 <- function(
    block,
    mar = c(1, 1, 3, 1),
    density_sealed = 10L,
    density_connected = 10L,
    col_main = "white",
    col_road = "lightgrey"
)
{
  get_fraction <- create_fraction_accessor(block)
  fetch <- create_accessor(block)

  fraction_main <- get_fraction("main")
  fraction_road <- get_fraction("road")

  rect_area <- function(name, col) new_rects(
    w = get_fraction(name), col = col, lbl_text = ""
  )

  rect_sealed <- function(name, lbl, col) new_rects(
    w = fraction_main, h = fetch(name), lbl_text = lbl, col = col,
    density = density_sealed
  )

  rect_legend <- function(lbl, col, density, angle) new_rects(
    w = 0.1, h = 0.06, lbl_text = lbl, col = col, density = density,
    angle = angle
  )

  r_area <- c(
    rect_area("main", col_main),
    rect_area("road", col_road)
  ) %>%
    stack(horizontal = TRUE)

  old_par <- graphics::par(mar = mar)
  on.exit(graphics::par(old_par))

  init_plot(axes = FALSE, ylim = c(0, 1), xlim = c(0, 2))

  graphics::title(paste("Block:", fetch("code")))

  plot(r_area)

  r_main <- c(
    rect_sealed("mainFractionUnbuiltSealed", "unbuiltSealed", "darkgrey"),
    rect_sealed("mainFractionBuiltSealed", "builtSealed", "orangered")
  ) %>%
    stack() %>%
    move(top = 1)

  r_main %>%
    dplyr::mutate(lbl_text = "") %>%
    plot()

  new_rects(
    w = fraction_road,
    h = fetch("roadFractionRoadSealed"),
    lbl_text = "",
    density = density_sealed
  ) %>%
    move(right = 1, top = 1) %>%
    plot()

  cols <- paste0("unbuiltSealedFractionSurface", 1:4)

  new_rects(
    w = unlist(fetch(cols)) * fraction_main,
    h = fetch("mainFractionUnbuiltSealed"),
    lbl_text = 1:4
  ) %>%
    stack(horizontal = TRUE) %>%
    move(top = 1-block$mainFractionBuiltSealed) %>%
    plot()

  cols <- paste0("roadSealedFractionSurface", 1:4)

  new_rects(
    w = fraction_road,
    h = unlist(fetch(cols)) * fetch("roadFractionRoadSealed"),
    lbl_text = 1:4
  ) %>%
    stack() %>%
    move(top = 1, right = 1) %>%
    plot()

  new_rects(
    w = fraction_main,
    h = get_fraction("main/unbuiltSealed/connected") / fraction_main,
    density = density_connected,
    angle = -45,
    lbl_text = "",
    col = "darkgrey"
  ) %>%
    move(top = r_main$lly[2L]) %>%
    plot()

  new_rects(
    w = fraction_main,
    h = get_fraction("main/builtSealed/connected") / fraction_main,
    density = density_connected,
    angle = -45,
    lbl_text = "",
    col = "orangered"
  ) %>%
    move(bottom = r_main$lly[2L]) %>%
    plot()

  new_rects(
    w = get_fraction("road") * fetch("roadSealedFractionConnected"),
    h = fetch("roadFractionRoadSealed"),
    density = density_connected,
    angle = -45,
    lbl_text = "",
    col = "black"
  ) %>%
    move(top = 1, left = r_area$llx[2L]) %>%
    plot()

  legend <- c(
    rect_legend("Non-road (Block)", col_main, -1L, NA),
    rect_legend("Road", col_road, -1L, NA),
    rect_legend("Built and sealed (roofs)", "red", density_sealed, 45),
    rect_legend("Unbuilt and sealed", "darkgrey", density_sealed, 45),
    rect_legend("Road, sealed", "black", density_sealed, 45),
    rect_legend("Connected", "black", density_connected, -45)
  ) %>%
    stack.rects(delta = 0.05, reverse = TRUE) %>%
    move(left = 1.1, top = 1)

  legend %>%
    dplyr::mutate(lbl_text = "") %>%
    plot()

  legend %>%
    dplyr::mutate(col = NA, density = -1, border = NA, lbl_align = "left") %>%
    move(dx = 0.15) %>%
    plot()
}
