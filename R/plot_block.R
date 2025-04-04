#kwb.rabimo::plot_block(kwb.rabimo::rabimo_inputs_2020$data[1L, ])
#kwb.rabimo::plot_block_2(block = kwb.rabimo::rabimo_inputs_2020$data[1L, ])
#kwb.utils::assignPackageObjects("kwb.rabimo")

# get_fraction -----------------------------------------------------------------
get_fraction <- function(path, block)
{
  is_connected <- grepl(path, "connected$")

  if (startsWith(path, "main")) {
    main <- block$main_frac
    if (identical(path, "main")) {
      return(main)
    }
    if (grepl("/builtSealed", path)) {
      return(main * block$roof * ifelse(is_connected, block$swg_roof, 1))
    }
    if (grepl("/unbuiltSealed", path)) {
      return(main * block$pvd * ifelse(is_connected, block$swg_pvd, 1))
    }
  }

  if (startsWith(path, "road")) {
    road <- block$road_frac
    if (identical(path, "road")) {
      return(road)
    }
    if (grepl("/roadSealed", path)) {
      return(road * block$pvd_r * ifelse(is_connected, block$swg_pvd_r, 1))
    }
  }

  stop("How to handle path '", path, "'?")
}

# plot_block -------------------------------------------------------------------

#' Plot Area Fractions and their Names for one Block
#'
#' @param block data frame with one row and columns as provided in the data
#'   frame \code{\link{rabimo_inputs_2025}$data}
#' @param cex character expansion factor to scale the texts
#' @param delta controls the space between rectangles
#' @importFrom rlang .data
#' @export
plot_block <- function(block, cex = 1, delta = 0.1)
{
  #block <- kwb.rabimo::rabimo_inputs_2020$data[1L, ]
  #kwb.utils::assignPackageObjects("kwb.rabimo")
  #`%>%` <- magrittr::`%>%`
  to_label <- kwb.rect:::to_label
  new_rects <- kwb.rect::new_rects
  stack <- kwb.rect::stack
  separate <- kwb.rect:::separate
  move <- kwb.rect::move
  unlabel_and_dash <- kwb.rect:::unlabel_and_dash

  col_rect <- function(col, size = 1, as_width = FALSE, lbl_text = NULL, ...) {
    value <- select_columns(block, col)
    lbl_text <- default_if_null(lbl_text, to_label(
      key = gsub("Fraction", "X", col),
      value = 100 * value
    ))
    if (as_width) {
      new_rects(w = value, h = size, lbl_text = lbl_text, ...)
    } else {
      new_rects(w = size, h = value, lbl_text = lbl_text, ...)
    }
  }

  col_rect_w <- function(...) {
    col_rect(..., as_width = TRUE)
  }

  # Create single rectangles
  area_rects <- c(
    col_rect("road_frac"),
    col_rect("main_frac")
  ) %>%
    stack()

  area_rects_dashed <- area_rects %>%
    separate(dy = 0.4) %>%
    move(dy = -0.2, dx = 1.2) %>%
    unlabel_and_dash()

  main_rects <- c(
    col_rect("roof"),
    col_rect("sealed")
  ) %>%
    dplyr::mutate(h = .data[["h"]] * block$main_frac) %>%
    stack(reverse = TRUE) %>%
    move(
      left = area_rects_dashed$llx[2L],
      top = area_rects_dashed$lly[2L] + area_rects_dashed$h[2L]
    )

  built_sealed_dashed <- main_rects[1, ] %>%
    move(dx = 1.2, dy = 0.2) %>%
    unlabel_and_dash()

  unbuilt_sealed_dashed <- c(
    main_rects[2, ] %>% move(dx = 1.2),
    main_rects[2, ] %>% move(dx = 2.4)
  ) %>%
    unlabel_and_dash()

  road_rect <- col_rect("pvd_r") %>%
    dplyr::mutate(h = .data[["h"]] * block$road_frac) %>%
    move(
      left = area_rects_dashed$llx[1L],
      bottom = area_rects_dashed$lly[1L]
    )

  road_rect_dashed <- c(
    move(road_rect, dx = 1.2),
    move(road_rect, dx = 2.4)
  ) %>%
    unlabel_and_dash()

  conn_rects <- c(
    col_rect_w(
      "swg_roof",
      get_fraction("main/builtSealed", block)
    ) %>%
      move(bottom = built_sealed_dashed$lly[1L]),
    col_rect_w(
      "swg_pvd",
      get_fraction("main/unbuiltSealed", block)
    ) %>% move(bottom = unbuilt_sealed_dashed$lly[2L]),
    col_rect_w(
      "swg_pvd_r",
      get_fraction("road/roadSealed", block)
    ) %>% move(bottom = road_rect_dashed$lly[1L])
  ) %>%
    move(left = road_rect_dashed$llx[1L]) %>%
    dplyr::mutate(
      lbl_align = "left",
      col = "lightgrey"
    )

  s1 <- get_fraction("main/unbuiltSealed", block)
  surf_rects_main <- c(
    col_rect_w("srf1_pvd", s1, lbl_text = "1"),
    col_rect_w("srf2_pvd", s1, lbl_text = "2"),
    col_rect_w("srf3_pvd", s1, lbl_text = "3"),
    col_rect_w("srf4_pvd", s1, lbl_text = "4")
  ) %>%
    stack(horizontal = TRUE) %>%
    move(
      left = unbuilt_sealed_dashed$llx[2L],
      bottom = unbuilt_sealed_dashed$lly[2L]
    )

  s2 <- get_fraction("road/roadSealed", block)
  surf_rects_road <- c(
    col_rect_w("srf1_pvd_r", s2, lbl_text = "1"),
    col_rect_w("srf2_pvd_r", s2, lbl_text = "2"),
    col_rect_w("srf3_pvd_r", s2, lbl_text = "3"),
    col_rect_w("srf4_pvd_r", s2, lbl_text = "4")
  ) %>%
    stack(horizontal = TRUE) %>%
    move(
      left = road_rect_dashed$llx[2L],
      bottom = road_rect_dashed$lly[2L]
    )

  plot(add = FALSE, cex = cex, c(
    area_rects,
    area_rects_dashed,
    main_rects,
    built_sealed_dashed,
    unbuilt_sealed_dashed,
    road_rect,
    road_rect_dashed,
    conn_rects,
    surf_rects_main,
    surf_rects_road
  ))
}

# plot_block_2 -----------------------------------------------------------------

#' Plot Area Fractions of one Block as Part of a Square
#'
#' @param block data frame with one row and columns as contained in
#'   \code{\link{rabimo_inputs_2025}$data}
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
  new_rects <- kwb.rect::new_rects
  stack <- kwb.rect::stack
  init_plot <- kwb.rect:::init_plot
  move <- kwb.rect::move

  fetch <- create_accessor(block)

  fraction_main <- get_fraction("main", block)
  fraction_road <- get_fraction("road", block)

  rect_area <- function(name, col) new_rects(
    w = get_fraction(name, block), col = col, lbl_text = ""
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
    rect_sealed("pvd", "unbuiltSealed", "darkgrey"),
    rect_sealed("roof", "builtSealed", "orangered")
  ) %>%
    stack() %>%
    move(top = 1)

  r_main %>%
    dplyr::mutate(lbl_text = "") %>%
    plot()

  new_rects(
    w = fraction_road,
    h = fetch("pvd_r"),
    lbl_text = "",
    density = density_sealed
  ) %>%
    move(right = 1, top = 1) %>%
    plot()

  cols <- sprintf("srf%d_pvd", 1:4)

  new_rects(
    w = unlist(fetch(cols)) * fraction_main,
    h = fetch("pvd"),
    lbl_text = 1:4
  ) %>%
    stack(horizontal = TRUE) %>%
    move(top = 1 - fetch("pvd")) %>%
    plot()

  cols <- sprintf("srf%d_pvd_r", 1:4)

  new_rects(
    w = fraction_road,
    h = unlist(fetch(cols)) * fetch("pvd_r"),
    lbl_text = 1:4
  ) %>%
    stack() %>%
    move(top = 1, right = 1) %>%
    plot()

  new_rects(
    w = fraction_main,
    h = get_fraction("main/unbuiltSealed/connected", block) / fraction_main,
    density = density_connected,
    angle = -45,
    lbl_text = "",
    col = "darkgrey"
  ) %>%
    move(top = r_main$lly[2L]) %>%
    plot()

  new_rects(
    w = fraction_main,
    h = get_fraction("main/builtSealed/connected", block) / fraction_main,
    density = density_connected,
    angle = -45,
    lbl_text = "",
    col = "orangered"
  ) %>%
    move(bottom = r_main$lly[2L]) %>%
    plot()

  new_rects(
    w = get_fraction("road", block) * fetch("swg_pvd_r"),
    h = fetch("swg_pvd_r"),
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
    stack(delta = 0.05, reverse = TRUE) %>%
    move(left = 1.1, top = 1)

  legend %>%
    dplyr::mutate(lbl_text = "") %>%
    plot()

  legend %>%
    dplyr::mutate(col = NA, density = -1, border = NA, lbl_align = "left") %>%
    move(dx = 0.15) %>%
    plot()
}
