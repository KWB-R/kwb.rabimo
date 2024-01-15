# plot_block -------------------------------------------------------------------

#' Plot Area Fractions and their Names for one Block
#'
#' @param block data frame with one row and columns as available
#'   in the data frame that is returned by \code{\link{prepare_input_data}}
#' @param cex character expansion factor to scale the texts
#' @param delta controls the space between rectangles
#' @importFrom rlang .data
#' @export
plot_block <- function(block, cex = 1, delta = 0.1)
{
  get_fraction <- create_fraction_accessor(block)

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
    col_rect("areaFractionRoad"),
    col_rect("areaFractionMain")
  ) %>%
    stack()

  area_rects_dashed <- area_rects %>%
    separate(dy = 0.4) %>%
    move(dy = -0.2, dx = 1.2) %>%
    unlabel_and_dash()

  main_rects <- c(
    col_rect("mainFractionBuiltSealed"),
    col_rect("mainFractionUnbuiltSealed")
  ) %>%
    dplyr::mutate(h = .data[["h"]] * block$areaFractionMain) %>%
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

  road_rect <- col_rect("roadFractionRoadSealed") %>%
    dplyr::mutate(h = .data[["h"]] * block$areaFractionRoad) %>%
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
      "builtSealedFractionConnected",
      get_fraction("main/builtSealed")
    ) %>%
      move(bottom = built_sealed_dashed$lly[1L]),
    col_rect_w(
      "unbuiltSealedFractionConnected",
      get_fraction("main/unbuiltSealed")
    ) %>% move(bottom = unbuilt_sealed_dashed$lly[2L]),
    col_rect_w(
      "roadSealedFractionConnected",
      get_fraction("road/roadSealed")
    ) %>% move(bottom = road_rect_dashed$lly[1L])
  ) %>%
    move(left = road_rect_dashed$llx[1L]) %>%
    dplyr::mutate(
      lbl_align = "left",
      col = "lightgrey"
    )

  s1 <- get_fraction("main/unbuiltSealed")
  surf_rects_main <- c(
    col_rect_w("unbuiltSealedFractionSurface1", s1, lbl_text = "1"),
    col_rect_w("unbuiltSealedFractionSurface2", s1, lbl_text = "2"),
    col_rect_w("unbuiltSealedFractionSurface3", s1, lbl_text = "3"),
    col_rect_w("unbuiltSealedFractionSurface4", s1, lbl_text = "4")
  ) %>%
    stack(horizontal = TRUE) %>%
    move(
      left = unbuilt_sealed_dashed$llx[2L],
      bottom = unbuilt_sealed_dashed$lly[2L]
    )

  s2 <- get_fraction("road/roadSealed")
  surf_rects_road <- c(
    col_rect_w("roadSealedFractionSurface1", s2, lbl_text = "1"),
    col_rect_w("roadSealedFractionSurface2", s2, lbl_text = "2"),
    col_rect_w("roadSealedFractionSurface3", s2, lbl_text = "3"),
    col_rect_w("roadSealedFractionSurface4", s2, lbl_text = "4")
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
