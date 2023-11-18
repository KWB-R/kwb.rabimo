# plot_block_2 -----------------------------------------------------------------

#' Plot Area Fractions of one Block as Part of a Square
#'
#' @param block data frame with one row and columns as available
#'   in the data frame that is returned by \code{\link{prepare_input_data}}
#' @param mar margin vector being passed to \code{\link{par}}
#' @export
#'
plot_block_2 <- function(block, mar = c(1, 1, 3, 1))
{
  get_fraction <- create_fraction_accessor(block)

  r <- new_rects(
    w = c(get_fraction("main"), get_fraction("road")),
    col = c("white", "lightgrey"),
    lbl_text = c("", "")
  ) %>% stack(horizontal = TRUE)

  density_sealed <- 10L
  density_connected <- 10L

  old_par <- graphics::par(mar = mar)
  on.exit(graphics::par(old_par))

  init_plot(axes = FALSE, ylim = c(0, 1), xlim = c(0, 2))

  graphics::title(paste("Block:", block$CODE))

  plot(r)

  rmain <- new_rects(
    w = get_fraction("main"),
    h = c(
      block$mainFractionUnbuiltSealed,
      block$mainFractionBuiltSealed
    ),
    lbl_text = c("unbuiltSealed", "builtSealed"),
    col = c("darkgrey", "orangered"),
    density = density_sealed
  ) %>%
    stack() %>%
    move(top = 1)

  r1 <- rmain %>%
    dplyr::mutate(lbl_text = "") %>% plot(add = TRUE)

  r2 <- new_rects(
    w = get_fraction("road"),
    h = block$roadFractionRoadSealed,
    lbl_text = "",
    density = density_sealed
  ) %>% move(right = 1, top = 1) %>% plot()

  r3 <- new_rects(
    w = unlist(unname(select_columns(
      block,
      paste0("unbuiltSealedFractionSurface", 1:4)
    ))) * get_fraction("main"),
    h = block$mainFractionUnbuiltSealed,
    lbl_text = paste0("S", 1:4)
  ) %>% stack(horizontal = TRUE) %>%
    move(top = 1-block$mainFractionBuiltSealed) %>% plot()

  r4 <- new_rects(
    h = unlist(unname(select_columns(
      block,
      paste0("roadSealedFractionSurface", 1:4)
    ))) * block$roadFractionRoadSealed,
    w = get_fraction("road"),
    lbl_text = paste0("S", 1:4)
  ) %>% stack() %>%
    move(top = 1, right = 1) %>% plot()

  r5 <- new_rects(
    w = get_fraction("main"),
    h = get_fraction("main/unbuiltSealed/connected") /
      get_fraction("main"),
    density = density_connected,
    angle = -45,
    lbl_text = "",
    col = "darkgrey"
  ) %>%
    move(top = rmain$lly[2L]) %>% plot()

  r6 <- new_rects(
    w = get_fraction("main"),
    h = get_fraction("main/builtSealed/connected") /
      get_fraction("main"),
    density = density_connected,
    angle = -45,
    lbl_text = "",
    col = "orangered"
  ) %>%
    move(bottom = rmain$lly[2L]) %>% plot()

  r7 <- new_rects(
    w = get_fraction("road/roadSealed/connected") /
      block$roadFractionRoadSealed,
    h = block$roadFractionRoadSealed,
    density = density_connected,
    angle = -45,
    lbl_text = "",
    col = "black"
  ) %>%
    move(top = 1, left = r$llx[2L]) %>% plot()

  # legend
  legend <- new_rects(
    lbl_text = c(
      "Non-road (Block)",
      "Road",
      "Built and sealed (roofs)",
      "Unbuilt and sealed",
      "Road, sealed",
      "Connected"
    ),
    w = 0.1,
    h = 0.06,
    col = c(
      "white",
      "lightgrey",
      "red",
      "darkgrey",
      "black",
      "black"
    ),
    density = c(
      -1L,
      -1L,
      density_sealed,
      density_sealed,
      density_sealed,
      density_connected
    ),
    angle = c(NA, NA, 45, 45, 45, -45)
  ) %>%
    stack.rects(delta = 0.05, reverse = TRUE) %>%
    move(left = 1.1, top = 1)

  r8 <- legend %>%
    dplyr::mutate(lbl_text = "") %>% plot()

  r9 <- legend %>% move(dx = 0.15) %>%
    dplyr::mutate(
      col = NA,
      density = -1,
      border = NA,
      lbl_align = "left"
    ) %>% plot()
}
