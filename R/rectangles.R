#kwb.utils::assignPackageObjects("kwb.rabimo")
`%>%` <- magrittr::`%>%`
select_columns <- kwb.utils::selectColumns
default_if_null <- kwb.utils::defaultIfNULL
remove_columns <- kwb.utils::removeColumns

# c.rects ----------------------------------------------------------------------
c.rects <- function(...)
{
  rbind(...)
}

# check_rects ------------------------------------------------------------------
check_rects <- function(x)
{
  stopifnot(inherits(x, "rects"))
}

# find_lim ---------------------------------------------------------------------
find_lim <- function(r, axis = "x")
{
  check_rects(r)
  coord <- select_columns(r, paste0("ll", axis))
  size <- select_columns(r, ifelse(axis == "x", "w", "h"))
  c(min(coord, na.rm = TRUE), max(coord + size, na.rm = TRUE))
}

# get_mids ---------------------------------------------------------------------
get_mids <- function(rdf)
{
  data <- select_columns(rdf, c("llx", "lly", "w", "h"))

  data.frame(
    x = data$llx + data$w/2,
    y = data$lly + data$h/2
  )
}

# init_plot --------------------------------------------------------------------
init_plot <- function(
    width = 1,
    height = 1,
    xlim = c(0, width),
    ylim = c(0, height),
    axes = FALSE
)
{
  plot(
    NA,
    xlim = xlim,
    ylim = ylim,
    xlab = "",
    ylab = "",
    asp = 1,
    axes = axes
  )
}

# move -------------------------------------------------------------------------
move <- function(x, ...)
{
  UseMethod("move")
}

# move.rects -------------------------------------------------------------------
move.rects <- function(
    rdf,
    dx = 0,
    dy = 0,
    top = NULL,
    bottom = NULL,
    left = NULL,
    right = NULL,
    each = FALSE
)
{
  check_rects(rdf)

  #dx = 0; dy = 0; top = NULL; bottom = NULL; left = NULL; right = NULL
  is_set <- !sapply(FUN = is.null, list(
    top = top,
    bottom = bottom,
    left = left,
    right = right
  ))

  #rdf <- as_data_frame(r)

  if (is_set["top"]) {
    upper_y <- rdf$lly + rdf$h
    dy <- top - if (each) upper_y else max(upper_y)
  } else if (is_set["bottom"]) {
    lower_y <- rdf$lly
    dy <- bottom - if (each) lower_y else min(lower_y)
  }

  if (is_set["left"]) {
    left_x <- rdf$llx
    dx <- left - if (each) left_x else min(left_x)
  } else if (is_set["right"]) {
    right_x <- rdf$llx + rdf$w
    dx <- right - if (each) right_x else max(right_x)
  }

  rdf$llx <- rdf$llx + dx
  rdf$lly <- rdf$lly + dy

  rdf
}
# new_rects --------------------------------------------------------------------
new_rects <- function(
    w = 1,
    h = 1,
    llx = 0,
    lly = 0,
    lbl_text = NULL,
    lbl_align = "centre",
    density = -1,
    angle = 45,
    col = NA,
    border = graphics::par("fg"),
    lty = graphics::par("lty"),
    lwd = graphics::par("lwd"),
    n = NULL
)
{
  #llx = 0; lly = 0; lty = 1; lwd = 1; label = NA
  #w = 2; h = 3; llx = 1:4

  vectors <- list(
    w = w,
    h = h,
    llx = llx,
    lly = lly,
    lbl_align = lbl_align,
    density = density,
    angle = angle,
    col = col,
    border = border,
    lty = lty,
    lwd = lwd
  )

  # length of the longest vector or length given in n
  n <- default_if_null(n, max(lengths(vectors)))

  # Resize all vectors to length n
  vectors <- lapply(vectors, rep, length.out = n)

  vectors[["lbl_text"]] <- if (is.null(lbl_text)) {
    paste0("r", seq_len(n))
  } else {
    rep(lbl_text, length.out = n)
  }

  vectors <- vectors[kwb.utils::pairwise(names(vectors))]

  # Convert to data frame and add class "rects"
  kwb.utils::addClass(as.data.frame(vectors), "rects")
}

# plot.rects -------------------------------------------------------------------
plot.rects <- function(r, add = !is.null(grDevices::dev.list()), cex = 1, ...)
{
  check_rects(r)

  if (!add) {
    init_plot(
      xlim = find_lim(r, "x"),
      ylim = find_lim(r, "y"),
      axes = TRUE
    )
  }

  args <- to_rect_args(remove_columns(r, pattern = "^lbl_"))

  do.call(graphics::rect, args)

  mids <- get_mids(r)

  labels <- kwb.utils::defaultIfNA(
    select_columns(r, "lbl_text"),
    seq_len(nrow(r))
  )

  align <- select_columns(r, "lbl_align")

  is_left <- align == "left"
  is_centre <- align == "centre"

  text <- function(x, y, labels, adj) {
    graphics::text(x = x, y = y, labels = labels, adj = adj, cex = cex)
  }

  # Left aligned labels
  if (any(is_left)) {
    text(
      x = r$llx[is_left],
      y = mids$y[is_left],
      labels = labels[is_left],
      adj = c(0, 0.5)
    )
  }

  # Centred labels
  if (any(is_centre)) {
    text(
      x = mids$x[is_centre],
      y = mids$y[is_centre],
      labels = labels[is_centre],
      adj = c(0.5, 0.5)
    )
  }

  invisible(r)
}

# separate ---------------------------------------------------------------------
separate <- function(x, ...)
{
  UseMethod("separate")
}

# separate.rects ---------------------------------------------------------------
separate.rects <- function(x, dx = 0, dy = 0)
{
  check_rects(x)
  mids <- get_mids(x)
  x$llx <- x$llx + (order(mids$x) - 1L) * dx
  x$lly <- x$lly + (order(mids$y) - 1L) * dy
  x
}

stack <- function(x, ...)
{
  UseMethod("stack")
}

# stack.rects ------------------------------------------------------------------
stack.rects <- function(r, horizontal = FALSE, delta = 0, reverse = FALSE)
{
  check_rects(r)

  x <- r[[ifelse(horizontal, "w", "h")]]

  positions <- stacked_positions(x, delta = delta, reverse = reverse)

  r[[ifelse(horizontal, "llx", "lly")]] <- positions[, "lower"]

  r
}

# stacked_positions ------------------------------------------------------------
stacked_positions <- function(x, delta = 0, reverse = FALSE)
{
  upper <- cumsum(x) + (seq_along(x) - 1L) * delta
  lower <- c(0, upper[-length(upper)] + delta)

  cbind(
    lower = if (reverse) -upper else lower,
    upper = if (reverse) -lower else upper
  )
}

# to_label ---------------------------------------------------------------------
to_label <- function(key, value)
{
  sprintf("%s: %0.1f", key, value)
}

# to_rect_args -----------------------------------------------------------------
to_rect_args <- function(rdf)
{
  cols <- c("llx", "lly", "w", "h")
  data <- select_columns(rdf, cols)

  coords <- data.frame(
    xleft = data$llx,
    ybottom = data$lly,
    xright = data$llx + data$w,
    ytop = data$lly + data$h
  )

  cbind(coords, rdf[, setdiff(names(rdf), cols), drop = FALSE])
}

# unlabel_and_dash -------------------------------------------------------------
unlabel_and_dash <- function(x)
{
  dplyr::mutate(x, lbl_text = "", lty = "dashed")
}
