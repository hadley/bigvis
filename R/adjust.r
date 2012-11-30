# Protect against floating point areas by slightly adjusting breaks.
# Adapted from graphics::hist.default.
adjust_breaks <- function(breaks, open = "right") {
  open <- match.arg(open, c("left", "right"))

  breaks <- sort(breaks)
  diddle <- 1e-07 * median(diff(breaks))
  if (open == "left") {
    fuzz <- c(-diddle, rep.int(diddle, length(breaks) - 1))
  } else {
    fuzz <- c(rep.int(-diddle, length(breaks) - 1), diddle)
  }
  breaks + fuzz
}
