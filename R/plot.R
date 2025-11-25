#' plot method for `fc` object. It is a wrapper for the [fc_draw] function.
#'
#' @title plot.fc
#' @param x A `fc` object created using flowchart functions
#' @param ... Arguments passed to [fc_draw].
#' @export
plot.fc <- function(x, ...) {

  is_class(x, "fc")

  res <- fc_draw(x, ...)

  invisible(res)
}
