#' print method for `fc` object
#'
#' @title print.fc
#' @param x A `fc` object created using flowchart functions
#' @param ... Not used
#' @export
print.fc <- function(x, ...) {

  is_class(x, "fc")

  if(length(x) == 2) {
    cli::cli_h1("{.cls fc} flowchart with {nrow(x$fc)} box{?es}")
    cli::cli_text()
    cli::cli_text("Skeleton stored in {.code $fc}:\n")
    print(x$fc)
    cat("\n")
  } else {
    cli::cli_h1("{attr(x, 'combine')} {length(x$fc)} flowchart{?s}")
    purrr::map(seq_along(x$fc), ~{
      cli::cli_h2("[[{.}]] {.cls fc} flowchart with {nrow(x$fc[[.]])} box{?es}")
      cli::cat_line()
      cli::cli_text("Skeleton stored in {.code $fc}:\n")
      print(x$fc[[.]])
      cat("\n")
    })
  }

  invisible(x)
}
