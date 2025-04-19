#' @title as_fc
#' @description This function allows to initialize a flow chart given any database. It will create a fc object showing the number of rows of the database. If a database is not available, the user can instead directly enter the number of rows in the study.
#'
#' @param .data Data frame to be initialised as a flowchart.
#' @param N Number of rows of the study in case `.data` is NULL.
#' @param label Character or expression with the text that will be shown in the box.
#' @param text_pattern Structure that will have the text in the box. It recognizes label, n, N and perc within brackets. By default it is "\{label\}\\n\{N\}". If label is an expression, the label is always placed at the beginning of the pattern, followed by a line break where the structure specified by text_pattern is placed.
#' @param just Justification for the text: left, center or right. Default is center.
#' @param text_color Color of the text. It is black by default. See the `col` parameter for \code{\link{gpar}}.
#' @param text_fs Font size of the text. It is 8 by default. See the `fontsize` parameter for \code{\link{gpar}}.
#' @param text_fface Font face of the text. It is 1 by default. See the `fontface` parameter for \code{\link{gpar}}.
#' @param text_ffamily Changes the font family of the text. Default is NA. See the `fontfamily` parameter for \code{\link{gpar}}.
#' @param text_padding Changes the text padding inside the box. Default is 1. This number has to be greater than 0.
#' @param bg_fill Box background color. It is white by default. See the `fill` parameter for \code{\link{gpar}}.
#' @param border_color Box border color. It is black by default. See the `col` parameter for \code{\link{gpar}}.
#' @param width Width of the box. If NA, it automatically adjusts to the content (default). Must be an object of class \code{\link{unit}} or a number between 0 and 1.
#' @param height Height of the box. If NA, it automatically adjusts to the content (default). Must be an object of class \code{\link{unit}} or a number between 0 and 1.
#' @param hide Logical value to hide the initial box or not. Default is FALSE. hide = TRUE can only be combined with fc_split().
#'
#' @return List with the dataset and the initialized flowchart parameters.
#'
#' @examples
#' safo |>
#' as_fc(label = "Patients assessed for eligibility") |>
#' fc_draw()
#'
#' @export

as_fc <- function(.data = NULL, N = NULL, label = "Initial dataframe", text_pattern = "{label}\n{N}", just = "center", text_color = "black", text_fs = 8, text_fface = 1, text_ffamily = NA, text_padding = 1, bg_fill = "white", border_color = "black", width = NA, height = NA, hide = FALSE) {

  if(is.null(.data) & is.null(N)) {
    cli::cli_abort("Either {.arg .data} or {.arg N} arguments must be specified.")
  }else if(!is.null(.data) & !is.null(N)) {
    cli::cli_abort("The {.arg .data} and {.arg N} arguments cannot be specified simultaneously.")
  }

  if(!is.null(.data)) {
    N <- nrow(.data)
  } else {
    .data <- tibble::tibble(id = 1:N)
  }

  if(text_padding == 0) {
    cli::cli_abort("Text padding cannot be equal to zero.")
  }

  if(!hide) {

    new_fc <- tibble::tibble(
      id = 1,
      x = 0.5,
      y = 0.5,
      n = N,
      N = N,
      perc = "100",
      type = "init",
      group = NA,
      just = just,
      text_color = text_color,
      text_fs = text_fs,
      text_fface = text_fface,
      text_ffamily = text_ffamily,
      text_padding = text_padding,
      bg_fill = bg_fill,
      border_color = border_color,
      width = width,
      height = height
    )

    if(is.character(label)) {

      new_fc <- new_fc |>
        dplyr::mutate(text = as.character(stringr::str_glue(text_pattern)))

    } else {

      if(is.expression(label)) {

        text_pattern_exp <- gsub("\\{label\\}", "", text_pattern)

        new_fc <- new_fc |>
          dplyr::mutate(text = list(substitute(atop(x, y), list(x = label[[1]], y = stringr::str_glue(text_pattern_exp)))))

      } else {

        cli::cli_abort("The label must be {.cls character} or {.cls expression}.")

      }
    }

    new_fc <- new_fc |>
      dplyr::relocate("text", .after = "perc")

  } else {

    cli::cli_warn("{.code hide = TRUE} can only be combined with {.fn fc_split}")
    new_fc <- NULL

  }

  #Initialize flowchart as x is a dataframe
  object <- list(
    data = .data |>
      dplyr::ungroup(),
    fc = new_fc
  )

  class(object) <- c("fc")

  object

}
