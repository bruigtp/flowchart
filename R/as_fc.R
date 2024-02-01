#' @title as_fc
#' @description This function allows to initialize a flow chart given any database. It will create a fc object showing the number of rows of the database.
#'
#' @param .data Data frame that we want to show the number of rows.
#' @param label Character with the text that will be shown in the box.
#' @param text_pattern Structure that will have the text in the box. It recognizes label, n, N and perc within brackets. For default it is "\{label\}\\n\{N\}".
#' @param just Justification for the text: left, center or right. Default is center.
#' @param text_color Color of the text. It is black by default.
#' @param text_fs Font size of the text. It is 8 by default.
#' @param bg_fill Box background color. It is white by default.
#' @param border_color Box border color. It is black by default.
#' @param hide Logical value to hide the initial box or not. Default is FALSE. hide = TRUE can only be combined with fc_split().
#'
#' @return List with the dataset and the initialized flowchart.
#'
#' @examples
#' clinic_patient %>%
#' as_fc(label = "All patients") %>%
#' fc_draw()
#'
#' @export

as_fc <- function(.data, label = "Initial dataframe", text_pattern = "{label}\n{N}", just = "center", text_color = "black", text_fs = 8, bg_fill = "white", border_color = "black", hide = FALSE) {

  N <- nrow(.data)

  if(!hide) {
    fc_new <- tibble::tibble(
      id = 1,
      x = 0.5,
      y = 0.5,
      n = N,
      N = N,
      perc = "100",
      text = as.character(stringr::str_glue(text_pattern)),
      type = "init",
      group = NA,
      just = just,
      text_color = text_color,
      text_fs = text_fs,
      bg_fill = bg_fill,
      border_color = border_color
    )
  } else {
    warning("Remember that hide = TRUE can only be combined with fc_split()")
    fc_new <- NULL
  }

  #Initialize flowchart as x is a dataframe
  object <- list(
    data = .data %>%
      dplyr::ungroup(),
    fc = fc_new
  )
  class(object) <- c("fc")
  object
}
