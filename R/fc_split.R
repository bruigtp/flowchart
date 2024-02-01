#' @title fc_split
#' @description This function allows to split the flowchart in function of the categories of a column of the database. It will generate as many boxes as categories has the column showing in each one the frequency of each category. It will additionally group the database per this column.
#'
#' @param object fc object that we want to split.
#' @param var variable column of the database from which it will be splitted.
#' @param label Vector of characters with the label of each category in order. It has to have as many elements as categories has the column. By default, it will put the labels of the categories.
#' @param text_pattern Structure that will have the text in each of the boxes. It recognizes label, n, N and perc within brackets. For default it is "\{label\}\\n \{n\} (\{perc\}\%)".
#' @param sel_group Specify if the splitting has to be done only by one of the previous groups. By default is NULL.
#' @param na.rm logical. Should missing values of the grouping variable be removed? Default is FALSE.
#' @param just Justification for the text: left, center or right. Default is center.
#' @param text_color Color of the text. It is black by default.
#' @param text_fs Font size of the text. It is 8 by default.
#' @param bg_fill Box background color. It is white by default.
#' @param border_color Box border color. It is black by default.
#' @return List with the dataset grouped by the splitting variable and the flowchart with the resulting split.
#'
#' @examples
#' clinic_patient %>%
#'   dplyr::filter(!is.na(group)) %>%
#'   as_fc(label = "Patients included") %>%
#'   fc_split(group) %>%
#'   fc_draw()
#'
#' @export
#' @importFrom rlang .data

#var can be either a string or a non-quoted name
fc_split <- function(object, var, label = NULL, text_pattern = "{label}\n {n} ({perc}%)", sel_group = NULL, na.rm = FALSE, just = "center", text_color = "black", text_fs = 8, bg_fill = "white", border_color = "black") {

  is_class(object, "fc")

  var <- substitute(var)

  if(!is.character(var)) {
    var <- deparse(var)
  }

  N <- nrow(object$data)

  if(na.rm) {
    object$data <- object$data %>%
      dplyr::filter_at(var, ~!is.na(.x))
  }

  new_fc <- object$data %>%
    dplyr::mutate_at(var, as.factor) %>%
    dplyr::count(label = get(var), .drop = FALSE) %>%
    #To save the original label previous to changing it (in case that label is specified)
    dplyr::mutate(label0 = .data$label)

  #In case the label is specified, change it in the text
  if(!is.null(label)) {
    new_fc$label <- factor(new_fc$label, levels = unique(new_fc$label), labels = label)
  }

  new_fc <- new_fc %>%
    dplyr::mutate(
      x = NA,
      y = NA,
      N = N,
      perc = round(.data$n*100/.data$N, 2),
      text = as.character(stringr::str_glue(text_pattern)),
      type = "split",
      just = just,
      text_color = text_color,
      text_fs = text_fs,
      bg_fill = bg_fill,
      border_color = border_color
    )

  #Quan fem un split la bbdd ha de quedar agrupada per saber després si les caixetes que venen a continuació s'han de fer per a cada grup o no!
  group0 <- names(attr(object$data, "groups"))
  group0 <- group0[group0 != ".rows"]

  object$data <- object$data %>%
    dplyr::group_by_at(c(group0, var), .drop = FALSE)

  #Fiquem les posicions horitzontals
  xval <-  seq(0, 1, by = 1/(nrow(attr(object$data, "groups")) + 1))
  xval <- xval[!xval %in% c(0, 1)]
  new_fc$x <- xval

  #First aggrupation to be able to filter by group

  if(length(group0) > 0) {

    new_fc <- new_fc %>%
      tidyr::unite("aux_group", tidyselect::all_of(group0), sep = ", ", remove = FALSE) %>%
      dplyr::ungroup()

    if(is.null(sel_group)) {

      new_fc <- new_fc %>%
        dplyr::select(-"aux_group")

    } else {

      new_fc <- new_fc %>%
        dplyr::filter(.data$aux_group %in% sel_group) %>%
        dplyr::select(-"aux_group")

    }
  }


  new_fc <- new_fc %>%
    tidyr::unite("group", c(tidyselect::all_of(group0), "label0"), sep = ", ") %>%
    dplyr::ungroup() %>%
    dplyr::select("x", "y", "n", "N", "perc", "text", "type", "group", "just", "text_color", "text_fs", "bg_fill", "border_color")

  #remove the id previous to adding the next one
  if(!is.null(object$fc)) {
    object$fc <- object$fc %>%
      dplyr::select(-"id")
  }

  object$fc <- rbind(
    object$fc,
    new_fc %>%
      tibble::as_tibble()
  ) %>%
    dplyr::mutate(
      y = update_y(.data$y, .data$type, .data$x),
      id = dplyr::row_number()
    ) %>%
    dplyr::relocate("id")

  object

}
