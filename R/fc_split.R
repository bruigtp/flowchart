#' @title fc_split
#' @description This function allows to split the flowchart in function of the categories of a column of the database. It will generate as many boxes as categories has the column showing in each one the frequency of each category. It will additionally group the database per this column.
#'
#' @param object fc object that we want to split.
#' @param var variable column of the database from which it will be splitted.
#' @param N Number of rows after the split in case `var` is NULL.
#' @param label Vector of characters with the label of each category in order. It has to have as many elements as categories has the column. By default, it will put the labels of the categories.
#' @param text_pattern Structure that will have the text in each of the boxes. It recognizes label, n, N and perc within brackets. For default it is "\{label\}\\n \{n\} (\{perc\}\%)".
#' @param sel_group Specify if the splitting has to be done only by one of the previous groups. By default is NULL.
#' @param na.rm logical. Should missing values of the grouping variable be removed? Default is FALSE.
#' @param show_zero logical. Should the levels of the grouping variable that don't have an event be shown? Default is FALSE.
#' @param round_digits Number of digits to round percentages. It is 2 by default.
#' @param just Justification for the text: left, center or right. Default is center.
#' @param text_color Color of the text. It is black by default.
#' @param text_fs Font size of the text. It is 8 by default.
#' @param bg_fill Box background color. It is white by default.
#' @param border_color Box border color. It is black by default.
#' @return List with the dataset grouped by the splitting variable and the flowchart parameters with the resulting split.
#'
#' @examples
#' clinic_patient |>
#'   dplyr::filter(!is.na(group)) |>
#'   as_fc(label = "Patients included") |>
#'   fc_split(group) |>
#'   fc_draw()
#'
#' @export
#' @importFrom rlang .data

#var can be either a string or a non-quoted name
fc_split <- function(object, var = NULL, N = NULL, label = NULL, text_pattern = "{label}\n {n} ({perc}%)", sel_group = NULL, na.rm = FALSE, show_zero = FALSE, round_digits = 2, just = "center", text_color = "black", text_fs = 8, bg_fill = "white", border_color = "black") {

  is_class(object, "fc")

  var <- substitute(var)

  if(!is.character(var)) {
    var <- deparse(var)
  }

  if(var == "NULL" & is.null(N)) {
    stop("Either `var` or `N` arguments have to be specified.")
  }else if(var != "NULL" & !is.null(N)) {
    stop("`var` and `N` arguments cannot be specified simultaneously.")
  }

  if(var == "NULL") {
    num <- length(grep("split\\d+", names(object$data)))
    var <- stringr::str_glue("split{num + 1}")

    if(!is.null(attr(object$data, "groups"))) {
      if(length(N) %% nrow(attr(object$data, "groups")) != 0) {
        stop(stringr::str_glue("The length of `N` has to be a multiple to the number of groups in the dataset: {nrow(attr(object$data, 'groups'))}"))
      }

      nsplit <- length(N)/nrow(attr(object$data, "groups"))
      ngroups <- nrow(attr(object$data, "groups"))
      message_group <- "in each group."

    } else {
      nsplit <- length(N)
      ngroups <- 1
      message_group <- "."
    }

    object$data$row_number_delete <- 1:nrow(object$data)

    #select rows to be true the filter
    nrows <- dplyr::group_rows(object$data)

    N_list <- split(N, rep(1:ngroups, rep(nsplit, ngroups)))

    split_rows <- purrr::map_df(seq_along(nrows), function (x) {

      if(sum(N_list[[x]]) != length(nrows[[x]])) {
        stop(paste0("The number of rows after the split specified in N has to be equal to the original number of rows", message_group))
      }

      tibble::tibble(group = paste("group", 1:nsplit)) |>
        dplyr::mutate(
          rows = split(nrows[[x]], rep(1:nsplit, N_list[[x]]))
        )

    })

    object$data[[var]] <- NA

    for(i in 1:nrow(split_rows)) {
      object$data[[var]] <- dplyr::case_when(
        object$data$row_number_delete %in% split_rows$rows[[i]] ~ split_rows$group[[i]],
        TRUE ~ object$data[[var]]
      )
    }

    object$data <- object$data |>
      dplyr::select(-"row_number_delete")

  }

  N <- nrow(object$data)

  if(na.rm) {
    object$data <- object$data |>
      dplyr::filter_at(var, ~!is.na(.x))
  }

  if(inherits(object$data[[var]], "factor")) {
    object$data <- object$data |>
      dplyr::mutate_at(var, as.factor)
  }

  new_fc <- object$data |>
    dplyr::count(label = get(var), .drop = FALSE) |>
    #To save the original label previous to changing it (in case that label is specified)
    dplyr::mutate(label0 = .data$label)

  #If we don't want to show the levels without an event:
  if(!show_zero) {
    new_fc <- new_fc |>
      dplyr::filter(.data$n != 0)
  }

  #In case the label is specified, change it in the text
  if(!is.null(label)) {
    new_fc$label <- factor(new_fc$label, levels = unique(new_fc$label), labels = label)
  }

  new_fc <- new_fc |>
    dplyr::mutate(
      x = NA,
      y = NA,
      N = N,
      perc = round(.data$n*100/.data$N, round_digits),
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

  object$data <- object$data |>
    dplyr::group_by_at(c(group0, var), .drop = FALSE)

  # x coordinate for the created boxes.
  #if there are no groups:
  if(is.null(group0)) {

    xval <-  purrr::map_dbl(0:(nrow(new_fc) - 1), ~ (1 + 2*.x)/(2*nrow(new_fc)))
    new_fc$x <- xval
    new_fc$group <- NA

  } else {

    new_fc <- new_fc |>
      tidyr::unite("group", c(tidyselect::all_of(group0)), sep = ", ")

    #Filter boxes in some groups if sel_group is specified
    if(!is.null(sel_group)) {
      new_fc <- new_fc |>
        dplyr::filter(.data$group %in% sel_group)
    }

    object_center <- object$fc |>
      dplyr::filter(.data$type != "exclude") |>
      dplyr::group_by(.data$group) |>
      dplyr::summarise(
        center = unique(.data$x)
      )

    xval <- new_fc |>
      dplyr::group_by(.data$group) |>
      dplyr::summarise(
        nboxes = dplyr::n()
      ) |>
      dplyr::left_join(
        object_center, by = "group"
      ) |>
      dplyr::mutate(
        margins = purrr::pmap(list(.data$center, dplyr::lag(.data$center), dplyr::lead(.data$center)), function (x, xlag, xlead) {
          if(is.na(xlag)) {
            c(0, (x + xlead)/2)
          }else if(is.na(xlead)) {
            c((xlag + x)/2, 1)
          } else {
            c((xlag + x)/2, (x + xlead)/2)
          }
        })
      ) |>
      dplyr::mutate(
        x = purrr::pmap(list(.data$center, .data$margins, .data$nboxes), function (xcenter, xmarg, xn) {

          min_marg <- min(xcenter - xmarg[1], xmarg[2] - xcenter)

          cuts <- purrr::map_dbl(0:(xn - 1), ~ (1 + 2*.x)/(2*xn))

          (xcenter - min_marg) + cuts*2*min_marg
          # x <- seq(xcenter - min_marg, xcenter + min_marg, by = 2*min_marg/(xn + 1))
          # x[!x %in% c(xcenter - min_marg, xcenter + min_marg)]

        })
      ) |>
      dplyr::pull("x")

    new_fc$x <- unlist(xval)

    }

  new_fc <- new_fc |>
    tidyr::unite("group", c("group", "label0"), sep = ", ", na.rm = TRUE) |>
    dplyr::ungroup() |>
    dplyr::select("x", "y", "n", "N", "perc", "text", "type", "group", "just", "text_color", "text_fs", "bg_fill", "border_color")

  #remove the id previous to adding the next one
  if(!is.null(object$fc)) {
    object$fc <- object$fc |>
      dplyr::select(-"id")
  }

  object$fc <- rbind(
    object$fc,
    new_fc |>
      tibble::as_tibble()
  ) |>
    dplyr::mutate(
      y = update_y(.data$y, .data$type, .data$x),
      id = dplyr::row_number()
    ) |>
    dplyr::relocate("id")

  object

}
