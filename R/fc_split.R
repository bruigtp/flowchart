#' @title fc_split
#' @description This function allows to split the flowchart in function of the categories of a column of the database. It will generate as many boxes as categories has the column showing in each one the frequency of each category. It will additionally group the database per this column.
#'
#' @param object fc object that we want to split.
#' @param var variable column of the database from which it will be splitted.
#' @param N Number of rows after the split in case `var` is NULL.
#' @param label Vector of characters with the label of each category in order. It has to have as many elements as categories has the column. By default, it will put the labels of the categories.
#' @param text_pattern Structure that will have the text in each of the boxes. It recognizes label, n, N and perc within brackets. For default it is "\{label\}\\n \{n\} (\{perc\}\%)".
#' @param perc_total logical. Should percentages be calculated using the total number of rows at the beginning of the flowchart? Default is FALSE, meaning that they will be calculated using the number at the parent leaf.
#' @param sel_group Specify if the splitting has to be done only by one of the previous groups. Default is NULL.
#' @param na.rm logical. Should missing values of the grouping variable be removed? Default is FALSE.
#' @param show_zero logical. Should the levels of the grouping variable that don't have data be shown? Default is FALSE.
#' @param round_digits Number of digits to round percentages. It is 2 by default.
#' @param just Justification for the text: left, center or right. Default is center.
#' @param text_color Color of the text. It is black by default.
#' @param text_fs Font size of the text. It is 8 by default.
#' @param text_fface Font face of the text. It is 1 by default. See the `fontface` parameter for \code{\link{gpar}}.
#' @param text_ffamily Changes the font family of the text. Default is NA. See the `fontfamily` parameter for \code{\link{gpar}}.
#' @param text_padding Changes the text padding inside the box. Default is 1. This number has to be greater than 0.
#' @param bg_fill Box background color. It is white by default.
#' @param border_color Box border color. It is black by default.
#' @param title Add a title box to the split. Default is NULL.
#' @param text_color_title Color of the title text. It is black by default.
#' @param text_fs_title Font size of the title text. It is 8 by default.
#' @param text_fface_title Font face of the title text. It is 1 by default. See the `fontface` parameter for \code{\link{gpar}}.
#' @param text_ffamily_title Changes the font family of the title text. Default is NA. See the `fontfamily` parameter for \code{\link{gpar}}.
#' @param text_padding_title Changes the title text padding inside the box. Default is 1. This number has to be greater than 0.
#' @param bg_fill_title Title box background color. It is white by default.
#' @param border_color_title Title box border color. It is black by default.
#' @param offset Amount of space to add to the distance between boxes (in the x coordinate). If positive, this distance will be larger. If negative, it will be smaller. This number has to be at least between 0 and 1 (plot limits) and the resulting x coordinate cannot exceed these plot limits. The default is NULL (no offset).
#' @return List with the dataset grouped by the splitting variable and the flowchart parameters with the resulting split.
#'
#' @examples
#' safo |>
#'   dplyr::filter(!is.na(group)) |>
#'   as_fc(label = "Randomized patients") |>
#'   fc_split(group) |>
#'   fc_draw()
#'
#' @export

fc_split <- function(object, var = NULL, N = NULL, label = NULL, text_pattern = "{label}\n {n} ({perc}%)", perc_total = FALSE, sel_group = NULL, na.rm = FALSE, show_zero = FALSE, round_digits = 2, just = "center", text_color = "black", text_fs = 8, text_fface = 1, text_ffamily = NA, text_padding = 1, bg_fill = "white", border_color = "black", title = NULL, text_color_title = "black", text_fs_title = 10, text_fface_title = 1, text_ffamily_title = NA, text_padding_title = 0.6, bg_fill_title = "white", border_color_title = "black", offset = NULL) {

  is_class(object, "fc")
  UseMethod("fc_split")

}


#' @export
#' @importFrom rlang .data

fc_split.fc <- function(object, var = NULL, N = NULL, label = NULL, text_pattern = "{label}\n {n} ({perc}%)", perc_total = FALSE, sel_group = NULL, na.rm = FALSE, show_zero = FALSE, round_digits = 2, just = "center", text_color = "black", text_fs = 8, text_fface = 1, text_ffamily = NA, text_padding = 1, bg_fill = "white", border_color = "black", title = NULL, text_color_title = "black", text_fs_title = 10, text_fface_title = 1, text_ffamily_title = NA, text_padding_title = 0.6, bg_fill_title = "white", border_color_title = "black", offset = NULL) {

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

  if(na.rm) {
    object$data <- object$data |>
      dplyr::filter_at(var, ~!is.na(.x))
  }

  if(!inherits(object$data[[var]], "factor")) {
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

  if(!is.null(attr(object$data, "groups"))) {

    Ndata <- object$data |>
      dplyr::count(name = "N")

    group0 <- names(attr(object$data, "groups"))
    group0 <- group0[group0 != ".rows"]

    new_fc <- new_fc |>
      dplyr::left_join(Ndata, by = group0)

  } else {

    new_fc <- new_fc |>
      dplyr::mutate(
        N = nrow(object$data)
      )

    group0 <- NULL

  }

  if(perc_total) {
    N_total <- unique(
      object$fc |>
        dplyr::filter(is.na(.data$group)) |>
        dplyr::pull("N")
    )
    new_fc <- new_fc |>
      dplyr::mutate(
        N_total = N_total
      )
  } else {
    new_fc <- new_fc |>
      dplyr::mutate(
        N_total = .data$N
      )
  }

  if(text_padding == 0) {
    stop("Text padding cannot be equal to zero")
  }

  new_fc <- new_fc |>
    dplyr::mutate(
      x = NA,
      y = NA,
      perc = round(.data$n*100/.data$N_total, round_digits),
      text = as.character(stringr::str_glue(text_pattern)),
      type = "split",
      just = just,
      text_color = text_color,
      text_fs = text_fs,
      text_fface = text_fface,
      text_ffamily = text_ffamily,
      text_padding = text_padding,
      bg_fill = bg_fill,
      border_color = border_color
    ) |>
    dplyr::select(-N_total)

  object$data <- object$data |>
    dplyr::group_by_at(c(group0, var), .drop = FALSE)

  # x coordinate for the created boxes.
  #if there are no groups:
  if(is.null(group0)) {

    xval <-  purrr::map_dbl(0:(nrow(new_fc) - 1), ~ (1 + 2*.x)/(2*nrow(new_fc)))

    #Offset distance between boxes:
    if(!is.null(offset)) {
      xval <- dplyr::case_when(
        xval > 0.5 ~ xval + offset,
        xval < 0.5 ~ xval - offset,
        .default = xval
      )

      if(!all(xval >= 0 & xval <= 1)) {
        stop("The x-coordinate cannot exceed the plot limits 0 and 1. The argument offset has to be set to a smaller number.")
      }

    }

    new_fc$x <- xval
    new_fc$group <- NA
    object_center <- new_fc |>
      dplyr::select("group") |>
      dplyr::mutate(center = 0.5)

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

    xval <- tibble::tibble(group = unique(new_fc$group)) |>
      dplyr::mutate(
        label = purrr::map(.data$group, ~new_fc |>
                             dplyr::filter(.data$group == .) |>
                             dplyr::distinct(.data$label)
                             ),
        nboxes = purrr::map_dbl(.data$label, nrow)
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
      tidyr::unnest(c("label", "x"))


    if(!is.null(offset)) {

      xval <- xval |>
        dplyr::mutate(
          x = dplyr::case_when(
            .data$x > center ~ .data$x + offset,
            .data$x < center ~ .data$x - offset,
            .default = .data$x
          )
        )

    }

    xval <- xval |>
      dplyr::select("group", "label", "x")

    #Juntar new_fc amb xval

    new_fc <- new_fc |>
      dplyr::select(-"x") |>
      dplyr::left_join(xval, by = c("group", "label")) |>
      dplyr::relocate("x", .before = "y")

  }

  group_old <- new_fc$group

  new_fc <- new_fc |>
    dplyr::mutate(label0 = dplyr::case_when(
      is.na(label0) ~ "NA",
      TRUE ~ label0
    )) |>
    tidyr::unite("group", c("group", "label0"), sep = ", ", na.rm = TRUE) |>
    dplyr::ungroup() |>
    dplyr::select("x", "y", "n", "N", "perc", "text", "type", "group", "just", "text_color", "text_fs", "text_fface", "text_ffamily", "text_padding", "bg_fill", "border_color")

  #remove the id previous to adding the next one
  if(!is.null(object$fc)) {
    object$fc <- object$fc |>
      dplyr::select(-"id")
  }

  object$fc <- rbind(
    object$fc |>
      dplyr::mutate(old = TRUE),
    new_fc |>
      tibble::as_tibble() |>
      dplyr::mutate(old = FALSE)
  ) |>
    dplyr::mutate(
      y = update_y(.data$y, .data$type, .data$x),
      id = dplyr::row_number()
    ) |>
    dplyr::relocate("id")

  #If we have to add a title
  if(!is.null(title)) {
    new_fc2 <- object$fc |>
      dplyr::filter(!.data$old) |>
      dplyr::mutate(group = group_old) |>
      dplyr::group_by(.data$group) |>
      dplyr::summarise(n_boxes = dplyr::n(),
                       y = unique(.data$y)) |>
      dplyr::filter(.data$n_boxes > 1) |>
      dplyr::left_join(object_center, by = "group") |>
      dplyr::mutate(
        id = NA,
        x = .data$center,
        n = NA,
        N = NA,
        n = NA,
        N = NA,
        perc = NA,
        text = title,
        type = "title_split",
        just = "center",
        text_color = text_color_title,
        text_fs = text_fs_title,
        text_fface = text_fface_title,
        text_ffamily = text_ffamily_title,
        text_padding = text_padding_title,
        bg_fill = bg_fill_title,
        border_color = border_color_title
      ) |>
      dplyr::select(-"center", -"n_boxes") |>
      dplyr::relocate("y", .after = "x") |>
      dplyr::relocate("group", .after = "type")

    object$fc <- rbind(
      object$fc,
      new_fc2 |>
        tibble::as_tibble() |>
        dplyr::mutate(old = FALSE)
    ) |>
      dplyr::mutate(id = dplyr::row_number()) |>
      dplyr::relocate("id")

  }


  object$fc <- object$fc |>
    dplyr::select(-"old")

  object

}
