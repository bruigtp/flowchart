# Internal functions used in the package:

#' @title update_x
#' @description Function to update the horizontal position of the previuosly created flow charts, to merge two flowcharts used in fc_merge().
#'
#'@param x old horizontal position of the boxes
#'@param i position of the flowchart within the list of flowcharts.
#'@param n number of total flowcharts to merge
#'@keywords internal
#'
update_x <- function(x, i, n) {

  scale <- 1/n

  xval <- seq((i - 1)*scale, i*scale, by = scale/(length(unique(x)) + 1))
  xval <- xval[-c(1, length(xval))]

  as.numeric(as.character(factor(x, levels = sort(unique(x)), labels = xval)))

}

#' @title update_y
#' @description Function to update the vertical position of the previuosly created boxes, to add the new ones with fc_split() or fc_filter().
#'
#'@param y old height of the boxes
#'@param type type of box
#'@param x horizontal position of the boxes
#'@keywords internal
#'
update_y <- function(y, type, x, group) {

  tbl_y <- tibble::tibble("y" = y, "type" = type, "x" = x, "group" = group) |>
    dplyr::filter(.data$type != "exclude", !grepl("title", .data$type)) |>
    dplyr::mutate(id = dplyr::row_number()) |>
    dplyr::mutate(
      id_pre = purrr::map(dplyr::row_number(), function (rn) {
        if(rn == 1) {
          NA
        } else {
          1:(rn - 1)
        }
      })
    )

  tbl_ynew <- tbl_y |>
    dplyr::filter(!is.na(.data$group) | is.na(.data$y)) |>
    dplyr::mutate(
      group_split = purrr::map(.data$group, ~unlist(stringr::str_split(., " // "))),
      all_groups = purrr::map(.data$group_split, function (x) purrr::map_chr(rev(1:length(x)), ~paste(x[1:.], collapse = " // "))),
      id_same_group = purrr::map2(.data$id_pre, .data$all_groups, function (x, y) {
        if(!all(is.na(x))) {
          x[purrr::map_lgl(x, ~is.na(tbl_y$group[.]) | any(y %in% tbl_y$group[.]))]
        } else {
          NA
        }
      })
    )

  for(i in 1:nrow(tbl_ynew)) {

    tbl_yadd <- tbl_y |>
      dplyr::filter(.data$id %in% c(tbl_ynew$id[i], tbl_ynew$id_same_group[[i]])) |>
      dplyr::mutate(
        nboxes = dplyr::n(),
        y = setdiff(rev(seq(0, 1, by = 1/(dplyr::n() + 1))), c(0, 1))
      ) |>
      dplyr::select("y", "id", "nboxes") |>
      dplyr::rename_all(~dplyr::case_when(. != "id" ~ stringr::str_glue("{.}{i}"), .default = .))

    tbl_y <- tbl_y |>
      dplyr::left_join(tbl_yadd, by = "id")

  }

  tbl_y <- tbl_y |>
    dplyr::group_by(y) |>
    tidyr::fill(tidyselect::starts_with("y"), tidyselect::starts_with("nboxes"), .direction = "downup") |>
    dplyr::rowwise() |>
    dplyr::mutate(
      nmax = as.numeric(which.max(dplyr::across(tidyselect::starts_with("nboxes"), ~ .)))
    )

  #Update those which are not of type exclude and title
  id_no_update <- which(type == "exclude" | grepl("title", type))

  if(length(id_no_update) > 0) {
    y[-id_no_update] <- purrr::map_dbl(1:nrow(tbl_y), ~tbl_y[[paste0("y", tbl_y$nmax[.])]][.])
  } else {
    y <- purrr::map_dbl(1:nrow(tbl_y), ~tbl_y[[paste0("y", tbl_y$nmax[.])]][.])
  }

  #Update those of type exclude (they have to be in-between the two boxes)
  id_exc <- which(type == "exclude")
  if(length(id_exc) > 0) {
    for(i in 1:length(id_exc)) {

      #Find the two boxes, one above another below the exclude box.
      x_filt <- x[id_exc[i] - 1]
      id_par <- which(x == x_filt)
      id_par <- utils::tail(id_par[id_par < (id_exc[i] - 1)], 1)
      y[id_exc[i]] <- (y[id_par] + y[id_exc[i] - 1])/2

    }
  }

  #Update those of type title
  id_title <- grep("title", type)

  if(length(id_title) > 0) {
    #Put the y-coordinate of the last filter/split box before it
    y[id_title] <- purrr::map_dbl(id_title, ~tail(y[which(type[1:.] == gsub("title_", "", type[.]))], 1))
  }


  y

}

#' @title update_y_stack
#' @description Function to update the vertical position of the previuosly created flowcharts, to stack the two flowcharts with fc_stack().
#'
#'@param y old vertical position of the boxes
#'@param x old horizontal position of the boxes
#'@param type type of the boxes
#'@param i position of the flowchart within the list of flowcharts.
#'@param n number of total flowcharts to merge
#'@keywords internal
#'
update_y_stack <- function(y, x, type, i, n) {

  scale <- 1/n

  i <- (n + 1) - i

  yval <- seq((i - 1)*scale, i*scale, by = scale/(length(unique(y)) + 1))
  yval <- yval[-c(1, length(yval))]

  y <- as.numeric(as.character(factor(y, levels = sort(unique(y)), labels = yval)))

  #Update those of type exclude (they have to be in-between the two boxes)
  id_exc <- which(type == "exclude")
  if(length(id_exc) > 0) {
    for(i in 1:length(id_exc)) {

      #Find the two boxes, one above another below the exclude box.
      x_filt <- x[id_exc[i] - 1]
      id_par <- which(x == x_filt)
      id_par <- utils::tail(id_par[id_par < (id_exc[i] - 1)], 1)
      y[id_exc[i]] <- (y[id_par] + y[id_exc[i] - 1])/2

    }
  }

  y

}

#' @title update_y_stack_unite
#' @description Function to update the vertical position of the previuosly created flowcharts, to stack the two flowcharts with fc_stack(), when `unite` is TRUE.
#'
#'@param y old vertical position of the boxes
#'@param x old horizontal position of the boxes
#'@param type type of the boxes
#'@keywords internal
#'
update_y_stack_unite <- function(y, x, type) {

  yval <- y[type != "exclude"]
  nbox <- cumsum(!duplicated(yval))
  ynew <- rev(setdiff(seq(0, 1, by = 1/(max(nbox) + 1)), c(0, 1)))
  yval <- ynew[nbox]

  y[type != "exclude"] <- yval

  #Update those of type exclude (they have to be in-between the two boxes)
  id_exc <- which(type == "exclude")
  if(length(id_exc) > 0) {
    for(i in 1:length(id_exc)) {

      #Find the two boxes, one above another below the exclude box.
      x_filt <- x[id_exc[i] - 1]
      id_par <- which(x == x_filt)
      id_par <- utils::tail(id_par[id_par < (id_exc[i] - 1)], 1)
      y[id_exc[i]] <- (y[id_par] + y[id_exc[i] - 1])/2

    }
  }

  y

}


#' @title is_class
#' @description Function to check if an object is from a given class.
#'
#'@param x element to check
#'@param class desired class to check
#'@keywords internal
#'
is_class <- function(x, class) {
  if (!inherits(x, class)) {
    cli::cli_abort("Expecting object of class {.cls {class}}", call = FALSE)
  }
}

#' @title quiet_prettyNum
#' @description Wrapper for `prettyNum()` that suppresses the original warning message if user `big.mark` equal to the character defined in the R environment `OutDec` option.
#'
#'@param x an atomic numerical or character object, possibly complex, typically a vector of real numbers.
#'@param big.mark character. Used to specify the thousands separator for patient count values.
#'@keywords internal
#'
quiet_prettyNum <- function(x, big.mark) {
  # Get the current decimal mark from user environment, if not set then return "."
  dec <- getOption("OutDec", ".")

  # if user specifies `big.mark = "."` and `dec == "."`, then suppress the warning that will pop up:
  #    "'big.mark' and 'decimal.mark' are both '.', which could be confusing"
  if (big.mark == dec) {
    suppressWarnings(prettyNum(x, scientific = FALSE, big.mark = big.mark))
  } else {
    prettyNum(x, scientific = FALSE, big.mark = big.mark)
  }
}

#' @title replace_num_in_expr
#' @description Helper function for `update_numbers()`.
#'
#'@param expr expression in `fc$text`.
#'@param row A row from the `fc` object containing `n`, `N`, and `perc` values.
#'@param big.mark character. Used to specify the thousands separator for patient count values.
#'@keywords internal
#'
replace_num_in_expr <- function(expr, row, big.mark) {
  if (is.null(expr)) {
    return(expr)
  }

  # Handle numeric values directly
  if (is.numeric(expr)) {
    if (!is.na(row$n) && identical(as.numeric(expr), as.numeric(row$n))) {
      return(quiet_prettyNum(expr, big.mark = big.mark))
    } else if (!is.na(row$N) && identical(as.numeric(expr), as.numeric(row$N))) {
      return(quiet_prettyNum(expr, big.mark = big.mark))
    } else {
      return(expr)
    }
  }

  # Handle character strings
  if (is.character(expr)) {
    # Try to extract and format numbers in the string
    formatted_text <- expr

    # Check for n value
    if (!is.na(row$n)) {
      n_pattern <- paste0("\\b", row$n, "\\b")
      if (grepl(n_pattern, formatted_text)) {
        n_formatted <- quiet_prettyNum(row$n, big.mark = big.mark)
        formatted_text <- gsub(n_pattern, n_formatted, formatted_text)
      }
    }

    # Check for N value
    if (!is.na(row$N)) {
      N_pattern <- paste0("\\b", row$N, "\\b")
      if (grepl(N_pattern, formatted_text)) {
        N_formatted <- quiet_prettyNum(row$N, big.mark = big.mark)
        formatted_text <- gsub(N_pattern, N_formatted, formatted_text)
      }
    }

    return(formatted_text)
  }

  # Handle language expressions (function calls)
  if (is.call(expr)) {
    # Process all arguments of function call
    for (i in seq_along(expr)) {
      expr[[i]] <- replace_num_in_expr(expr[[i]], row, big.mark)
    }
    return(expr)
  }
  # For formal expression objects
  else if (is.expression(expr)) {
    return(as.expression(lapply(expr, replace_num_in_expr, row = row, big.mark = big.mark)))
  }
  # Return as is for other types
  else {
    return(expr)
  }
}

#' @title update_numbers
#' @description Updates values of `n` or `N` referenced in the `text` column when user specifies `big.mark` argument in `fc_draw`.
#'
#'@param object fc object that we want to draw.
#'@param big.mark character. Used to specify the thousands separator for patient count values. Defaults is no separator (`""`); if not empty used as mark between every 3 digits (ex: `big.mark = ","` results in `1,000` instead of `1000`).
#'@keywords internal
#'
update_numbers <- function(object, big.mark = "") {

  # Get the current decimal mark from user environment, if not set then return "."
  dec <- getOption("OutDec", ".")

  # if user specifies `big.mark == dec`, then provide informative warning
  if (big.mark == dec) {
    cli::cli_warn(c(
      "You have set {.code big.mark} equal to your environment's {.code OutDec}
         ('{dec}') - it can be confusing if your flowchart uses the same mark for both.",
      "i" = "Consider an alternative decimal mark.",
      ">" = "To change the decimal mark, run:
         {.code options(OutDec = \"<alternative decimal mark>\")}"
    ))
  }

  # Handle both tibble and list formats
  fc_list <- if(tibble::is_tibble(object$fc)) list(object$fc) else object$fc

  fc_list <- lapply(fc_list, function(df) {
    for(i in 1:nrow(df)) {
      row <- df[i, ]

      # Get the text element
      text_element <- row$text

      # Handle case where text is a character vector within a list
      if (is.list(text_element) && length(text_element) == 1 && is.character(text_element[[1]])) {
        # Format numbers in the text string
        if (!is.na(row$n)) {
          n_formatted <- quiet_prettyNum(row$n, big.mark = big.mark)
          df$text[[i]] <- gsub(paste0("\\b", row$n, "\\b"), n_formatted, df$text[[i]])
        }

        if (!is.na(row$N)) {
          N_formatted <- quiet_prettyNum(row$N, big.mark = big.mark)
          df$text[[i]] <- gsub(paste0("\\b", row$N, "\\b"), N_formatted, df$text[[i]])
        }
      }
      # Handle case where text is a plain character string
      else if (is.character(text_element)) {
        # Format numbers in the text string
        if (!is.na(row$n)) {
          n_formatted <- quiet_prettyNum(row$n, big.mark = big.mark)
          df$text[i] <- gsub(paste0("\\b", row$n, "\\b"), n_formatted, df$text[i])
        }

        if (!is.na(row$N)) {
          N_formatted <- quiet_prettyNum(row$N, big.mark = big.mark)
          df$text[i] <- gsub(paste0("\\b", row$N, "\\b"), N_formatted, df$text[i])
        }
      }
      # Handle language expression
      else if (is.list(text_element) && length(text_element) > 0 && is.language(text_element[[1]])) {
        df$text[[i]] <- replace_num_in_expr(text_element[[1]], row = row, big.mark = big.mark)
      }
    }
    return(df)
  })

  # Update the object with the processed fc_list
  if(tibble::is_tibble(object$fc)) {
    object$fc <- fc_list[[1]]
  } else {
    object$fc <- fc_list
  }

  return(object)
}


#' @title format_percentage
#' @description rounds percentage values while allowing the option to `trim_trailing_zeros`
#'
#'@param value numeric; percentage value to be rounded
#'@param round_digits integer; number of digits past the decimal to round to
#'@param trim_trailing_zeros logical; trim trailing zeros or not in returned value.
#'@keywords internal
#'
format_percentage <- function(value, round_digits, trim_trailing_zeros) {
  rounded_perc <- round(value, round_digits)
  if (trim_trailing_zeros) {
    return(as.character(rounded_perc))
  } else {
    return(prettyNum(rounded_perc, nsmall = round_digits))
  }
}
