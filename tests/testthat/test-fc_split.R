
test_that("errors with neither var nor N", {
  fc <- as_fc(N = 10)
  expect_snapshot(fc_split(fc), error = TRUE)
})

test_that("errors with both var and N", {
  fc <- as_fc(N = 10)
  expect_snapshot(fc_split(fc, var = "group", N = 5), error = TRUE)
})

test_that("errors when text_padding is zero", {
  fc <- as_fc(N = 10)
  expect_snapshot(fc_split(fc, N = c(5,5), text_padding = 0), error = TRUE)
})

test_that("errors with invalid label type", {
  fc <- as_fc(N = 10)
  expect_snapshot(fc_split(fc, N = c(5,5), label = 1), error = TRUE)
})

test_that("errors when sel_group used without previous split", {
  fc <- as_fc(N = 10)
  expect_snapshot(fc_split(fc, N = c(5,5), sel_group = "A"), error = TRUE)
})

test_that("handles sel_group in a split", {
  expect_warning(
    fc <- as_fc(N = 100) |>
      fc_split(N = c(60, 40)) |>
      fc_split(N = c(30, 10), sel_group = "group 2")
  )
  expect_no_error(fc |> fc_draw())
  expect_equal(nrow(fc$fc), 5)
  expect_equal(fc$fc$text[5], "group 2\n10 (25.00%)")
})

test_that("handles split after a sel_group", {
  expect_warning(
    fc <- as_fc(N = 100) |>
      fc_split(N = c(60, 40)) |>
      fc_split(N = c(30, 10), sel_group = "group 2")
  )

  expect_warning(
    fc2 <- fc |>
      fc_split(N = c(50, 10), sel_group = "group 1")
  )

  expect_no_error(fc2 |> fc_draw())
  expect_equal(nrow(fc2$fc), 7)
  expect_equal(fc2$fc$text[7], "group 2\n10 (16.67%)")
})

test_that("handles numeric splits correctly", {
  fc <- as_fc(N = 10)
  result <- fc_split(fc, N = c(5,5))
  expect_equal(nrow(result$fc), 3)  # Initial box + 2 split boxes
  expect_equal(unique(result$fc$type[2:3]), "split")
})

test_that("handles factor splits correctly", {
  df <- data.frame(group = factor(rep(c("A","B"), each = 5)))
  fc <- as_fc(df)
  result <- fc_split(fc, var = group)
  expect_equal(nrow(result$fc), 3)  # Initial box + 2 split boxes
  expect_equal(sum(result$fc$n[2:3]), 10)
})

test_that("handles custom labels", {
  fc <- as_fc(N = 10)
  result <- fc_split(fc, N = c(5,5), label = c("Group A", "Group B"))
  expect_match(result$fc$text[2], "Group A")
  expect_match(result$fc$text[3], "Group B")
})

test_that("handles title correctly", {
  fc <- as_fc(N = 10)
  result <- fc_split(fc, N = c(5,5), title = "Test Title")
  expect_equal(sum(result$fc$type == "title_split"), 1)
  expect_equal(result$fc$text[result$fc$type == "title_split"], "Test Title")
})

test_that("handles hide=TRUE correctly", {
  fc <- suppressWarnings(as_fc(N = 10, hide = TRUE))
  result <- fc_split(fc, N = c(6,4))
  expect_equal(nrow(result$fc), 2)
  expect_equal(unique(result$fc$type), "split")
})

test_that("preserves styling parameters", {
  fc <- as_fc(N = 10)
  result <- fc_split(fc, N = c(5,5),
                     text_color = "red",
                     text_fs = 12,
                     bg_fill = "yellow",
                     border_color = "blue")
  new_boxes <- result$fc |> dplyr::filter(type == "split")
  expect_equal(unique(new_boxes$text_color), "red")
  expect_equal(unique(new_boxes$text_fs), 12)
  expect_equal(unique(new_boxes$bg_fill), "yellow")
  expect_equal(unique(new_boxes$border_color), "blue")
})


test_that("trimming trailing zeros", {

  fc1 <- as_fc(N = 100) |>
    fc_split(N = c(25, 75), trim_trailing_zeros = FALSE)

  perc1 <- unique(fc1$fc$perc[fc1$fc$type != "init"])
  expect_equal(perc1, c("25.00", "75.00"))

  fc2 <- as_fc(N = 100) |>
    fc_split(N = c(25, 75), trim_trailing_zeros = TRUE)

  perc2 <- unique(fc2$fc$perc[fc2$fc$type != "init"])
  expect_equal(perc2, c("25", "75"))

})

test_that("add title", {

  fc <- as_fc(N = 100, label = "Assessed for eligibility", title = "Enrollment") |>
    fc_split(N = c(60, 40), label = c("Allocated to control", "Allocated to intervention"), title = "Allocation")

  expect_equal(tail(fc$fc$type, 1), "title_split")
  expect_equal(tail(fc$fc$x, 1), 0.1)
  expect_equal(round(tail(fc$fc$y, 1), 3), 0.333)
  expect_equal(tail(fc$fc$text, 1), "Allocation")

})
