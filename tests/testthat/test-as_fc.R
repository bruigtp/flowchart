
test_that("errors with neither .data nor N", {
  expect_snapshot(as_fc(), error = TRUE)
})

test_that("errors with both .data and N", {
  expect_snapshot(as_fc(mtcars, N = 32), error = TRUE)
})

test_that("errors when text_padding is zero", {
  expect_snapshot(as_fc(N = 100, text_padding = 0), error = TRUE)
})

test_that("initializes flowchart from data frame", {
  df <- data.frame(x = 1:3)
  fc <- as_fc(df)
  expect_s3_class(fc, "fc")
  expect_equal(fc$data, dplyr::ungroup(df))
  expect_equal(nrow(fc$fc), 1)
  expect_equal(fc$fc$N, 3)
  expect_equal(fc$fc$text, "Initial dataframe\n3")
})

test_that("initializes flowchart from N", {
  fc <- as_fc(N = 10)
  expect_s3_class(fc, "fc")
  expect_equal(fc$data, tibble::tibble(id = 1:10))
  expect_equal(nrow(fc$fc), 1)
  expect_equal(fc$fc$N, 10)
  expect_equal(fc$fc$text, "Initial dataframe\n10")
})

test_that("accepts custom label and text pattern", {
  fc <- as_fc(N = 10, label = "Test", text_pattern = "{label}: {N} total")
  expect_equal(fc$fc$text, "Test: 10 total")
})

test_that("accepts expression label", {
  fc <- as_fc(N = 10, label = expression(alpha))
  expect_type(fc$fc$text[[1]], "language")
})

test_that("errors on invalid label type", {
  expect_snapshot(as_fc(N = 10, label = 1), error = TRUE)
})

test_that("warns and returns NULL fc when hide = TRUE", {
  expect_snapshot(fc <- as_fc(N = 10, hide = TRUE))
  expect_null(fc$fc)
  expect_equal(fc$data, tibble::tibble(id = 1:10))
})

test_that("preserves styling parameters", {
  fc <- as_fc(N = 10,
              text_color = "red",
              text_fs = 12,
              text_fface = 2,
              text_ffamily = "serif",
              bg_fill = "yellow",
              border_color = "blue")
  expect_equal(fc$fc$text_color, "red")
  expect_equal(fc$fc$text_fs, 12)
  expect_equal(fc$fc$text_fface, 2)
  expect_equal(fc$fc$text_ffamily, "serif")
  expect_equal(fc$fc$bg_fill, "yellow")
  expect_equal(fc$fc$border_color, "blue")
})
