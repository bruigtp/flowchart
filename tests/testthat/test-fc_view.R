
test_that("errors with invalid what argument", {
  fc <- as_fc(N = 10)
  expect_snapshot(fc_view(fc, "invalid"), error = TRUE)
})

test_that("returns data component correctly", {
  fc <- as_fc(N = 10)
  expect_equal(fc_view(fc, "data"), fc$data)
  expect_equal(nrow(fc_view(fc, "data")), 10)
})

test_that("returns fc component correctly", {
  fc <- as_fc(N = 10)
  expect_equal(fc_view(fc, "fc"), fc$fc)
  expect_true(tibble::is_tibble(fc_view(fc, "fc")))
})

test_that("preserves tibble structure in output", {
  df <- tibble::tibble(x = 1:3, y = letters[1:3])
  fc <- as_fc(df)
  expect_true(tibble::is_tibble(fc_view(fc, "data")))
  expect_true(tibble::is_tibble(fc_view(fc, "fc")))
})

test_that("accepts valid what arguments", {
  fc <- as_fc(N = 10)
  expect_no_error(fc_view(fc, "data"))
  expect_no_error(fc_view(fc, "fc"))
})

test_that("returns correct component type", {
  fc <- as_fc(N = 10)
  expect_type(fc_view(fc, "data"), "list")  # tibble is a list
  expect_s3_class(fc_view(fc, "data"), "tbl_df")
  expect_type(fc_view(fc, "fc"), "list")
  expect_s3_class(fc_view(fc, "fc"), "tbl_df")
})
