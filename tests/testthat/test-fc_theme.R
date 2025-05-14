test_that("modifies flowchart with new parameter", {
  fc <- as_fc(N = 10)
  result <- fc_theme(fc, text_fs = 12)
  expect_equal(result$fc$text_fs, 12)
  expect_s3_class(result, "fc")
})

test_that("preserves other attributes when modifying", {
  fc <- as_fc(N = 10)
  original_n <- fc$fc$n
  result <- fc_theme(fc, text_fs = 12)
  expect_equal(result$fc$n, original_n)
  expect_equal(result$data, fc$data)
})

test_that("modifies only excluded boxes", {
  fc <- as_fc(N = 10) |>
    fc_filter(N = 2, show_exc = TRUE)
  result <- fc_theme(fc, text_fs_exc = 12)
  expect_equal(result$fc$text_fs[result$fc$type == "exclude"], 12)
  expect_equal(result$fc$text_fs[result$fc$type != "exclude"], c(8, 8))
  expect_s3_class(result, "fc")
})

test_that("modifies only title boxes", {
  fc <- as_fc(N = 10) |>
    fc_split(N = c(5, 5), title = "Group")
  result <- fc_theme(fc, text_fs_title = 12)
  expect_equal(result$fc$text_fs[result$fc$type == "title_split"], 12)
  expect_equal(result$fc$text_fs[result$fc$type != "title_split"], c(8, 8, 8))
  expect_s3_class(result, "fc")
})

test_that("don't modify flowchart", {
  fc <- as_fc(N = 10)
  result <- suppressWarnings(fc_theme(fc))
  expect_equal(fc$fc, result$fc)
  expect_s3_class(result, "fc")
})
