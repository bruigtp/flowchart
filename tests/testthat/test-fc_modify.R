
test_that("modifies flowchart with function", {
  fc <- as_fc(N = 10)
  result <- fc_modify(fc, function(x) dplyr::mutate(x, text_fs = 12))
  expect_equal(result$fc$text_fs, 12)
  expect_s3_class(result, "fc")
})

test_that("modifies flowchart with formula", {
  fc <- as_fc(N = 10)
  result <- fc_modify(fc, ~dplyr::mutate(.x, text_fs = 12))
  expect_equal(result$fc$text_fs, 12)
  expect_s3_class(result, "fc")
})

test_that("preserves other attributes when modifying", {
  fc <- as_fc(N = 10)
  original_n <- fc$fc$n
  result <- fc_modify(fc, ~dplyr::mutate(.x, text_fs = 12))
  expect_equal(result$fc$n, original_n)
  expect_equal(result$data, fc$data)
})

test_that("handles additional arguments", {
  fc <- as_fc(N = 10)
  modify_fn <- function(x, new_size) dplyr::mutate(x, text_fs = new_size)
  result <- fc_modify(fc, modify_fn, new_size = 14)
  expect_equal(result$fc$text_fs, 14)
})

test_that("works with tibble and list fc components", {
  fc1 <- as_fc(N = 10)
  fc2 <- structure(
    list(
      data = fc1$data,
      fc = list(fc1$fc)
    ),
    class = "fc"
  )

  result1 <- fc_modify(fc1, ~dplyr::mutate(.x, text_fs = 12))
  result2 <- fc_modify(fc2, ~dplyr::mutate(.x, text_fs = 12))

  expect_true(tibble::is_tibble(result1$fc))
  expect_type(result2$fc, "list")
})
