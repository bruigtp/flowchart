
test_that("handles NULL expression", {
  row <- list(n = 10, N = 100)
  expect_null(replace_num_in_expr(NULL, row, ","))
})

test_that("formats numeric values", {
  row <- list(n = 1000, N = 2000)
  expect_equal(replace_num_in_expr(1000, row, ","), "1,000")
  expect_equal(replace_num_in_expr(2000, row, ","), "2,000")
})

test_that("formats numbers in character strings", {
  row <- list(n = 1000, N = 2000)
  expect_equal(
    replace_num_in_expr("Total n = 1000 out of 2000", row, ","),
    "Total n = 1,000 out of 2,000"
  )
})

test_that("preserves non-matching numbers in strings", {
  row <- list(n = 1000, N = 2000)
  expect_equal(
    replace_num_in_expr("Other numbers 3000 and 4000", row, ","),
    "Other numbers 3000 and 4000"
  )
})

test_that("handles expression objects", {
  row <- list(n = 1000, N = 2000)
  expr <- expression(paste("n =", 1000))
  result <- replace_num_in_expr(expr, row, ",")
  expect_type(result, "expression")
})

test_that("handles language objects", {
  row <- list(n = 1000, N = 2000)
  call <- quote(paste("n =", 1000))
  result <- replace_num_in_expr(call, row, ",")
  expect_type(result, "language")
})

test_that("handles NA values in row", {
  row <- list(n = NA, N = 1000)
  expect_equal(replace_num_in_expr("Text 1000", row, ","), "Text 1,000")
})

test_that("warns when big.mark equals OutDec", {
  fc <- as_fc(N = 1000)
  withr::local_options(OutDec = ".")
  expect_snapshot(update_numbers(fc, big.mark = "."))
})

test_that("formats numbers in tibble fc", {
  fc <- as_fc(N = 1000)
  result <- update_numbers(fc, big.mark = ",")
  expect_match(result$fc$text, "1,000")
})

test_that("formats numbers in list fc", {
  fc <- as_fc(N = 1000)
  fc$fc <- list(fc$fc)
  result <- update_numbers(fc, big.mark = ",")
  expect_match(result$fc[[1]]$text, "1,000")
})

test_that("handles missing values correctly", {
  fc <- as_fc(N = 10)
  fc$fc$n <- NA
  result <- update_numbers(fc, big.mark = ",")
  expect_equal(result$fc$text, fc$fc$text)
})

test_that("preserves expression text", {
  fc <- as_fc(N = 1000, label = expression(alpha))
  result <- update_numbers(fc, big.mark = ",")
  expect_type(result$fc$text[[1]], "language")
})

test_that("handles character list text", {
  fc <- as_fc(N = 1000)
  fc$fc$text <- list("Text 1000")
  result <- update_numbers(fc, big.mark = ",")
  expect_equal(result$fc$text[[1]], "Text 1,000")
})

test_that("preserves other object attributes", {
  fc <- as_fc(N = 1000)
  original_attrs <- attributes(fc)
  result <- update_numbers(fc, big.mark = ",")
  expect_equal(attributes(result)[names(original_attrs)], original_attrs)
})
