
test_that("stacks multiple flowcharts", {
  fc1 <- as_fc(N = 10)
  fc2 <- as_fc(N = 15)
  result <- fc_stack(list(fc1, fc2))
  expect_s3_class(result, "fc")
  expect_equal(length(result$data), 2)
  expect_equal(length(result$fc), 2)
  expect_equal(result$data[[1]], fc1$data)
  expect_equal(result$data[[2]], fc2$data)
})

test_that("updates y coordinates when stacking", {
  fc1 <- as_fc(N = 10)
  fc2 <- as_fc(N = 15)
  result <- fc_stack(list(fc1, fc2))
  y_coords1 <- result$fc[[1]]$y
  y_coords2 <- result$fc[[2]]$y
  expect_true(all(y_coords1 != y_coords2))
})

test_that("handles unite = TRUE with compatible charts", {
  fc1 <- as_fc(N = 10)
  fc2 <- as_fc(N = 10)
  result <- fc_stack(list(fc1, fc2), unite = TRUE)
  expect_type(result$fc, "list")
  expect_true("fc" %in% names(result))
  expect_equal(result$fc$type[2], "stack")
})

test_that("successfully handles stacking when unite = TRUE", {
  fc1 <- as_fc(N = 10) |> fc_split(N = c(5, 5))
  fc2 <- as_fc(N = 10) |> fc_split(N = c(3, 3, 4))

  result <- fc_stack(list(fc1, fc2), unite = TRUE)

  expect_s3_class(result, "fc")
})

test_that("preserves original data in stacked charts", {
  fc1 <- as_fc(N = 10)
  fc2 <- as_fc(N = 15)
  result <- fc_stack(list(fc1, fc2))
  expect_equal(result$data[[1]], fc1$data)
  expect_equal(result$data[[2]], fc2$data)
})


