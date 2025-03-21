
test_that("merges multiple flowcharts", {
  fc1 <- as_fc(N = 10)
  fc2 <- as_fc(N = 15)
  result <- fc_merge(list(fc1, fc2))
  expect_s3_class(result, "fc")
  expect_equal(length(result$data), 2)
  expect_equal(length(result$fc), 2)
  expect_equal(result$data[[1]], fc1$data)
  expect_equal(result$data[[2]], fc2$data)
})

test_that("updates x coordinates for merged charts", {
  fc1 <- as_fc(N = 10)
  fc2 <- as_fc(N = 15)
  result <- fc_merge(list(fc1, fc2))
  x_coords1 <- result$fc[[1]]$x
  x_coords2 <- result$fc[[2]]$x
  expect_true(all(x_coords1 != x_coords2))
})

test_that("preserves input data and structure", {
  fc1 <- as_fc(N = 10)
  fc2 <- as_fc(N = 15)
  result <- fc_merge(list(fc1, fc2))
  expect_equal(nrow(result$fc[[1]]), nrow(fc1$fc))
  expect_equal(nrow(result$fc[[2]]), nrow(fc2$fc))
  expect_named(result, c("id", "data", "fc"))
})

test_that("handles single flowchart correctly", {
  fc1 <- as_fc(N = 10)
  result <- fc_merge(list(fc1))
  expect_equal(length(result$data), 1)
  expect_equal(length(result$fc), 1)
  expect_equal(result$data[[1]], fc1$data)
})

test_that("preserves attributes and classes", {
  fc1 <- as_fc(N = 10)
  fc2 <- as_fc(N = 15)
  result <- fc_merge(list(fc1, fc2))
  expect_s3_class(result, "fc")
  expect_true(rlang::is_list(result))
  expect_true(tibble::is_tibble(result$fc[[1]]))
  expect_true(tibble::is_tibble(result$fc[[2]]))
})
