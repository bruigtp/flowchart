
test_that("errors with invalid box_corners", {
  fc <- as_fc(N = 10)
  expect_snapshot(fc_draw(fc, box_corners = "invalid"), error = TRUE)
})

test_that("accepts valid box_corners values", {
  fc <- as_fc(N = 10)
  expect_no_error(fc_draw(fc, box_corners = "round"))
  expect_no_error(fc_draw(fc, box_corners = "sharp"))
})

test_that("sets arrow parameters in attributes", {
  fc <- as_fc(N = 10)
  result <- fc_draw(fc)
  attrs <- attr(result$fc, "draw")
  expect_equal(attrs$arrow_angle, 30)
  expect_equal(attrs$arrow_ends, "last")
  expect_equal(attrs$arrow_type, "closed")
  expect_s3_class(attrs$arrow_length, "unit")
})

test_that("coerces tibble fc to list", {
  fc <- as_fc(N = 10)
  expect_type(fc$fc, "list")
  result <- fc_draw(fc)
  expect_type(result$fc, "list")
})
