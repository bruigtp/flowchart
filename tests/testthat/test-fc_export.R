
test_that("errors without draw parameters", {
  fc <- as_fc(N = 10)
  expect_snapshot(fc_export(fc, "test.png"), error = TRUE)
})

test_that("errors with invalid format", {
  fc <- as_fc(N = 10) |> fc_draw()
  expect_snapshot(fc_export(fc, "test.invalid"), error = TRUE)
})

test_that("errors when format doesn't match extension", {
  fc <- as_fc(N = 10) |> fc_draw()
  expect_snapshot(fc_export(fc, "test.png", format = "pdf"), error = TRUE)
})

test_that("errors with no extension and no format", {
  fc <- as_fc(N = 10) |> fc_draw()
  expect_snapshot(fc_export(fc, "test"), error = TRUE)
})

test_that("errors with invalid vector format units", {
  fc <- as_fc(N = 10) |> fc_draw()
  expect_snapshot(fc_export(fc, "test.pdf", units = "px"), error = TRUE)
})

test_that("errors with invalid bitmap format units", {
  fc <- as_fc(N = 10) |> fc_draw()
  expect_snapshot(fc_export(fc, "test.png", units = "invalid"), error = TRUE)
})

test_that("warns about default dimensions for vector formats", {
  fc <- as_fc(N = 10) |> fc_draw()
  expect_snapshot(fc_export(fc, "test.pdf", units = "cm"))
})

test_that("warns about default dimensions for bitmap formats", {
  fc <- as_fc(N = 10) |> fc_draw()
  expect_snapshot(fc_export(fc, "test.png", units = "in"), error = TRUE)
})

test_that("adds extension when format specified without one", {
  fc <- as_fc(N = 10) |> fc_draw()
  tempdir <- tempdir()
  filename <- file.path(tempdir, "test")
  expect_no_error(fc_export(fc, filename, format = "png"))
  expect_true(file.exists(file.path(tempdir, "test.png")))
})

test_that("uses path when specified", {
  fc <- as_fc(N = 10) |> fc_draw()
  tempdir <- tempdir()
  expect_no_error(fc_export(fc, "test.png", path = tempdir))
  expect_true(file.exists(file.path(tempdir, "test.png")))
})
