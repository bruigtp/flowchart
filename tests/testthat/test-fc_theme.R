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

test_that("modify text pattern boxes", {
  fc <- as_fc(N = 10) |>
    fc_split(N = c(5, 5)) |>
    fc_filter(N = c(1, 1), show_exc = TRUE)

  result <- fc_theme(fc, text_pattern = "{label}, {n}", text_pattern_init = "{label}: {N}", text_pattern_exc = "{label} -> {n}/{N}")

  expect_equal(unique(result$fc$text_pattern[! result$fc$type %in% c("init", "exclude", "title_split")]), "{label}, {n}")
  expect_equal(unique(result$fc$text_pattern[result$fc$type == "init"]), "{label}: {N}")
  expect_equal(unique(result$fc$text_pattern[result$fc$type == "exclude"]), "{label} -> {n}/{N}")

  expect_equal(result$fc$text[1], "Initial dataframe: 10")
  expect_equal(result$fc$text[2], "group 1, 5")
  expect_equal(result$fc$text[3], "group 2, 5")
  expect_equal(result$fc$text[5], "Excluded -> 4/5")

})

test_that("modify text pattern boxes with expression", {
  fc <- as_fc(N = 10) |>
    fc_split(N = c(5, 5)) |>
    fc_filter(N = c(1, 1), show_exc = TRUE)

  result <- fc_theme(fc, text_pattern =  expression(paste("{label}", bold("{n}"))), text_pattern_init = expression(paste("{label}", bold("{n}"))), text_pattern_exc =  expression(paste("{label}", bold("{n}"))))

  expect_equal(unique(result$fc$text_pattern[! result$fc$type %in% c("init", "exclude", "title_split")])[[1]], expression(paste("{label}", bold("{n}"))))
  expect_equal(unique(result$fc$text_pattern[result$fc$type == "init"])[[1]], expression(paste("{label}", bold("{n}"))))
  expect_equal(unique(result$fc$text_pattern[result$fc$type == "exclude"])[[1]], expression(paste("{label}", bold("{n}"))))

  expect_equal(result$fc$text[[1]], rlang::expr(atop("Initial dataframe", paste("", bold("10")))))
  expect_equal(result$fc$text[[2]], rlang::expr(atop("group 1", paste("", bold("5")))))
  expect_equal(result$fc$text[[3]], rlang::expr(atop("group 2", paste("", bold("5")))))
  expect_equal(result$fc$text[[5]], rlang::expr(atop("Excluded", paste("", bold("4")))))

})

test_that("modify text pattern boxes with expression 2", {
  fc <- as_fc(N = 10, label = expression(paste("Patients ", italic("assessed"), " for ", bold("eligibility")))) |>
    fc_split(N = c(5, 5)) |>
    fc_filter(N = c(1, 1), show_exc = TRUE)

  result <- fc_theme(fc, text_pattern =  expression(paste("{label}", bold("{n}"))), text_pattern_init = expression(paste("{label}", bold("{n}"))), text_pattern_exc =  expression(paste("{label}", bold("{n}"))))

  expect_equal(unique(result$fc$text_pattern[! result$fc$type %in% c("init", "exclude", "title_split")])[[1]], expression(paste("{label}", bold("{n}"))))
  expect_equal(unique(result$fc$text_pattern[result$fc$type == "init"])[[1]], expression(paste("{label}", bold("{n}"))))
  expect_equal(unique(result$fc$text_pattern[result$fc$type == "exclude"])[[1]], expression(paste("{label}", bold("{n}"))))

  expect_equal(result$fc$text[[1]], rlang::expr(atop(paste("Patients ", italic("assessed"), " for ", bold("eligibility")),
                                                     paste("", bold("10")))))
  expect_equal(result$fc$text[[2]], rlang::expr(atop("group 1", paste("", bold("5")))))
  expect_equal(result$fc$text[[3]], rlang::expr(atop("group 2", paste("", bold("5")))))
  expect_equal(result$fc$text[[5]], rlang::expr(atop("Excluded", paste("", bold("4")))))

})


test_that("don't modify flowchart", {
  fc <- as_fc(N = 10)
  result <- suppressWarnings(fc_theme(fc))
  expect_equal(fc$fc, result$fc)
  expect_s3_class(result, "fc")
})
