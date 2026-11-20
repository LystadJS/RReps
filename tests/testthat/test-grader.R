summary_spec <- function() {
  list(
    seed = 100L,
    contract = list(kind = "table"),
    fixtures = list(
      list(df = data.frame(team = c("a", "a", "b"), hours = c(2, 4, 5))),
      list(df = data.frame(team = c("b", "a", "b"), hours = c(1, NA_real_, 9))),
      list(df = data.frame(team = character(), hours = numeric()))
    ),
    solution = "df |> group_by(team) |> summarise(avg = mean(hours, na.rm = TRUE), .groups = 'drop')"
  )
}

testthat::test_that("equivalent code passes all fixtures, including missing values and empty data", {
  spec <- summary_spec()
  alternate <- "df |> summarise(avg = mean(hours, na.rm = TRUE), .by = team) |> arrange(desc(team))"
  result <- assess_answer(spec, alternate)
  testthat::expect_equal(result$status, "pass", info = paste(result$checks$detail, collapse = "\n"))
  testthat::expect_true(all(result$checks$passed))
  testthat::expect_true(any(startsWith(result$checks$label, "Hidden 2")))
})

testthat::test_that("hard-coded preview, incorrect names and incorrect types fail", {
  spec <- summary_spec()
  hardcoded <- assess_answer(spec, "data.frame(team = c('a','b'), avg = c(3,5))")
  testthat::expect_equal(hardcoded$status, "incorrect")
  testthat::expect_true(all(hardcoded$checks$passed[startsWith(hardcoded$checks$label, "Preview")]))
  wrong_name <- assess_answer(spec, paste0(spec$solution, " |> rename(wrong = avg)"))
  testthat::expect_equal(wrong_name$status, "incorrect")
  wrong_type <- assess_answer(spec, paste0(spec$solution, " |> mutate(avg = as.character(avg))"))
  testthat::expect_equal(wrong_type$status, "incorrect")
})

testthat::test_that("syntax, runtime, timeout, and broken exercise remain distinct", {
  spec <- summary_spec()
  testthat::expect_equal(assess_answer(spec, "mean(")$status, "syntax_error")
  testthat::expect_equal(assess_answer(spec, "stop('deliberate')")$status, "runtime_error")
  testthat::expect_equal(assess_answer(spec, "Sys.sleep(10)", timeout = 1)$status, "timeout")
  spec$solution <- "stop('unavailable reference')"
  testthat::expect_equal(assess_answer(spec, "df")$status, "infrastructure_error")
})

testthat::test_that("warnings and console output accompany the actual assessment", {
  spec <- summary_spec()
  answer <- paste("cat('checking rows\\n'); message('my message'); warning('example warning');", spec$solution)
  review <- assess_answer(spec, answer)
  testthat::expect_equal(review$status, "pass")
  testthat::expect_true(any(grepl("checking rows", review$output)))
  testthat::expect_true(any(grepl("my message", review$output)))
  testthat::expect_true("example warning" %in% review$warnings)
})

testthat::test_that("functions must handle edge cases and actually run in the worker", {
  spec <- list(fixtures = list(list(offset = 4), list(offset = 2)),
               solution = "function(x) x + offset",
               contract = list(kind = "function", cases = list(list(c(1, NA_real_, 8)), list(numeric()))))
  testthat::expect_equal(assess_answer(spec, "function(v) offset + v")$status, "pass")
  testthat::expect_equal(assess_answer(spec, "function(v) 4 + v")$status, "incorrect")
  testthat::expect_equal(assess_answer(spec, "function(v) if (length(v)) offset + v else stop('empty')")$status,
                         "runtime_error")
  spec$contract$cases <- NULL
  testthat::expect_equal(assess_answer(spec, spec$solution)$status, "infrastructure_error")
})

testthat::test_that("plot contracts compare layer meaning, scale, panels and theme", {
  spec <- summary_spec()
  spec$fixtures <- list(list(df = data.frame(x = 2:6, y = 12:16, g = c("a","a","b","c","c"))),
                        list(df = data.frame(x = 3:8, y = c(2,9,1,20,16,4), g = rep(c("p","q"), 3))))
  spec$contract <- list(kind = "plot", theme = TRUE)
  spec$solution <- "ggplot(df, aes(x, y, colour = g)) + geom_point() + facet_wrap(vars(g)) + scale_x_log10() + theme_minimal()"
  alternate <- "ggplot() + geom_point(data = arrange(df, desc(y)), aes(y = y, x = x, color = g), size = 3) + facet_wrap(~g) + scale_x_continuous(transform = 'log10') + theme_minimal()"
  review <- assess_answer(spec, alternate)
  testthat::expect_equal(review$status, "pass", info = paste(review$checks$detail, collapse = "\n"))
  equivalent_factor <- sub("colour = g", "colour = factor(g)", spec$solution, fixed = TRUE)
  testthat::expect_equal(assess_answer(spec, equivalent_factor)$status, "pass")
  alias <- sub("facet_wrap(vars(g))", "facet_wrap(vars(area = g))", spec$solution, fixed = TRUE)
  testthat::expect_equal(assess_answer(spec, alias)$status, "pass")
  wrong_key <- paste(spec$solution, "+ scale_colour_discrete(labels = rev)")
  testthat::expect_equal(assess_answer(spec, wrong_key)$status, "incorrect")
  wrong_scale <- sub("scale_x_log10()", "scale_x_sqrt()", spec$solution, fixed = TRUE)
  testthat::expect_equal(assess_answer(spec, wrong_scale)$status, "incorrect")
  wrong_theme <- sub("theme_minimal()", "theme_bw()", spec$solution, fixed = TRUE)
  testthat::expect_equal(assess_answer(spec, wrong_theme)$status, "incorrect")
  wrong_geom <- sub("geom_point()", "geom_line()", spec$solution, fixed = TRUE)
  testthat::expect_equal(assess_answer(spec, wrong_geom)$status, "incorrect")
  wrong_facet <- sub(" + facet_wrap(vars(g))", "", spec$solution, fixed = TRUE)
  testthat::expect_equal(assess_answer(spec, wrong_facet)$status, "incorrect")
  wrong_mapping <- sub("aes(x, y,", "aes(y, x,", spec$solution, fixed = TRUE)
  testthat::expect_equal(assess_answer(spec, wrong_mapping)$status, "incorrect")
  testthat::expect_equal(assess_answer(spec, paste(spec$solution, "+ coord_flip()"))$status, "incorrect")
  spec$contract$default_style <- TRUE
  wrong_size <- sub("geom_point()", "geom_point(size = 0)", spec$solution, fixed = TRUE)
  testthat::expect_equal(assess_answer(spec, wrong_size)$status, "incorrect")
  broken_mapping <- sub("aes(x, y,", "aes(unknown_col, y,", spec$solution, fixed = TRUE)
  testthat::expect_equal(assess_answer(spec, broken_mapping)$status, "runtime_error")
})

testthat::test_that("ordering, grouping, shape and type follow the exercise contract", {
  spec <- summary_spec()
  spec$contract$ordered <- TRUE
  testthat::expect_equal(assess_answer(spec, paste0(spec$solution, " |> arrange(desc(team))"))$status, "incorrect")
  testthat::expect_equal(assess_answer(spec, paste0(spec$solution, " |> group_by(team)"))$status, "incorrect")
  testthat::expect_equal(assess_answer(spec, paste0(spec$solution, " |> rowwise()"))$status, "incorrect")
  spec$contract <- list(kind = "value")
  spec$solution <- "df$hours"
  testthat::expect_equal(assess_answer(spec, "as.character(df$hours)")$status, "incorrect")
  testthat::expect_equal(assess_answer(spec, "matrix(df$hours)")$status, "incorrect")
})

testthat::test_that("misspelled contracts and missing reference outputs fail closed", {
  spec <- summary_spec()
  spec$contract$orderred <- TRUE
  testthat::expect_equal(assess_answer(spec, spec$solution)$status, "infrastructure_error")
  spec$contract <- list(kind = "value")
  spec$solution <- "NULL"
  testthat::expect_equal(assess_answer(spec, "NULL")$status, "infrastructure_error")
})

testthat::test_that("submission working directory is disposable and the process is separate", {
  spec <- summary_spec()
  answer <- paste("cat(getwd(), '\\n', Sys.getpid(), '\\n', sep='');", spec$solution)
  review <- assess_answer(spec, answer)
  testthat::expect_equal(review$status, "pass")
  testthat::expect_false(dir.exists(review$output[[1L]]))
  testthat::expect_false(identical(as.integer(review$output[[2L]]), Sys.getpid()))
})
