exercise_library_filtering <- function() {
  category <- "Filtering and selection"
  list(
    filter_rows = exercise_template(category, "Filter rows by a condition", 1, function(ctx, level) {
      op <- sample(c(">", ">=", "<", "<="), 1)
      condition <- sprintf("%s %s %d", ctx$x, op, ctx$cut)
      if (level >= 2L) condition <- sprintf('%s & %s != "West"', condition, ctx$g)
      if (level >= 3L) condition <- sprintf("(%s) | is.na(%s)", condition, ctx$x)
      exercise_plan(
        sprintf("Filter df to rows where this condition is true: %s. Preserve all columns and the input row order.", condition),
        sprintf("df |> filter(%s)", condition),
        "Decide which rows satisfy the entire condition.",
        "Pass the logical expression to filter().",
        "df |> filter(...)",
        "filter() keeps rows where the combined predicate evaluates to true.",
        exercise_table(TRUE, TRUE)
      )
    }),
    select_columns = exercise_template(category, "Select and order columns", 1, function(ctx, level) {
      cols <- sample(c(ctx$x, ctx$y, ctx$g, ctx$h, ctx$key), if (level >= 2L) 4L else 2L)
      keys <- paste(cols, collapse = ", ")
      selection <- keys
      rename <- ""
      if (level == 3L) {
        rename <- sprintf(" Name the first output column primary_%s.", cols[1])
        selection <- paste0("primary_", cols[1], " = ", keys)
      }
      exercise_plan(
        sprintf("From df, return only these columns in this order: %s. Keep the row order.%s", keys, rename),
        sprintf("df |> select(%s)", selection), "Choose variables without subsetting any rows.",
        "Use select() with the names in the requested sequence.",
        sprintf("df |> select(%s, ...)", cols[1]),
        "The order of names supplied to select() becomes the order of output columns.",
        exercise_table(TRUE, TRUE)
      )
    }),
    arrange_rows = exercise_template(category, "Sort with tie breakers", 2, function(ctx, level) {
      var <- if (level == 1L) ctx$y else ctx$x
      keys <- c(sprintf("desc(%s)", var), if (level >= 3L) ctx$g, if (level >= 2L) ctx$key)
      criteria <- c(sprintf("%s descending (missing values last)", var),
                    if (level >= 3L) paste(ctx$g, "ascending"), if (level >= 2L) paste(ctx$key, "ascending"))
      exercise_plan(
        sprintf("Return df with all its columns. Sort by %s. Preserve input order for tied keys.", paste(criteria, collapse = ", then ")),
        sprintf("df |> arrange(%s)", paste(keys, collapse = ", ")),
        "Sort the records once using all the ordering keys together.",
        "Use desc() around a decreasing key inside arrange().",
        sprintf("df |> arrange(desc(%s), ...)", var),
        "arrange() examines later keys only when earlier keys tie.",
        exercise_table(TRUE, TRUE), function(ctx, variant) {
          data <- exercise_frame(ctx, variant)
          data$df <- data$df[sample.int(nrow(data$df)), ]
          data
        }
      )
    }),
    filter_within_group = exercise_template(category, "Filter against a group average", 3, function(ctx, level) {
      stat <- sample(c("mean", "median"), 1)
      keys <- if (level >= 2L) paste(ctx$g, ctx$h, sep = ", ") else ctx$g
      threshold <- sprintf("%s(%s, na.rm = TRUE)", stat, ctx$x)
      margin <- if (level == 3L) sprintf(" plus %d", ctx$by) else ""
      if (level == 3L) threshold <- sprintf("(%s + %d)", threshold, ctx$by)
      exercise_plan(
        sprintf("Within each group defined by %s, keep rows with %s strictly above the group's %s of observed %s%s. Return all original columns, ungrouped, in their original row order.", keys, ctx$x, stat, ctx$x, margin),
        sprintf("df |>\n  group_by(%s) |>\n  filter(%s > %s) |>\n  ungroup()", keys, ctx$x, threshold),
        "Each row needs the threshold for its own group.",
        "Group before computing the summary inside filter(), then ungroup().",
        sprintf("df |> group_by(%s) |> filter(...)", keys),
        "Grouping makes the threshold local. Missing measurements and rows at the threshold fail the strict comparison.",
        exercise_table(TRUE, TRUE)
      )
    })
  )
}

exercise_library_construction <- function() {
  category <- "Variable construction"
  list(
    mutate_measure = exercise_template(category, "Construct a numeric column", 1, function(ctx, level) {
      name <- paste0("adjusted_", ctx$x)
      expr <- sprintf("%s / %d", ctx$x, ctx$by)
      if (level >= 2L) expr <- sprintf("(%s) + %s", expr, ctx$y)
      if (level == 3L) expr <- sprintf("round(%s, 1)", expr)
      exercise_plan(
        sprintf("Return df with a new numeric column named %s calculated as %s. Keep all rows in input order and preserve the original columns.", name, expr),
        sprintf("df |> mutate(%s = %s)", name, expr), "Compute a row-wise value without reducing the number of rows.",
        "Use mutate() with the requested output name.",
        sprintf("df |> mutate(%s = ...)", name),
        "Vector arithmetic combines values from the same row and propagates missing measurements.",
        exercise_table(TRUE)
      )
    }),
    classify_values = exercise_template(category, "Classify with case_when", 2, function(ctx, level) {
      lower <- ctx$cut
      upper <- lower + ctx$by
      name <- paste0(ctx$x, "_band")
      middle <- if (level >= 2L) sprintf(' values from %d up to but excluding %d as "mid",', lower, upper) else ""
      mid_clause <- if (level >= 2L) sprintf('\n    %s < %d ~ "mid",', ctx$x, upper) else ""
      extra <- if (level == 3L) sprintf(' Within the high band, use "review" when %s is below %d.', ctx$y, lower) else ""
      review_clause <- if (level == 3L) sprintf('\n    %s < %d ~ "review",', ctx$y, lower) else ""
      exercise_plan(
        sprintf('Append the character column %s to df in its original row order. Classify missing %s as "unknown", values below %d as "low",%s and all other values as "high".%s', name, ctx$x, lower, middle, extra),
        sprintf('df |>\n  mutate(%s = case_when(\n    is.na(%s) ~ "unknown",\n    %s < %d ~ "low",%s%s\n    TRUE ~ "high"\n  ))', name, ctx$x, ctx$x, lower, mid_clause, review_clause),
        "Start with missing values, then work upward through the boundaries.",
        "case_when() selects the first true condition for each row.",
        sprintf('df |> mutate(%s = case_when(is.na(%s) ~ "unknown", ...))', name, ctx$x),
        "Checking lower thresholds first makes each later comparison apply to the remaining values.",
        exercise_table(TRUE),
        function(ctx, variant) {
          data <- exercise_frame(ctx, variant)
          data$df[[ctx$x]][1:6] <- c(NA, lower - 1, lower, upper - .5, upper, upper + 1)
          data$df[[ctx$y]][5:6] <- c(lower - 1L, lower)
          data
        }
      )
    }),
    across_transform = exercise_template(category, "Transform several columns", 3, function(ctx, level) {
      vars <- paste(ctx$x, ctx$y, sep = ", ")
      replace <- level == 1L
      new_names <- if (replace) "" else ', .names = "scaled_{.col}"'
      target <- if (replace) "Replace those two columns in place." else "Keep both originals and prefix the two new column names with scaled_."
      transform <- sprintf("value / %d", ctx$by)
      if (level == 3L) {
        target <- paste(target, "Center each scaled column on its own observed mean.")
        transform <- sprintf("(value - mean(value, na.rm = TRUE)) / %d", ctx$by)
      }
      exercise_plan(
        sprintf("In df, divide both %s and %s by %d. %s Keep all other columns and the input row order.", ctx$x, ctx$y, ctx$by, target),
        sprintf("df |> mutate(across(c(%s), function(value) %s%s))", vars, transform, new_names),
        "Apply the same transformation to both requested variables.",
        "Use across() inside mutate(). Its .names argument sets output names.",
        sprintf("df |> mutate(across(c(%s), ...))", vars),
        "across() applies the function once to each selected column.",
        exercise_table(TRUE)
      )
    })
  )
}

exercise_library_groups_extra <- function() {
  category <- "Grouped operations"
  list(
    count_groups = exercise_template(category, "Count records by group", 1, function(ctx, level) {
      keys <- if (level >= 2L) paste(ctx$g, ctx$h, sep = ", ") else ctx$g
      name <- sample(c("records", "frequency", "total_rows"), 1)
      sort_rule <- if (level == 3L) " Sort by descending count, then by all grouping keys ascending in the given order." else ""
      sorting <- if (level == 3L) sprintf(" |> arrange(desc(%s), %s)", name, keys) else ""
      exercise_plan(
        sprintf("Count every row of df within each observed combination of %s. Return an ungrouped table containing only the key columns and a count named %s.%s", keys, name, sort_rule),
        sprintf('df |> count(%s, name = "%s")%s', keys, name, sorting),
        "Each record contributes one to its group's total, including records with missing measurements.",
        "Use count() and name the total with its name argument.",
        sprintf("df |> count(%s, ...)", keys),
        "count() creates one row per observed combination of grouping keys.", exercise_table(level == 3L)
      )
    }),
    centered_groups = exercise_template(category, "Center observations within groups", 2, function(ctx, level) {
      stat <- sample(c("mean", "median"), 1)
      keys <- if (level >= 2L) paste(ctx$g, ctx$h, sep = ", ") else ctx$g
      name <- paste0(ctx$x, "_centered")
      columns <- if (level == 3L) c(ctx$x, ctx$y) else ctx$x
      definitions <- sprintf("%s_centered = %s - %s(%s, na.rm = TRUE)", columns, columns, stat, columns)
      extra <- if (level == 3L) sprintf(" Also center %s on its own group %s and append %s_centered.", ctx$y, stat, ctx$y) else ""
      exercise_plan(
        sprintf("Append %s to df. Subtract the %s of observed %s within %s from each row's %s.%s Keep every original row in order, preserve the other columns, and return ungrouped data.", name, stat, ctx$x, keys, ctx$x, extra),
        sprintf("df |> group_by(%s) |>\n  mutate(%s) |>\n  ungroup()", keys, paste(definitions, collapse = ", ")),
        "The center is shared within a group while the centered value varies by row.",
        "Use group_by() and compute the difference inside mutate().",
        sprintf("df |> group_by(%s) |> mutate(%s = ...)", keys, name),
        "mutate() repeats each group's center alongside its observations before subtraction.", exercise_table(TRUE)
      )
    }),
    count_empty_groups = exercise_template(category, "Count unused factor levels", 3, function(ctx, level) {
      name <- sample(c("records", "frequency", "total_rows"), 1)
      keys <- if (level == 1L) ctx$g else paste(ctx$g, ctx$h, sep = ", ")
      count <- if (level == 3L) sprintf("Sum row weights from %s", ctx$y) else "Count rows"
      weight <- if (level == 3L) sprintf(", wt = %s", ctx$y) else ""
      exercise_plan(
        sprintf("%s in df for every combination of factor levels of %s, including unused combinations with zero. Return an ungrouped table with the factor keys and the total %s.", count, keys, name),
        sprintf('df |> count(%s%s, name = "%s", .drop = FALSE)', keys, weight, name),
        "A factor can describe categories missing from the observations.",
        "Pass .drop = FALSE to count() to retain unused levels.",
        sprintf("df |> count(%s, .drop = ...)", ctx$g),
        "Keeping unused factor levels gives empty categories a row with a count of zero.",
        exercise_table(), function(ctx, variant) {
          data <- exercise_frame(ctx, variant)
          data$df[[ctx$g]] <- factor(data$df[[ctx$g]], levels = c("South", "East", "North", "West", "Central"))
          if (level >= 2L) data$df[[ctx$h]] <- factor(data$df[[ctx$h]], levels = 2021:2024)
          data
        }
      )
    })
  )
}
