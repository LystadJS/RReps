exercise_join_plan <- function(ctx, level, type) {
  foreign <- if (level >= 2L) "lookup_id" else ctx$key
  key_spec <- if (foreign == ctx$key) sprintf('"%s"', ctx$key) else sprintf('c("%s" = "%s")', ctx$key, foreign)
  second <- ""
  if (level == 3L) {
    key_spec <- sprintf('c("%s" = "%s", "%s" = "lookup_period")', ctx$key, foreign, ctx$h)
    second <- sprintf(" Also match df.%s to lookup.lookup_period; both keys must agree.", ctx$h)
  }
  label <- sample(c("label", "division", "series"), 1)
  goal <- switch(type,
    left_join = sprintf("Preserve every row and column of df, adding %s from lookup wherever the key matches. Use a missing value for unmatched rows.", label),
    inner_join = sprintf("Keep the matching rows of df and append %s from lookup. Retain all original df columns.", label),
    anti_join = "Keep only rows of df whose key is absent from lookup. Retain exactly the df columns."
  )
  exercise_plan(
    sprintf("%s Match df.%s to lookup.%s.%s Return an ungrouped table in the original df row order.", goal, ctx$key, foreign, second),
    sprintf("df |> %s(lookup, by = %s)", type, key_spec),
    "Identify which table supplies the rows and which supplies the match keys.",
    sprintf("Use %s() with an explicit by argument.", type),
    sprintf("df |> %s(lookup, by = ...)", type),
    switch(type,
      left_join = "A left join keeps unmatched left rows and fills their new columns with missing values.",
      inner_join = "An inner join keeps key matches and brings along the lookup columns.",
      anti_join = "An anti join uses the lookup for membership and returns unmatched left rows."
    ),
    exercise_table(TRUE),
    function(ctx, variant) {
      data <- exercise_frame(ctx, variant)
      df <- data$df
      size <- nrow(df)
      df[[ctx$key]] <- sample(rep(seq_len(ceiling(size / 2)), length.out = size))
      ids <- c(sample(unique(df[[ctx$key]]), floor(size / 4)), size + 1L)
      lookup <- data.frame(id = ids, text = sample(LETTERS, length(ids)), stringsAsFactors = FALSE)
      names(lookup) <- c(foreign, label)
      if (level == 3L) {
        period <- as.integer(df[[ctx$h]][match(ids, df[[ctx$key]])])
        period[is.na(period)] <- 2021L
        lookup$lookup_period <- period
        # One primary ID has two lookup records, distinguishable by period.
        other <- lookup[1L, , drop = FALSE]
        other$lookup_period <- if (period[1L] == 2023L) 2022L else 2023L
        other[[label]] <- "Supplement"
        lookup <- rbind(lookup, other)
        # A secondary-key mismatch must remove a row even for an anti join.
        rows <- which(df[[ctx$key]] == ids[2L])
        df[[ctx$h]][rows[1L]] <- 2024L
      }
      list(df = df, lookup = lookup)
    }
  )
}

exercise_library_joins <- function() {
  category <- "Joining datasets"
  list(
    join_left = exercise_template(category, "Augment rows with a left join", 1,
      function(ctx, level) exercise_join_plan(ctx, level, "left_join")),
    join_inner = exercise_template(category, "Keep key matches with an inner join", 2,
      function(ctx, level) exercise_join_plan(ctx, level, "inner_join")),
    join_unmatched = exercise_template(category, "Find unmatched rows", 3,
      function(ctx, level) exercise_join_plan(ctx, level, "anti_join"))
  )
}

exercise_pivot_fixture <- function(ctx, variant) {
  df <- exercise_frame(ctx, variant)$df[c(ctx$key, ctx$x, ctx$y)]
  list(df = df)
}

exercise_library_reshape <- function() {
  category <- "Reshaping and missing values"
  list(
    drop_missing = exercise_template(category, "Remove incomplete observations", 1, function(ctx, level) {
      cols <- paste(c(ctx$x, if (level >= 2L) ctx$y, if (level == 3L) ctx$g), collapse = ", ")
      exercise_plan(
        sprintf("From df, keep rows complete on these columns: %s. Ignore missing values in other columns. Preserve all columns and original row order.", cols),
        sprintf("df |> drop_na(%s)", cols),
        "A row qualifies only if all the requested fields are present.",
        "Use drop_na() with the target column names.",
        "df |> drop_na(...)",
        "Selecting columns confines the completeness check to the fields named in the task.",
        exercise_table(TRUE), function(ctx, variant) {
          data <- exercise_frame(ctx, variant)
          data$df[[ctx$y]][2:3] <- NA
          data$df[[ctx$g]][4:6] <- NA
          data
        }
      )
    }),
    pivot_long = exercise_template(category, "Reshape wide data to long", 2, function(ctx, level) {
      names_to <- sample(c("measure", "metric", "field"), 1)
      values_to <- sample(c("reading", "amount", "observed"), 1)
      drop <- level >= 2L
      pivot_cols <- c(ctx$x, ctx$y)
      if (level == 3L) pivot_cols <- paste0("reading_", pivot_cols)
      prefix <- if (level == 3L) ', names_prefix = "reading_"' else ""
      strip <- if (level == 3L) " Remove the reading_ prefix from each measurement label." else ""
      exercise_plan(
        sprintf("Stack columns %s of df into long form. Return columns %s, %s (measurement label), and %s (numeric value).%s %s Row order is unrestricted.", paste(pivot_cols, collapse = " and "), ctx$key, names_to, values_to, strip, if (drop) "Omit missing measurements." else "Keep missing measurements."),
        sprintf('df |> pivot_longer(c(%s), names_to = "%s", values_to = "%s", values_drop_na = %s%s)', paste(pivot_cols, collapse = ", "), names_to, values_to, drop, prefix),
        "Each original measurement column becomes a set of rows.",
        "Use pivot_longer() and specify both output column names.",
        sprintf("df |> pivot_longer(c(%s), ...)", paste(pivot_cols, collapse = ", ")),
        "The identifier repeats as measurement names and values move into two columns.",
        exercise_table(column_order = TRUE), function(ctx, variant) {
          data <- exercise_pivot_fixture(ctx, variant)
          names(data$df)[2:3] <- pivot_cols
          data
        }
      )
    }),
    pivot_wide = exercise_template(category, "Reshape long data to wide", 3, function(ctx, level) {
      key <- sample(c("metric", "field", "measure"), 1)
      val <- sample(c("reading", "amount", "observed"), 1)
      fill <- level >= 2L
      duplicates <- if (level == 3L) " For repeated measurements of the same identifier and field, use their mean (a missing record makes the mean missing)." else ""
      aggregate <- if (level == 3L) ", values_fn = mean" else ""
      exercise_plan(
        sprintf("Widen df to one row per %s, with one numeric column for each observed %s and entries taken from %s. %s%s", ctx$key, key, val, if (fill) "Fill absent combinations with zero and preserve explicitly missing entries." else "Leave absent combinations missing.", duplicates),
        sprintf("df |> pivot_wider(names_from = %s, values_from = %s%s%s)", key, val, if (fill) ", values_fill = 0" else "", aggregate),
        "Use the measurement labels as new column names.",
        "Use pivot_wider() to identify the names and values columns.",
        sprintf("df |> pivot_wider(names_from = %s, ...)", key),
        "The remaining identifier defines each output row. Unobserved combinations are distinct from recorded missing values.",
        exercise_table(), function(ctx, variant) {
          df <- exercise_pivot_fixture(ctx, variant)$df
          long <- tidyr::pivot_longer(df, dplyr::all_of(c(ctx$x, ctx$y)), names_to = key, values_to = val)
          long <- long[-c(2L, 5L), ]
          if (level == 3L) {
            repeated <- utils::head(long[!is.na(long[[val]]), ], 3L)
            repeated[[val]] <- repeated[[val]] + 4
            long <- rbind(long, repeated)
          }
          list(df = long)
        }
      )
    })
  )
}
