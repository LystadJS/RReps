exercise_vectors <- function(ctx, variant) {
  values <- sample(seq(-15, 30, by = .5), sample(7:14, 1), replace = TRUE)
  values[1:2] <- c(-4, 0)
  if (variant == 3L) values <- rev(values)
  list(values = values)
}

exercise_library_basics <- function() {
  category <- "R fundamentals"
  list(
    vector_assignment = exercise_template(category, "Assign and transform a vector", 1, function(ctx, level) {
      extra <- if (level > 1L) sprintf(" and then subtract %d", ctx$cut) else ""
      formula <- sprintf("values * %d%s", ctx$by, if (level > 1L) sprintf(" - %d", ctx$cut) else "")
      if (level == 3L) {
        extra <- paste0(extra, ". Use the native pipe to round the transformed values to whole numbers with round()")
        formula <- sprintf("(%s) |> round()", formula)
      }
      exercise_plan(
        sprintf("Multiply each element of values by %d%s. Practice assigning the result to adjusted and return the resulting numeric vector in its original order.", ctx$by, extra),
        paste0("adjusted <- ", formula, "\nadjusted"),
        "Arithmetic on a vector operates on each element.",
        "Use <- to bind the transformed vector to a name.",
        "adjusted <- values * ...",
        "The assignment stores the full vector. Reading the name on the final line returns it.",
        fixture = exercise_vectors
      )
    }),
    vector_index = exercise_template(category, "Index a numeric vector", 2, function(ctx, level) {
      op <- sample(c(">", "<", ">="), 1)
      condition <- sprintf("values %s %d", op, ctx$by)
      direction <- if (op == "<") -1L else 1L
      if (level >= 2L) condition <- sprintf("(%s) & values != %d", condition, ctx$by + direction)
      solution <- sprintf("values[%s]", condition)
      if (level >= 3L) solution <- sprintf("unique(%s)", solution)
      exercise_plan(
        sprintf("Return just the elements of values satisfying %s, in their original order. %s", condition,
                if (level >= 3L) "Keep the first occurrence of each distinct qualifying value." else "Keep repeated values."),
        solution,
        "Make a logical flag for each element before choosing the elements.",
        "Use square brackets with a logical condition.",
        "values[values ...]",
        "Logical indexing retains one element for each true flag, preserving input order.",
        fixture = function(ctx, variant) {
          data <- exercise_vectors(ctx, variant)
          data$values[3:5] <- ctx$by + c(-1, 0, 1)
          data$values[6:7] <- ctx$by + 2L * direction
          data
        }
      )
    }),
    conditional_function = exercise_template(category, "Write a branching function", 2, function(ctx, level) {
      upper <- ctx$cut + ctx$by
      middle <- if (level >= 2L) sprintf(" Otherwise return \"middle\" below %d and \"high\" at or above it.", upper) else " Otherwise return \"high\"."
      solution <- if (level >= 2L) {
        sprintf('function(value) {\n  if (is.na(value)) return("unknown")\n  if (value < %d) return("low")\n  if (value < %d) return("middle")\n  "high"\n}', ctx$cut, upper)
      } else {
        sprintf('function(value) {\n  if (is.na(value)) return("unknown")\n  if (value < %d) "low" else "high"\n}', ctx$cut)
      }
      if (level == 3L) {
        middle <- paste(middle, 'Treat nonfinite numeric inputs, including positive and negative infinity, as "unknown".')
        solution <- sub("is.na(value)", "!is.finite(value)", solution, fixed = TRUE)
      }
      cases <- lapply(c(NA_real_, if (level == 3L) c(Inf, -Inf), -1, ctx$cut - 1, ctx$cut, upper - .5, upper, upper + 10), list)
      exercise_plan(
        sprintf('Return a function of one numeric value. Return the character string "unknown" for a missing value, and "low" for a value below %d.%s The values vector contains example inputs.', ctx$cut, middle),
        solution, "Handle a missing argument before comparing it to a threshold.",
        "Write a function with if and an early return for missing values.",
        'function(value) {\n  if (is.na(value)) return("unknown")\n  ...\n}',
        "Checking missingness first gives each subsequent comparison a definite result.",
        list(kind = "function", cases = cases), fixture = exercise_vectors
      )
    }),
    loop_accumulate = exercise_template(category, "Accumulate with a loop", 3, function(ctx, level) {
      operation <- sample(c("sum", "maximum"), 1)
      action <- if (operation == "sum") "total + value" else "max(total, value)"
      start <- if (operation == "sum") "0" else "-Inf"
      rule <- switch(level, "", " Missing entries keep the previous accumulator value.",
                     " Missing entries reset the accumulator to its starting value before the next observation. Emit the starting value at the missing entry.")
      if (level >= 2L) {
        missing <- if (level == 2L) "total" else start
        action <- sprintf("if (is.na(value)) %s else %s", missing, action)
      }
      exercise_plan(
        sprintf("Return the running %s of values as a numeric vector of the same length. Start the accumulator at %s.%s Practice writing a for loop.", operation, start, rule),
        sprintf("answer <- numeric(length(values))\ntotal <- %s\nfor (i in seq_along(values)) {\n  value <- values[[i]]\n  total <- %s\n  answer[[i]] <- total\n}\nanswer", start, action),
        "Store the current total or extreme after processing each value.",
        "Preallocate a numeric vector and loop over seq_along(values).",
        sprintf("answer <- numeric(length(values))\ntotal <- %s\nfor (i in seq_along(values)) { ... }", start),
        "The accumulator carries the previous prefix summary into the next iteration.",
        fixture = function(ctx, variant) {
          data <- exercise_vectors(ctx, variant)
          if (level >= 2L) data$values[c(2, 5)] <- NA_real_
          data
        }
      )
    })
  )
}
