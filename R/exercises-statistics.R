exercise_model_fixture <- function(ctx, variant) {
  size <- sample(25:40, 1L)
  predictor <- c(2, 2, 8, 8, 2, 2, sample(1:10, size, TRUE))
  exposure <- c(1, 1, 1, 1, 5, 5, sample(1:5, size, TRUE))
  response <- 3 + predictor * ctx$by + stats::rnorm(length(predictor), sd = 4)
  # Opposing outcomes at three noncollinear covariate pairs keep the adjusted logistic fit finite.
  binary <- c(0L, 1L, 0L, 1L, 0L, 1L, stats::rbinom(size, 1L, stats::plogis(-1 + predictor[-(1:6)] / 5)))
  df <- data.frame(predictor, response, event = binary, exposure = exposure)
  names(df)[1:2] <- c(ctx$x, ctx$y)
  list(df = df)
}

exercise_library_statistics <- function() {
  category <- "Statistical programming"
  list(
    descriptive_statistic = exercise_template(category, "Summarize a measured variable", 1, function(ctx, level) {
      stat <- sample(c("sd", "IQR", "median"), 1)
      definition <- switch(stat, sd = "sample standard deviation", IQR = "interquartile range using R's default quantiles", median = "median")
      group <- sample(c("North", "East", "West"), 2L)
      summarize <- function(label = NULL) {
        column <- sprintf("df$%s", ctx$x)
        if (!is.null(label)) column <- sprintf('%s[df$%s == "%s"]', column, ctx$g, label)
        sprintf("%s(%s, na.rm = TRUE)", stat, column)
      }
      task <- sprintf("Return the %s of the observed values in df$%s", definition, ctx$x)
      solution <- summarize()
      if (level >= 2L) {
        task <- sprintf('%s for rows where %s is "%s"', task, ctx$g, group[1])
        solution <- summarize(group[1])
      }
      if (level == 3L) {
        task <- sprintf('%s, minus the same statistic where %s is "%s"', task, ctx$g, group[2])
        solution <- paste(solution, summarize(group[2]), sep = " - ")
      }
      exercise_plan(
        paste(task, "as a single unnamed number. Exclude missing observations."),
        solution,
        "Extract the measurement column and omit missing values from the calculation.",
        sprintf("Use %s() with na.rm.", stat),
        sprintf("%s(df$%s, ...)", stat, ctx$x),
        sprintf("The function reduces the observed measurements to one %s.", definition)
      )
    }),
    normal_probability = exercise_template(category, "Compute a Normal tail probability", 2, function(ctx, level) {
      upper <- sample(c(TRUE, FALSE), 1L)
      question <- if (upper) "above" else "at or below"
      parameter <- if (level == 1L) "standard deviation spread" else "variance variance_value"
      sd <- if (level == 1L) "spread" else "sqrt(variance_value)"
      task <- sprintf("For every value in cutoffs, return the probability that a Normal draw is %s it, in input order.", question)
      solution <- sprintf("pnorm(cutoffs, mean = center, sd = %s, lower.tail = %s)", sd, !upper)
      if (level == 3L) {
        task <- "For each paired lower_bound and upper_bound, return the probability that a Normal draw falls between those bounds, in input order."
        solution <- sprintf("pnorm(upper_bound, center, %s) - pnorm(lower_bound, center, %s)", sd, sd)
      }
      exercise_plan(
        sprintf("%s Use the supplied fixed mean center and %s. Return an unnamed numeric vector.", task, parameter),
        solution,
        if (level == 3L) "Subtract the accumulated probability at the lower bound from the one at the upper bound." else "The side of the cutoff determines which tail to calculate.",
        if (level == 1L) "pnorm() uses the standard deviation and can return either tail directly."
        else "pnorm() takes a standard deviation. Take the square root of the supplied variance.",
        sprintf("pnorm(%s, mean = center, sd = %s, ...)", if (level == 3L) "upper_bound" else "cutoffs", sd),
        "The distribution function evaluates all cutoffs in order. Its sd argument is a standard deviation.",
        fixture = function(ctx, variant) {
          center <- sample(5:15, 1L)
          spread <- sample(2:5, 1L)
          cutoffs <- center + spread * sample(seq(-3, 3, .25), 5L)
          bounds <- if (level == 3L) list(lower_bound = cutoffs, upper_bound = cutoffs + sample(1:7, 5L)) else list(cutoffs = cutoffs)
          scale <- if (level == 1L) list(spread = spread) else list(variance_value = spread^2)
          c(bounds, list(center = center), scale)
        }
      )
    }),
    simulation_rate = exercise_template(category, "Estimate a probability by simulation", 2, function(ctx, level) {
      upper <- sample(c(TRUE, FALSE), 1L)
      op <- if (upper) ">" else "<"
      event <- if (level == 1L) sprintf("draws %s cutoff", op) else "draws >= lower_bound & draws <= upper_bound"
      requirement <- if (level == 1L) sprintf("strictly %s cutoff", if (upper) "above" else "below") else "in the closed interval from lower_bound to upper_bound"
      conditioning <- if (level == 3L) " Restrict both the numerator and denominator to transformed draws strictly above eligibility." else ""
      selection <- if (level == 3L) "\ndraws <- draws[draws > eligibility]" else ""
      exercise_plan(
        sprintf("noise contains simulated draws from a Normal distribution with mean zero and variance one. Transform them to draws with fixed mean center and standard deviation spread. Return the fraction of transformed draws %s as one number.%s Reuse the supplied noise.", requirement, conditioning),
        sprintf("draws <- center + spread * noise%s\nmean(%s)", selection, event),
        "Scale and shift the simulated draws, then flag the event on each draw.",
        "mean() on a logical vector gives the share of true entries.",
        "draws <- center + spread * noise\nmean(...)",
        "Scaling sets the standard deviation, shifting sets the mean, and averaging the event flags estimates its probability.",
        fixture = function(ctx, variant) {
          center <- sample(10:30, 1L)
          spread <- sample(3:10, 1L)
          cutoff <- center + sample(c(-1, 0, 1), 1L) * spread
          args <- list(noise = stats::rnorm(sample(31:75, 1L)), center = center, spread = spread)
          if (level == 1L) return(c(args, list(cutoff = cutoff)))
          args$lower_bound <- center - spread
          args$upper_bound <- center + spread
          if (level == 3L) args$eligibility <- center - .5 * spread
          args
        }
      )
    }),
    linear_slope = exercise_template(category, "Interpret a fitted linear model", 3, function(ctx, level) {
      change <- if (level == 1L) 1L else ctx$by
      predictors <- paste0(ctx$x, if (level == 3L) " + exposure" else "")
      adjustment <- if (level == 3L) ", holding the additional predictor exposure fixed" else ""
      exercise_plan(
        sprintf("Fit an ordinary least-squares linear model with an intercept predicting %s from %s in df. Return one unnamed number giving the fitted change in the mean response for a %d-unit increase in %s%s.", ctx$y, predictors, change, ctx$x, adjustment),
        sprintf('fit <- lm(%s ~ %s, data = df)\nunname(coef(fit)[["%s"]] * %d)', ctx$y, predictors, ctx$x, change),
        "The slope measures fitted change per one unit of the predictor.",
        "Fit with lm() and extract the predictor coefficient using coef().",
        sprintf("fit <- lm(%s ~ %s, data = df)\ncoef(fit)[[...]] * ...", ctx$y, predictors),
        "Linear fitted changes scale in direct proportion to the predictor increase. The intercept cancels in a difference.",
        fixture = exercise_model_fixture
      )
    }),
    logistic_prediction = exercise_template(category, "Predict a fitted event probability", 3, function(ctx, level) {
      at <- sample(2:9, 1)
      predictors <- paste0(ctx$x, if (level == 3L) " + exposure" else "")
      targets <- if (level == 1L) as.character(at) else sprintf("c(%d, %d, %d)", at, at + 1L, at - 1L)
      extra <- if (level == 3L) ", exposure = c(1, 3, 5)" else ""
      condition <- if (level == 3L) " Set exposure to 1, 3 and 5, respectively." else ""
      outcome <- if (level == 1L) "one unnamed number" else "an unnamed numeric vector in the specified order"
      exercise_plan(
        sprintf("Fit a binomial logistic regression with an intercept for event using %s in df. Return %s giving the fitted event probability at %s equal to %s.%s", predictors, outcome, ctx$x, targets, condition),
        sprintf('fit <- glm(event ~ %s, data = df, family = binomial())\nunname(predict(fit, newdata = data.frame(%s = %s%s), type = "response"))', predictors, ctx$x, targets, extra),
        "The requested prediction is on the probability scale.",
        "Fit with glm(family = binomial()) and supply type = \"response\" to predict().",
        sprintf('fit <- glm(event ~ %s, data = df, family = binomial())\npredict(fit, newdata = ..., type = "response")', predictors),
        "The response prediction converts the fitted log odds to an event probability.",
        fixture = exercise_model_fixture
      )
    })
  )
}
