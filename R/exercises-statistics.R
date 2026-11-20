exercise_model_fixture <- function(ctx, variant) {
  size <- sample(25:40, 1L)
  predictor <- c(2, 2, 8, 8, sample(1:10, size, TRUE))
  response <- 3 + predictor * ctx$by + stats::rnorm(length(predictor), sd = 4)
  # Opposite outcomes at two shared predictor values keep the logistic fit finite.
  binary <- c(0L, 1L, 0L, 1L, stats::rbinom(size, 1L, stats::plogis(-1 + predictor[-(1:4)] / 5)))
  df <- data.frame(predictor, response, event = binary)
  names(df)[1:2] <- c(ctx$x, ctx$y)
  list(df = df)
}

exercise_library_statistics <- function() {
  category <- "Statistical programming"
  list(
    descriptive_statistic = exercise_template(category, "Summarize a measured variable", 1, function(ctx, level) {
      stat <- sample(c("sd", "IQR", "median"), 1)
      definition <- switch(stat, sd = "sample standard deviation", IQR = "interquartile range using R's default quantiles", median = "median")
      exercise_plan(
        sprintf("Return the %s of the observed values in df$%s as a single unnamed number. Exclude missing observations.", definition, ctx$x),
        sprintf("%s(df$%s, na.rm = TRUE)", stat, ctx$x),
        "Extract the measurement column and omit missing values from the calculation.",
        sprintf("Use %s() with na.rm.", stat),
        sprintf("%s(df$%s, ...)", stat, ctx$x),
        sprintf("The function reduces the observed measurements to one %s.", definition)
      )
    }),
    normal_probability = exercise_template(category, "Compute a Normal tail probability", 2, function(ctx, level) {
      upper <- sample(c(TRUE, FALSE), 1L)
      question <- if (upper) "above" else "at or below"
      exercise_plan(
        sprintf("For every value in cutoffs, return the probability that a Normal draw is %s it, in input order. Use the supplied fixed mean center and standard deviation spread. Return an unnamed numeric vector.", question),
        sprintf("pnorm(cutoffs, mean = center, sd = spread, lower.tail = %s)", !upper),
        "The side of the cutoff determines which tail to calculate.",
        "pnorm() uses the standard deviation and can return either tail directly.",
        "pnorm(cutoffs, mean = center, sd = spread, ...)",
        "The distribution function evaluates all cutoffs in order. Its sd argument is a standard deviation.",
        fixture = function(ctx, variant) {
          center <- sample(5:15, 1L)
          spread <- sample(2:5, 1L)
          list(cutoffs = center + spread * sample(seq(-3, 3, .25), 5L), center = center, spread = spread)
        }
      )
    }),
    simulation_rate = exercise_template(category, "Estimate a probability by simulation", 2, function(ctx, level) {
      upper <- sample(c(TRUE, FALSE), 1L)
      op <- if (upper) ">" else "<"
      exercise_plan(
        sprintf("noise contains simulated draws from a Normal distribution with mean zero and variance one. Transform them to draws with fixed mean center and standard deviation spread. Return the fraction of transformed draws strictly %s cutoff as one number. Reuse the supplied noise.", if (upper) "above" else "below"),
        sprintf("draws <- center + spread * noise\nmean(draws %s cutoff)", op),
        "Scale and shift the simulated draws, then flag the event on each draw.",
        "mean() on a logical vector gives the share of true entries.",
        "draws <- center + spread * noise\nmean(...)",
        "Scaling sets the standard deviation, shifting sets the mean, and averaging the event flags estimates its probability.",
        fixture = function(ctx, variant) {
          center <- sample(10:30, 1L)
          spread <- sample(3:10, 1L)
          cutoff <- center + sample(c(-1, 0, 1), 1L) * spread
          list(noise = stats::rnorm(sample(31:75, 1L)), center = center, spread = spread, cutoff = cutoff)
        }
      )
    }),
    linear_slope = exercise_template(category, "Interpret a fitted linear model", 3, function(ctx, level) {
      change <- if (level == 1L) 1L else ctx$by
      exercise_plan(
        sprintf("Fit an ordinary least-squares linear model with an intercept predicting %s from %s in df. Return one unnamed number giving the fitted change in the mean response for a %d-unit increase in %s.", ctx$y, ctx$x, change, ctx$x),
        sprintf('fit <- lm(%s ~ %s, data = df)\nunname(coef(fit)[["%s"]] * %d)', ctx$y, ctx$x, ctx$x, change),
        "The slope measures fitted change per one unit of the predictor.",
        "Fit with lm() and extract the predictor coefficient using coef().",
        sprintf("fit <- lm(%s ~ %s, data = df)\ncoef(fit)[[...]] * ...", ctx$y, ctx$x),
        "Linear fitted changes scale in direct proportion to the predictor increase. The intercept cancels in a difference.",
        fixture = exercise_model_fixture
      )
    }),
    logistic_prediction = exercise_template(category, "Predict a fitted event probability", 3, function(ctx, level) {
      at <- sample(2:9, 1)
      exercise_plan(
        sprintf("Fit a binomial logistic regression with an intercept for event using %s in df. Return one unnamed number giving the fitted probability of event equal to one when %s is %d.", ctx$x, ctx$x, at),
        sprintf('fit <- glm(event ~ %s, data = df, family = binomial())\nunname(predict(fit, newdata = data.frame(%s = %d), type = "response"))', ctx$x, ctx$x, at),
        "The requested prediction is on the probability scale.",
        "Fit with glm(family = binomial()) and supply type = \"response\" to predict().",
        sprintf('fit <- glm(event ~ %s, data = df, family = binomial())\npredict(fit, newdata = ..., type = "response")', ctx$x),
        "The response prediction converts the fitted log odds to an event probability.",
        fixture = exercise_model_fixture
      )
    })
  )
}
