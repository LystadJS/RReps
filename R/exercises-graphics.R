exercise_library_graphics <- function() {
  category <- "Visualization"
  list(
    scatter_mapping = exercise_template(category, "Map a scatterplot", 1, function(ctx, level) {
      colour <- if (level >= 2L) sprintf(", colour = %s", ctx$g) else ""
      description <- if (level >= 2L) sprintf(" Map colour to %s.", ctx$g) else ""
      exercise_plan(
        sprintf("Return a ggplot scatterplot of df, mapping %s to the horizontal axis and %s to the vertical axis.%s Use one point layer with default styling.", ctx$x, ctx$y, description),
        sprintf("ggplot(df, aes(x = %s, y = %s%s)) + geom_point()", ctx$x, ctx$y, colour),
        "Place the two variables in the coordinate mapping.",
        "Use ggplot(), aes(), and geom_point().",
        sprintf("ggplot(df, aes(x = %s, ...)) + ...", ctx$x),
        "The mappings bind input columns to visual positions and the point layer draws observations.",
        list(kind = "plot", default_style = TRUE)
      )
    }),
    bar_counts = exercise_template(category, "Show counts with bars", 1, function(ctx, level) {
      exercise_plan(
        sprintf("Return a ggplot of df with %s on the horizontal axis. Use one vertical bar layer that counts records per category from the raw rows, using default styling.", ctx$g),
        sprintf("ggplot(df, aes(x = %s)) + geom_bar()", ctx$g),
        "The bar height must count rows within each category.",
        "geom_bar() counts observations by default.",
        sprintf("ggplot(df, aes(x = %s)) + ...", ctx$g),
        "Each incoming row contributes to the count for its category.", list(kind = "plot", default_style = TRUE)
      )
    }),
    facet_scatter = exercise_template(category, "Facet a scatterplot", 2, function(ctx, level) {
      facet <- if (level >= 2L) ctx$h else ctx$g
      exercise_plan(
        sprintf("Make a ggplot scatterplot of df with %s horizontally and %s vertically. Split the point layer into wrapped panels by %s with shared axes, default styling and the default panel layout.", ctx$x, ctx$y, facet),
        sprintf("ggplot(df, aes(%s, %s)) + geom_point() + facet_wrap(vars(%s))", ctx$x, ctx$y, facet),
        "Use one panel per value of the faceting variable.",
        "Add facet_wrap() to a scatterplot.",
        sprintf("ggplot(df, aes(%s, %s)) + geom_point() + ...", ctx$x, ctx$y),
        "The facets partition observations while keeping the variable mappings common to all panels.", list(kind = "plot", default_style = TRUE)
      )
    }),
    scale_theme = exercise_template(category, "Apply a log scale and theme", 3, function(ctx, level) {
      axis <- sample(c("x", "y"), 1)
      theme <- sample(c("theme_minimal", "theme_classic"), 1)
      exercise_plan(
        sprintf("Return a ggplot scatterplot of df mapping %s horizontally and %s vertically. Use a base-ten log scale on the %s axis and apply %s() with its defaults.", ctx$x, ctx$y, axis, theme),
        sprintf("ggplot(df, aes(%s, %s)) +\n  geom_point() + scale_%s_log10() + %s()", ctx$x, ctx$y, axis, theme),
        "A scale transformation changes the axis itself, including the tick labels.",
        sprintf("Use scale_%s_log10() and a complete theme.", axis),
        sprintf("ggplot(df, aes(%s, %s)) + geom_point() + ...", ctx$x, ctx$y),
        "The scale transforms coordinates while retaining labels in the original units. The theme controls non-data elements.",
        list(kind = "plot", theme = TRUE)
      )
    })
  )
}
