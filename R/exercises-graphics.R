exercise_library_graphics <- function() {
  category <- "Visualization"
  list(
    scatter_mapping = exercise_template(category, "Map a scatterplot", 1, function(ctx, level) {
      colour <- if (level >= 2L) sprintf(", colour = %s", ctx$g) else ""
      description <- if (level >= 2L) sprintf(" Map colour to %s.", ctx$g) else ""
      if (level == 3L) {
        colour <- paste0(colour, sprintf(", shape = factor(%s)", ctx$h))
        description <- paste(description, sprintf("Map point shape to the distinct categories in %s.", ctx$h))
      }
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
      fill <- if (level >= 2L) sprintf(", fill = factor(%s)", ctx$h) else ""
      grouping <- if (level >= 2L) sprintf(" Stack bars by %s, treating its values as discrete fill categories.", ctx$h) else ""
      proportional <- if (level == 3L) " Normalize each stack to show within-category proportions." else ""
      position <- if (level == 3L) 'position = "fill"' else ""
      exercise_plan(
        sprintf("Return a ggplot of df with %s on the horizontal axis. Use one vertical bar layer that counts records per category from the raw rows, using default styling.%s%s", ctx$g, grouping, proportional),
        sprintf("ggplot(df, aes(x = %s%s)) + geom_bar(%s)", ctx$g, fill, position),
        "The bar height must count rows within each category.",
        "geom_bar() counts observations by default.",
        sprintf("ggplot(df, aes(x = %s)) + ...", ctx$g),
        "Each incoming row contributes to the count for its category.", list(kind = "plot", default_style = TRUE)
      )
    }),
    facet_scatter = exercise_template(category, "Facet a scatterplot", 2, function(ctx, level) {
      facet <- paste(c(ctx$g, if (level >= 2L) ctx$h), collapse = ", ")
      axes <- if (level == 3L) "independent vertical scales and a shared horizontal scale" else "shared axes"
      scales <- if (level == 3L) ', scales = "free_y"' else ""
      exercise_plan(
        sprintf("Make a ggplot scatterplot of df with %s horizontally and %s vertically. Split the point layer into wrapped panels by %s with %s, default styling and the default panel layout.", ctx$x, ctx$y, facet, axes),
        sprintf("ggplot(df, aes(%s, %s)) + geom_point() + facet_wrap(vars(%s)%s)", ctx$x, ctx$y, facet, scales),
        "Use one panel per value of the faceting variable.",
        "Add facet_wrap() to a scatterplot.",
        sprintf("ggplot(df, aes(%s, %s)) + geom_point() + ...", ctx$x, ctx$y),
        "The facets partition observations while keeping the variable mappings common to all panels.", list(kind = "plot", default_style = TRUE)
      )
    }),
    scale_theme = exercise_template(category, "Apply a log scale and theme", 3, function(ctx, level) {
      axis <- sample(c("x", "y"), 1)
      theme <- sample(c("theme_minimal", "theme_classic"), 1)
      other <- if (axis == "x") "y" else "x"
      scales <- sprintf("scale_%s_log10()", axis)
      scale_task <- sprintf("Use a base-ten log scale on the %s axis.", axis)
      if (level == 3L) {
        scales <- paste(scales, sprintf("scale_%s_sqrt()", other), sep = " + ")
        scale_task <- paste(scale_task, sprintf("Use a square-root scale on the %s axis.", other))
      }
      layout <- if (level >= 2L) paste0(" + ", theme, "()") else ""
      styling <- if (level >= 2L) sprintf(" Apply %s() with its defaults.", theme) else ""
      exercise_plan(
        sprintf("Return a ggplot scatterplot of df mapping %s horizontally and %s vertically. Use default point styling. %s%s", ctx$x, ctx$y, scale_task, styling),
        sprintf("ggplot(df, aes(%s, %s)) +\n  geom_point() + %s%s", ctx$x, ctx$y, scales, layout),
        "A scale transformation changes the axis itself, including the tick labels.",
        sprintf("Use scale_%s_log10() for the logarithmic axis.", axis),
        sprintf("ggplot(df, aes(%s, %s)) + geom_point() + ...", ctx$x, ctx$y),
        "The scale transforms coordinates while retaining labels in the original units. The theme controls non-data elements.",
        list(kind = "plot", default_style = TRUE, theme = level >= 2L)
      )
    })
  )
}
