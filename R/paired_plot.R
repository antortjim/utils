#' Quantify the effect of a treatment on a numeric variable
#' 
#' Produce a paired plot that represents the value of a numeric variable
#' in the same individual before and after some treatment
#' 
#' @param data Data frame with columns:
#' * y_var: numeric from -1 to 1
#' * test: one of PRE or POST
#' * id: unique to each animal. The same animal must have one PRE and one POST value,
#'     and no more
#' * a column named according to the input argument 'group', used to separate
#'     animals by some category e.g. genotype, treatment, etc
#'  @param group A column in the data frame data, see argument data
#'  @import data.table
#'  @import ggplot2
#'  @export
paired_plot <- function(
    data, y_var, group, direction = "horizontal",
    test = "paired",
    map_signif_level = TRUE,
    y_limits = c(-1, 1),
    trend_statistic = TREND_STATISTIC,
    error_statistic = ERROR_STATISTIC,
    colors = NULL,
    y_annotation = NULL,
    x_annotation = 1.5,
    text_hjust = 0.5,
    y_annotation_n = -1,
    text_y_size = TEXT_SIZE,
    title_y_size = TITLE_SIZE,
    starsize = STARSIZE,
    textsize = N_TEXT_SIZE,
    y_step = 0.5,
    expansion_y_bottom = EXPANSION_Y_BOTTOM,
    expansion_y_top = EXPANSION_Y_TOP,
    distribution_color = DISTRIBUTION_COLOR,
    linewidth = LINEWIDTH,
    point_size = POINT_SIZE,
    linewidth_mean = LINEWIDTH_MEAN,
    point_size_mean = POINT_SIZE_MEAN,
    family = FONT,
    vjust = VJUST,
    angle_n = 45,
    text_vjust = 0,
    offset = 0,
    correction = NULL,
    group_levels = NULL,
    drop = FALSE,
    alternative="greater",
    y_label=NULL,
    n_y_ticks=NULL
    ) {

  if (is.null(group)) {
    data$group__ <- "A"
    group <- "group__"
  } else if (!(group %in% colnames(data))) {
    data$group__ <- group
  } else {
    data$group__ <- data[[group]]
  }
  
  if (is.null(group_levels)) group_levels <- levels(data[[group]])

  if (is.null(y_label)) {
      y_label<-y_var
  }

  if (!is.null(test)) test <- get(paste0(test, "_wilcoxon_test"))

  . <- std_error <- id <- annotations <- x <- group__ <- N <- NULL

  data$y_var <- data[[y_var]]
  

  if (is.na(y_limits[1])) {
      y_limits[1] <- min(data$y_var)
  }
  if (is.na(y_limits[2])) {
      y_limits[2] <- max(data$y_var)
  }

  group <- "group__"
  stopifnot("id" %in% colnames(data))

  annotation_df <- make_annotation_df(
    df = data, y_var,
    variable = group,
    test_F = test,
    trend_statistic = trend_statistic,
    error_statistic = error_statistic,
    correction = correction,
    alternative = alternative
  )
  annotation_df$y_var <- annotation_df[[y_var]]
  levels(data[[group]]) <- group_levels
  levels(annotation_df[[group]]) <- group_levels
  
  data[, x := ifelse(test == "PRE", 1+0, 2-0)]
  annotation_df[, x := ifelse(test == "PRE", 1+0, 2-0)]
  
  n_facets <- length(unique(data$group__))

  panel <- ggplot(data = data, aes(x = x, y = y_var)) +
    geom_point(
      size = point_size,
      color = distribution_color
    ) +
    geom_line(
      aes(group = id),
      color = distribution_color,
      linewidth = linewidth
    )

  panel <- add_trend_geom(
    panel, annotation_df,
    colors = colors,
    point_size = point_size_mean,
    linewidth = linewidth_mean,
    ERRORBAR_WIDTH
  )

  if (!is.null(y_annotation_n)) {
    panel <- add_n_annotation(
      panel, annotation_df,
      text_vjust = text_vjust,
      text_hjust = text_hjust,
      textsize = textsize,
      family = family,
      x_annotation = x_annotation,
      y_annotation = y_annotation_n,
      angle = angle_n
    )
  }
  panel <- add_facet(panel, direction, drop=drop)
  
  

  if (!is.null(test)) {
    if (is.null(y_annotation)) {
      y_annotation <- y_limits[2]
      
    }
    panel <- tryCatch({
      add_significance_marks(
        panel, test, annotation_df, y_annotation, vjust,
        textsize = starsize,
        map_signif_level = map_signif_level,
        family = family, offset = 0
      )},
      error = function(e) {
        print(e)
        return(panel)
    })
  }
  
  if (is.null(n_y_ticks)) {
    y_breaks <- waiver()
  } else {
    y_breaks <- seq(from = y_limits[1], to = y_limits[2], length.out=n_y_ticks)
  }
  print(y_breaks)

  panel <- panel +
    scale_y_continuous(breaks=y_breaks, expand = expansion(add = c(0, 0)), name=y_label) +
    scale_x_continuous(expand = expansion(add=c(offset,0))) +
    coord_cartesian(clip = "off", ylim = y_limits) +
    paired_plot_theme

  data$group__ <- NULL
  return(list(
    gg = panel, n_facets = n_facets,
    direction = direction, annotation = annotation_df
  ))
}

learning_plot <- function(...) {
  warning("Deprecated. Use paired_plot")
  return(paired_plot(...))
}

save_paired_plot <- function(plot, ratio, size_unit = 5, ...) {
  if (plot$direction == "horizontal") {
    width <- plot$n_facets * size_unit
    height <- size_unit * ratio
  } else if (plot$direction == "vertical") {
    height <- plot$n_facets * size_unit / ratio
    width <- size_unit * ratio
  }
  suppressWarnings({
    svg(width = width, height = height, ...)
    print(plot$gg)
    dev.off()
  })
}

add_trend_geom <- function(
    panel, annotation_df,
    colors = NULL,
    point_size = POINT_SIZE_MEAN,
    linewidth = LINEWIDTH_MEAN,
    errorbar_width = ERRORBAR_WIDTH) {

  error <- group__ <- x <- y_var <- NULL
  
  
  margin <- 0.0 # 0.04
  annotation_df[, margin := margin]
  annotation_df[, ymin := ifelse((y_var - error) < (y_var - margin), (y_var - error), (y_var - margin))]
  annotation_df[, ymax := ifelse((y_var + error) > (y_var + margin), (y_var + error), (y_var + margin))]

  
  if (is.null(colors)) {
    panel <- panel +
      geom_line(
        data = annotation_df,
        aes(
          x = x, y = y_var,
          color = group__,
          group = group__
        ),
        linewidth = linewidth
      ) +
      geom_point(
        data = annotation_df,
        mapping = aes(
          x = x, y = y_var,
          color = group__,
          group = group__
        ),
        size = point_size
      )
  } else {

    panel <- panel +
      geom_line(
        data = annotation_df,
        aes(
          x = x, y = y_var,
          col = group__,
          group = group__
        ),
        linewidth = linewidth
      ) +
      geom_point(
        data = annotation_df,
        size = point_size,
        # shape = 1,
        mapping = aes(
          color = group__,
          x = x, y = y_var,
          group = group__
        )
      )
    if(length(colors) != length(unique(annotation_df$group__))) {
      warning("Passed colors does not match number of facets")
    } else {
      panel <- panel + scale_fill_manual(values = colors) +
      scale_color_manual(values = colors)
    }
  }
  

  return(panel)
}


add_facet <- function(panel, direction, drop=TRUE) {
  if (direction == "horizontal") {
    panel <- panel + facet_grid(. ~ group__, drop=drop)
  } else if (direction == "vertical") {
    panel <- panel + facet_grid(group__ ~ ., drop=drop)
  }
  return(panel)
}
