# Convert p values to significance labels: "NS" (p > 0.05), "*" (p <= 0.05),
# "**" (p <= 0.01) or "***" (p <= 0.005), with thresholds optionally corrected
# for multiple testing. NA p values give NA.
pvalue2stars <- function(p, correction, alphas = c(0.05, 0.01, 0.005)) {
  
  alphas <- multiple_testing_correction(alphas, p, correction)
  
  stars <- ifelse(
    p > alphas[1],
    "NS",
    ifelse(
      p > alphas[2],
      "*",
      ifelse(
        p > alphas[3],
        "**",
        "***"
      )
    )
  )
  return(stars)
}



# Correct significance thresholds for the number of p values tested.
# Only "bonferroni" is implemented; NULL means no correction.
multiple_testing_correction <- function(alphas, p, correction) {
  if (!is.null(correction) && correction=="bonferroni") {
    message(paste0("Applying bonferroni correction: dividing alpha by number of tests = ", length(p)))
    alphas <- alphas / length(p)
  } else if (!is.null(correction)) {
    warning(paste0("Correction method ", correction, " not implemented. Ignoring."))
  }
  return(alphas)
}


# Test PRE vs POST within each group of `variable` and compute summary
# statistics. Returns one row per group and test with columns p (numeric),
# p_label, estimate (mean POST - mean PRE), stars, N (animals), average,
# median, std, std_error, error (= error_statistic) and `y_var`
# (= trend_statistic). Note the statistics are computed on the column named
# literally "y_var", which paired_plot() creates.
#' @import data.table
make_annotation_df <- function(df, y_var, variable, test_F, trend_statistic, error_statistic, correction, alternative, ...) {
  test <- var__ <- std_error <- y_std <- N <- . <- NULL
  
  
  values <- levels(df[[variable]])
  if (is.null(values)) {
    values <- unique(df[[variable]])
  }
  if (is.null(test_F)) {
    warning("No test passed, significance will not be evaluated")
  }
  min_n_points <- 2


  test_out <- lapply(values, function(val) {
    x <- df[df[[variable]] == val & test == "PRE", y_var]
    y <- df[df[[variable]] == val & test == "POST", y_var]
    estim <- round(mean(y) - mean(x), 2)

    if (alternative == "one.sided") {
      alt <- ifelse(estim > 0, "less", "greater")
    } else {
      stopifnot(alternative %in% c("less", "greater", "two.sided"))
      alt <- alternative
    }

    if (length(x) < min_n_points | is.null(test_F)) {
      return(list(p.value = NA, estimate = NA))
    }


    if (!is.null(test_F)) {
      out <- test_F(
        x = x,
        y = y,
        alternative = alt,
        ...
      )
      out$estimate <- estim
    } else {
      out <- list(p.value = NA, estimate = estim)
    }
    out
  })
  p_values <- sapply(test_out, function(x) x$p.value)
  estimates <- sapply(test_out, function(x) x$estimate)
  
  annotation_df <- data.table(
    y_position = 0.5,
    p = round(p_values, 4),
    estimate = estimates,
    stars =  pvalue2stars(p_values, correction)
  )
  annotation_df[[variable]] <- factor(values, levels = values)
  annotation_df$p_label <- ifelse(
    !is.na(annotation_df$p) & annotation_df$p < 0.001,
    "< 0.001",
    as.character(annotation_df$p)
  )
  # divide by two because each fly is represented twice (once for the pre and once for the post)
  annotation_df$N <- sapply(
    annotation_df[[variable]], function(val) {
      nrow(df[df[[variable]] == val, ]) / 2
    }
  )
  
  df$var__ <- df[[variable]]
  stats_df <- df[, .(
    average = mean(y_var),
    median = median(y_var),
    std = sd(y_var)
  ), by = .(var__, test)]
  stats_df[[variable]] <- stats_df$var__
  stats_df$var__ <- NULL
  stats_df <- merge(annotation_df, stats_df, by = variable)
  
  
  stats_df[, std_error := std / sqrt(N)]
  
  stopifnot(error_statistic %in% colnames(stats_df))
  stopifnot(trend_statistic %in% colnames(stats_df))
  stats_df$error <- stats_df[[error_statistic]]
  stats_df[[y_var]] <- stats_df[[trend_statistic]]
  return(stats_df)
}

# Label each facet with its number of animals. If x_annotation is NULL the x
# position is taken from an x_pos column of annotation_df.
add_n_annotation <- function(panel, annotation_df, x_annotation = NULL, y_annotation = -Inf, text_vjust = 0, text_hjust = 0, textsize = TEXT_SIZE, angle = 0) {
  if (!is.null(x_annotation)) {
    panel <- panel +
      geom_text(
        data = annotation_df, y = y_annotation, size = textsize, angle = angle,
        hjust = text_hjust,
        vjust = text_vjust,
        mapping = aes(label = paste0("n = ", N)),
        x = x_annotation
      )
  } else {
    panel <- panel +
      geom_text(
        data = annotation_df,
        mapping = aes(label = paste0("n = ", N), x = x_pos),
        y = y_annotation, size = textsize, angle = angle,
        hjust = text_hjust,
        vjust = text_vjust
      )
  }
  return(panel)
}


# Write the data behind a figure panel to csv files under OUTPUT_FOLDER:
# one file per group (restricted to `columns`) and one file with the
# `y_column` of every group side by side.
export_csvs <- function(panel_data, grouping_column, groups, figure_count, columns, y_column = "POST") {
  y_columns <- list()
  available_groups <- c()
  panel_data[["group__"]] <- panel_data[[grouping_column]]
  
  for (group in groups) {
    panel_data_subset <- panel_data[
      group__ == group,
    ]
    if (!is.null(columns)) {
      panel_data_subset <- panel_data_subset[, columns, with = F]
      if (nrow(panel_data_subset) == 0) {
        warning(paste0("No data found for ", group))
        next
      }
      out <- paste0(OUTPUT_FOLDER, "/Fig", substr(figure_count, 1, 1), "/Figure_", figure_count, "_", group, ".csv")
      message(out)
      data.table::fwrite(
        x = panel_data_subset,
        file = out,
        quote = TRUE
      )
    }
    available_groups <- c(available_groups, group)
    y_columns <- c(y_columns, list(panel_data_subset[[y_column]]))
  }
  
  y_columns <- Reduce(cbind, y_columns)
  colnames(y_columns) <- available_groups
  out <- paste0(OUTPUT_FOLDER, "/Fig", substr(figure_count, 1, 1), "/Figure_", figure_count, "_boxplot.csv")
  message(out)
  data.table::fwrite(x = y_columns, file = out, quote = TRUE)
}



# Reshape wide IDOC data (one PRE and one POST column per animal) into the
# long format expected by paired_plot().
melt_idoc_data <- function(panel_data, y_var) {
  panel_data_long <- melt(panel_data, measure.vars = c("PRE", "POST"), value.name = y_var, variable.name = "test")
  panel_data_long[, test := factor(test, levels = c("PRE", "POST"))]
  return(panel_data_long)
}


# Keep only the IDOC animals that also have ethoscope (sleep) data, matching
# on date, machine and region parsed from the ethoscope id.
# Note: adds date, machine_name and region_id to sleep_data by reference.
keep_only_with_ethoscope_data <- function(idoc_data, sleep_data) {
  sleep_data[, date := as.character(substr(id, 1, 10))]
  sleep_data[, machine_name := paste0("ETHOSCOPE_", as.character(substr(id, 21, 23)))]
  sleep_data[, region_id := as.integer(substr(id, 28, 29))]
  
  
  sleep_index <- unique(sleep_data[, .(machine_name, date, region_id)])
  
  # only use flies for which we have ethoscope data
  idoc_data <- merge(
    idoc_data,
    sleep_index,
    by=c("region_id", "machine_name", "date"),
    all.x=FALSE, all.y=FALSE
  )
  return(idoc_data)
  
}


#' Annotate significance of data
#'
#' Add a significance bar between PRE (x = 1) and POST (x = 2) to every facet,
#' and print the p value and effect size of each group.
#'
#' @param panel A ggplot object.
#' @param test Function used to compute the p value. Not called here: p values
#'   are precomputed in `annotation_df`.
#' @param annotation_df data.table with columns `group__`, `p`, `stars` and
#'   `estimate`, as returned by `make_annotation_df()`.
#' @param y_annotation Position along the y axis of the bar.
#' @param vjust Vertical justification of the label.
#' @param textsize Text size of the label.
#' @param map_signif_level If `TRUE` label with stars, otherwise with the
#'   p value rounded up to two decimals.
#' @param xmin,xmax x positions of the two ends of the bar.
#' @param offset How many x units to trim from each end of the bar.
#' @return The panel with the annotation added.
#' @keywords internal

add_significance_marks <- function(
    panel, test, annotation_df, y_annotation,
    vjust, textsize, map_signif_level,
    xmin = 1, xmax = 2, offset
) {
  group__ <- estimate <- p <- NULL
  
  xmin <- xmin + offset
  xmax <- xmax - offset
  
  if (map_signif_level) {
    panel <- panel + ggsignif::geom_signif(
      data = annotation_df,
      mapping = aes(annotations = stars, color=NULL, fill=NULL),
      y_position = y_annotation, test = test,
      manual = TRUE, tip_length = 0,
      vjust = vjust,
      textsize = textsize, size = 1,
      xmin = xmin, xmax = xmax
    )
  } else {
    panel <- panel + ggsignif::geom_signif(
      data = annotation_df,
      mapping = aes(annotations = paste0("< ", ceiling(p*100)/100), color=NULL, fill=NULL),
      y_position = y_annotation, test = test,
      manual = TRUE, tip_length = 0,
      vjust = vjust,
      xmin = xmin, xmax = xmax
    )
  }
  
  for (group in unique(annotation_df$group__)) {
    p <- annotation_df[group__ == group, p]
    estimate <- annotation_df[group__ == group, estimate][1]
    
    print(paste0(
      "Group ", group, " P value: ", p,
      " Effect size: ", estimate
    ))
  }
  return(panel)
}


# Like make_annotation_df() but for comparisons between groups: `comparisons`
# is a list of pairs of group__ values whose PI columns are compared with
# `test`. Requires an x_pos column in data.
make_annotation_df_boxplots <- function(data, comparisons, correction, test) {
  
  group__ <- p <- PI <- NULL
  
  test_results <- list()
  for (i in seq_along(comparisons)) {
    comparison <- comparisons[[i]]
    
    n1 <- nrow(data[group__ == comparison[1], ])
    n2 <- nrow(data[group__ == comparison[2], ])
    if (n1 > 2 && n2 > 2) {
      test_out <- test(
        data[group__ == comparison[1], PI],
        data[group__ == comparison[2], PI]
      )
    } else {
      test_out <- list(p.value = NA, estimate = NA)
    }
    test_results <- c(test_results, list(test_out))
  }
  
  
  groups <- levels(data$group__)
  test_results_df <- data.table(
    p = sapply(test_results, function(x) x$p.value),
    group__ = sapply(comparisons, function(x) paste(x, collapse = "_vs_")),
    xmin = sapply(comparisons, function(x) min(data[group__ == x[1], x_pos][1], data[group__ == x[2], x_pos][1])),
    xmax = sapply(comparisons, function(x)  max(data[group__ == x[1], x_pos][1], data[group__ == x[2], x_pos][1])),
    estimate = sapply(test_results, function(x) x$estimate[2])
  )
  test_results_df[, stars := pvalue2stars(p, correction)]
  return(test_results_df)
}


# Load ethoscope data for the animals in `metadata`, score sleep and bin it
# in 30 minute windows (asleep = minutes asleep per bin). Requires the scopr,
# sleepr and behavr packages and the ethoscope result/cache folders, and
# saves the result to dt_bin.RDS in the working directory.
load_ethoscope_data <- function(metadata=NULL) {
  
  if (is.null(metadata)) metadata <- load_metadata_fig5()
  
  metadata_linked <- scopr::link_ethoscope_metadata(metadata, result_dir = "/ethoscope_data/results")
  dt <- scopr::load_ethoscope(
    metadata_linked,
    verbose = FALSE,
    reference_hour = NA,
    cache = "/ethoscope_data/cache",
    FUN = sleepr::sleep_annotation, velocity_correction_coef = 0.0048, time_window_length = 10, min_time_immobile = 300
  )
  dt_bin <- behavr::bin_apply_all(dt, y = "asleep", x_bin_length = behavr::mins(30), summary_FUN = mean)
  dt_bin$asleep <- dt_bin$asleep * 30
  metadata_linked <- dt_bin[, meta = TRUE]
  
  metadata_linked <- merge(
    metadata_linked[, .(id, machine_name, date = as.character(substr(datetime, 1, 10)), region_id)],
    metadata,
    by = c("machine_name", "date", "region_id"), all = TRUE
  )
  setkey(metadata_linked, id)
  behavr::setmeta(dt_bin, metadata_linked)
  saveRDS(object = dt_bin, file = "dt_bin.RDS")
  # ggplot(data = behavr::rejoin(dt_bin)[Genotype == "Iso31" & interactor == "DefaultStimulator", ], aes(x = t, y = asleep, color = Training)) +
  #   stat_pop_etho() +
  #   scale_x_hours(name = "ZT")
  return(dt_bin)
}
