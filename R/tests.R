# Statistical tests available to paired_plot() and friends. They are looked up
# by name as paste0(test, "_wilcoxon_test"), so the prefix is what users pass.

unpaired_t_test <- function(x, y, ...) {
  t.test(x = x, y = y, paired = FALSE, ...)
}

paired_t_test <- function(x, y, ...) {
  t.test(x = x, y = y, paired = TRUE, ...)
}

unpaired_wilcoxon_test <- function(x, y, ...) {
  wilcox.test(x = x, y = y, paired = FALSE, ...)
}

paired_wilcoxon_test <- function(x, y, ...) {
  wilcox.test(x = x, y = y, paired = TRUE, ...)
}
