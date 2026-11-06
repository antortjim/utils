# ggplot2 themes shared by the package plots. Each theme builds on the
# previous one.

# Base theme: font sizes, no facet strips or plot tags
sleep_plot_theme <- ggplot2::theme(
  axis.text = ggplot2::element_text(size = TEXT_SIZE, face = "plain"),
  axis.title = ggplot2::element_text(size = TITLE_SIZE, face = "plain"),
  strip.text = ggplot2::element_blank(),
  plot.tag = ggplot2::element_blank(),
  legend.key.width = unit(1, "null")
)

# paired_plot(): no x axis (PRE/POST position is implicit), no facet spacing
paired_plot_theme <- sleep_plot_theme + theme(
  axis.title.x = ggplot2::element_blank(),
  axis.ticks.x = ggplot2::element_blank(),
  axis.text.x = ggplot2::element_blank(),
  axis.line.x = ggplot2::element_blank(),
  panel.spacing = unit(0, "npc"),
  panel.spacing.x = unit(0, "pt")
)
summary_plot_theme <- paired_plot_theme + ggplot2::theme(
  panel.spacing = unit(0, "npc")
)

# Time traces: framed panels, small x labels, no y axis
traces_plot_theme <- summary_plot_theme + ggplot2::theme(
  axis.title.y = ggplot2::element_blank(),
  axis.text.x = ggplot2::element_text(size = TEXT_SIZE *0.8),
  axis.line.y = ggplot2::element_blank(),
  axis.ticks.length = unit(.05, "cm"),
  title = ggplot2::element_blank(),
  strip.text = ggplot2::element_blank(),
  panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 1)
)

# Deprecated alias
learning_plot_theme <- paired_plot_theme
