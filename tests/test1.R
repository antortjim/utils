# Smoke test: build a paired plot from random data and save it to
# tests/panel.svg for visual inspection. Run from the package root.
devtools::load_all()
set.seed(1)
n_animals <- 10
test_data <- data.table::data.table(
    # every animal (id) has one PRE and one POST measurement
    test = rep(c("PRE", "POST"), each = 2 * n_animals),
    id = rep(seq_len(2 * n_animals), times = 2),
    asleep = runif(n = 4 * n_animals, max = 1),
    phase = rep(rep(c("L", "D"), each = n_animals), times = 2)
)
panel <- paired_plot(
    data = test_data,
    group = "phase",
    y_var = "asleep",
    alternative = "two.sided",
    point_size = 4,
    point_size_mean = 6,
    y_limits = c(0, 1.2),
    n_y_ticks = 5,
    colors = c("black", "black"),
    angle_n = 0,
    test = "paired",
    offset = .25
)
ggsave(filename = "tests/panel.svg", plot = panel$gg)
print(panel$gg)
