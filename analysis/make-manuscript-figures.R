options(stringsAsFactors = FALSE)

needed_packages <- c("readxl", "dplyr", "tidyr", "ggplot2")
missing_packages <- needed_packages[!vapply(needed_packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
if (length(missing_packages) > 0) {
  stop(
    "Please install the following packages before running this script: ",
    paste(missing_packages, collapse = ", ")
  )
}

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyverse)

root_dir <- normalizePath("/Users/mariacuellar/Github/stoney-analysis", mustWork = TRUE)
figure_dir <- file.path(root_dir, "figure", "manuscript")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

clean_colnames <- function(x) {
  x <- gsub("\\r|\\n", " ", x)
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  tolower(x)
}

binom_ci <- function(successes, trials, conf.level = 0.95) {
  if (trials == 0) {
    return(c(lower = NA_real_, upper = NA_real_))
  }
  out <- binom.test(successes, trials, conf.level = conf.level)$conf.int
  c(lower = out[1], upper = out[2])
}

percent_ci_table <- function(data, successes_col, trials_col, prefix) {
  success_vals <- data[[successes_col]]
  trial_vals <- data[[trials_col]]
  ci_vals <- t(vapply(
    seq_along(success_vals),
    function(i) binom_ci(success_vals[i], trial_vals[i]),
    FUN.VALUE = c(lower = 0, upper = 0)
  ))
  data[[paste0(prefix, "_lower")]] <- ci_vals[, "lower"]
  data[[paste0(prefix, "_upper")]] <- ci_vals[, "upper"]
  data
}

theme_stoney <- function() {
  theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11),
      axis.title = element_text(face = "bold"),
      legend.title = element_blank(),
      legend.position = "top"
    )
}

scale_pct <- function(x) sprintf("%1.0f%%", x * 100)

loads_path <- file.path(root_dir, "data", "LOADS 4-28-26.xlsx")
marks_path <- file.path(root_dir, "data", "MARKS 4-28-26.xlsx")

loads_sheet <- if ("Loads" %in% excel_sheets(loads_path)) "Loads" else excel_sheets(loads_path)[1]
marks_sheet <- if ("Marks" %in% excel_sheets(marks_path)) "Marks" else excel_sheets(marks_path)[1]

loads_raw <- read_excel(loads_path, sheet = loads_sheet)
marks_raw <- read_excel(marks_path, sheet = marks_sheet)

names(loads_raw) <- clean_colnames(names(loads_raw))
names(marks_raw) <- clean_colnames(names(marks_raw))

loads <- loads_raw |>
  transmute(
    round_designation = round_designation,
    load = as.integer(load),
    load_quantity = as.integer(load_quantity),
    category = as.character(category)
  ) |>
  filter(!is.na(load))

marks <- marks_raw |>
  transmute(
    load = as.integer(load),
    cartridge = as.integer(cartridge),
    mark = as.character(mark),
    minutiae = as.integer(minutiae),
    eslr = suppressWarnings(as.numeric(eslr)),
    log10_eslr = suppressWarnings(as.numeric(log10eslr)),
    id = as.integer(id)
  ) |>
  filter(!is.na(load), !is.na(cartridge), !is.na(mark))

cartridge_all_marks <- marks |>
  group_by(load, cartridge) |>
  summarize(
    any_mark = 1L,
    any_id = as.integer(any(id == 1, na.rm = TRUE)),
    any_nifm = as.integer(any(id == 0, na.rm = TRUE)),
    .groups = "drop"
  )

load_occurrence <- loads |>
  left_join(
    cartridge_all_marks |>
      group_by(load) |>
      summarize(
        rounds_with_any_mark = sum(any_mark),
        rounds_with_id = sum(any_id),
        rounds_with_nifm = sum(any_nifm),
        .groups = "drop"
      ),
    by = "load"
  ) |>
  mutate(
    rounds_with_any_mark = tidyr::replace_na(rounds_with_any_mark, 0L),
    rounds_with_id = tidyr::replace_na(rounds_with_id, 0L),
    rounds_with_nifm = tidyr::replace_na(rounds_with_nifm, 0L)
  )

occurrence_by_category <- load_occurrence |>
  group_by(category) |>
  summarize(
    rounds_tested = sum(load_quantity),
    rounds_with_any_mark = sum(rounds_with_any_mark),
    rounds_with_id = sum(rounds_with_id),
    rounds_with_nifm = sum(rounds_with_nifm),
    .groups = "drop"
  ) |>
  mutate(
    any_mark_rate = rounds_with_any_mark / rounds_tested,
    id_rate = rounds_with_id / rounds_tested,
    nifm_rate = rounds_with_nifm / rounds_tested
  )

occurrence_by_category <- percent_ci_table(occurrence_by_category, "rounds_with_any_mark", "rounds_tested", "any_mark")
occurrence_by_category <- percent_ci_table(occurrence_by_category, "rounds_with_id", "rounds_tested", "id")
occurrence_by_category <- percent_ci_table(occurrence_by_category, "rounds_with_nifm", "rounds_tested", "nifm")

category_levels <- occurrence_by_category |>
  arrange(nifm_rate, id_rate) |>
  pull(category)

occurrence_by_category <- occurrence_by_category |>
  mutate(category = factor(category, levels = category_levels))

yield_long <- occurrence_by_category |>
  transmute(
    category,
    `Identifiable marks only` = id_rate,
    `Identifiable marks + NIFMs` = any_mark_rate
  ) |>
  pivot_longer(
    cols = c(`Identifiable marks only`, `Identifiable marks + NIFMs`),
    names_to = "series",
    values_to = "rate"
  )

fig1 <- ggplot() +
  geom_segment(
    data = occurrence_by_category,
    aes(
      y = category,
      yend = category,
      x = id_rate,
      xend = any_mark_rate
    ),
    linewidth = 1.1,
    color = "#9CA3AF"
  ) +
  geom_point(
    data = yield_long,
    aes(x = rate, y = category, color = series),
    size = 3.2
  ) +
  scale_color_manual(values = c(
    "Identifiable marks only" = "#1F4E79",
    "Identifiable marks + NIFMs" = "#B23A48"
  )) +
  scale_x_continuous(labels = scale_pct, limits = c(0, max(occurrence_by_category$any_mark_rate) * 1.12)) +
  labs(
    title = "Including NIFMs substantially increases cartridge-level yield",
    subtitle = "Points show the proportion of tested cartridges yielding identifiable marks alone versus any observed mark.",
    x = "Yield per tested cartridge",
    y = NULL
  ) +
  theme_stoney()

fig2 <- ggplot(
  occurrence_by_category,
  aes(x = nifm_rate, y = category)
) +
  geom_errorbar(
    aes(xmin = nifm_lower, xmax = nifm_upper),
    orientation = "y",
    width = 0.18,
    linewidth = 0.8,
    color = "#4B5563"
  ) +
  geom_point(size = 3, color = "#1F4E79") +
  scale_x_continuous(labels = scale_pct, limits = c(0, max(occurrence_by_category$nifm_upper) * 1.08)) +
  labs(
    title = "NIFM occurrence varies across handgun categories",
    subtitle = "Points show the observed NIFM rate; horizontal bars show exact 95% confidence intervals.",
    x = "Cartridges with at least one NIFM",
    y = NULL
  ) +
  theme_stoney()

nifm_eslr <- marks |>
  filter(id == 0, !is.na(log10_eslr)) |>
  left_join(loads |> select(load, category), by = "load")

strong_support_by_category <- nifm_eslr |>
  group_by(category) |>
  summarize(
    n_nifm_marks = n(),
    strong_support = sum(log10_eslr >= 3, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    strong_support_rate = strong_support / n_nifm_marks
  )

strong_support_by_category <- percent_ci_table(
  strong_support_by_category,
  "strong_support",
  "n_nifm_marks",
  "strong_support"
)

strong_support_by_category <- strong_support_by_category |>
  mutate(category = factor(category, levels = category_levels))

fig3 <- ggplot(
  strong_support_by_category,
  aes(x = strong_support_rate, y = category)
) +
  geom_errorbar(
    aes(xmin = strong_support_lower, xmax = strong_support_upper),
    orientation = "y",
    width = 0.18,
    linewidth = 0.8,
    color = "#4B5563"
  ) +
  geom_point(size = 3, color = "#B23A48") +
  scale_x_continuous(labels = scale_pct, limits = c(0, 1)) +
  labs(
    title = "Most observed NIFMs provide strong support for association",
    subtitle = "Strong support is defined here as log10 ESLR of at least 3.",
    x = "NIFMs classified as strong support",
    y = NULL
  ) +
  theme_stoney()

ggsave(file.path(figure_dir, "figure1-yield-comparison.png"), fig1, width = 8.5, height = 5.2, dpi = 320)
ggsave(file.path(figure_dir, "figure2-nifm-occurrence-ci.png"), fig2, width = 8.5, height = 5.2, dpi = 320)
ggsave(file.path(figure_dir, "figure3-strong-support-ci.png"), fig3, width = 8.5, height = 5.2, dpi = 320)

ggsave(file.path(figure_dir, "figure1-yield-comparison.pdf"), fig1, width = 8.5, height = 5.2)
ggsave(file.path(figure_dir, "figure2-nifm-occurrence-ci.pdf"), fig2, width = 8.5, height = 5.2)
ggsave(file.path(figure_dir, "figure3-strong-support-ci.pdf"), fig3, width = 8.5, height = 5.2)

message("Saved manuscript figures to: ", figure_dir)
