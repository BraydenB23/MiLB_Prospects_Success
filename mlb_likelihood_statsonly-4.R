# =============================================================================
# Prospect MLB Likelihood — Stats Only Model
# =============================================================================
# Features:  MiLB stats only — ceiling, floor, average, trajectory
# Target:    2+ MLB seasons with 100+ PA (hitters) or 20+ IP (pitchers)
# Models:    Logistic Regression, Random Forest, XGBoost
#
# Display info (name, org, position, age, fv) comes from grades_clean.csv
# joined onto MiLB features by normalised player name — no ID crosswalk needed.
# =============================================================================

pkgs <- c("dplyr", "tidyr", "readr", "stringr", "purrr",
          "caret", "randomForest", "xgboost", "ggplot2", "scales",
          "yardstick", "tibble", "themis", "factoextra")
invisible(lapply(pkgs, function(p) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}))
cat("Packages loaded.\n\n")


# =============================================================================
# PART 1 — Load data
# =============================================================================

grades <- read_csv("grades_clean.csv",   show_col_types = FALSE)
milb   <- read_csv("milb_stats_all.csv", show_col_types = FALSE)
mlb    <- read_csv("mlb_stats_all.csv",  show_col_types = FALSE)


# =============================================================================
# PART 2 — Target variable: 2+ qualified MLB seasons
# =============================================================================

mlb_hit_qual <- mlb |>
  filter(stat_type == "hitting") |>
  mutate(player_id         = as.integer(player_id),
         plate_appearances = as.integer(plate_appearances)) |>
  filter(plate_appearances >= 100) |>
  group_by(player_id) |>
  summarise(mlb_hit_seasons = n_distinct(season), .groups = "drop")

mlb_pit_qual <- mlb |>
  filter(stat_type == "pitching") |>
  mutate(player_id = as.integer(player_id),
         ip        = as.numeric(innings_pitched)) |>
  filter(ip >= 20) |>
  group_by(player_id) |>
  summarise(mlb_pit_seasons = n_distinct(season), .groups = "drop")

target <- full_join(mlb_hit_qual, mlb_pit_qual, by = "player_id") |>
  mutate(
    mlb_seasons   = pmax(replace_na(mlb_hit_seasons, 0L),
                         replace_na(mlb_pit_seasons, 0L)),
    sustained_mlb = factor(if_else(mlb_seasons >= 2, "yes", "no"),
                           levels = c("no", "yes"))
  ) |>
  select(player_id, mlb_seasons, sustained_mlb)

cat(sprintf("Sustained MLB (yes): %d\n", sum(target$sustained_mlb == "yes")))
cat(sprintf("Did not sustain (no): %d\n\n", sum(target$sustained_mlb == "no")))


# =============================================================================
# PART 3 — Build MiLB features: ceiling, floor, average, trajectory
# =============================================================================

# --- 3a. Hitting --------------------------------------------------------------
milb_hit_all <- milb |>
  filter(stat_type == "hitting") |>
  mutate(
    player_id         = as.integer(player_id),
    season            = as.integer(season),
    plate_appearances = as.integer(plate_appearances),
    base_on_balls     = as.numeric(base_on_balls),
    strike_outs       = as.numeric(strike_outs),
    avg    = as.numeric(avg), obp = as.numeric(obp),
    slg    = as.numeric(slg), ops = as.numeric(ops),
    bb_pct = base_on_balls / plate_appearances,
    k_pct  = strike_outs  / plate_appearances
  ) |>
  filter(plate_appearances >= 100)

hit_ceiling <- milb_hit_all |>
  group_by(player_id) |>
  dplyr::slice_max(ops, n = 1, with_ties = FALSE) |>
  ungroup() |>
  transmute(player_id,
    ceil_avg = avg, ceil_obp = obp, ceil_slg = slg, ceil_ops = ops,
    ceil_bb_pct = bb_pct, ceil_k_pct = k_pct)

hit_floor <- milb_hit_all |>
  group_by(player_id) |>
  dplyr::slice_min(ops, n = 1, with_ties = FALSE) |>
  ungroup() |>
  transmute(player_id,
    floor_avg = avg, floor_obp = obp, floor_slg = slg, floor_ops = ops,
    floor_bb_pct = bb_pct, floor_k_pct = k_pct)

hit_avg <- milb_hit_all |>
  group_by(player_id) |>
  summarise(
    avg_avg    = weighted.mean(avg,    plate_appearances, na.rm = TRUE),
    avg_obp    = weighted.mean(obp,    plate_appearances, na.rm = TRUE),
    avg_slg    = weighted.mean(slg,    plate_appearances, na.rm = TRUE),
    avg_ops    = weighted.mean(ops,    plate_appearances, na.rm = TRUE),
    avg_bb_pct = weighted.mean(bb_pct, plate_appearances, na.rm = TRUE),
    avg_k_pct  = weighted.mean(k_pct,  plate_appearances, na.rm = TRUE),
    total_pa   = as.numeric(sum(plate_appearances, na.rm = TRUE)),
    .groups = "drop")

hit_traj <- milb_hit_all |>
  group_by(player_id) |>
  filter(n_distinct(season) >= 2) |>
  arrange(player_id, season) |>
  mutate(season_idx = as.numeric(season - min(season))) |>
  summarise(
    ops_slope    = tryCatch(coef(lm(ops    ~ season_idx))[2], error = function(e) NA_real_),
    avg_slope    = tryCatch(coef(lm(avg    ~ season_idx))[2], error = function(e) NA_real_),
    obp_slope    = tryCatch(coef(lm(obp    ~ season_idx))[2], error = function(e) NA_real_),
    bb_pct_slope = tryCatch(coef(lm(bb_pct ~ season_idx))[2], error = function(e) NA_real_),
    k_pct_slope  = tryCatch(coef(lm(k_pct  ~ season_idx))[2], error = function(e) NA_real_),
    .groups = "drop")

milb_hit_features <- hit_ceiling |>
  left_join(hit_floor, by = "player_id") |>
  left_join(hit_avg,   by = "player_id") |>
  left_join(hit_traj,  by = "player_id")

cat(sprintf("MiLB hitting features: %d players\n", nrow(milb_hit_features)))

# --- 3b. Pitching -------------------------------------------------------------
milb_pit_all <- milb |>
  filter(stat_type == "pitching") |>
  mutate(
    player_id     = as.integer(player_id),
    season        = as.integer(season),
    ip            = as.numeric(innings_pitched),
    era           = as.numeric(era),
    whip          = as.numeric(whip),
    strike_outs   = as.numeric(strike_outs),
    base_on_balls = as.numeric(base_on_balls),
    k9            = strike_outs / ip * 9,
    bb9           = base_on_balls / ip * 9
  ) |>
  filter(ip >= 20)

pit_ceiling <- milb_pit_all |>
  group_by(player_id) |>
  dplyr::slice_min(era, n = 1, with_ties = FALSE) |>
  ungroup() |>
  transmute(player_id,
    ceil_era = era, ceil_whip = whip, ceil_k9 = k9, ceil_bb9 = bb9)

pit_floor <- milb_pit_all |>
  group_by(player_id) |>
  dplyr::slice_max(era, n = 1, with_ties = FALSE) |>
  ungroup() |>
  transmute(player_id,
    floor_era = era, floor_whip = whip, floor_k9 = k9, floor_bb9 = bb9)

pit_avg <- milb_pit_all |>
  group_by(player_id) |>
  summarise(
    avg_era  = weighted.mean(era,  ip, na.rm = TRUE),
    avg_whip = weighted.mean(whip, ip, na.rm = TRUE),
    avg_k9   = weighted.mean(k9,   ip, na.rm = TRUE),
    avg_bb9  = weighted.mean(bb9,  ip, na.rm = TRUE),
    total_ip = as.numeric(sum(ip,  na.rm = TRUE)),
    .groups = "drop")

pit_traj <- milb_pit_all |>
  group_by(player_id) |>
  filter(n_distinct(season) >= 2) |>
  arrange(player_id, season) |>
  mutate(season_idx = as.numeric(season - min(season))) |>
  summarise(
    era_slope = tryCatch(coef(lm(era ~ season_idx))[2], error = function(e) NA_real_),
    k9_slope  = tryCatch(coef(lm(k9  ~ season_idx))[2], error = function(e) NA_real_),
    bb9_slope = tryCatch(coef(lm(bb9 ~ season_idx))[2], error = function(e) NA_real_),
    .groups = "drop")

milb_pit_features <- pit_ceiling |>
  left_join(pit_floor, by = "player_id") |>
  left_join(pit_avg,   by = "player_id") |>
  left_join(pit_traj,  by = "player_id")

cat(sprintf("MiLB pitching features: %d players\n\n", nrow(milb_pit_features)))


# =============================================================================
# PART 4 — Attach display info via name join
# =============================================================================
# grades_clean  -> player_name, player_type, org, position, age, fv
# milb_stats_all -> player_full_name, player_id
# Join on normalised name. PA/IP filter already applied above so we are only
# matching players with meaningful stats, keeping name collisions minimal.
# =============================================================================

normalise_name <- function(x) {
  x |> str_to_lower() |>
    str_replace_all("[áàäâ]", "a") |> str_replace_all("[éèëê]", "e") |>
    str_replace_all("[íìïî]", "i") |> str_replace_all("[óòöô]", "o") |>
    str_replace_all("[úùüû]", "u") |> str_replace_all("ñ", "n") |>
    str_replace_all("[^a-z ]", "") |> str_squish()
}

# One display-info row per player_name x player_type (most recent grade year)
grades_info <- grades |>
  filter(!is.na(player_name), !is.na(player_type)) |>
  arrange(player_name, player_type, desc(year)) |>
  group_by(player_name, player_type) |>
  dplyr::slice(1) |>
  ungroup() |>
  mutate(name_key = normalise_name(player_name)) |>
  select(name_key, player_name, player_type, position, org, age, fv,
         grade_year = year)

# One player_id per normalised name from MiLB stats
milb_names <- milb |>
  filter(!is.na(player_full_name), !is.na(player_id)) |>
  mutate(player_id = as.integer(player_id),
         name_key  = normalise_name(player_full_name)) |>
  distinct(player_id, name_key)

# grades_info joined to player_id
grades_with_id <- grades_info |>
  inner_join(milb_names, by = "name_key") |>
  select(-name_key)

cat(sprintf("Grades matched to MiLB stats by name: %d players\n\n",
            nrow(grades_with_id)))

graded_ids <- unique(grades_with_id$player_id)

# Hitter dataset
hitter_data <- milb_hit_features |>
  mutate(is_graded = player_id %in% graded_ids) |>
  filter(is_graded | total_pa >= 200) |>
  left_join(target, by = "player_id") |>
  mutate(sustained_mlb = replace_na(sustained_mlb,
                                    factor("no", levels = c("no", "yes")))) |>
  left_join(grades_with_id |> filter(player_type == "hitter"),
            by = "player_id")

# Pitcher dataset
pitcher_data <- milb_pit_features |>
  mutate(is_graded = player_id %in% graded_ids) |>
  filter(is_graded | total_ip >= 40) |>
  left_join(target, by = "player_id") |>
  mutate(sustained_mlb = replace_na(sustained_mlb,
                                    factor("no", levels = c("no", "yes")))) |>
  left_join(grades_with_id |> filter(player_type == "pitcher"),
            by = "player_id")

cat(sprintf("Hitter dataset:  %d players | yes: %d | no: %d\n",
    nrow(hitter_data),
    sum(hitter_data$sustained_mlb == "yes"),
    sum(hitter_data$sustained_mlb == "no")))
cat(sprintf("Pitcher dataset: %d players | yes: %d | no: %d\n\n",
    nrow(pitcher_data),
    sum(pitcher_data$sustained_mlb == "yes"),
    sum(pitcher_data$sustained_mlb == "no")))


# =============================================================================
# PART 5 — Feature definitions
# =============================================================================

hitter_features <- c(
  "ceil_avg", "ceil_obp", "ceil_slg", "ceil_ops", "ceil_bb_pct", "ceil_k_pct",
  "floor_avg", "floor_obp", "floor_slg", "floor_ops", "floor_bb_pct", "floor_k_pct",
  "avg_avg", "avg_obp", "avg_slg", "avg_ops", "avg_bb_pct", "avg_k_pct",
  "ops_slope", "avg_slope", "obp_slope", "bb_pct_slope", "k_pct_slope"
)

pitcher_features <- c(
  "ceil_era", "ceil_whip", "ceil_k9", "ceil_bb9",
  "floor_era", "floor_whip", "floor_k9", "floor_bb9",
  "avg_era", "avg_whip", "avg_k9", "avg_bb9",
  "era_slope", "k9_slope", "bb9_slope"
)


# =============================================================================
# PART 6 — Shared colours & theme
# =============================================================================

col_lr    <- "#5BA4CF"
col_rf    <- "#E8B84B"
col_xgb   <- "#E06C75"
col_bg    <- "#0D1B2A"
col_panel <- "#132236"
col_grid  <- "#1C3048"
col_text  <- "#C8D4DF"

model_colours <- c("Logistic Regression" = col_lr,
                   "Random Forest"       = col_rf,
                   "XGBoost"             = col_xgb)

outcome_colours <- c(
  "True Positive"  = "#4CAF82", "True Negative"  = "#5BA4CF",
  "False Positive" = "#E8B84B", "False Negative" = "#E06C75"
)

base_theme <- theme_minimal(base_size = 10) +
  theme(
    plot.background   = element_rect(fill = col_bg,    colour = NA),
    panel.background  = element_rect(fill = col_panel, colour = NA),
    panel.grid.major  = element_line(colour = col_grid, linewidth = 0.4),
    panel.grid.minor  = element_blank(),
    axis.text         = element_text(colour = col_text),
    axis.title        = element_text(colour = col_text),
    plot.title        = element_text(colour = "white", face = "bold",
                                     size = 12, margin = ggplot2::margin(b = 4)),
    plot.subtitle     = element_text(colour = col_text, size = 9,
                                     margin = ggplot2::margin(b = 10)),
    plot.caption      = element_text(colour = col_grid, size = 7, hjust = 0),
    legend.background = element_rect(fill = col_bg, colour = NA),
    legend.text       = element_text(colour = col_text),
    legend.title      = element_text(colour = col_text),
    plot.margin       = ggplot2::margin(14, 16, 10, 14)
  )


# =============================================================================
# PART 7 — Training & evaluation helpers
# =============================================================================

ctrl <- trainControl(
  method          = "cv",
  number          = 5,
  classProbs      = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final",
  sampling        = "smote"
)

impute_medians <- function(df) {
  df |>
    mutate(across(everything(), as.numeric)) |>
    mutate(across(everything(), function(col) {
      replace_na(col, median(col, na.rm = TRUE))
    }))
}

eval_model <- function(model, name, X_test, y_test) {
  probs <- predict(model, X_test, type = "prob")[["yes"]]
  preds <- predict(model, X_test)
  res   <- tibble(truth = y_test, estimate = preds, prob_yes = probs)
  tibble(
    model       = name,
    auc         = roc_auc(res,  truth, prob_yes,  event_level = "second")$.estimate,
    accuracy    = accuracy(res, truth, estimate)$.estimate,
    sensitivity = sens(res,    truth, estimate,   event_level = "second")$.estimate,
    specificity = spec(res,    truth, estimate,   event_level = "second")$.estimate
  )
}

get_roc <- function(model, name, X_test, y_test) {
  probs <- predict(model, X_test, type = "prob")[["yes"]]
  tibble(truth = y_test, prob = probs, model = name) |>
    arrange(desc(prob)) |>
    mutate(
      tpr = cumsum(truth == "yes") / sum(truth == "yes"),
      fpr = cumsum(truth == "no")  / sum(truth == "no")
    )
}


# =============================================================================
# PART 8 — Plot helpers
# =============================================================================

plot_model_comparison <- function(results_tbl, label) {
  results_tbl |>
    pivot_longer(c(auc, accuracy, sensitivity, specificity),
                 names_to = "metric", values_to = "value") |>
    mutate(metric = str_to_title(metric)) |>
    ggplot(aes(x = metric, y = value, fill = model)) +
    geom_col(position = position_dodge(0.7), width = 0.6) +
    geom_text(aes(label = sprintf("%.2f", value)),
              position = position_dodge(0.7),
              vjust = -0.4, size = 2.8, colour = col_text) +
    scale_fill_manual(values = model_colours, name = NULL) +
    scale_y_continuous(limits = c(0, 1.1), labels = percent_format()) +
    labs(title = sprintf("%s — Model Comparison", label), x = NULL, y = "Score") +
    base_theme + theme(legend.position = "bottom")
}

plot_roc_curves <- function(roc_data, results_tbl, label) {
  auc_labels <- results_tbl |>
    mutate(lbl = sprintf("%s (AUC = %.3f)", model, auc))
  auc_map <- setNames(auc_labels$lbl, auc_labels$model)
  roc_data |>
    mutate(model_label = auc_map[model]) |>
    ggplot(aes(x = fpr, y = tpr, colour = model_label)) +
    geom_abline(slope = 1, intercept = 0, colour = col_grid,
                linetype = "dashed", linewidth = 0.5) +
    geom_line(linewidth = 1.1) +
    scale_colour_manual(
      values = setNames(model_colours, auc_map[names(model_colours)]),
      name = NULL) +
    scale_x_continuous(labels = percent_format(), name = "False Positive Rate") +
    scale_y_continuous(labels = percent_format(), name = "True Positive Rate") +
    labs(title = sprintf("%s — ROC Curves", label)) +
    base_theme + theme(legend.position = "bottom")
}

plot_feature_importance <- function(model, model_name, colour, label) {
  varImp(model)$importance |>
    rownames_to_column("feature") |>
    arrange(desc(Overall)) |>
    head(15) |>
    mutate(feature = reorder(feature, Overall)) |>
    ggplot(aes(x = feature, y = Overall)) +
    geom_col(fill = colour, width = 0.7) +
    geom_text(aes(label = sprintf("%.1f", Overall)),
              hjust = -0.2, size = 3, colour = col_text) +
    coord_flip() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(title = sprintf("%s — Feature Importance (%s)", label, model_name),
         x = NULL, y = "Importance") +
    base_theme
}

plot_confusion_matrices <- function(models_list, X_tests, y_test, label) {
  pmap_dfr(list(models_list, X_tests, names(models_list)),
    function(model, xtest, mname) {
      probs <- predict(model, xtest, type = "prob")[["yes"]]
      preds <- if_else(probs >= 0.5, "yes", "no")
      tibble(actual    = factor(as.character(y_test), levels = c("yes", "no")),
             predicted = factor(preds, levels = c("yes", "no")),
             model     = mname)
    }) |>
    count(model, actual, predicted) |>
    group_by(model) |>
    mutate(
      fill_val      = if_else(actual == predicted, "correct", "incorrect"),
      cell_label    = sprintf("%d\n(%.1f%%)", n, n / sum(n) * 100),
      misclass_rate = sum(n[actual != predicted]) / sum(n) * 100
    ) |>
    ungroup() |>
    mutate(model       = factor(model, levels = names(models_list)),
           model_label = sprintf("%s\n%.1f%% misclass", model, misclass_rate)) |>
    ggplot(aes(x = predicted, y = actual, fill = fill_val)) +
    geom_tile(colour = col_bg, linewidth = 2) +
    geom_text(aes(label = cell_label),
              colour = "white", size = 4, fontface = "bold", lineheight = 1.4) +
    scale_fill_manual(values = c("correct" = "#2E6B4F", "incorrect" = "#7A2A2A"),
                      guide = "none") +
    scale_x_discrete(position = "top",
                     labels = c("yes" = "Predicted: Yes", "no" = "Predicted: No")) +
    scale_y_discrete(labels = c("yes" = "Actual: Yes", "no" = "Actual: No")) +
    facet_wrap(~ model_label, ncol = 3) +
    labs(title = sprintf("%s — Confusion Matrices", label), x = NULL, y = NULL) +
    base_theme +
    theme(panel.grid = element_blank(),
          axis.text  = element_text(colour = col_text, size = 9, face = "bold"),
          axis.ticks = element_blank(),
          strip.text = element_text(colour = col_text, size = 9,
                                    margin = ggplot2::margin(b = 6)))
}

plot_predicted_vs_actual <- function(known_players, label) {
  known_players |>
    mutate(actual_jitter = as.integer(actual == "yes") +
             runif(n(), -0.08, 0.08)) |>
    ggplot(aes(x = prob_mlb, y = actual_jitter, colour = outcome_label)) +
    geom_vline(xintercept = 0.5, colour = col_grid,
               linetype = "dashed", linewidth = 0.6) +
    geom_point(alpha = 0.7, size = 2) +
    scale_colour_manual(values = outcome_colours, name = "Outcome") +
    scale_x_continuous(labels = percent_format(),
                       name = "Predicted probability of sustained MLB career") +
    scale_y_continuous(breaks = c(0, 1),
                       labels = c("Did not sustain", "Sustained (2+ seasons)"),
                       name = NULL) +
    labs(title    = sprintf("%s — Predicted vs Actual", label),
         subtitle = sprintf("%d players  |  Accuracy: %.1f%%",
                            nrow(known_players),
                            mean(known_players$correct) * 100)) +
    base_theme + theme(legend.position = "bottom")
}

plot_calibration <- function(known_players, label) {
  known_players |>
    mutate(prob_bin = cut(prob_mlb, breaks = seq(0, 1, 0.1),
                          include.lowest = TRUE)) |>
    group_by(prob_bin) |>
    summarise(n = n(), actual_rate = mean(actual == "yes"),
              mean_pred = mean(prob_mlb), .groups = "drop") |>
    ggplot(aes(x = mean_pred, y = actual_rate)) +
    geom_abline(slope = 1, intercept = 0, colour = col_grid,
                linetype = "dashed", linewidth = 0.6) +
    geom_line(colour = col_rf, linewidth = 1) +
    geom_point(aes(size = n), colour = col_rf, alpha = 0.85) +
    geom_text(aes(label = sprintf("n=%d", n)),
              vjust = -1.1, size = 2.8, colour = col_text) +
    scale_x_continuous(labels = percent_format(), limits = c(0, 1),
                       name = "Mean predicted probability") +
    scale_y_continuous(labels = percent_format(), limits = c(0, 1),
                       name = "Actual MLB rate") +
    scale_size_continuous(name = "Players", range = c(2, 8)) +
    labs(title    = sprintf("%s — Calibration Plot", label),
         subtitle = "Points on dashed line = perfectly calibrated") +
    base_theme + theme(legend.position = "bottom")
}


# =============================================================================
# PART 9 — Clustering helper
# =============================================================================

run_clustering <- function(data, features, label, k_range = 2:8, seed = 42) {
  cat(sprintf("\n=== %s Clustering ===\n", label))

  df <- data |>
    select(player_id, player_name, grade_year, prob_mlb, all_of(features)) |>
    drop_na()

  cat(sprintf("Players with complete features: %d\n", nrow(df)))
  if (nrow(df) < 20) { cat("Too few — skipping.\n"); return(NULL) }

  X_clust <- df |> select(all_of(features)) |>
    mutate(across(everything(), as.numeric)) |> scale()

  set.seed(seed)
  wss <- map_dbl(k_range, \(k)
    kmeans(X_clust, centers = k, nstart = 25, iter.max = 100)$tot.withinss)

  print(tibble(k = k_range, wss = wss) |>
    ggplot(aes(k, wss)) +
    geom_line(colour = col_rf, linewidth = 1.1) +
    geom_point(colour = col_rf, size = 3, shape = 21,
               fill = col_bg, stroke = 2) +
    scale_x_continuous(breaks = k_range) +
    labs(title = sprintf("%s — Elbow Plot", label), x = "K",
         y = "Total within-cluster SS") +
    base_theme)

  optimal_k <- 3L
  set.seed(seed)
  km <- kmeans(X_clust, centers = optimal_k, nstart = 25, iter.max = 100)
  df <- df |> mutate(cluster = factor(km$cluster))

  profile <- df |>
    group_by(cluster) |>
    summarise(n = n(), avg_mlb_prob = mean(prob_mlb),
              across(all_of(features), \(x) mean(x, na.rm = TRUE)),
              .groups = "drop") |>
    arrange(desc(avg_mlb_prob))

  cat("\nCluster profiles:\n")
  print(profile, n = Inf)

  list(clusters = df, profiles = profile, km = km)
}


# =============================================================================
# PART 10 — Main pipeline
# =============================================================================

run_pipeline <- function(data, features, label) {

  cat(sprintf("\n%s\n  PIPELINE: %s\n%s\n\n",
      strrep("=", 70), label, strrep("=", 70)))

  cat(sprintf("Players: %d  |  yes: %d  |  no: %d\n",
      nrow(data),
      sum(data$sustained_mlb == "yes"),
      sum(data$sustained_mlb == "no")))

  X <- data |> select(all_of(features)) |> impute_medians()
  y <- data$sustained_mlb

  cat(sprintf("Feature matrix: %d x %d\n\n", nrow(X), ncol(X)))

  set.seed(42)
  train_idx <- createDataPartition(y, p = 0.8, list = FALSE)
  X_train <- X[train_idx, ];  y_train <- y[train_idx]
  X_test  <- X[-train_idx, ]; y_test  <- y[-train_idx]

  cat(sprintf("Train: %d  |  Test: %d\n", nrow(X_train), nrow(X_test)))

  cat(sprintf("--- %s: Logistic Regression ---\n", label))
  m_lr <- train(x = X_train, y = y_train, method = "glm", family = "binomial",
                metric = "ROC", trControl = ctrl,
                preProcess = c("center", "scale"))
  cat(sprintf("  CV ROC: %.3f\n", max(m_lr$results$ROC)))

  cat(sprintf("--- %s: Random Forest ---\n", label))
  m_rf <- train(x = X_train, y = y_train, method = "rf",
                metric = "ROC", trControl = ctrl,
                tuneGrid = expand.grid(mtry = c(3, 5, 7)), ntree = 500)
  cat(sprintf("  CV ROC: %.3f\n", max(m_rf$results$ROC)))

  cat(sprintf("--- %s: XGBoost ---\n", label))
  m_xgb <- train(x = X_train, y = y_train, method = "xgbTree",
                 metric = "ROC", trControl = ctrl,
                 tuneGrid = expand.grid(
                   nrounds = c(100, 200), max_depth = c(3, 5),
                   eta = c(0.05, 0.1), gamma = 0,
                   colsample_bytree = 0.8, min_child_weight = 1,
                   subsample = 0.8),
                 verbose = 0)
  cat(sprintf("  CV ROC: %.3f\n", max(m_xgb$results$ROC)))

  models <- list("Logistic Regression" = m_lr,
                 "Random Forest"       = m_rf,
                 "XGBoost"             = m_xgb)

  results_tbl <- bind_rows(
    eval_model(m_lr,  "Logistic Regression", X_test, y_test),
    eval_model(m_rf,  "Random Forest",       X_test, y_test),
    eval_model(m_xgb, "XGBoost",             X_test, y_test)
  )
  cat(sprintf("\n=== %s Test Performance ===\n", label))
  print(results_tbl, n = Inf)

  roc_data <- bind_rows(
    get_roc(m_lr,  "Logistic Regression", X_test, y_test),
    get_roc(m_rf,  "Random Forest",       X_test, y_test),
    get_roc(m_xgb, "XGBoost",             X_test, y_test)
  )
  print(plot_model_comparison(results_tbl, label))
  print(plot_roc_curves(roc_data, results_tbl, label))
  print(plot_feature_importance(m_rf,  "Random Forest", col_rf,  label))
  print(plot_feature_importance(m_xgb, "XGBoost",       col_xgb, label))
  print(plot_confusion_matrices(models, list(X_test, X_test, X_test),
                                y_test, label))

  best_name  <- results_tbl |> dplyr::slice_max(auc, n = 1) |> pull(model)
  best_model <- models[[best_name]]
  cat(sprintf("\nBest model (%s): %s (AUC = %.3f)\n", label, best_name,
      results_tbl |> filter(model == best_name) |> pull(auc)))

  # Score all players in this dataset
  X_full <- data |> select(all_of(features)) |> impute_medians()

  predictions <- data |>
    select(player_id, player_type, is_graded,
           player_name, grade_year, position, org, age, fv) |>
    mutate(
      prob_mlb   = predict(best_model, X_full, type = "prob")[["yes"]],
      predicted  = if_else(prob_mlb >= 0.5, "yes", "no"),
      best_model = best_name
    ) |>
    arrange(desc(prob_mlb))

  # Validation plots for graded players with known outcomes
  known_players <- predictions |>
    left_join(
      target |> transmute(player_id,
                          actual_flag = if_else(sustained_mlb == "yes", 1L, 0L)),
      by = "player_id"
    ) |>
    mutate(
      actual_flag   = replace_na(actual_flag, 0L),
      actual        = factor(if_else(actual_flag >= 1, "yes", "no"),
                             levels = c("no", "yes")),
      correct       = predicted == as.character(actual),
      outcome_label = case_when(
        actual == "yes" & predicted == "yes" ~ "True Positive",
        actual == "no"  & predicted == "no"  ~ "True Negative",
        actual == "no"  & predicted == "yes" ~ "False Positive",
        actual == "yes" & predicted == "no"  ~ "False Negative"
      )
    ) |>
    filter(!is.na(actual), is_graded)

  print(plot_predicted_vs_actual(known_players, label))
  print(plot_calibration(known_players, label))

  cluster_data <- predictions |>
    filter(is_graded) |>
    left_join(X_full |> mutate(player_id = data$player_id), by = "player_id")

  clust_result <- run_clustering(cluster_data, features, label)

  list(models = models, best_name = best_name, results_tbl = results_tbl,
       predictions = predictions, known = known_players,
       clusters = clust_result)
}


# =============================================================================
# PART 11 — Run pipelines
# =============================================================================

hitter_results  <- run_pipeline(hitter_data,  hitter_features,  "Hitters")
pitcher_results <- run_pipeline(pitcher_data, pitcher_features, "Pitchers")


# =============================================================================
# PART 12 — Score graded prospects with no qualifying MiLB stats yet
# =============================================================================
# Prospects on the FanGraphs board who have never hit 100 PA or 20 IP in a
# single affiliated season won't be in milb_hit/pit_features at all.
# Score them with median-imputed features so they appear in the dashboard.
# =============================================================================

score_no_stats_prospects <- function(ptype, features, best_model,
                                     already_scored_ids) {
  # Players already scored have a player_id that appeared in the feature set
  already_names <- grades_with_id |>
    filter(player_id %in% as.integer(already_scored_ids)) |>
    pull(player_name)

  new_df <- grades_info |>
    filter(player_type == ptype,
           !is.na(player_name),
           !player_name %in% already_names)

  if (nrow(new_df) == 0) return(NULL)

  X_new <- matrix(NA_real_, nrow = nrow(new_df), ncol = length(features)) |>
    as.data.frame() |>
    setNames(features) |>
    impute_medians()

  new_df |>
    select(player_name, player_type, position, org, age, fv, grade_year) |>
    mutate(
      player_id  = NA_integer_,
      is_graded  = TRUE,
      prob_mlb   = predict(best_model, X_new, type = "prob")[["yes"]],
      predicted  = if_else(prob_mlb >= 0.5, "yes", "no"),
      best_model = NA_character_
    )
}

no_stats_preds <- bind_rows(
  score_no_stats_prospects("hitter",  hitter_features,
                           hitter_results$models[[hitter_results$best_name]],
                           hitter_data$player_id),
  score_no_stats_prospects("pitcher", pitcher_features,
                           pitcher_results$models[[pitcher_results$best_name]],
                           pitcher_data$player_id)
)
cat(sprintf("\nGraded prospects with no qualifying MiLB stats: %d\n",
            nrow(no_stats_preds)))


# =============================================================================
# PART 13 — Save outputs
# =============================================================================

all_predictions <- bind_rows(
  hitter_results$predictions,
  pitcher_results$predictions,
  no_stats_preds
) |> arrange(desc(prob_mlb))

all_clusters <- bind_rows(
  if (!is.null(hitter_results$clusters))
    hitter_results$clusters$clusters |> mutate(player_type = "hitter"),
  if (!is.null(pitcher_results$clusters))
    pitcher_results$clusters$clusters |> mutate(player_type = "pitcher")
) |> select(any_of(c("player_id", "player_name", "grade_year",
                      "player_type", "cluster", "prob_mlb")))

write_csv(all_predictions, "mlb_likelihood_statsonly.csv")
write_csv(all_clusters,    "prospect_clusters_statsonly.csv")

cat(sprintf("\nSaved: mlb_likelihood_statsonly.csv (%d rows)\n", nrow(all_predictions)))
cat("Saved: prospect_clusters_statsonly.csv\n")
cat("\nDone!\n")
