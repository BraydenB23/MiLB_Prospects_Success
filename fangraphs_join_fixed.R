# =============================================================================
# Prospect Grades + MiLB / MLB Stats Pipeline
# =============================================================================
# Inputs: FanGraphs The Board CSVs (place in same folder as this script,
#         or update fg_folder below)
#
# Outputs:
#   grades_clean.csv           all grade rows, all years, standardised columns
#   milb_stats_all.csv         MiLB hitting + pitching, all levels, 2017-2025
#   mlb_stats_all.csv          MLB hitting + pitching, 2017-2025
#   prospects_milb_only.csv    grades + MiLB stats for players with no MLB data
#   prospects_mlb_and_milb.csv grades + both MLB and MiLB stats for players
#                              who reached the majors
# =============================================================================

# --- 0. Packages -------------------------------------------------------------
pkgs <- c("baseballr", "dplyr", "tidyr", "stringr", "purrr", "readr", "janitor")
invisible(lapply(pkgs, function(p) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}))
cat("Packages loaded.\n\n")


# =============================================================================
# PART 1 — Hardcoded file mapping
# =============================================================================
# Filename sequence confirmed from uploaded files + user description.
# Files 13-18 were not available (missing from FanGraphs export history).
#
# Column formats:
#   Format A  2021-2026 hitter:   Hit, Pitch Sel, Bat Ctrl, [Con Style,]
#                                  Game Pwr, Raw Pwr, Spd, Fld, [Versa,]
#                                  [Avg EV,] [Hard Hit%,] [Max EV,] FV
#   Format B  2021-2026 pitcher:  [TJ Date,] [FB Type,] FB, SL, CB, CH, CMD,
#                                  [RPM FB,] [RPM Break,] [Sits,] [Tops,] FV
#   Format C  2019-2020 hitter:   Hit, Game Pwr, Raw Pwr, Spd, Fld, Arm, FV
#   Format D  2019-2020 pitcher:  [TJ Date,] FB, SL, CB, CH, CMD,
#                                  RPM FB, RPM Break, Sits, Tops, FV
#   Format E  2017-2018 combined: Hit, Game Pwr, Raw Pwr, Spd, Fld, Arm,
#                                  FB, SL, CB, CH, CMD  (no FV column)
# =============================================================================

fg_folder <- getwd()   # <- update if CSVs are in a subfolder

file_map <- tribble(
  ~filename,                        ~year, ~report_type, ~player_type,
  "fangraphs-the-board.csv",        2026,  "report",     "hitter",
  "fangraphs-the-board-2.csv",      2026,  "report",     "pitcher",
  "fangraphs-the-board-3.csv",      2025,  "updated",    "hitter",
  "fangraphs-the-board-4.csv",      2025,  "updated",    "pitcher",
  "fangraphs-the-board-5.csv",      2025,  "report",     "hitter",
  "fangraphs-the-board-6.csv",      2025,  "report",     "pitcher",
  "fangraphs-the-board-7.csv",      2024,  "updated",    "hitter",
  "fangraphs-the-board-8.csv",      2024,  "updated",    "pitcher",
  "fangraphs-the-board-9.csv",      2024,  "report",     "hitter",
  "fangraphs-the-board-10.csv",     2024,  "report",     "pitcher",
  "fangraphs-the-board-11.csv",     2023,  "updated",    "hitter",
  "fangraphs-the-board-12.csv",     2023,  "updated",    "pitcher",
  "fangraphs-the-board-19.csv",     2023,  "report",     "hitter",
  "fangraphs-the-board-20.csv",     2023,  "report",     "pitcher",
  "fangraphs-the-board-21.csv",     2022,  "updated",    "hitter",
  "fangraphs-the-board-22.csv",     2022,  "updated",    "pitcher",
  "fangraphs-the-board-23.csv",     2022,  "report",     "hitter",
  "fangraphs-the-board-24.csv",     2022,  "report",     "pitcher",
  "fangraphs-the-board-25.csv",     2021,  "updated",    "hitter",
  "fangraphs-the-board-26.csv",     2021,  "updated",    "pitcher",
  "fangraphs-the-board-27.csv",     2021,  "report",     "hitter",
  "fangraphs-the-board-28.csv",     2021,  "report",     "pitcher",
  "fangraphs-the-board-29.csv",     2020,  "updated",    "hitter",
  "fangraphs-the-board-30.csv",     2020,  "updated",    "pitcher",
  "fangraphs-the-board-31.csv",     2020,  "report",     "hitter",
  "fangraphs-the-board-32.csv",     2020,  "report",     "pitcher",
  "fangraphs-the-board-33.csv",     2019,  "updated",    "hitter",
  "fangraphs-the-board-34.csv",     2019,  "updated",    "pitcher",
  "fangraphs-the-board-35.csv",     2019,  "report",     "hitter",
  "fangraphs-the-board-36.csv",     2019,  "report",     "pitcher",
  "fangraphs-the-board-37.csv",     2018,  "updated",    "combined",
  "fangraphs-the-board-38.csv",     2018,  "report",     "combined",
  "fangraphs-the-board-39.csv",     2017,  "report",     "combined"
)


# =============================================================================
# PART 2 — Load & standardise grade files
# =============================================================================

# Parse "45 / 60" -> pv=45, fv=60  |  "60" -> pv=60, fv=60  |  "" / NA -> NA
parse_grade_col <- function(x) {
  x <- as.character(x)
  x[x == "" | x == "NA"] <- NA_character_
  has_slash <- !is.na(x) & str_detect(x, "/")
  pv <- case_when(
    is.na(x)   ~ NA_integer_,
    has_slash  ~ str_extract(x, "^\\s*(\\d+)", group = 1) |> as.integer(),
    TRUE       ~ str_extract(x, "\\d+") |> as.integer()
  )
  fv <- case_when(
    is.na(x)   ~ NA_integer_,
    has_slash  ~ str_extract(x, "/\\s*(\\d+)", group = 1) |> as.integer(),
    TRUE       ~ str_extract(x, "\\d+") |> as.integer()
  )
  list(pv = pv, fv = fv)
}

expand_grade <- function(df, col) {
  if (!col %in% names(df)) {
    df[[paste0(col, "_pv")]] <- NA_integer_
    df[[paste0(col, "_fv")]] <- NA_integer_
    return(df)
  }
  g <- parse_grade_col(df[[col]])
  df[[paste0(col, "_pv")]] <- g$pv
  df[[paste0(col, "_fv")]] <- g$fv
  df[[col]] <- NULL
  df
}

load_fg_file <- function(filename, year, report_type, player_type) {
  path <- file.path(fg_folder, filename)
  if (!file.exists(path)) {
    message("Skipping (not found): ", filename)
    return(NULL)
  }

  df <- read_csv(path, show_col_types = FALSE) |> clean_names()
  df <- rename(df, player_name = name, position = pos)

  # Helper: get column if present, otherwise return a default vector
  gc <- function(col, default = NA) {
    if (col %in% names(df)) df[[col]] else rep(default, nrow(df))
  }

  df <- df |> mutate(
    year        = year,
    report_type = report_type,
    player_type = player_type,
    top_100     = suppressWarnings(as.integer(gc("top_100"))),
    org_rk      = suppressWarnings(as.integer(gc("org_rk"))),
    fv          = suppressWarnings(as.integer(gc("fv"))),
    age         = suppressWarnings(as.numeric(gc("age"))),
    con_style   = as.character(gc("con_style")),
    versa       = as.character(gc("versa")),
    avg_ev      = suppressWarnings(as.numeric(gc("avg_ev"))),
    hard_hit    = suppressWarnings(as.numeric(gc("hard_hit"))),
    max_ev      = suppressWarnings(as.numeric(gc("max_ev"))),
    tj_date     = as.character(gc("tj_date")),
    fb_type     = as.character(gc("fb_type")),
    rpm_fb      = suppressWarnings(as.numeric(gc("rpm_fb"))),
    rpm_break   = suppressWarnings(as.numeric(gc("rpm_break"))),
    sits        = as.character(gc("sits")),
    tops        = as.character(gc("tops")),
    player_id_fg = as.character(gc("player_id"))
  )

  # Expand grade columns to _pv / _fv pairs
  for (col in c("hit", "pitch_sel", "bat_ctrl", "game_pwr", "raw_pwr",
                "spd", "fld", "arm", "fb", "sl", "cb", "ch", "cmd")) {
    df <- expand_grade(df, col)
  }

  df <- df |> mutate(
    fg_id_type  = case_when(
      is.na(player_id_fg)                    ~ NA_character_,
      str_detect(player_id_fg, "^sa")        ~ "milb",
      TRUE                                   ~ "mlb"
    ),
    fg_id_clean = if_else(
      !is.na(player_id_fg),
      str_remove(player_id_fg, "^sa"),
      NA_character_
    )
  )

  df |> select(
    year, report_type, player_type, player_name, position, org,
    age, top_100, org_rk, fv,
    player_id_fg, fg_id_type, fg_id_clean,
    con_style, versa, avg_ev, hard_hit, max_ev,
    tj_date, fb_type, rpm_fb, rpm_break, sits, tops,
    ends_with("_pv"), ends_with("_fv")
  )
}

cat("=== Loading FanGraphs grade files ===\n")
grades_raw <- pmap(file_map, load_fg_file) |> compact() |> bind_rows()

cat(sprintf(
  "\nGrade rows loaded: %d | Years: %s\n\n",
  nrow(grades_raw),
  paste(sort(unique(grades_raw$year)), collapse = ", ")
))


# =============================================================================
# PART 3 — Map FanGraphs IDs -> MLBAM IDs
# =============================================================================

cat("Building FanGraphs -> MLBAM ID crosswalk...\n")
id_map <- tryCatch({
  chadwick_player_lu() |>
    filter(!is.na(key_fangraphs), !is.na(key_mlbam)) |>
    transmute(
      fg_id_clean = as.character(key_fangraphs),
      mlbam_id    = as.integer(key_mlbam)
    )
}, error = function(e) {
  message("Crosswalk unavailable: ", e$message)
  tibble(fg_id_clean = character(), mlbam_id = integer())
})

cat(sprintf("Crosswalk: %d mappings\n\n", nrow(id_map)))
cat(names(grades_raw), sep = "\n")
grades_clean <- grades_raw |>
  left_join(id_map, by = "fg_id_clean")

cat(sprintf(
  "MLBAM ID resolved: %d / %d rows\n\n",
  sum(!is.na(grades_clean$mlbam_id)), nrow(grades_clean)
))


# =============================================================================
# PART 4 — Pull MiLB stats (2017-2025, all affiliated levels)
# =============================================================================

level_labels <- c("11" = "AAA", "12" = "AA", "13" = "High-A", "14" = "Single-A")
milb_levels  <- c(11, 12, 13, 14)
milb_seasons <- c(2017:2019, 2021:2025)   # no MiLB in 2020

fetch_level_stats <- function(sport_id, stat_group, season, page_size = 1000) {
  level_name <- level_labels[as.character(sport_id)]
  all_pages  <- list(); offset <- 0; page <- 1
  repeat {
    Sys.sleep(0.4)
    result <- tryCatch(
      mlb_stats(
        stat_type = "season", stat_group = stat_group,
        season = season, sport_ids = sport_id,
        player_pool = "All", limit = page_size, offset = offset
      ),
      error = function(e) { message("  Error: ", e$message); NULL }
    )
    if (is.null(result) || nrow(result) == 0) break
    result <- mutate(result, level = level_name, sport_id = sport_id,
                     season = season, stat_group = stat_group)
    all_pages[[page]] <- result
    if (nrow(result) < page_size) break
    offset <- offset + page_size; page <- page + 1
  }
  if (length(all_pages) == 0) return(NULL)
  bind_rows(all_pages)
}

cat("=== Pulling MiLB stats (2017-2025) ===\n")
milb_raw <- map(milb_seasons, function(yr) {
  cat(sprintf("  %d\n", yr))
  bind_rows(
    map(milb_levels, \(s) fetch_level_stats(s, "hitting",  yr)),
    map(milb_levels, \(s) fetch_level_stats(s, "pitching", yr))
  )
}) |> bind_rows()
cat(sprintf("MiLB raw rows: %d\n\n", nrow(milb_raw)))


# =============================================================================
# PART 5 — Pull MLB stats (2017-2025)
# =============================================================================

fetch_mlb_stats <- function(stat_group, season, page_size = 1000) {
  all_pages <- list(); offset <- 0; page <- 1
  repeat {
    Sys.sleep(0.4)
    result <- tryCatch(
      mlb_stats(
        stat_type = "season", stat_group = stat_group,
        season = season, sport_ids = 1,
        player_pool = "All", limit = page_size, offset = offset
      ),
      error = function(e) { message("  Error: ", e$message); NULL }
    )
    if (is.null(result) || nrow(result) == 0) break
    result <- mutate(result, level = "MLB", sport_id = 1,
                     season = season, stat_group = stat_group)
    all_pages[[page]] <- result
    if (nrow(result) < page_size) break
    offset <- offset + page_size; page <- page + 1
  }
  if (length(all_pages) == 0) return(NULL)
  bind_rows(all_pages)
}

cat("=== Pulling MLB stats (2017-2025) ===\n")
mlb_raw <- map(2017:2025, function(yr) {
  cat(sprintf("  %d\n", yr))
  bind_rows(fetch_mlb_stats("hitting", yr), fetch_mlb_stats("pitching", yr))
}) |> bind_rows()
cat(sprintf("MLB raw rows: %d\n\n", nrow(mlb_raw)))


# =============================================================================
# PART 6 — Clean stats
# =============================================================================

clean_hitting <- function(df) {
  df |> filter(stat_group == "hitting") |>
    select(any_of(c(
      "player_full_name", "player_id", "team_name", "level", "season",
      "games_played", "plate_appearances", "at_bats", "hits",
      "doubles", "triples", "home_runs", "runs", "rbi",
      "stolen_bases", "caught_stealing", "base_on_balls", "strike_outs",
      "hit_by_pitch", "avg", "obp", "slg", "ops", "babip",
      "total_bases", "sac_flies", "sac_bunts", "ground_into_double_play"
    ))) |>
    mutate(
      across(c(avg, obp, slg, ops, babip), as.numeric),
      plate_appearances = as.integer(plate_appearances),
      iso    = as.numeric(slg) - as.numeric(avg),
      bb_pct = as.numeric(base_on_balls) / plate_appearances,
      k_pct  = as.numeric(strike_outs)   / plate_appearances,
      stat_type = "hitting"
    )
}

clean_pitching <- function(df) {
  df |> filter(stat_group == "pitching") |>
    select(any_of(c(
      "player_full_name", "player_id", "team_name", "level", "season",
      "games_played", "games_started", "innings_pitched",
      "hits", "runs", "earned_runs", "home_runs",
      "base_on_balls", "strike_outs", "hit_batsmen",
      "era", "whip", "wins", "losses", "saves", "holds",
      "batters_faced", "wild_pitches", "complete_games", "shutouts"
    ))) |>
    mutate(
      era  = as.numeric(era),
      whip = as.numeric(whip),
      ip   = as.numeric(innings_pitched),
      k9   = as.numeric(strike_outs)   / ip * 9,
      bb9  = as.numeric(base_on_balls) / ip * 9,
      k_bb = ifelse(as.numeric(base_on_balls) > 0,
                    as.numeric(strike_outs) / as.numeric(base_on_balls), NA_real_),
      hr9  = as.numeric(home_runs) / ip * 9,
      stat_type = "pitching"
    )
}

milb_stats <- bind_rows(clean_hitting(milb_raw), clean_pitching(milb_raw))
mlb_stats  <- bind_rows(clean_hitting(mlb_raw),  clean_pitching(mlb_raw))

cat(sprintf("MiLB stats cleaned: %d rows\n", nrow(milb_stats)))
cat(sprintf("MLB  stats cleaned: %d rows\n\n", nrow(mlb_stats)))


# =============================================================================
# PART 7 — Join grades to stats
# =============================================================================
# Primary:  MLBAM player_id integer join (reliable for MLB-experienced players)
# Fallback: normalised name join for sa-prefix MiLB-only prospects
#
# Grades are joined to ALL stat seasons so you can track development across
# years. `year` = grade snapshot year; `season` = stat year.
# =============================================================================

normalise_name <- function(x) {
  x |> str_to_lower() |>
    str_replace_all("[áàäâ]", "a") |> str_replace_all("[éèëê]", "e") |>
    str_replace_all("[íìïî]", "i") |> str_replace_all("[óòöô]", "o") |>
    str_replace_all("[úùüû]", "u") |> str_replace_all("ñ", "n") |>
    str_replace_all("[^a-z ]", "") |> str_squish()
}

join_grades_to_stats <- function(grades_df, stats_df, label) {
  stats_df <- stats_df |> mutate(player_id = as.integer(player_id))

  # Primary: MLBAM ID
  g_id <- grades_df |> filter(!is.na(mlbam_id))
  joined_id <- inner_join(
    g_id, stats_df,
    by = c("mlbam_id" = "player_id"),
    relationship = "many-to-many"
  ) |> mutate(match_type = "mlbam_id")
  cat(sprintf("  %s ID matched:   %d players\n",
              label, n_distinct(joined_id$player_name)))

  # Fallback: name match for sa-prefix prospects
  g_name <- grades_df |> filter(is.na(mlbam_id)) |>
    mutate(name_key = normalise_name(player_name))
  s_name <- stats_df |> mutate(name_key = normalise_name(player_full_name))
  joined_name <- inner_join(
    g_name, s_name, by = "name_key", relationship = "many-to-many"
  ) |> mutate(match_type = "name") |> select(-name_key)
  cat(sprintf("  %s name matched: %d players\n",
              label, n_distinct(joined_name$player_name)))

  bind_rows(joined_id, joined_name)
}

cat("=== Joining grades to MiLB stats ===\n")
joined_milb <- join_grades_to_stats(grades_clean, milb_stats, "MiLB")

cat("\n=== Joining grades to MLB stats ===\n")
joined_mlb  <- join_grades_to_stats(grades_clean, mlb_stats,  "MLB")

players_with_mlb <- intersect(
  unique(joined_mlb$player_name),
  unique(joined_milb$player_name)
)
cat(sprintf("\nPlayers with both MLB + MiLB data: %d\n\n",
            length(players_with_mlb)))

# Dataset 1: pure MiLB prospects (no MLB appearances in our data)
prospects_milb_only <- joined_milb |>
  filter(!player_name %in% players_with_mlb)

# Dataset 2: players who reached MLB — all their MLB and MiLB seasons combined
prospects_mlb_and_milb <- bind_rows(
  joined_mlb  |> filter(player_name %in% players_with_mlb) |>
    mutate(data_source = "MLB"),
  joined_milb |> filter(player_name %in% players_with_mlb) |>
    mutate(data_source = "MiLB")
) |> arrange(player_name, year, data_source, season)


# =============================================================================
# PART 8 — Save outputs
# =============================================================================

save_csv <- function(df, fname) {
  if (!is.null(df) && nrow(df) > 0) {
    write_csv(df, fname)
    cat(sprintf("  %-45s %d rows\n", fname, nrow(df)))
  } else {
    cat(sprintf("  %-45s (empty)\n", fname))
  }
}

cat("=== Saving outputs ===\n")
save_csv(grades_clean,           "grades_clean.csv")
save_csv(milb_stats,             "milb_stats_all.csv")
save_csv(mlb_stats,              "mlb_stats_all.csv")
save_csv(prospects_milb_only,    "prospects_milb_only.csv")
save_csv(prospects_mlb_and_milb, "prospects_mlb_and_milb.csv")

cat(sprintf("\nAll done. Files saved to: %s\n", getwd()))
