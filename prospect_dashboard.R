# =============================================================================
# Prospect MLB Likelihood — Shiny Dashboard
# =============================================================================
# Place in same folder as:
#   mlb_likelihood_statsonly.csv
#   prospect_clusters_statsonly.csv
#   grades_clean.csv
#   milb_stats_all.csv
#   mlb_stats_all.csv
# =============================================================================

pkgs <- c("shiny", "dplyr", "tidyr", "readr", "stringr",
          "ggplot2", "scales", "plotly", "DT", "purrr")
invisible(lapply(pkgs, function(p) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}))

# =============================================================================
# DATA
# =============================================================================

predictions <- read_csv("mlb_likelihood_statsonly.csv",    show_col_types = FALSE)
clusters    <- read_csv("prospect_clusters_statsonly.csv", show_col_types = FALSE)
milb        <- read_csv("milb_stats_all.csv",              show_col_types = FALSE)
mlb_stats   <- read_csv("mlb_stats_all.csv",               show_col_types = FALSE)

# player_name, player_type, position, org, age, fv are all written into
# mlb_likelihood_statsonly.csv by the model script — no secondary join needed.
player_data <- predictions |>
  mutate(player_id = as.integer(player_id)) |>
  filter(!is.na(player_name)) |>
  arrange(desc(prob_mlb))

milb_clean <- milb |>
  mutate(player_id = as.integer(player_id),
         season    = as.integer(season))

mlb_clean <- mlb_stats |>
  mutate(player_id = as.integer(player_id),
         season    = as.integer(season))

mlb_players <- mlb_clean |>
  group_by(player_id) |>
  summarise(
    mlb_seasons   = n_distinct(season),
    mlb_debut_year = min(season, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(mlb_seasons >= 2)

# =============================================================================
# HELPERS
# =============================================================================

get_headshot_url <- function(player_id) {
  if (is.na(player_id) || is.null(player_id)) return(NULL)
  paste0(
    "https://img.mlbstatic.com/mlb-photos/image/upload/",
    "d_people:generic:headshot:67:current.png/w_213,q_auto:best/v1/people/",
    player_id,
    "/headshot/67/current"
  )
}

prob_colour <- function(p) {
  if (is.na(p)) return("#888888")
  if (p >= 0.65) return("#4CAF82")
  if (p >= 0.40) return("#E8B84B")
  return("#E06C75")
}

find_comps <- function(pid, ptype, grade_year, n = 5) {
  stat_type_str <- if (ptype == "hitter") "hitting" else "pitching"

  prospect_milb <- milb_clean |>
    filter(player_id == pid, stat_type == stat_type_str)
  if (nrow(prospect_milb) == 0) return(NULL)

  # Only include MLB players who debuted BEFORE this prospect was graded
  # prevents comparing a prospect to players who hadn't yet reached MLB at grade time
  mlb_pid <- mlb_players |>
    filter(player_id != pid,
           !is.na(grade_year) & mlb_debut_year <= grade_year) |>
    pull(player_id)

  mlb_milb <- milb_clean |>
    filter(player_id %in% mlb_pid, stat_type == stat_type_str)
  if (nrow(mlb_milb) == 0) return(NULL)

  if (ptype == "hitter") {
    p_avg <- prospect_milb |>
      summarise(
        ops    = weighted.mean(as.numeric(ops), as.numeric(plate_appearances), na.rm = TRUE),
        obp    = weighted.mean(as.numeric(obp), as.numeric(plate_appearances), na.rm = TRUE),
        slg    = weighted.mean(as.numeric(slg), as.numeric(plate_appearances), na.rm = TRUE),
        bb_pct = mean(as.numeric(base_on_balls) / as.numeric(plate_appearances), na.rm = TRUE),
        k_pct  = mean(as.numeric(strike_outs)   / as.numeric(plate_appearances), na.rm = TRUE)
      )

    mlb_avgs <- mlb_milb |>
      group_by(player_id) |>
      summarise(
        ops    = weighted.mean(as.numeric(ops), as.numeric(plate_appearances), na.rm = TRUE),
        obp    = weighted.mean(as.numeric(obp), as.numeric(plate_appearances), na.rm = TRUE),
        slg    = weighted.mean(as.numeric(slg), as.numeric(plate_appearances), na.rm = TRUE),
        bb_pct = mean(as.numeric(base_on_balls) / as.numeric(plate_appearances), na.rm = TRUE),
        k_pct  = mean(as.numeric(strike_outs)   / as.numeric(plate_appearances), na.rm = TRUE),
        .groups = "drop"
      ) |>
      filter(complete.cases(pick(everything())))

    dists <- mlb_avgs |>
      mutate(dist = sqrt(
        (ops    - p_avg$ops)^2 +
        (obp    - p_avg$obp)^2 +
        (slg    - p_avg$slg)^2 +
        (bb_pct - p_avg$bb_pct)^2 +
        (k_pct  - p_avg$k_pct)^2
      )) |>
      arrange(dist) |>
      dplyr::slice(1:n)

  } else {
    p_avg <- prospect_milb |>
      summarise(
        era  = weighted.mean(as.numeric(era),  as.numeric(innings_pitched), na.rm = TRUE),
        whip = weighted.mean(as.numeric(whip), as.numeric(innings_pitched), na.rm = TRUE),
        k9   = mean(as.numeric(strike_outs)   / as.numeric(innings_pitched) * 9, na.rm = TRUE),
        bb9  = mean(as.numeric(base_on_balls) / as.numeric(innings_pitched) * 9, na.rm = TRUE)
      )

    mlb_avgs <- mlb_milb |>
      group_by(player_id) |>
      summarise(
        era  = weighted.mean(as.numeric(era),  as.numeric(innings_pitched), na.rm = TRUE),
        whip = weighted.mean(as.numeric(whip), as.numeric(innings_pitched), na.rm = TRUE),
        k9   = mean(as.numeric(strike_outs)   / as.numeric(innings_pitched) * 9, na.rm = TRUE),
        bb9  = mean(as.numeric(base_on_balls) / as.numeric(innings_pitched) * 9, na.rm = TRUE),
        .groups = "drop"
      ) |>
      filter(complete.cases(pick(everything())))

    dists <- mlb_avgs |>
      mutate(dist = sqrt(
        (era  - p_avg$era)^2 +
        (whip - p_avg$whip)^2 +
        (k9   - p_avg$k9)^2 +
        (bb9  - p_avg$bb9)^2
      )) |>
      arrange(dist) |>
      dplyr::slice(1:n)
  }

  comp_ids <- dists$player_id

  comp_names <- mlb_clean |>
    filter(player_id %in% comp_ids) |>
    select(player_id, player_name = player_full_name) |>
    distinct() |>
    group_by(player_id) |>
    dplyr::slice(1) |>
    ungroup()

  comp_mlb <- mlb_clean |>
    filter(player_id %in% comp_ids, stat_type == stat_type_str)
  if (nrow(comp_mlb) == 0) return(NULL)

  if (ptype == "hitter") {
    comp_mlb |>
      group_by(player_id) |>
      summarise(
        Seasons = n_distinct(season),
        AVG = round(weighted.mean(as.numeric(avg), as.numeric(plate_appearances), na.rm = TRUE), 3),
        OBP = round(weighted.mean(as.numeric(obp), as.numeric(plate_appearances), na.rm = TRUE), 3),
        SLG = round(weighted.mean(as.numeric(slg), as.numeric(plate_appearances), na.rm = TRUE), 3),
        OPS = round(weighted.mean(as.numeric(ops), as.numeric(plate_appearances), na.rm = TRUE), 3),
        HR  = round(mean(as.numeric(home_runs), na.rm = TRUE), 1),
        .groups = "drop"
      ) |>
      left_join(comp_names, by = "player_id") |>
      left_join(dists |> select(player_id, dist), by = "player_id") |>
      arrange(dist) |>
      select(Player = player_name, Seasons, AVG, OBP, SLG, OPS, HR)
  } else {
    comp_mlb |>
      group_by(player_id) |>
      summarise(
        Seasons = n_distinct(season),
        ERA  = round(weighted.mean(as.numeric(era),  as.numeric(innings_pitched), na.rm = TRUE), 2),
        WHIP = round(weighted.mean(as.numeric(whip), as.numeric(innings_pitched), na.rm = TRUE), 2),
        K9   = round(mean(as.numeric(strike_outs)   / as.numeric(innings_pitched) * 9, na.rm = TRUE), 1),
        BB9  = round(mean(as.numeric(base_on_balls) / as.numeric(innings_pitched) * 9, na.rm = TRUE), 1),
        IP   = round(mean(as.numeric(innings_pitched), na.rm = TRUE), 1),
        .groups = "drop"
      ) |>
      left_join(comp_names, by = "player_id") |>
      left_join(dists |> select(player_id, dist), by = "player_id") |>
      arrange(dist) |>
      select(Player = player_name, Seasons, ERA, WHIP, K9, BB9, IP)
  }
}

predict_mlb_stats <- function(comp_tbl, ptype) {
  if (is.null(comp_tbl) || nrow(comp_tbl) == 0) return(NULL)
  if (ptype == "hitter") {
    tibble(
      Stat = c("AVG", "OBP", "SLG", "OPS", "HR/season"),
      Projected = c(
        sprintf(".%03d", round(mean(comp_tbl$AVG, na.rm = TRUE) * 1000)),
        sprintf(".%03d", round(mean(comp_tbl$OBP, na.rm = TRUE) * 1000)),
        sprintf(".%03d", round(mean(comp_tbl$SLG, na.rm = TRUE) * 1000)),
        sprintf(".%03d", round(mean(comp_tbl$OPS, na.rm = TRUE) * 1000)),
        sprintf("%.1f",  mean(comp_tbl$HR,  na.rm = TRUE))
      ),
      Range = c(
        sprintf(".%03d - .%03d", round(min(comp_tbl$AVG)*1000), round(max(comp_tbl$AVG)*1000)),
        sprintf(".%03d - .%03d", round(min(comp_tbl$OBP)*1000), round(max(comp_tbl$OBP)*1000)),
        sprintf(".%03d - .%03d", round(min(comp_tbl$SLG)*1000), round(max(comp_tbl$SLG)*1000)),
        sprintf(".%03d - .%03d", round(min(comp_tbl$OPS)*1000), round(max(comp_tbl$OPS)*1000)),
        sprintf("%.1f - %.1f",   min(comp_tbl$HR),              max(comp_tbl$HR))
      )
    )
  } else {
    tibble(
      Stat = c("ERA", "WHIP", "K/9", "BB/9", "IP/season"),
      Projected = c(
        sprintf("%.2f", mean(comp_tbl$ERA,  na.rm = TRUE)),
        sprintf("%.2f", mean(comp_tbl$WHIP, na.rm = TRUE)),
        sprintf("%.1f", mean(comp_tbl$K9,   na.rm = TRUE)),
        sprintf("%.1f", mean(comp_tbl$BB9,  na.rm = TRUE)),
        sprintf("%.1f", mean(comp_tbl$IP,   na.rm = TRUE))
      ),
      Range = c(
        sprintf("%.2f - %.2f", min(comp_tbl$ERA),  max(comp_tbl$ERA)),
        sprintf("%.2f - %.2f", min(comp_tbl$WHIP), max(comp_tbl$WHIP)),
        sprintf("%.1f - %.1f", min(comp_tbl$K9),   max(comp_tbl$K9)),
        sprintf("%.1f - %.1f", min(comp_tbl$BB9),  max(comp_tbl$BB9)),
        sprintf("%.1f - %.1f", min(comp_tbl$IP),   max(comp_tbl$IP))
      )
    )
  }
}

base_theme <- theme_minimal(base_size = 11) +
  theme(
    plot.background  = element_rect(fill = "#0D1B2A", colour = NA),
    panel.background = element_rect(fill = "#132236", colour = NA),
    panel.grid.major = element_line(colour = "#1C3048", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    axis.text        = element_text(colour = "#C8D4DF"),
    axis.title       = element_text(colour = "#C8D4DF"),
    plot.title       = element_text(colour = "white", face = "bold", size = 13),
    legend.background = element_rect(fill = "#0D1B2A", colour = NA),
    legend.text      = element_text(colour = "#C8D4DF"),
    plot.margin      = ggplot2::margin(t=12, r=14, b=10, l=12)
  )

# =============================================================================
# CSS — defined as plain character, no sprintf
# =============================================================================

app_css <- paste(
  "body { background-color:#0D1B2A; color:#C8D4DF; font-family:'Segoe UI',Arial,sans-serif; }",
  ".top-bar { background:#132236; border-radius:10px; padding:14px 20px; margin-bottom:16px; border:1px solid #1C3048; }",
  ".top-bar h2 { color:white; margin:0; font-weight:bold; }",
  ".prob-box { background:#132236; border-radius:10px; padding:24px 12px; border:1px solid #1C3048; text-align:center; }",
  ".prob-label { font-size:11px; color:#C8D4DF; text-transform:uppercase; letter-spacing:1px; margin-bottom:8px; }",
  ".prob-circle { width:130px; height:130px; border-radius:50%; background:#0D1B2A; display:flex; flex-direction:column; align-items:center; justify-content:center; margin:0 auto; }",
  ".prob-value { font-size:34px; font-weight:bold; line-height:1; }",
  ".prob-sub { font-size:10px; color:#C8D4DF; margin-top:4px; }",
  ".badge-yes { background:#4CAF82; color:white; border-radius:20px; padding:5px 14px; font-size:11px; font-weight:bold; display:inline-block; margin-top:10px; }",
  ".badge-no { background:#E06C75; color:white; border-radius:20px; padding:5px 14px; font-size:11px; font-weight:bold; display:inline-block; margin-top:10px; }",
  ".player-card { background:#132236; border-radius:10px; padding:18px; border:1px solid #1C3048; height:100%; }",
  ".headshot { width:96px; height:96px; border-radius:50%; object-fit:cover; border:3px solid #E8B84B; display:block; margin:0 auto 10px; }",
  ".player-name { font-size:20px; font-weight:bold; color:white; text-align:center; margin-bottom:3px; }",
  ".player-sub { font-size:12px; color:#C8D4DF; text-align:center; margin-bottom:14px; }",
  ".info-grid { display:grid; grid-template-columns:1fr 1fr; gap:8px; }",
  ".info-item { background:#0D1B2A; border-radius:6px; padding:8px 10px; }",
  ".info-lbl { font-size:10px; color:#C8D4DF; text-transform:uppercase; }",
  ".info-val { font-size:14px; font-weight:bold; color:white; }",
  ".fv-badge { background:#E8B84B; color:#0D1B2A; border-radius:20px; padding:3px 14px; font-size:13px; font-weight:bold; display:inline-block; margin:6px auto; }",
  ".section-card { background:#132236; border-radius:10px; padding:18px; margin-bottom:16px; border:1px solid #1C3048; }",
  ".nav-tabs { margin-bottom:-1px !important; position:relative; z-index:2; border-bottom:none; }",
  ".tab-content { border:1px solid #1C3048; border-radius:0 0 8px 8px; padding:16px; background:#132236; }",
  ".section-title { font-size:14px; font-weight:bold; color:white; margin-bottom:12px; padding-bottom:8px; border-bottom:1px solid #1C3048; }",
  ".nav-tabs > li > a { color:#C8D4DF !important; background:#0D1B2A !important; border-color:#1C3048 !important; border-bottom-color:#132236 !important; margin-bottom:-1px; }",
  ".nav-tabs > li.active > a { background:#132236 !important; color:white !important; border-bottom-color:#132236 !important; }",
  ".selectize-input { background:#132236 !important; color:#C8D4DF !important; border-color:#1C3048 !important; }",
  ".selectize-dropdown { background:#132236 !important; color:#C8D4DF !important; }",
  ".selectize-dropdown .option:hover { background:#1C3048 !important; }",
  "table.dataTable thead th { background:#0D1B2A !important; color:#C8D4DF !important; border-color:#1C3048 !important; }",
  "table.dataTable tbody td { background:#132236 !important; color:#C8D4DF !important; border-color:#1C3048 !important; }",
  "table.dataTable tbody tr:hover td { background:#1C3048 !important; }",
  ".dataTables_wrapper { color:#C8D4DF; }",
  ".search-suggestions { position:absolute; top:100%; left:0; right:0; background:#132236; border:1px solid #1C3048; border-radius:0 0 6px 6px; z-index:1000; max-height:220px; overflow-y:auto; }",
  ".search-suggestion { padding:8px 12px; cursor:pointer; color:#C8D4DF; font-size:13px; }",
  ".search-suggestion:hover { background:#1C3048; color:white; }",
  sep = "\n"
)

# =============================================================================
# UI
# =============================================================================

ui <- fluidPage(
  tags$head(tags$style(HTML(app_css))),
  title = "Prospect MLB Dashboard",

  div(style = "padding:20px;",

    # Title bar + search
    div(class = "top-bar",
      fluidRow(
        column(8, tags$h2("Prospect MLB Dashboard")),
        column(4,
          tags$div(style = "position:relative;",
            tags$input(
              id = "player_search",
              type = "text",
              class = "form-control",
              placeholder = "Search for a player...",
              style = "background:#132236; color:#C8D4DF; border:1px solid #1C3048; border-radius:6px; padding:8px 12px;"
            ),
            uiOutput("search_suggestions")
          )
        )
      )
    ),

    # Top section
    fluidRow(

      column(2,
        div(class = "prob-box",
          div(class = "prob-label", "MLB Career Likelihood"),
          div(class = "prob-circle", uiOutput("prob_value_ui")),
          uiOutput("prediction_badge_ui")
        )
      ),

      column(10,
        div(class = "player-card",
          fluidRow(
            column(3,
              uiOutput("headshot_ui"),
              div(style = "text-align:center;", uiOutput("fv_ui"))
            ),
            column(9,
              div(class = "player-name", textOutput("player_name_txt")),
              div(class = "player-sub",  textOutput("player_sub_txt")),
              div(class = "info-grid",
                div(class = "info-item",
                  div(class = "info-lbl", "Position"),
                  div(class = "info-val", textOutput("info_pos"))
                ),
                div(class = "info-item",
                  div(class = "info-lbl", "Organization"),
                  div(class = "info-val", textOutput("info_org"))
                ),
                div(class = "info-item",
                  div(class = "info-lbl", "Age"),
                  div(class = "info-val", textOutput("info_age"))
                ),
                div(class = "info-item",
                  div(class = "info-lbl", "Player Type"),
                  div(class = "info-val", textOutput("info_type"))
                ),
                div(class = "info-item",
                  div(class = "info-lbl", "Grade Year"),
                  div(class = "info-val", textOutput("info_year"))
                ),
                div(class = "info-item",
                  div(class = "info-lbl", "Archetype"),
                  div(class = "info-val", textOutput("info_cluster"))
                )
              )
            )
          )
        )
      )
    ),

    br(),

    # Bottom tabs
    div(class = "section-card",
      tabsetPanel(

        tabPanel("MiLB Stats",
          br(),
          DTOutput("milb_table")
        ),

        tabPanel("Trends",
          br(),
          fluidRow(
            column(6, plotlyOutput("trend1", height = "300px")),
            column(6, plotlyOutput("trend2", height = "300px"))
          ),
          br(),
          fluidRow(
            column(6, plotlyOutput("trend3", height = "300px")),
            column(6, plotlyOutput("trend4", height = "300px"))
          )
        ),

        tabPanel("MLB Projection",
          br(),
          fluidRow(
            column(6,
              div(class = "section-title", "Projected MLB Stats"),
              DTOutput("proj_table")
            ),
            column(6,
              div(class = "section-title", "Comparable MLB Players"),
              DTOutput("comps_table")
            )
          )
        )

      )
    )
  )
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {

  all_names <- sort(unique(player_data$player_name[!is.na(player_data$player_name)]))

  # Track selected player name separately from raw search text
  selected_name <- reactiveVal(player_data$player_name[1])

  observeEvent(input$player_search, {
    txt <- input$player_search
    if (!is.null(txt) && txt %in% all_names) {
      selected_name(txt)
    }
  }, ignoreInit = FALSE)

  # Suggestions dropdown
  output$search_suggestions <- renderUI({
    txt <- input$player_search
    if (is.null(txt) || nchar(trimws(txt)) < 1) return(NULL)
    matches <- all_names[str_detect(str_to_lower(all_names), str_to_lower(trimws(txt)))]
    if (length(matches) == 0 || (length(matches) == 1 && matches == txt)) return(NULL)
    matches <- head(matches, 8)
    div(class = "search-suggestions",
      lapply(matches, function(nm) {
        div(class = "search-suggestion",
          onclick = paste0("Shiny.setInputValue('player_search', '", nm, "', {priority: 'event'}); document.getElementById('player_search').value = '", nm, "'; document.querySelector('.search-suggestions').style.display='none';"),
          nm
        )
      })
    )
  })

  sel <- reactive({
    req(selected_name())
    player_data |> filter(player_name == selected_name()) |> dplyr::slice(1)
  })

  player_milb <- reactive({
    req(sel())
    ptype <- sel()$player_type
    stype <- if (!is.na(ptype) && ptype == "hitter") "hitting" else "pitching"
    milb_clean |>
      filter(player_id == sel()$player_id, stat_type == stype) |>
      arrange(season)
  })

  output$prob_value_ui <- renderUI({
    p   <- sel()$prob_mlb
    if (is.na(p)) p <- 0
    col <- prob_colour(p)
    tagList(
      div(class = "prob-value", style = paste0("color:", col, ";"),
        paste0(round(p * 100), "%")),
      div(class = "prob-sub", "chance of sustaining MLB")
    )
  })

  output$prediction_badge_ui <- renderUI({
    pred <- sel()$predicted
    if (is.na(pred)) return(NULL)
    cls <- if (pred == "yes") "badge-yes" else "badge-no"
    lbl <- if (pred == "yes") "Projects to MLB" else "Unlikely to sustain"
    div(class = cls, lbl)
  })

  output$headshot_ui <- renderUI({
    pid     <- sel()$player_id
    url     <- get_headshot_url(pid)
    generic <- "https://img.mlbstatic.com/mlb-photos/image/upload/d_people:generic:headshot:67:current.png/v1/people/generic/headshot/67/current"
    tags$img(src = url, class = "headshot",
      onerror = paste0("this.src='", generic, "'"))
  })

  output$fv_ui <- renderUI({
    fv <- sel()$fv
    if (is.na(fv)) return(NULL)
    div(class = "fv-badge", paste0("FV: ", fv))
  })

  output$player_name_txt <- renderText({
    coalesce(sel()$player_name, "Unknown")
  })

  output$player_sub_txt <- renderText({
    ptype <- str_to_title(coalesce(sel()$player_type, ""))
    org   <- coalesce(sel()$org, "")
    if (nchar(org) > 0) paste(ptype, "\u2022", org) else ptype
  })

  output$info_pos  <- renderText({ coalesce(sel()$position, "\u2014") })
  output$info_org  <- renderText({ coalesce(sel()$org, "\u2014") })
  output$info_age  <- renderText({
    age <- sel()$age
    if (is.na(age)) "\u2014" else as.character(round(age, 1))
  })
  output$info_type <- renderText({ str_to_title(coalesce(sel()$player_type, "\u2014")) })
  output$info_year <- renderText({
    yr <- coalesce(sel()$grade_year, sel()$year)
    if (is.na(yr)) "\u2014" else as.character(yr)
  })
  # Archetype labels based on cluster number
  archetype_labels <- c(
    "1" = "Elite Prospect",
    "2" = "Solid Contributor",
    "3" = "Fringe MLB",
    "4" = "Developmental"
  )

  output$info_cluster <- renderText({
    pid <- sel()$player_id
    cl  <- clusters |> filter(player_id == pid) |> pull(cluster)
    if (length(cl) == 0 || is.na(cl[1])) "\u2014" else {
      lbl <- archetype_labels[as.character(cl[1])]
      if (is.na(lbl)) paste("Cluster", cl[1]) else lbl
    }
  })

  output$milb_table <- renderDT({
    df    <- player_milb()
    ptype <- sel()$player_type
    if (nrow(df) == 0) {
      return(datatable(tibble(Message = "No MiLB stats found."), rownames = FALSE))
    }
    if (!is.na(ptype) && ptype == "hitter") {
      out <- df |>
        transmute(
          Season = season,
          Level  = level,
          PA     = as.integer(plate_appearances),
          AVG    = sprintf("%.3f", as.numeric(avg)),
          OBP    = sprintf("%.3f", as.numeric(obp)),
          SLG    = sprintf("%.3f", as.numeric(slg)),
          OPS    = sprintf("%.3f", as.numeric(ops)),
          HR     = as.integer(home_runs),
          BB     = as.integer(base_on_balls),
          K      = as.integer(strike_outs)
        )
    } else {
      out <- df |>
        transmute(
          Season = season,
          Level  = level,
          IP     = as.numeric(innings_pitched),
          ERA    = sprintf("%.2f", as.numeric(era)),
          WHIP   = sprintf("%.2f", as.numeric(whip)),
          K      = as.integer(strike_outs),
          BB     = as.integer(base_on_balls),
          W      = as.integer(wins),
          L      = as.integer(losses)
        )
    }
    datatable(out, rownames = FALSE, class = "compact stripe",
      options = list(pageLength = 15, dom = "t", ordering = FALSE))
  })

  make_trend <- function(df, col, label, colour) {
    df2 <- df |> mutate(y = as.numeric(.data[[col]])) |> filter(!is.na(y))
    if (nrow(df2) == 0) return(plotly_empty())
    p <- ggplot(df2, aes(x = season, y = y)) +
      geom_line(colour = colour, linewidth = 1.2) +
      geom_point(colour = colour, size = 3) +
      labs(title = label, x = "Season", y = label) +
      base_theme
    ggplotly(p) |>
      layout(paper_bgcolor = "#0D1B2A", plot_bgcolor = "#132236",
             font = list(color = "#C8D4DF"))
  }

  output$trend1 <- renderPlotly({
    df    <- player_milb()
    ptype <- sel()$player_type
    if (nrow(df) == 0) return(plotly_empty())
    if (!is.na(ptype) && ptype == "hitter") make_trend(df, "ops",  "OPS",  "#5BA4CF")
    else                                    make_trend(df, "era",  "ERA",  "#E06C75")
  })

  output$trend2 <- renderPlotly({
    df    <- player_milb()
    ptype <- sel()$player_type
    if (nrow(df) == 0) return(plotly_empty())
    if (!is.na(ptype) && ptype == "hitter") make_trend(df, "avg",  "AVG",  "#4CAF82")
    else                                    make_trend(df, "whip", "WHIP", "#E8B84B")
  })

  output$trend3 <- renderPlotly({
    df    <- player_milb()
    ptype <- sel()$player_type
    if (nrow(df) == 0) return(plotly_empty())
    if (!is.na(ptype) && ptype == "hitter") {
      make_trend(df, "obp", "OBP", "#E8B84B")
    } else {
      df2 <- df |> mutate(k9 = as.numeric(strike_outs) / as.numeric(innings_pitched) * 9)
      make_trend(df2, "k9", "K/9", "#4CAF82")
    }
  })

  output$trend4 <- renderPlotly({
    df    <- player_milb()
    ptype <- sel()$player_type
    if (nrow(df) == 0) return(plotly_empty())
    if (!is.na(ptype) && ptype == "hitter") {
      make_trend(df, "slg", "SLG", "#E06C75")
    } else {
      df2 <- df |> mutate(bb9 = as.numeric(base_on_balls) / as.numeric(innings_pitched) * 9)
      make_trend(df2, "bb9", "BB/9", "#5BA4CF")
    }
  })

  comps <- reactive({
    req(sel())
    find_comps(sel()$player_id, sel()$player_type, coalesce(sel()$grade_year, sel()$year), n = 5)
  })

  output$comps_table <- renderDT({
    df <- comps()
    if (is.null(df) || nrow(df) == 0)
      return(datatable(tibble(Message = "No comps found."), rownames = FALSE))
    datatable(df, rownames = FALSE, class = "compact stripe",
      options = list(pageLength = 5, dom = "t", ordering = FALSE))
  })

  output$proj_table <- renderDT({
    df <- predict_mlb_stats(comps(), sel()$player_type)
    if (is.null(df))
      return(datatable(tibble(Message = "Unable to project."), rownames = FALSE))
    datatable(df, rownames = FALSE, class = "compact stripe",
      options = list(pageLength = 10, dom = "t", ordering = FALSE))
  })

}

shinyApp(ui = ui, server = server)
