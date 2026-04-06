library(shiny)
library(rhandsontable)
library(dplyr)
library(tibble)
library(readr)
library(bslib)
library(plotly)
library(DT)
library(lubridate)
library(tidyr)

# -------------------------------
# Configuration
# -------------------------------
compliance_cols <- c("Bin", "Segregation Compliance")
weights_cols <- c("Assigned Location", "Categories", "Weights")

category_choices <- c("Biodegradable", "Non-Biodegradable", "Recyclable", "Special Waste")
compliance_choices <- c("Properly Segregated", "Partially Segregated", "Not Segregated")
base_weight_categories <- c("Biodegradable", "Non-Biodegradable", "Recyclable", "Special Waste")

assigned_location_choices <- sort(c(
  "Admin Building",
  "AS Building (left wing)",
  "AS Building (right wing)",
  "AS Extension Building",
  "Dorm (Adlaw)",
  "Dorm (Hangin)",
  "FabLab",
  "Grounds (Admin)",
  "Grounds (AS)",
  "Grounds (Main)",
  "Library",
  "PAH",
  "Science Building",
  "SOM Building",
  "TIC Building",
  "TLRC/BAC/OSA",
  "Undergraduate Building",
  "UP High School"
))

compliance_file <- "segregation_compliance.csv"
weights_file <- "waste_weights.csv"

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# -------------------------------
# Helpers
# -------------------------------
parse_date_safe <- function(x) {
  if (inherits(x, "Date")) return(x)
  
  x <- as.character(x)
  x <- trimws(x)
  x[x == ""] <- NA_character_
  
  out <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
  
  need <- is.na(out) & !is.na(x)
  if (any(need)) {
    parsed <- suppressWarnings(lubridate::parse_date_time(
      x[need],
      orders = c(
        "Y-m-d", "Y/m/d",
        "m/d/Y", "d/m/Y",
        "m-d-Y", "d-m-Y",
        "b d Y", "B d Y",
        "d b Y", "d B Y",
        "Ymd", "mdY", "dmY"
      ),
      quiet = TRUE
    ))
    out[need] <- as.Date(parsed)
  }
  
  out
}

safe_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

empty_compliance_row <- function() {
  tibble(
    Bin = "",
    `Segregation Compliance` = ""
  )
}

blank_compliance_sheet <- function(n = 10) {
  bind_rows(replicate(n, empty_compliance_row(), simplify = FALSE))
}

make_weight_rows <- function(locations) {
  if (length(locations) == 0) {
    return(
      tibble(
        `Assigned Location` = character(),
        Categories = character(),
        Weights = character()
      )
    )
  }
  
  locs <- sort(unique(locations))
  
  bind_rows(lapply(locs, function(loc) {
    tibble(
      `Assigned Location` = loc,
      Categories = base_weight_categories,
      Weights = ""
    )
  })) %>%
    as_tibble()
}

ensure_cols <- function(df, cols) {
  missing_cols <- setdiff(cols, names(df))
  
  if (length(missing_cols) > 0) {
    for (col in missing_cols) {
      df[[col]] <- ""
    }
  }
  
  df <- df[, cols, drop = FALSE]
  df[] <- lapply(df, function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x
  })
  
  as_tibble(df)
}

clean_df <- function(df, cols) {
  ensure_cols(df, cols) %>%
    mutate(across(everything(), ~ trimws(.x))) %>%
    filter(if_any(everything(), ~ .x != ""))
}

safe_read_csv <- function(path) {
  suppressWarnings(
    read_csv(
      path,
      show_col_types = FALSE,
      col_types = cols(.default = col_character())
    )
  )
}

initialize_submission_databases <- function() {
  if (!file.exists(compliance_file)) {
    write_csv(
      tibble(
        submission_id = character(),
        submitted_at = character(),
        name = character(),
        assigned_location = character(),
        date_of_collection = character(),
        time_of_collection = character(),
        Bin = character(),
        `Segregation Compliance` = character()
      ),
      compliance_file
    )
  }
  
  if (!file.exists(weights_file)) {
    write_csv(
      tibble(
        submission_id = character(),
        submitted_at = character(),
        name = character(),
        assigned_location = character(),
        date_of_collection = character(),
        time_of_collection = character(),
        `Assigned Location` = character(),
        Categories = character(),
        Weights = character()
      ),
      weights_file
    )
  }
}

read_submission_data <- function(which = c("compliance", "weights")) {
  which <- match.arg(which)
  initialize_submission_databases()
  
  file <- if (which == "compliance") compliance_file else weights_file
  
  tryCatch(
    safe_read_csv(file),
    error = function(e) tibble()
  )
}

save_submission <- function(name, assigned_location, date_of_collection, time_of_collection, compliance_data, weights_data) {
  initialize_submission_databases()
  
  cleaned_compliance <- clean_df(compliance_data, compliance_cols)
  cleaned_weights <- clean_df(weights_data, weights_cols)
  
  if (nrow(cleaned_compliance) == 0 && nrow(cleaned_weights) == 0) {
    return(FALSE)
  }
  
  submission_id <- paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "-", sample(1000:9999, 1))
  submitted_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  assigned_text <- paste(sort(unique(assigned_location)), collapse = "; ")
  
  if (nrow(cleaned_compliance) > 0) {
    out_c <- cleaned_compliance %>%
      mutate(
        submission_id = submission_id,
        submitted_at = submitted_at,
        name = name,
        assigned_location = assigned_text,
        date_of_collection = as.character(date_of_collection),
        time_of_collection = time_of_collection,
        .before = 1
      )
    write_csv(out_c, compliance_file, append = TRUE)
  }
  
  if (nrow(cleaned_weights) > 0) {
    out_w <- cleaned_weights %>%
      mutate(
        submission_id = submission_id,
        submitted_at = submitted_at,
        name = name,
        assigned_location = assigned_text,
        date_of_collection = as.character(date_of_collection),
        time_of_collection = time_of_collection,
        .before = 1
      )
    write_csv(out_w, weights_file, append = TRUE)
  }
  
  TRUE
}

make_compliance_hot <- function(df, height = 420) {
  hot <- rhandsontable(
    df,
    rowHeaders = TRUE,
    stretchH = "all",
    width = "100%",
    height = height,
    manualColumnResize = TRUE,
    manualRowResize = TRUE,
    contextMenu = TRUE,
    allowInsertRow = TRUE,
    allowRemoveRow = TRUE,
    outsideClickDeselects = FALSE,
    license = "non-commercial-and-evaluation"
  )
  
  hot <- hot_col(hot, "Bin", type = "text")
  hot <- hot_col(
    hot,
    "Segregation Compliance",
    type = "dropdown",
    source = compliance_choices,
    allowInvalid = FALSE,
    strict = TRUE
  )
  
  hot
}

make_weight_hot <- function(df, location_source, height = 420) {
  hot <- rhandsontable(
    df,
    rowHeaders = TRUE,
    stretchH = "all",
    width = "100%",
    height = height,
    manualColumnResize = TRUE,
    manualRowResize = TRUE,
    contextMenu = FALSE,
    outsideClickDeselects = FALSE,
    license = "non-commercial-and-evaluation"
  )
  
  hot <- hot_col(
    hot,
    "Assigned Location",
    type = "dropdown",
    source = sort(unique(location_source)),
    allowInvalid = FALSE,
    strict = TRUE,
    readOnly = TRUE
  )
  
  hot <- hot_col(
    hot,
    "Categories",
    type = "dropdown",
    source = category_choices,
    allowInvalid = FALSE,
    strict = TRUE,
    readOnly = TRUE
  )
  
  hot <- hot_col(
    hot,
    "Weights",
    type = "numeric",
    format = "0,0.00"
  )
  
  hot
}

metric_box <- function(bg, fg, value_output, label_text, subtitle_text) {
  div(
    class = "metric-card loading-target",
    style = paste0("background:", bg, ";"),
    div(class = "help-note", style = paste0("color:", fg, ";"), subtitle_text),
    div(class = "metric-value", style = paste0("color:", fg, ";"), textOutput(value_output, inline = TRUE)),
    div(class = "metric-label", style = paste0("color:", fg, "; margin-top: 6px;"), label_text)
  )
}

build_submission_summary <- function(compliance_df, weights_df) {
  if (nrow(compliance_df) == 0 && nrow(weights_df) == 0) {
    return(tibble(
      Date = character(),
      Time = character(),
      Name = character(),
      `Assigned Location` = character(),
      `Properly Segregated` = numeric(),
      `Partially Segregated` = numeric(),
      `Not Segregated` = numeric(),
      Biodegradable = numeric(),
      `Non-Biodegradable` = numeric(),
      Recyclable = numeric(),
      `Special Waste` = numeric()
    ))
  }
  
  comp_sum <- compliance_df %>%
    mutate(
      `Properly Segregated` = if_else(`Segregation Compliance` == "Properly Segregated", 1, 0),
      `Partially Segregated` = if_else(`Segregation Compliance` == "Partially Segregated", 1, 0),
      `Not Segregated` = if_else(`Segregation Compliance` == "Not Segregated", 1, 0)
    ) %>%
    group_by(submission_id, date_of_collection, time_of_collection, name, assigned_location) %>%
    summarise(
      `Properly Segregated` = sum(`Properly Segregated`, na.rm = TRUE),
      `Partially Segregated` = sum(`Partially Segregated`, na.rm = TRUE),
      `Not Segregated` = sum(`Not Segregated`, na.rm = TRUE),
      .groups = "drop"
    )
  
  weight_sum <- weights_df %>%
    mutate(weight_num = safe_numeric(Weights)) %>%
    group_by(submission_id, date_of_collection, time_of_collection, name, assigned_location, Categories) %>%
    summarise(total_weight = sum(weight_num, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(
      names_from = Categories,
      values_from = total_weight,
      values_fill = 0
    )
  
  full_join(
    comp_sum,
    weight_sum,
    by = c("submission_id", "date_of_collection", "time_of_collection", "name", "assigned_location")
  ) %>%
    mutate(
      Date = coalesce(date_of_collection, ""),
      Time = coalesce(time_of_collection, ""),
      Name = coalesce(name, ""),
      `Assigned Location` = coalesce(assigned_location, ""),
      `Properly Segregated` = coalesce(`Properly Segregated`, 0),
      `Partially Segregated` = coalesce(`Partially Segregated`, 0),
      `Not Segregated` = coalesce(`Not Segregated`, 0),
      Biodegradable = coalesce(Biodegradable, 0),
      `Non-Biodegradable` = coalesce(`Non-Biodegradable`, 0),
      Recyclable = coalesce(Recyclable, 0),
      `Special Waste` = coalesce(`Special Waste`, 0)
    ) %>%
    select(
      Date,
      Time,
      Name,
      `Assigned Location`,
      `Properly Segregated`,
      `Partially Segregated`,
      `Not Segregated`,
      Biodegradable,
      `Non-Biodegradable`,
      Recyclable,
      `Special Waste`
    )
}

# -------------------------------
# UI
# -------------------------------
ui <- page_fluid(
  theme = bs_theme(version = 5, primary = "#1b5e20", bg = "#f1f8f4", fg = "#1b1b1b"),
  tags$head(
    tags$style(HTML("
      body { background-color: #f1f8f4; font-family: Arial, sans-serif; }
      .sheet-wrap { max-width: 1080px; margin: 30px auto; }
      .sheet-title { font-size: 1.8rem; font-weight: 600; margin-bottom: 0.3rem; }
      .sheet-subtitle { color: #4e5d52; margin-bottom: 1.2rem; }
      .section-block { margin-bottom: 18px; }
      .plain-section { background: white; border-radius: 12px; padding: 18px; box-shadow: 0 2px 6px rgba(0,0,0,0.08); }
      .action-btn { border-radius: 8px !important; }
      .submit-wrap { display: flex; justify-content: center; gap: 10px; margin-top: 20px; flex-wrap: wrap; }
      .handsontable th { background: #e8f5e9; font-weight: 600; }
      .help-note { color: #5f6368; font-size: 0.92rem; margin-top: 8px; }
      .metric-card { border-radius: 12px; padding: 16px; box-shadow: 0 2px 6px rgba(0,0,0,0.08); margin-bottom: 14px; min-height: 135px; }
      .metric-label { font-size: 0.95rem; font-weight: 600; }
      .metric-value { font-size: 1.6rem; font-weight: 700; }
      .plot-wrap { background: white; border-radius: 12px; padding: 12px; box-shadow: 0 2px 6px rgba(0,0,0,0.08); margin-bottom: 16px; }

      /* Revised login layout: occupies 25% to 75% of page */
      .access-box {
        width: 75%;
        margin: 10px auto 20px auto;
        text-align: left;
      }
      .login-fields-wrap {
        width: 100%;
        margin-left: auto;
        margin-right: auto;
      }
      .access-box .sheet-title,
      .access-box .sheet-subtitle {
        text-align: left;
        margin-left: 0;
      }

      .wizard-step-title { font-size: 1.2rem; font-weight: 600; margin-bottom: 12px; text-align: left; }
      .admin-sheet-wrap { overflow-x: auto; width: 100%; }
      .step-wrap { width: 40%; min-width: 320px; max-width: 560px; margin-left: auto; margin-right: auto; }
      .step-wide-wrap { max-width: 860px; margin-left: auto; margin-right: auto; }
      .step-wrap .form-group { text-align: left; }
      .step-wrap .control-label { display:block; text-align:left; font-weight:600; }
      .step-wrap .form-control,
      .step-wrap .selectize-control { width: 100%; }
      .step-wrap .wizard-step-title { text-align: left; }
      .login-btn-wrap { display: flex;  justify-content: flex-start;  margin-top: 18px;}
    .next-btn-wrap { display: flex; justify-content: center; margin-top: 18px; }
      .loading-target.recalculating {
        opacity: 0.55;
        position: relative;
      }
      .loading-target.recalculating:after {
        content: '';
        position: absolute;
        top: 50%;
        left: 50%;
        width: 34px;
        height: 34px;
        margin-left: -17px;
        margin-top: -17px;
        border: 4px solid #d9d9d9;
        border-top-color: #1b5e20;
        border-radius: 50%;
        animation: spin 0.8s linear infinite;
        z-index: 10;
        background: transparent;
      }
      @keyframes spin {
        to { transform: rotate(360deg); }
      }
    "))
  ),
  div(
    class = "sheet-wrap",
    tabsetPanel(
      id = "main_tabs",
      type = "tabs",
      
      tabPanel(
        "Waste Audit",
        div(
          class = "plain-section section-block",
          fluidRow(
            column(
              4,
              selectInput(
                "wa_location",
                "Location",
                choices = c("All", assigned_location_choices),
                selected = "All"
              )
            ),
            column(4, uiOutput("wa_start_date_ui")),
            column(4, uiOutput("wa_end_date_ui"))
          )
        ),
        tabsetPanel(
          id = "waste_audit_tabs",
          type = "tabs",
          
          tabPanel(
            "Summary",
            fluidRow(
              column(4, metric_box("#1b5e20", "#ffffff", "sum_avg_bio", "Biodegradable", "Average daily waste generation")),
              column(4, metric_box("#1b5e20", "#ffffff", "sum_avg_nonbio", "Non-Biodegradable", "Average daily waste generation")),
              column(4, metric_box("#1b5e20", "#ffffff", "sum_avg_recy", "Recyclable", "Average daily waste generation"))
            ),
            fluidRow(
              column(4, metric_box("#d4af37", "#000000", "sum_pct_bio", "Biodegradable", "Waste composition")),
              column(4, metric_box("#d4af37", "#000000", "sum_pct_nonbio", "Non-Biodegradable", "Waste composition")),
              column(4, metric_box("#d4af37", "#000000", "sum_pct_recy", "Recyclable", "Waste composition"))
            ),
            fluidRow(
              column(4, metric_box("#7b1e3a", "#ffffff", "sum_pct_proper", "Properly Segregated", "Segregation status distribution")),
              column(4, metric_box("#7b1e3a", "#ffffff", "sum_pct_partial", "Partially Segregated", "Segregation status distribution")),
              column(4, metric_box("#7b1e3a", "#ffffff", "sum_pct_notseg", "Not Segregated", "Segregation status distribution"))
            )
          ),
          
          tabPanel(
            "Waste Generation",
            div(
              class = "plain-section section-block",
              selectInput(
                "wg_category",
                "Waste Category",
                choices = c("All", category_choices),
                selected = "All"
              )
            ),
            div(class = "plot-wrap loading-target", plotlyOutput("waste_generation_plot", height = "420px"))
          ),
          
          tabPanel(
            "Waste Composition",
            div(class = "plot-wrap loading-target", plotlyOutput("waste_composition_plot", height = "420px"))
          ),
          
          tabPanel(
            "Waste Segregation",
            div(class = "plot-wrap loading-target", plotlyOutput("waste_segregation_plot", height = "420px"))
          )
        )
      ),
      
      tabPanel(
        "Staff",
        conditionalPanel(
          condition = "output.staff_logged_in === false",
          div(
            class = "plain-section section-block access-box",
            div(class = "sheet-title", "Staff Access"),
            div(class = "sheet-subtitle", "Log in to submit report."),
            div(
              class = "login-fields-wrap",
              textInput("staff_username", "Username"),
              passwordInput("staff_password", "Password")
            ),
            div(
              class = "login-btn-wrap",
              actionButton("open_staff_login", "Log In", class = "btn btn-success action-btn")
            )
          )
        ),
        conditionalPanel(
          condition = "output.staff_logged_in === true",
          div(class = "plain-section section-block", uiOutput("staff_status_ui"))
        )
      ),
      
      tabPanel(
        "Admin",
        conditionalPanel(
          condition = "output.admin_logged_in === false",
          div(
            class = "plain-section section-block access-box",
            div(class = "sheet-title", "Admin Access"),
            div(class = "sheet-subtitle", "Log in to access report submitted."),
            div(
              class = "login-fields-wrap",
              textInput("admin_username", "Username"),
              passwordInput("admin_password", "Password")
            ),
            div(
              class = "login-btn-wrap",
              actionButton("open_admin_login", "Log In", class = "btn btn-success action-btn")
            )
          )
        ),
        conditionalPanel(
          condition = "output.admin_logged_in === true",
          div(
            class = "plain-section section-block",
            tabsetPanel(
              id = "admin_subtabs",
              type = "tabs",
              
              tabPanel(
                "Data",
                selectInput(
                  "admin_data_choice",
                  "Select Spreadsheet",
                  choices = c("Segregation Compliance", "Waste Weights"),
                  selected = "Segregation Compliance"
                ),
                div(class = "admin-sheet-wrap loading-target", DTOutput("admin_table"))
              ),
              
              tabPanel(
                "Summary",
                div(class = "admin-sheet-wrap loading-target", DTOutput("admin_summary_table"))
              )
            ),
            div(
              class = "submit-wrap",
              downloadButton("download_admin_csv", "Download CSV", class = "btn btn-success action-btn"),
              actionButton("admin_logout", "Log Out", class = "btn btn-outline-secondary action-btn")
            )
          )
        )
      )
    )
  )
)

# -------------------------------
# Server
# -------------------------------
server <- function(input, output, session) {
  initialize_submission_databases()
  
  staff_auth <- reactiveVal(FALSE)
  admin_auth <- reactiveVal(FALSE)
  staff_completed <- reactiveVal(FALSE)
  wizard_step <- reactiveVal(1)
  
  data_refresh <- reactiveVal(Sys.time())
  audit_refresh <- reactiveVal(Sys.time())
  
  output$staff_logged_in <- reactive(staff_auth())
  output$admin_logged_in <- reactive(admin_auth())
  outputOptions(output, "staff_logged_in", suspendWhenHidden = FALSE)
  outputOptions(output, "admin_logged_in", suspendWhenHidden = FALSE)
  
  observe({
    req(input$main_tabs)
    if (identical(input$main_tabs, "Waste Audit")) {
      invalidateLater(5000, session)
      audit_refresh(Sys.time())
    }
  })
  
  compliance_data_all <- reactive({
    data_refresh()
    read_submission_data("compliance")
  })
  
  weights_data_all <- reactive({
    data_refresh()
    read_submission_data("weights")
  })
  
  audit_compliance_data <- reactive({
    data_refresh()
    audit_refresh()
    read_submission_data("compliance")
  })
  
  audit_weights_data <- reactive({
    data_refresh()
    audit_refresh()
    read_submission_data("weights")
  })
  
  rv <- reactiveValues(
    compliance_data = blank_compliance_sheet(10),
    weight_data = make_weight_rows(character(0))
  )
  
  all_record_dates <- reactive({
    dates <- c(
      parse_date_safe(audit_compliance_data()$date_of_collection),
      parse_date_safe(audit_weights_data()$date_of_collection)
    )
    dates <- dates[!is.na(dates)]
    dates
  })
  
  observeEvent(all_record_dates(), {
    dates <- all_record_dates()
    if (length(dates) > 0) {
      if (is.null(input$wa_start_date)) {
        updateDateInput(session, "wa_start_date", value = min(dates))
      }
      if (is.null(input$wa_end_date)) {
        updateDateInput(session, "wa_end_date", value = max(dates))
      }
    }
  }, ignoreInit = FALSE)
  
  output$wa_start_date_ui <- renderUI({
    dates <- all_record_dates()
    default_start <- if (length(dates) > 0) min(dates) else Sys.Date()
    
    dateInput(
      "wa_start_date",
      "Start Date",
      value = input$wa_start_date %||% default_start
    )
  })
  
  output$wa_end_date_ui <- renderUI({
    dates <- all_record_dates()
    default_end <- if (length(dates) > 0) max(dates) else Sys.Date()
    
    dateInput(
      "wa_end_date",
      "End Date",
      value = input$wa_end_date %||% default_end
    )
  })
  
  filtered_compliance <- reactive({
    df <- audit_compliance_data()
    if (nrow(df) == 0) return(df)
    
    if (!is.null(input$wa_location) && input$wa_location != "All") {
      df <- df %>% filter(grepl(input$wa_location, assigned_location, fixed = TRUE))
    }
    
    if (!is.null(input$wa_start_date) && !is.null(input$wa_end_date)) {
      start_date <- as.Date(input$wa_start_date)
      end_date <- as.Date(input$wa_end_date)
      
      df <- df %>%
        mutate(date_parsed = parse_date_safe(date_of_collection)) %>%
        filter(!is.na(date_parsed)) %>%
        filter(date_parsed >= start_date & date_parsed <= end_date)
    }
    
    df
  })
  
  filtered_weights <- reactive({
    df <- audit_weights_data()
    if (nrow(df) == 0) return(df)
    
    if (!is.null(input$wa_location) && input$wa_location != "All") {
      df <- df %>%
        filter(
          grepl(input$wa_location, assigned_location, fixed = TRUE) |
            `Assigned Location` == input$wa_location
        )
    }
    
    if (!is.null(input$wa_start_date) && !is.null(input$wa_end_date)) {
      start_date <- as.Date(input$wa_start_date)
      end_date <- as.Date(input$wa_end_date)
      
      df <- df %>%
        mutate(date_parsed = parse_date_safe(date_of_collection)) %>%
        filter(!is.na(date_parsed)) %>%
        filter(date_parsed >= start_date & date_parsed <= end_date)
    }
    
    df
  })
  
  output$sum_avg_bio <- renderText({
    df <- filtered_weights() %>%
      filter(Categories == "Biodegradable") %>%
      mutate(w = safe_numeric(Weights), d = parse_date_safe(date_of_collection))
    days <- dplyr::n_distinct(df$d[!is.na(df$d)])
    paste0(sprintf("%.2f", ifelse(days > 0, sum(df$w, na.rm = TRUE) / days, 0)), " kg")
  })
  
  output$sum_avg_nonbio <- renderText({
    df <- filtered_weights() %>%
      filter(Categories == "Non-Biodegradable") %>%
      mutate(w = safe_numeric(Weights), d = parse_date_safe(date_of_collection))
    days <- dplyr::n_distinct(df$d[!is.na(df$d)])
    paste0(sprintf("%.2f", ifelse(days > 0, sum(df$w, na.rm = TRUE) / days, 0)), " kg")
  })
  
  output$sum_avg_recy <- renderText({
    df <- filtered_weights() %>%
      filter(Categories == "Recyclable") %>%
      mutate(w = safe_numeric(Weights), d = parse_date_safe(date_of_collection))
    days <- dplyr::n_distinct(df$d[!is.na(df$d)])
    paste0(sprintf("%.2f", ifelse(days > 0, sum(df$w, na.rm = TRUE) / days, 0)), " kg")
  })
  
  output$sum_pct_bio <- renderText({
    df <- filtered_weights() %>% mutate(w = safe_numeric(Weights))
    total <- sum(df$w, na.rm = TRUE)
    val <- sum(df$w[df$Categories == "Biodegradable"], na.rm = TRUE)
    sprintf("%.1f%%", ifelse(total > 0, 100 * val / total, 0))
  })
  
  output$sum_pct_nonbio <- renderText({
    df <- filtered_weights() %>% mutate(w = safe_numeric(Weights))
    total <- sum(df$w, na.rm = TRUE)
    val <- sum(df$w[df$Categories == "Non-Biodegradable"], na.rm = TRUE)
    sprintf("%.1f%%", ifelse(total > 0, 100 * val / total, 0))
  })
  
  output$sum_pct_recy <- renderText({
    df <- filtered_weights() %>% mutate(w = safe_numeric(Weights))
    total <- sum(df$w, na.rm = TRUE)
    val <- sum(df$w[df$Categories == "Recyclable"], na.rm = TRUE)
    sprintf("%.1f%%", ifelse(total > 0, 100 * val / total, 0))
  })
  
  output$sum_pct_proper <- renderText({
    df <- filtered_compliance()
    total <- nrow(df)
    val <- sum(df$`Segregation Compliance` == "Properly Segregated", na.rm = TRUE)
    sprintf("%.1f%%", ifelse(total > 0, 100 * val / total, 0))
  })
  
  output$sum_pct_partial <- renderText({
    df <- filtered_compliance()
    total <- nrow(df)
    val <- sum(df$`Segregation Compliance` == "Partially Segregated", na.rm = TRUE)
    sprintf("%.1f%%", ifelse(total > 0, 100 * val / total, 0))
  })
  
  output$sum_pct_notseg <- renderText({
    df <- filtered_compliance()
    total <- nrow(df)
    val <- sum(df$`Segregation Compliance` == "Not Segregated", na.rm = TRUE)
    sprintf("%.1f%%", ifelse(total > 0, 100 * val / total, 0))
  })
  
  output$waste_generation_plot <- renderPlotly({
    df <- filtered_weights() %>%
      mutate(
        w = safe_numeric(Weights),
        d = parse_date_safe(date_of_collection)
      ) %>%
      filter(!is.na(d))
    
    color_map <- c(
      "All" = "#000000",
      "Biodegradable" = "#1b5e20",
      "Non-Biodegradable" = "#7b1e3a",
      "Recyclable" = "#d4af37",
      "Special Waste" = "#000000"
    )
    
    if (!is.null(input$wg_category) && input$wg_category != "All") {
      df <- df %>%
        filter(Categories == input$wg_category) %>%
        group_by(d) %>%
        summarise(total_weight = sum(w, na.rm = TRUE), .groups = "drop") %>%
        mutate(
          d_chr = format(d, "%Y-%m-%d"),
          hover_text = paste0(
            "Date: ", d_chr,
            "<br>Total: ", sprintf("%.2f", total_weight), " kg",
            "<br>Category: ", input$wg_category
          )
        )
      
      if (nrow(df) == 0) return(plotly_empty())
      
      plot_ly(
        df,
        x = ~d_chr,
        y = ~total_weight,
        type = "scatter",
        mode = "lines+markers",
        line = list(color = color_map[[input$wg_category]]),
        marker = list(color = color_map[[input$wg_category]]),
        name = input$wg_category,
        hovertext = ~hover_text,
        hoverinfo = "text"
      ) %>%
        layout(
          xaxis = list(title = "Date", type = "category"),
          yaxis = list(title = "Total Weight (kg)")
        )
    } else {
      df <- df %>%
        group_by(d) %>%
        summarise(total_weight = sum(w, na.rm = TRUE), .groups = "drop") %>%
        mutate(
          d_chr = format(d, "%Y-%m-%d"),
          hover_text = paste0(
            "Date: ", d_chr,
            "<br>Total: ", sprintf("%.2f", total_weight), " kg",
            "<br>Category: All"
          )
        )
      
      if (nrow(df) == 0) return(plotly_empty())
      
      plot_ly(
        df,
        x = ~d_chr,
        y = ~total_weight,
        type = "scatter",
        mode = "lines+markers",
        line = list(color = color_map[["All"]]),
        marker = list(color = color_map[["All"]]),
        name = "All",
        hovertext = ~hover_text,
        hoverinfo = "text"
      ) %>%
        layout(
          xaxis = list(title = "Date", type = "category"),
          yaxis = list(title = "Total Weight (kg)")
        )
    }
  })
  
  output$waste_composition_plot <- renderPlotly({
    df <- filtered_weights() %>%
      mutate(
        w = safe_numeric(Weights),
        d = parse_date_safe(date_of_collection)
      ) %>%
      filter(!is.na(d), Categories != "") %>%
      group_by(d, Categories) %>%
      summarise(total_weight = sum(w, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        d_chr = format(d, "%Y-%m-%d"),
        hover_text = paste0(
          "Date: ", d_chr,
          "<br>Total: ", sprintf("%.2f", total_weight), " kg",
          "<br>Category: ", Categories
        )
      )
    
    if (nrow(df) == 0) return(plotly_empty())
    
    plot_ly(
      df,
      x = ~d_chr,
      y = ~total_weight,
      color = ~Categories,
      colors = c(
        "Biodegradable" = "#1b5e20",
        "Non-Biodegradable" = "#7b1e3a",
        "Recyclable" = "#d4af37",
        "Special Waste" = "#000000"
      ),
      type = "bar",
      hovertext = ~hover_text,
      hoverinfo = "text",
      text = ""
    ) %>%
      layout(
        barmode = "stack",
        xaxis = list(title = "Date", type = "category"),
        yaxis = list(title = "Total Weight (kg)")
      )
  })
  
  output$waste_segregation_plot <- renderPlotly({
    df <- filtered_compliance() %>%
      mutate(d = parse_date_safe(date_of_collection)) %>%
      filter(!is.na(d), `Segregation Compliance` != "") %>%
      group_by(d, `Segregation Compliance`) %>%
      summarise(n = n(), .groups = "drop") %>%
      mutate(
        d_chr = format(d, "%Y-%m-%d"),
        hover_text = paste0(
          "Date: ", d_chr,
          "<br>Total: ", n, " bins",
          "<br>Category: ", `Segregation Compliance`
        )
      )
    
    if (nrow(df) == 0) return(plotly_empty())
    
    plot_ly(
      df,
      x = ~d_chr,
      y = ~n,
      color = ~`Segregation Compliance`,
      colors = c(
        "Properly Segregated" = "#1b5e20",
        "Partially Segregated" = "#d4af37",
        "Not Segregated" = "#7b1e3a"
      ),
      type = "bar",
      hovertext = ~hover_text,
      hoverinfo = "text",
      text = ""
    ) %>%
      layout(
        barmode = "stack",
        xaxis = list(title = "Date", type = "category"),
        yaxis = list(title = "Total Number of Bins Surveyed")
      )
  })
  
  output$compliance_sheet <- renderRHandsontable({
    make_compliance_hot(rv$compliance_data, 420)
  })
  
  output$weight_sheet <- renderRHandsontable({
    location_source <- unique(c(assigned_location_choices, input$wizard_assigned_location %||% character(0)))
    make_weight_hot(rv$weight_data, location_source, 420)
  })
  
  output$staff_status_ui <- renderUI({
    if (isTRUE(staff_completed())) {
      tagList(
        div(class = "wizard-step-title step-wrap", "Submission Completed"),
        div(class = "help-note step-wrap", "Your entry has been submitted successfully."),
        div(
          class = "submit-wrap",
          actionButton("submit_another", "Submit Another Entry", class = "btn btn-success action-btn"),
          actionButton("staff_logout", "Log Out", class = "btn btn-outline-secondary action-btn")
        )
      )
    } else {
      step <- wizard_step()
      
      if (step == 1) {
        tagList(
          div(
            class = "step-wrap",
            div(class = "wizard-step-title", "Step 1. Collection Information"),
            textInput("wizard_name", "Name"),
            selectizeInput(
              "wizard_assigned_location",
              "Assigned Location",
              choices = assigned_location_choices,
              selected = character(0),
              multiple = TRUE,
              options = list(
                placeholder = "Select assigned location(s)",
                plugins = list("remove_button")
              )
            ),
            dateInput("wizard_date", "Date of Collection", value = Sys.Date()),
            selectInput("wizard_time", "Time of Collection", choices = c("AM", "PM"), selected = "AM"),
            div(
              class = "next-btn-wrap",
              actionButton("wizard_next_1", "Next", class = "btn btn-success action-btn")
            )
          )
        )
      } else if (step == 2) {
        tagList(
          div(
            class = "step-wide-wrap",
            div(class = "wizard-step-title", "Step 2. Segregation Compliance"),
            div(class = "loading-target", rHandsontableOutput("compliance_sheet", height = "420px")),
            div(
              class = "submit-wrap",
              actionButton("add_compliance_row", "Add Row", class = "btn btn-outline-secondary action-btn"),
              actionButton("wizard_back_2", "Back", class = "btn btn-outline-secondary action-btn"),
              actionButton("wizard_next_2", "Next", class = "btn btn-success action-btn")
            )
          )
        )
      } else {
        tagList(
          div(
            class = "step-wide-wrap",
            div(class = "wizard-step-title", "Step 3. Waste Weights"),
            div(class = "loading-target", rHandsontableOutput("weight_sheet", height = "420px")),
            div(
              class = "submit-wrap",
              actionButton("wizard_back_3", "Back", class = "btn btn-outline-secondary action-btn"),
              actionButton("wizard_submit", "Submit", class = "btn btn-success action-btn")
            )
          )
        )
      }
    }
  })
  
  observe({
    req(input$compliance_sheet)
    converted <- tryCatch(
      hot_to_r(input$compliance_sheet),
      error = function(e) NULL
    )
    if (!is.null(converted)) {
      rv$compliance_data <- ensure_cols(converted, compliance_cols)
    }
  })
  
  observe({
    req(input$weight_sheet)
    converted <- tryCatch(
      hot_to_r(input$weight_sheet),
      error = function(e) NULL
    )
    if (!is.null(converted)) {
      rv$weight_data <- ensure_cols(converted, weights_cols)
    }
  })
  
  observeEvent(input$wizard_assigned_location, {
    selected_locations <- sort(unique(input$wizard_assigned_location %||% character(0)))
    rv$weight_data <- make_weight_rows(selected_locations)
  }, ignoreInit = TRUE)
  
  observeEvent(input$add_compliance_row, {
    rv$compliance_data <- bind_rows(rv$compliance_data, empty_compliance_row())
  })
  
  observeEvent(input$open_staff_login, {
    if (identical(input$staff_username, "staff") && identical(input$staff_password, "staff")) {
      staff_auth(TRUE)
      admin_auth(FALSE)
      staff_completed(FALSE)
      wizard_step(1)
      rv$compliance_data <- blank_compliance_sheet(10)
      rv$weight_data <- make_weight_rows(character(0))
      showNotification("Staff login successful.", type = "message")
    } else {
      showNotification("Invalid staff credentials.", type = "error")
    }
  })
  
  observeEvent(input$open_admin_login, {
    if (identical(input$admin_username, "admin") && identical(input$admin_password, "admin")) {
      admin_auth(TRUE)
      staff_auth(FALSE)
      data_refresh(Sys.time())
      showNotification("Admin login successful.", type = "message")
    } else {
      showNotification("Invalid admin credentials.", type = "error")
    }
  })
  
  observeEvent(input$wizard_next_1, {
    if (trimws(input$wizard_name %||% "") == "") {
      showNotification("Please enter your name.", type = "error")
      return()
    }
    if (length(input$wizard_assigned_location %||% character(0)) == 0) {
      showNotification("Please select at least one assigned location.", type = "error")
      return()
    }
    wizard_step(2)
  })
  
  observeEvent(input$wizard_back_2, {
    wizard_step(1)
  })
  
  observeEvent(input$wizard_next_2, {
    wizard_step(3)
  })
  
  observeEvent(input$wizard_back_3, {
    wizard_step(2)
  })
  
  observeEvent(input$wizard_submit, {
    name <- trimws(input$wizard_name %||% "")
    assigned <- input$wizard_assigned_location %||% character(0)
    
    ok <- save_submission(
      name = name,
      assigned_location = assigned,
      date_of_collection = input$wizard_date,
      time_of_collection = input$wizard_time,
      compliance_data = rv$compliance_data,
      weights_data = rv$weight_data
    )
    
    if (isTRUE(ok)) {
      data_refresh(Sys.time())
      rv$compliance_data <- blank_compliance_sheet(10)
      rv$weight_data <- make_weight_rows(character(0))
      staff_completed(FALSE)
      wizard_step(1)
      
      updateTextInput(session, "wizard_name", value = "")
      updateSelectizeInput(
        session,
        "wizard_assigned_location",
        choices = assigned_location_choices,
        selected = character(0),
        server = FALSE
      )
      updateDateInput(session, "wizard_date", value = Sys.Date())
      updateSelectInput(session, "wizard_time", selected = "AM")
      
      showNotification("Submission acknowledged successfully.", type = "message")
      session$reload()
    } else {
      showNotification("Please complete at least one row before submitting.", type = "error")
    }
  })
  
  observeEvent(input$submit_another, {
    rv$compliance_data <- blank_compliance_sheet(10)
    rv$weight_data <- make_weight_rows(character(0))
    staff_completed(FALSE)
    wizard_step(1)
    updateTextInput(session, "wizard_name", value = "")
    updateSelectizeInput(
      session,
      "wizard_assigned_location",
      choices = assigned_location_choices,
      selected = character(0),
      server = FALSE
    )
    updateDateInput(session, "wizard_date", value = Sys.Date())
    updateSelectInput(session, "wizard_time", selected = "AM")
    session$reload()
  })
  
  observeEvent(input$staff_logout, {
    staff_auth(FALSE)
    staff_completed(FALSE)
    wizard_step(1)
    rv$compliance_data <- blank_compliance_sheet(10)
    rv$weight_data <- make_weight_rows(character(0))
    updateTextInput(session, "staff_username", value = "")
    updateTextInput(session, "staff_password", value = "")
  })
  
  observeEvent(input$admin_logout, {
    admin_auth(FALSE)
    updateTextInput(session, "admin_username", value = "")
    updateTextInput(session, "admin_password", value = "")
  })
  
  output$admin_table <- renderDT({
    req(admin_auth())
    
    df <- if (identical(input$admin_data_choice, "Waste Weights")) {
      weights_data_all()
    } else {
      compliance_data_all()
    }
    
    datatable(
      df,
      editable = TRUE,
      rownames = FALSE,
      filter = "top",
      class = "compact stripe",
      options = list(
        scrollX = TRUE,
        autoWidth = TRUE,
        pageLength = 10,
        searchHighlight = TRUE,
        dom = "tip"
      )
    )
  }, server = TRUE)
  
  output$admin_summary_table <- renderDT({
    req(admin_auth())
    
    summary_df <- build_submission_summary(
      compliance_data_all(),
      weights_data_all()
    )
    
    datatable(
      summary_df,
      rownames = FALSE,
      filter = "top",
      class = "compact stripe",
      options = list(
        scrollX = TRUE,
        autoWidth = TRUE,
        pageLength = 10,
        searchHighlight = TRUE,
        dom = "tip"
      )
    )
  }, server = TRUE)
  
  observeEvent(input$admin_table_cell_edit, {
    req(admin_auth())
    
    info <- input$admin_table_cell_edit
    df <- if (identical(input$admin_data_choice, "Waste Weights")) {
      weights_data_all()
    } else {
      compliance_data_all()
    }
    
    if (nrow(df) == 0) return()
    
    df[info$row, info$col] <- info$value
    
    file <- if (identical(input$admin_data_choice, "Waste Weights")) {
      weights_file
    } else {
      compliance_file
    }
    
    write_csv(df, file)
    data_refresh(Sys.time())
  })
  
  output$download_admin_csv <- downloadHandler(
    filename = function() {
      if (!is.null(input$admin_subtabs) && identical(input$admin_subtabs, "Summary")) {
        paste0("submission_summary_", Sys.Date(), ".csv")
      } else if (identical(input$admin_data_choice, "Waste Weights")) {
        paste0("waste_weights_", Sys.Date(), ".csv")
      } else {
        paste0("segregation_compliance_", Sys.Date(), ".csv")
      }
    },
    content = function(file) {
      if (!is.null(input$admin_subtabs) && identical(input$admin_subtabs, "Summary")) {
        write_csv(build_submission_summary(compliance_data_all(), weights_data_all()), file)
      } else {
        df <- if (identical(input$admin_data_choice, "Waste Weights")) {
          weights_data_all()
        } else {
          compliance_data_all()
        }
        write_csv(df, file)
      }
    }
  )
}

shinyApp(ui, server)
