# Specs Quality and Integrity Checker - Modular Version
#
# SETUP:
# 1. Create a project folder.
# 2. Save this file as 'app.R' in the root of that folder.
# 3. Create a 'config.json' file in the same folder.
# 4. Create a sub-folder named 'modules'.
# 5. Save your validation function scripts inside 'modules'.
# 6. Install required packages:
#    install.packages(c("shiny", "shinyjs", "DT", "dplyr", "purrr", "haven", "rlang", "bslib", "openxlsx", "jsonlite", "htmltools", "knitr", "rmarkdown", "ggplot2", "plotly", "htmlwidgets", "textclean"))
# 7. Make sure Pandoc is installed on the system running the app. It is usually installed with RStudio.
# 8. Run the app from the project root directory.

library(shiny)
library(shinyjs)
library(DT)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(haven)
library(rlang)
library(bslib)
library(openxlsx)
library(jsonlite)
library(htmltools)
library(knitr)
library(textclean)
library(rmarkdown) # For document previews
library(ggplot2)   # For ggplot visualizations
library(plotly)    # For plotly visualizations
library(htmlwidgets) # For saving widgets to HTML

# --- Load Configuration at Startup ---
config <- try(fromJSON("config.json", simplifyDataFrame = FALSE), silent = TRUE)
if (inherits(config, "try-error")) {
  stop("Could not find or parse config.json. Please ensure it exists and is valid.")
}

# ==============================================================================
# UI Definition
# ==============================================================================
ui <- page_sidebar(
  theme = bs_theme(version = 5, bootswatch = "spacelab"),
  tags$head(tags$style(HTML("
    .dataTables_scrollBody td {
      max-height: 60px;
      overflow-y: auto;
      white-space: pre-wrap;
      word-wrap: break-word;
    }
    .popover {
      max-width: 850px !important; /* Increased for plots */
    }
    /* Ensure modal appears above everything */
    .modal {
      z-index: 2000 !important;
    }
    .modal-backdrop {
      z-index: 1999 !important;
    }
  "))),
  title = "Specs Quality and Integrity Checker",
  
  sidebar = sidebar(
    accordion(
      open = TRUE,
      accordion_panel(
        "Step 1: Upload & Validate",
        fileInput("upload_excel", "Upload Excel File", accept = c(".xlsx")),
        uiOutput("sheet_selector_ui"),
        actionButton("validate_btn", "Validate Selected Sheets", icon = icon("check"), class = "btn-primary")
      )
    )
  ),
  
  navset_card_tab(
    id = "main_content_tabs",
    nav_panel("Specification Rules", DTOutput("spec_table")),
    nav_panel("Validation Results", uiOutput("validation_tabs")),
    nav_panel("Error Summary",
              DTOutput("error_summary_table"),
              br(),
              uiOutput("download_errors_ui")
    ),
    nav_panel("Processed Data", 
              DTOutput("processed_data_table"),
              br(),
              uiOutput("save_processed_data_ui")
    )
  ),
  # Add a container for the dynamically generated modals
  uiOutput("modal_container")
)

# ==============================================================================
# Server Logic
# ==============================================================================
server <- function(input, output, session) {

  perform_ards_check <- function(data, params) {
    # --- 1. File and Parameter Validation ---
    if (is.null(params$filter)) {
      return(list(is_valid = FALSE, message = "JSON is missing the required 'filter' key."))
    }

    ards_data <- data

    # --- 3. Business Logic ---
    df_result <- tryCatch({
      # Extract parameters from JSON, providing defaults
      filter_query       <- params$filter %||% NA
      measure            <- params$measure %||% NA
      difference_measure <- params$difference_measure %||% NA
      difference_lci     <- params$difference_lci %||% NA
      difference_uci     <- params$difference_uci %||% NA
      cmp_name           <- params$cmp_name %||% "Placebo"
      ref_column         <- tolower(params$ref_column %||% "reftrt")
      trt_column         <- tolower(params$trt_column %||% "trt")
      resulttype_column  <- tolower(params$resulttype_column %||% "resulttype")
      result_column      <- tolower(params$result_column %||% "result")

      ards_data <- ards_data %>% rename_all(tolower)

      ref_column <- tolower(ref_column)
      trt_column <- tolower(trt_column)

      # In the filter query replace all variable with their lowercase versions
      filter_query_masked <- str_replace_all(filter_query, "'[^']*'|"[^"]*"", "__STRING__")
      regex_pattern <- '\\b[[:alnum:]_\\.]+(?=\\s*(==|!=|<=|>=|<|>|%IN%|%in%))'
      vars <- str_extract_all(filter_query_masked, regex_pattern)[[1]]

      for (v in unique(vars)) {
        filter_query <- str_replace_all(filter_query,
                                        paste0('\\b', v, '\\b(?=\\s*(==|!=|<=|>=|<|>|%IN%|%in%))'),
                                        tolower(v))
      }

      if (!measure %in% unique(ards_data[[resulttype_column]][ards_data[[ref_column]]==""])) {
        stop(sprintf("Measure %s not in:\n%s.", measure, paste(unique(ards_data[[resulttype_column]][ards_data[[ref_column]]==""]), collapse='\n')))
      }

      if (!all(c(difference_measure, difference_lci, difference_uci) %in% unique(ards_data[[resulttype_column]][ards_data[[ref_column]]!=""]))) {
        stop(sprintf("Difference Measure (or CIs) not in:\n%s.", paste(unique(ards_data[[resulttype_column]][ards_data[[ref_column]]!=""]), collapse='\n')))
      }

      filter_expr <- rlang::parse_expr(filter_query)

      general_query <- gsub(paste0(ref_column,"\\s*==\\s*['\"]+.+?['\"]"), 'TRUE', filter_query)
      general_expr <- rlang::parse_expr(sprintf("%s & %s==''", general_query, ref_column))

      p1 <-
        ards_data %>%
        filter(!!general_expr) %>%
        select_at(unique(c(trt_column, resulttype_column, result_column))) %>%
        filter(.data[[resulttype_column]] %in% c(measure)) %>%
        pivot_wider(id_cols=!!trt_column, names_from=!!resulttype_column, values_from=!!result_column) %>%
        select_at(c(trt_column, measure)) %>%
        rename(
          trt_value := !!measure
        ) %>%
        mutate(
          cmp_value = trt_value[.data[[trt_column]]==cmp_name]
        )

      ards_data %>%
        filter(!!filter_expr) %>%
        filter(.data[[ref_column]]==!!cmp_name) %>%
        select_at(unique(c(trt_column, resulttype_column, result_column))) %>%
        filter(.data[[resulttype_column]] %in% c(difference_measure, difference_lci, difference_uci)) %>%
        pivot_wider(id_cols=!!trt_column, names_from=!!resulttype_column, values_from=!!result_column) %>%
        select_at(c(trt_column, difference_measure, difference_lci, difference_uci)) %>%
        rename(
          effect_estimate := !!difference_measure,
          effect_lower_ci := !!difference_lci,
          effect_upper_ci := !!difference_uci
        ) %>%
        left_join(p1) %>%
        mutate(cmp_name=!!cmp_name, measure=!!measure, difference_measure=!!difference_measure) %>%
        select(measure, cmp_name, cmp_value, trt_name=trt, trt_value, difference_measure, effect_estimate, effect_lower_ci, effect_upper_ci) %>%
        filter(trt_name!=!!cmp_name)

    }, error = function(e) {
      return(list(is_valid = FALSE, message = paste("Error during data extraction/filtering:", e$message)))
    })

    if (!is.data.frame(df_result)) return(df_result)
    if (nrow(df_result) == 0) return(list(is_valid = FALSE, message = "Final query resulted in zero rows."))

    # --- 4. Return Success ---
    return(list(is_valid = TRUE, message = df_result))
  }



  
  rv <- reactiveValues(
    specs = tibble(Name = character(), Type = character(), Values = character()),
    uploaded_excel_data = list(),
    validation_results = list(),
    error_summary = tibble(),
    modal_htmls = list(), # To store UI for document preview modals
    processed_data = NULL
  )
  
  # --- Load Specs Automatically ---
  observe({
    df <- try(read.csv("data-specs.csv", stringsAsFactors = FALSE, check.names = FALSE))
    if(inherits(df, "try-error")){
      showNotification("Failed to read data-specs.csv. Please ensure it exists and is a valid CSV.", type = "error")
      return()
    }
    if(!all(c("Name", "Type", "Values") %in% colnames(df))){
      showNotification("data-specs.csv must contain 'Name', 'Type', and 'Values' columns.", type = "error")
      rv$specs <- tibble(Name = character(), Type = character(), Values = character())
    } else {
      rv$specs <- as_tibble(df)
      showNotification("Specifications loaded successfully from data-specs.csv.", type = "message")
    }
  })
  
  output$spec_table <- renderDT({
    datatable(rv$specs, options = list(pageLength = 10, dom = 'tip'), rownames = FALSE, selection = 'none')
  })
  
  # --- Data Upload Logic ---
  observeEvent(input$upload_excel, {
    req(input$upload_excel)
    path <- input$upload_excel$datapath
    tryCatch({
      sheet_names <- openxlsx::getSheetNames(path)
      rv$uploaded_excel_data <- set_names(map(sheet_names, ~openxlsx::read.xlsx(path, sheet = .x)), sheet_names)
      output$sheet_selector_ui <- renderUI({
        checkboxGroupInput("selected_sheets", "Select Sheets to Validate:", choices = sheet_names, selected = sheet_names)
      })
      showNotification(paste("Excel file loaded with", length(sheet_names), "sheets."), type = "message")
    }, error = function(e) {
      showNotification(paste("Error reading Excel file:", e$message), type = "error")
      output$sheet_selector_ui <- renderUI({ helpText("Could not read the uploaded file.") })
    })
  })
  
  # --- Core Validation Function ---
  run_validation <- function() {
    req(input$selected_sheets, nrow(rv$specs) > 0, length(rv$uploaded_excel_data) > 0)
    
    selected <- input$selected_sheets
    specs <- rv$specs
    all_modals <- list() # Store modals for all sheets in this run
    
    withProgress(message = 'Validating data...', value = 0, {
      results <- map(selected, function(sheet_name) {
        if (!sheet_name %in% names(rv$uploaded_excel_data)) return(NULL)
        incProgress(1/length(selected), detail = paste("Processing sheet:", sheet_name))
        data_sheet <- rv$uploaded_excel_data[[sheet_name]]
        
        # Create a character matrix for rendering, escaping original data to prevent XSS
        render_matrix <- as.data.frame(lapply(data_sheet, function(c) htmltools::htmlEscape(as.character(c))), stringsAsFactors = FALSE)
        colnames(render_matrix) <- colnames(data_sheet)
        
        error_matrix <- matrix(NA_character_, nrow = nrow(data_sheet), ncol = ncol(data_sheet))
        popover_content_matrix <- matrix(NA_character_, nrow = nrow(data_sheet), ncol = ncol(data_sheet))
        popover_title_matrix <- matrix(NA_character_, nrow = nrow(data_sheet), ncol = ncol(data_sheet))
        colnames(error_matrix) <- colnames(popover_content_matrix) <- colnames(popover_title_matrix) <- colnames(data_sheet)
        
        append_msg <- function(existing_msg, new_msg) {
          if (is.na(existing_msg)) return(new_msg) else paste(new_msg, sep = " | ")
        }
        
        get_param <- function(params_str, p_name) {
          val <- strsplit(params_str, ";")[[1]] %>% detect(~startsWith(.x, paste0(p_name, "=")))
          if (is.null(val)) return(NA_character_)
          sub(paste0(p_name, "="), "", val)
        }
        
        for (spec_rule_row in 1:nrow(specs)) {
          rule <- specs[spec_rule_row, ]
          selected_type <- purrr::detect(config$data_types, ~.x$id == rule$Type)
          
          if(selected_type$category == "standard") {
            col_name <- rule$Name
            if (col_name %in% colnames(data_sheet)) {
              col_idx <- which(colnames(data_sheet) == col_name)
              for (row_idx in 1:nrow(data_sheet)) {
                value <- data_sheet[[col_name]][row_idx]
                validation_output <- source("modules/standard_validators.R")$value(value, rule)
                if (!validation_output$is_valid) {
                  error_msg <- validation_output$message
                  error_matrix[row_idx, col_idx] <- append_msg(error_matrix[row_idx, col_idx], error_msg)
                  popover_content_matrix[row_idx, col_idx] <- append_msg(popover_content_matrix[row_idx, col_idx], error_msg)
                  popover_title_matrix[row_idx, col_idx] <- append_msg(popover_title_matrix[row_idx, col_idx], "Validation Error")
                } else {
                  if (rule$Type == "File Path" && !is.na(value) && value != "") {
                    ext <- tolower(tools::file_ext(value))
                    if (ext %in% c("docx", "doc", "rtf") && file.exists(value)) {
                      html_file <- tempfile(fileext = ".html")
                      try(pandoc_convert(value, to = "html5", output = html_file), silent = TRUE)
                      
                      if(file.exists(html_file)){
                        preview_html <- paste(readLines(html_file, warn=FALSE), collapse="\n")
                        modal_id <- paste("preview-modal", make.names(sheet_name), row_idx, col_idx, sep="-")
                        modal_ui <- tags$div(
                          class = "modal fade", id = modal_id, tabindex = "-1",
                          `aria-labelledby` = paste0(modal_id, "-label"), `aria-hidden` = "true",
                          tags$div(class = "modal-dialog modal-xl modal-dialog-scrollable",
                                   tags$div(class = "modal-content",
                                            tags$div(class = "modal-header",
                                                     tags$h5(class = "modal-title", id = paste0(modal_id, "-label"), basename(value)),
                                                     tags$button(type = "button", class = "btn-close", `data-bs-dismiss` = "modal", `aria-label` = "Close")
                                            ),
                                            tags$div(class = "modal-body", HTML(preview_html))
                                   )
                          )
                        )
                        all_modals[[modal_id]] <<- modal_ui
                        button_html <- as.character(tags$button(
                          type = "button",
                          class = "btn btn-sm btn-outline-secondary py-0",
                          `data-bs-toggle` = "modal",
                          `data-bs-target` = paste0("#", modal_id),
                          "View Document"
                        ))
                        render_matrix[row_idx, col_idx] <- paste(render_matrix[row_idx, col_idx], button_html, sep="<br>")
                      }
                    }
                  }
                }
              }
            }
          } else if (selected_type$category == "complex") {
            path_col_name <- get_param(rule$Values, "path_col")
            filter_col_name <- get_param(rule$Values, "filter_col")
            if (is.na(path_col_name) || is.na(filter_col_name) || !all(c(path_col_name, filter_col_name) %in% colnames(data_sheet))) next
            
            filter_col_idx <- which(colnames(data_sheet) == filter_col_name)
            
            for (row_idx in 1:nrow(data_sheet)) {
              path_value <- data_sheet[[path_col_name]][row_idx]
              json_str <- replace_html(data_sheet[[filter_col_name]][row_idx])
              
              if (is.na(path_value) || path_value == "" || is.na(json_str) || json_str == "") next
              
              json_params <- try(fromJSON(json_str), silent = TRUE)
              if(inherits(json_params, "try-error")) {
                error_matrix[row_idx, filter_col_idx] <- append_msg(error_matrix[row_idx, filter_col_idx], "Invalid JSON format.")
                popover_content_matrix[row_idx, filter_col_idx] <- append_msg(popover_content_matrix[row_idx, filter_col_idx], "Invalid JSON format in cell.")
                popover_title_matrix[row_idx, filter_col_idx] <- append_msg(popover_title_matrix[row_idx, filter_col_idx], "Validation Error")
                next
              }
              
              function_name <- json_params$validation_module
              if(is.null(function_name)) {
                next
              }
              
              # Find the function configuration by its ID
              function_config <- NULL
              for (key in names(config$function_mapping)) {
                if (config$function_mapping[[key]]$id == function_name) {
                  function_config <- config$function_mapping[[key]]
                  function_config$function_name <- key
                  break
                }
              }
              
              if(is.null(function_config)) {
                next
              }
              
              # Add default values for optional parameters
              optional_args <- function_config$args$optional
              if (!is.null(optional_args)) {
                for (arg_name in names(optional_args)) {
                  if (is.null(json_params[[arg_name]])) {
                    json_params[[arg_name]] <- optional_args[[arg_name]]
                  }
                }
              }
              
              required_args <- function_config$args$required
              missing_args <- setdiff(required_args, names(json_params))
              if (length(missing_args) > 0) {
                let_msg <- paste("JSON is missing required keys:", paste(missing_args, collapse = ", "))
                error_matrix[row_idx, filter_col_idx] <- append_msg(error_matrix[row_idx, filter_col_idx], let_msg)
                popover_content_matrix[row_idx, filter_col_idx] <- append_msg(popover_content_matrix[row_idx, filter_col_idx], let_msg)
                popover_title_matrix[row_idx, filter_col_idx] <- append_msg(popover_title_matrix[row_idx, filter_col_idx], "Configuration Error")
                next
              }
              
              module_path <- function_config$path
              if(!file.exists(module_path)) {
                let_msg <- paste("Module file not found:", module_path)
                error_matrix[row_idx, filter_col_idx] <- append_msg(error_matrix[row_idx, filter_col_idx], let_msg)
                popover_content_matrix[row_idx, filter_col_idx] <- append_msg(popover_content_matrix[row_idx, filter_col_idx], let_msg)
                popover_title_matrix[row_idx, filter_col_idx] <- append_msg(popover_title_matrix[row_idx, filter_col_idx], "Configuration Error")
                next
              }
              
              module_env <- new.env()
              source(module_path, local = module_env)
              
              if(!exists(function_config$function_name, envir = module_env)) {
                let_msg <- paste("Function", function_config$function_name, "not found in module", module_path)
                error_matrix[row_idx, filter_col_idx] <- append_msg(error_matrix[row_idx, filter_col_idx], let_msg)
                popover_content_matrix[row_idx, filter_col_idx] <- append_msg(popover_content_matrix[row_idx, filter_col_idx], let_msg)
                popover_title_matrix[row_idx, filter_col_idx] <- append_msg(popover_title_matrix[row_idx, filter_col_idx], "Configuration Error")
                next
              }
              
              validation_func <- module_env[[function_config$function_name]]
              validation_output <- validation_func(path_value, json_params)
              
              # --- MODIFIED: Handle plots, data frames, and text ---
              if (inherits(validation_output$message, "plotly") || inherits(validation_output$message, "htmlwidget")) {
                tmp_html <- tempfile(fileext = ".html")
                htmlwidgets::saveWidget(validation_output$message, tmp_html, selfcontained = TRUE)
                widget_html <- paste(readLines(tmp_html, warn = FALSE), collapse = "\n")
                wrapper_div <- as.character(tags$iframe(
                  srcdoc = widget_html,
                  style = "width: 750px; height: 450px; border: none;",
                  seamless = "seamless"
                ))
                popover_content_matrix[row_idx, filter_col_idx] <- append_msg(popover_content_matrix[row_idx, filter_col_idx], wrapper_div)
                popover_title_matrix[row_idx, filter_col_idx] <- append_msg(popover_title_matrix[row_idx, filter_col_idx], "Plot Visualization")
                render_matrix[row_idx, filter_col_idx] <- as.character(tags$button(
                  type = "button", class = "btn btn-sm btn-info py-0", function_config$label
                ))

              } else if (inherits(validation_output$message, "ggplot")) {
                tmp_file <- tempfile(fileext = ".png")
                tryCatch({
                  ggsave(tmp_file, plot = validation_output$message, width = 7, height = 5, units = "in", dpi = 150)
                  img_uri <- knitr::image_uri(tmp_file)
                  wrapper_div <- as.character(tags$div(
                    style = "max-width: 750px; max-height: 500px; overflow: auto;",
                    tags$img(src = img_uri, style = "width: 100%; height: auto;")
                  ))
                  popover_content_matrix[row_idx, filter_col_idx] <- append_msg(popover_content_matrix[row_idx, filter_col_idx], wrapper_div)
                  popover_title_matrix[row_idx, filter_col_idx] <- append_msg(popover_title_matrix[row_idx, filter_col_idx], "Plot Visualization")
                  render_matrix[row_idx, filter_col_idx] <- as.character(tags$button(
                    type = "button", class = "btn btn-sm btn-info py-0", function_config$label
                  ))
                }, error = function(e) {
                  err_msg <- paste("Failed to render ggplot:", e$message)
                  popover_content_matrix[row_idx, filter_col_idx] <- append_msg(popover_content_matrix[row_idx, filter_col_idx], err_msg)
                  popover_title_matrix[row_idx, filter_col_idx] <- append_msg(popover_title_matrix[row_idx, filter_col_idx], "Render Error")
                })
              } else if(is.data.frame(validation_output$message)) {
                html_table <- knitr::kable(validation_output$message, format = "html", table.attr = "class='table table-sm table-bordered' style='margin-bottom:0;' ")
                wrapper_div <- as.character(tags$div(style = "max-width: 800px; max-height: 400px; overflow: auto; background-color: white; color: black;", HTML(html_table)))
                popover_content_matrix[row_idx, filter_col_idx] <- append_msg(popover_content_matrix[row_idx, filter_col_idx], wrapper_div)
                popover_title_matrix[row_idx, filter_col_idx] <- append_msg(popover_title_matrix[row_idx, filter_col_idx], "Data Check Result")
                render_matrix[row_idx, filter_col_idx] <- as.character(tags$button(
                  type = "button", class = "btn btn-sm btn-success py-0", function_config$label
                ))
              } else if (!is.null(attr(validation_output$message, "type")) && attr(validation_output$message, "type")=="rtfmt") {
                wrapper_div <- as.character(tags$div(style = paste(
                  "min-width: 1200px;",
                  "max-height: 400px;",
                  "overflow: auto;",
                  "background-color: white; color: black;",
                  "white-space: pre-wrap; ", 
                  "font-size: 10px;"
                ),
                HTML(validation_output$message)))
                popover_content_matrix[row_idx, filter_col_idx] <- append_msg(popover_content_matrix[row_idx, filter_col_idx], wrapper_div)
                popover_title_matrix[row_idx, filter_col_idx] <- append_msg(popover_title_matrix[row_idx, filter_col_idx], "Validation Info")
                render_matrix[row_idx, filter_col_idx] <- as.character(tags$button(
                  type = "button", class = "btn btn-sm btn-success py-0", function_config$label
                ))
              } else if (is.character(validation_output$message) && !is.na(validation_output$message) && validation_output$message != "") {
                wrapper_div <- as.character(tags$div(style = paste(
                  "min-width: 800px;",
                  "max-height: 400px;",
                  "overflow: auto;",
                  "background-color: white; color: black;",
                  "white-space: pre-wrap; ", 
                  "font-size: 10px;"
                ),
                HTML(validation_output$message)))
                popover_content_matrix[row_idx, filter_col_idx] <- append_msg(popover_content_matrix[row_idx, filter_col_idx], wrapper_div)
                popover_title_matrix[row_idx, filter_col_idx] <- append_msg(popover_title_matrix[row_idx, filter_col_idx], "Validation Info")
              }
              
              if (!validation_output$is_valid) {
                error_msg <- "Complex validation failed."
                if(is.data.frame(validation_output$message) && nrow(validation_output$message) > 0) {
                  error_msg <- "Multiple issues found (see popover for details)."
                } else if (is.character(validation_output$message) && !is.na(validation_output$message) && validation_output$message != "") {
                  error_msg <- str_trunc(validation_output$message, 100)
                } else if (inherits(validation_output$message, "ggplot") || inherits(validation_output$message, "plotly")) {
                  error_msg <- "Validation produced a plot (see popover)."
                }
                error_matrix[row_idx, filter_col_idx] <- append_msg(error_matrix[row_idx, filter_col_idx], error_msg)
              }
            }
          }
        }
        
        return(list(
          data = data_sheet,
          render_matrix = render_matrix,
          error_matrix = error_matrix,
          popover_content = popover_content_matrix,
          popover_title = popover_title_matrix
        ))
      })
    }) # End withProgress
    
    results <- results[!sapply(results, is.null)]
    rv$validation_results <- set_names(results, selected[selected %in% names(rv$uploaded_excel_data)])
    rv$modal_htmls <- all_modals
    
    error_summary_df <- imap_dfr(rv$validation_results, ~{
      error_matrix <- .x$error_matrix
      error_indices <- which(!is.na(error_matrix), arr.ind = TRUE)
      if(nrow(error_indices) > 0) {
        map_dfr(1:nrow(error_indices), function(i) {
          row_idx <- error_indices[i, "row"]; col_idx <- error_indices[i, "col"]
          tibble(Sheet = .y, Row = row_idx, Column = colnames(.x$data)[col_idx],
                 Value = as.character(.x$data[row_idx, col_idx]), Reason = error_matrix[row_idx, col_idx])
        })
      } else { tibble() }
    })
    rv$error_summary <- error_summary_df
    
    if(!is.null(shiny::getDefaultReactiveDomain())) { 
      showNotification("Validation complete!", type = "message")
      if (nrow(error_summary_df) == 0) {
        output$save_processed_data_ui <- renderUI({
          actionButton("process_data_btn", "Process Data", icon = icon("cogs"))
        })
      }
    }
  }
  
  # --- Event Triggers ---
  observeEvent(input$validate_btn, { run_validation() })
  
  observeEvent(input$process_data_btn, {
    req(rv$validation_results)
    
    # Combine data from all validated sheets
    data_raw <- bind_rows(lapply(rv$validation_results, function(res) res$data))
    
    # Get params from ARDS Condition column
    json_str <- data_raw$"ARDS Condition"[1]
    params <- try(fromJSON(json_str), silent=TRUE)

    if (inherits(params, "try-error")) {
      showNotification("Failed to parse JSON from ARDS Condition column.", type = "error")
      return()
    }

    # Perform the data transformation
    transformed_data_result <- perform_ards_check(data_raw, params)

    if (!transformed_data_result$is_valid) {
      showNotification(transformed_data_result$message, type = "error")
      return()
    }

    data_with_extra_cols <- transformed_data_result$message

    processed_data <- data_with_extra_cols %>%
      mutate(
        cmp = round(cmp_value, 2),
        trt = round(trt_value, 2),
        value = round(ifelse(is.na(effect_estimate), 0, effect_estimate), 2),
        lower = round(ifelse(is.na(effect_lower_ci), -1, effect_lower_ci), 2),
        upper = round(ifelse(is.na(effect_upper_ci), 1, effect_upper_ci), 2)
      ) %>%
      mutate(
        logscale=ifelse(grepl('(^OR)|(^HR)', measure) | (effect_type=='Risk' & difference_measure=='Measure'), TRUE, FALSE),
        logbase=2,
        reversed=improvement_direction!='increase',
      ) %>%
      mutate(
        value = ifelse(logscale & value <= 0, NA, value),
        lower = ifelse(logscale & lower <= 0, NA, lower),
        upper = ifelse(logscale & upper <= 0, NA, upper)
      ) %>%
      rowwise() %>%
      mutate(effect=paste(strwrap(variable_type, 23), collapse = '\n')) %>% # break long labels
      ungroup() %>%
      mutate(axis_name = case_when(
        effect_type=='Risk' & difference_measure=='Measure' ~ 'Relative Rate per Year (95% CI)',
        effect_type=='Risk' ~ 'Risk Difference (95% CI)',
        grepl('^OR', difference_measure) ~ 'Odds Ratio (95% CI)',
        grepl('^HR', difference_measure) ~ 'Hazard Ratio (95% CI)',
        grepl('^Relative Rate', difference_measure) ~ 'Risk Difference (95% CI)',
        grepl('Proportion|Percentage|%', variable_type) | grepl('Percent', measure) ~ 'Difference in Percent Change (95% CI)',
        TRUE ~ 'Change Difference (95% CI)'
      )) %>%
      mutate(
        non_inferiority_margin = NA
      ) %>%
      mutate(
        trt_txt = case_when(
          grepl('Proportion|Percentage|%', variable_type) | grepl('Percent', measure) | grepl('^Relative Rate', difference_measure) ~ sprintf('%.2f%%', trt),
          !grepl('Percent', measure) & !grepl('^Relative Rate', difference_measure) ~ sprintf('%.2f', trt),
          TRUE ~ '-'
        ),
        cmp_txt = case_when(
          grepl('Proportion|Percentage|%', variable_type) | grepl('Percent', measure) | grepl('^Relative Rate', difference_measure) ~ sprintf('%.2f%%', cmp),
          !grepl('Percent', measure) & !grepl('^Relative Rate', difference_measure) ~ sprintf('%.2f', cmp),
          TRUE ~ '-'
        )
      )
    
    rv$processed_data <- processed_data
    showNotification("Data processed successfully.", type = "message")
    
    output$processed_data_table <- renderDT({
      datatable(rv$processed_data, editable = TRUE, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
    })
    
    output$save_processed_data_ui <- renderUI({
      downloadButton("save_processed_data_btn", "Save Processed Data")
    })
  })
  
  observeEvent(input$processed_data_table_cell_edit, {
    info <- input$processed_data_table_cell_edit
    str(info)
    i <- info$row
    j <- info$col + 1
    v <- info$value
    rv$processed_data[i, j] <- isolate(DT::coerceValue(v, rv$processed_data[i, j]))
  })
  
  output$save_processed_data_btn <- downloadHandler(
    filename = function() { "../processed-data.csv" },
    content = function(file) {
      write.csv(rv$processed_data, file, row.names = FALSE)
    }
  )

  # --- UI Rendering ---
  
  output$modal_container <- renderUI({
    tagList(unname(rv$modal_htmls))
  })
  
  output$validation_tabs <- renderUI({
    if (length(rv$validation_results) == 0) {
      return(helpText("Validation results will appear here."))
    }
    
    sheet_tabs <- imap(rv$validation_results, ~{
      tabPanel(title = .y, DTOutput(paste0("table_", .y)))
    })
    
    do.call(tabsetPanel, c(id="sheet_tabs_panel", unname(sheet_tabs)))
  })
  
  output$error_summary_table <- renderDT({
    if (nrow(rv$error_summary) == 0 && length(rv$validation_results) > 0) {
      return(datatable(data.frame(Message = "No validation errors found."), rownames = FALSE, options = list(dom = 't')))
    }
    req(nrow(rv$error_summary) > 0)
    datatable(rv$error_summary, rownames = FALSE, filter = 'top', selection = 'none',
              extensions = 'FixedHeader',
              options = list(pageLength = 10, scrollX = TRUE, dom = 'frtip', fixedHeader = TRUE))
  })
  
  output$download_errors_ui <- renderUI({
    req(nrow(rv$error_summary) > 0)
    downloadButton("download_error_summary_btn", "Download Full Error Report")
  })
  
  output$download_error_summary_btn <- downloadHandler(
    filename = function() { paste0("validation-error-summary-", Sys.Date(), ".xlsx") },
    content = function(file) {
      req(nrow(rv$error_summary) > 0)
      sorted_summary <- rv$error_summary %>% arrange(Sheet, as.numeric(Row), Column)
      wb <- createWorkbook()
      addWorksheet(wb, "Error Summary")
      writeData(wb, "Error Summary", "Specs Quality and Integrity Checker", startCol = 1, startRow = 1)
      mergeCells(wb, "Error Summary", cols = 1:ncol(sorted_summary), rows = 1)
      titleStyle <- createStyle(fontSize = 14, textDecoration = "bold", halign = "center")
      addStyle(wb, "Error Summary", style = titleStyle, rows = 1, cols = 1)
      writeData(wb, "Error Summary", sorted_summary, startRow = 3)
      headerStyle <- createStyle(textDecoration = "bold")
      addStyle(wb, "Error Summary", style = headerStyle, rows = 3, cols = 1:ncol(sorted_summary), gridExpand = TRUE)
      setColWidths(wb, "Error Summary", cols = 1:ncol(sorted_summary), widths = "auto")
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  # --- Dynamic Observers for DT tables ---
  observe({
    req(length(rv$validation_results) > 0)
    walk(names(rv$validation_results), function(sheet_name) {
      
      output[[paste0("table_", sheet_name)]] <- renderDT({
        res <- rv$validation_results[[sheet_name]]
        dt <- datatable(res$render_matrix, rownames = FALSE, escape = FALSE, selection = 'none',
                        extensions = 'FixedHeader',
                        options = list(
                          pageLength = 10,
                          scrollX = TRUE,
                          scrollY = "calc(100vh - 400px)",
                          fixedHeader = TRUE,
                          rowCallback = JS(
                            "function(row, data, index) {",
                            "  var errorMatrix = ", jsonlite::toJSON(res$error_matrix, na = "null"), ";",
                            "  var popoverContentMatrix = ", jsonlite::toJSON(res$popover_content, na = "null"), ";",
                            "  var popoverTitleMatrix = ", jsonlite::toJSON(res$popover_title, na = "null"), ";",
                            "  for (var j=0; j < data.length; j++) {",
                            "    var cell = $(row).find('td').eq(j);",
                            "    if (popoverContentMatrix[index] && popoverContentMatrix[index][j] !== null) {",
                            "      $(cell).attr('data-bs-toggle', 'popover')",
                            "             .attr('data-bs-html', 'true')",
                            "             .attr('data-bs-trigger', 'click')",
                            "             .attr('data-bs-title', popoverTitleMatrix[index][j])",
                            "             .attr('data-bs-content', popoverContentMatrix[index][j]);",
                            "    }",
                            "    if (errorMatrix[index] && errorMatrix[index][j] !== null) {",
                            "      cell.css('background-color', 'rgba(255, 135, 135, 0.7)');",
                            "    }",
                            "  }",
                            "}"
                          ),
                          drawCallback = JS(
                            "function(settings) {",
                            "  var allowlist = bootstrap.Popover.Default.allowList;",
                            "  allowlist.table = []; allowlist.thead = []; allowlist.tbody = []; allowlist.tr = [];",
                            "  allowlist.td = ['style']; allowlist.th = ['style']; allowlist.div = ['style'];",
                            "  allowlist.p = []; allowlist.h1 = []; allowlist.h2 = []; allowlist.h3 = [];",
                            "  allowlist.ul = []; allowlist.ol = []; allowlist.li = [];",
                            "  allowlist.strong = []; allowlist.em = [];",
                            "  allowlist.br = [];",
                            "  allowlist.iframe = ['srcdoc', 'style', 'seamless', 'width', 'height', 'frameborder'];",
                            "  allowlist.img = ['src', 'style', 'width', 'height'];",
                            
                            "  var table = this.api().table();",
                            "  $(table.body()).find('[data-bs-toggle=\"popover\"]').each(function() {",
                            "    var popover = bootstrap.Popover.getInstance(this);",
                            "    if (popover) { popover.dispose(); }",
                            "  });",
                            "  $(table.body()).find('[data-bs-toggle=\"popover\"]').each(function() {",
                            "    new bootstrap.Popover(this, { html: true, container: 'body', sanitize: false });",
                            "  });",
                            "}"
                          )
                        )
        )
        dt
      })
    })
  })
}

shinyApp(ui = ui, server = server)
