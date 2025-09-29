# Load necessary libraries
# Make sure to install them first: install.packages(c("shiny", "shinyjs", "colourpicker", "dplyr", "tidyr", "R6"))
# You also need to have the 'brrr' package installed, e.g., via devtools::install_github("mrppdex/brrr")
library(shiny)
library(shinyjs)
library(colourpicker)
library(dplyr)
library(tidyr)
library(grid)
library(R6)
library(brrr)
library(DT)
library(gt)
library(rlang)

# --- App Data and Setup ---

# Load mock data from the brrr package
data(mock_data)
data(mock_data_risks)

# Get default options from the brrr package's page_options R6 class
default_options <- brrr::page_options$new()

# Helper function to create a numeric input with a reset button
numericInputWithReset <- function(inputId, label, value) {
  div(
    numericInput(inputId, label, value, step = 0.01),
    actionButton(paste0("reset_", inputId), "Reset", class = "btn-xs")
  )
}

# --- UI Definition ---

ui <- navbarPage(
  title = "{brrr} Plot Builder",
  id = "main_nav",
  collapsible = TRUE,
  
  # Use shinyjs
  useShinyjs(),
  
  # --- Data Tab ---
  tabPanel("1. Data",
           sidebarLayout(
             sidebarPanel(
               h4("Upload Data"),
               fileInput("file1", "Choose CSV or RDS File",
                         accept = c(".csv", ".rds")),
               tags$hr(),
               h4("Use Mock Data"),
               actionButton("load_mock_benefit", "Load Benefit Mock Data"),
               actionButton("load_mock_risk", "Load Risk Mock Data"),
               tags$hr(),
               h4("Required Columns"),
               p("Your data should contain these columns:"),
               tags$ul(
                 tags$li(strong("value")),
                 tags$li(strong("lower")),
                 tags$li(strong("upper")),
                 tags$li(strong("axis_number")),
                 tags$li(strong("logscale")),
                 tags$li(strong("logbase")),
                 tags$li(strong("reversed")),
                 tags$li(strong("improvement_direction")),
                 tags$li(strong("non_inferiority_margin"))
               )
             ),
             mainPanel(
               h3("Current Data"),
               dataTableOutput("contents")
             )
           )),
  
  
  
  # --- Endpoints Tab ---
  tabPanel("2. Endpoints",
           fluidPage(
             h3("Endpoint & Axis Configuration"),
             p("Define the main components of your plot, such as benefit and risk sections."),
             actionButton("add_endpoint_section", "Add New Endpoint Section", icon = icon("plus")),
             tags$hr(),
             uiOutput("endpoint_sections_ui")
           )),
  
  # --- Settings Tab ---
  tabPanel("3. Global Settings",
           sidebarLayout(
             sidebarPanel(
               h4("General Page Layout"),
               numericInputWithReset("PAGE_TOP_MARGIN", "Top Margin", default_options$PAGE_TOP_MARGIN),
               numericInputWithReset("PAGE_BOTTOM_MARGIN", "Bottom Margin", default_options$PAGE_BOTTOM_MARGIN),
               numericInputWithReset("PAGE_LEFT_MARGIN", "Left Margin", default_options$PAGE_LEFT_MARGIN),
               numericInputWithReset("PAGE_RIGHT_MARGIN", "Right Margin", default_options$PAGE_RIGHT_MARGIN),
               
               h4("Plot Output Size"),
               numericInput("plot_width", "Plot Width (inches)", value = default_options$plot.width),
               numericInput("plot_height", "Plot Height (inches)", value = default_options$plot.height)
             ),
             mainPanel(
               h3("Detailed Plot Options"),
               tabsetPanel(
                 tabPanel("Header", 
                          numericInputWithReset("HEADER_HEIGHT", "Header Height", default_options$HEADER_HEIGHT),
                          numericInputWithReset("header.label.font.size", "Header Label Font Size", default_options$header.label.font.size),
                          colourInput("header.label.color", "Header Label Color", value = default_options$header.label.color),
                          colourInput("header.background.color", "Header Background Color", value = default_options$header.background.color),
                          textInput("header_arrow_label_treatment", "Arrow label: Treatment", value = "Favors\\nTreatment", placeholder = "Use \\n for a new line"),
                          textInput("header_arrow_label_comparator", "Arrow label: Comparator", value = "Favors\\nPlacebo", placeholder = "Use \\n for a new line"),
                          helpText("Tip: type \\n to insert a line break in the label (e.g., Favors\\nTreatment).")
                 ),
                 tabPanel("Labels & Text", 
                          numericInputWithReset("label.font.size", "Label Font Size", default_options$label.font.size),
                          checkboxInput("label.font.usecolors", "Use Colors for Label Fonts", value = default_options$label.font.usecolors),
                          checkboxInput("label.use.separation.line", "Use Separation Lines", value = default_options$label.use.separation.line)
                 ),
                 tabPanel("Axis",
                          numericInputWithReset("axis.label.font.size", "Axis Label Font Size", default_options$axis.label.font.size),
                          numericInputWithReset("axis.ticks.font.size", "Axis Ticks Font Size", default_options$axis.ticks.font.size),
                          numericInput("axis.ticks.font.rotation", "Axis Ticks Rotation", value = default_options$axis.ticks.font.rotation),
                          checkboxInput("axis.ticks.label.nice", "Use 'Nice' Axis Tick Labels", value = default_options$axis.ticks.label.nice)
                 ),
                 tabPanel("Boxes",
                          numericInputWithReset("box.spacing", "Spacing Between Boxes", default_options$box.spacing),
                          # single_category_height is handled in endpoints
                          colourInput("box.fill.color", "Box Fill Color", value = default_options$box.fill.color)
                 ),
                 tabPanel("Forest Plot",
                          numericInput("forest.line.type", "Forest Line Type (lty)", value = default_options$forest.line.type, min=1, max=6),
                          numericInput("forest.line.width", "Forest Line Width (lwd)", value = default_options$forest.line.width, min=0),
                          numericInput("forest.pch.shift", "Forest Symbol Shift (pch)", value = default_options$forest.pch.shift, min=0)
                 )
               )
             )
           )),
  
  # --- Plot Preview Tab ---
  tabPanel("4. Plot Preview & Download",
           fluidPage(
             fluidRow(
               column(3, 
                      h4("Download"),
                      downloadButton("downloadPlot", "Download Plot (SVG)")
               ),
               column(9,
                      h3("Live Plot Preview")
               )
             ),
             hr(),
             plotOutput("br_plot", height = "800px")
           )),
  # --- Effects Table Tab ---
  tabPanel("5. Effects Table",
           fluidPage(
             h3("EMA-style Effects Table"),
             p("Summarise the same endpoints as your forest plot; optionally include a description column from your data, and export as a publication-ready table."),
             fluidRow(
               column(4,
                      checkboxInput("effects_show_ci", "Show 95% CI column", value = TRUE),
                      checkboxInput("effects_show_measure", "Show effect measure / estimator", value = TRUE),
                      checkboxInput("effects_include_desc", "Include description column", value = FALSE),
                      selectInput("effects_desc_col", "Description column (from data)", choices = c(""), selected = ""),
                      textInput("effects_title", "Table title", value = "Effects Table"),
                      textInput("effects_subtitle", "Subtitle (optional)", value = ""),
                      textAreaInput("effects_footnote", "Footnote (optional)", value = "",
                                    placeholder = "e.g., CI = Confidence Interval; higher values indicate improvement." , rows = 3)
               ),
               column(8,
                      h4("Preview Effects Table"),
                      gt_output("effects_gt"),
                      br(),
                      fluidRow(
                        column(4, downloadButton("dl_effects_html", "Download HTML")),
                        column(4, downloadButton("dl_effects_csv", "Download CSV"))
                      )
               )
             )
           )
  )
)

# --- Server Logic ---

server <- function(input, output, session) {
  # ------------------------------
  # Global settings that should trigger plot refresh
  # ------------------------------
  global_setting_ids <- c(
    "PAGE_TOP_MARGIN", "PAGE_BOTTOM_MARGIN", "PAGE_LEFT_MARGIN", "PAGE_RIGHT_MARGIN",
    "plot_width", "plot_height", "HEADER_HEIGHT", "header.label.font.size",
    "header.label.color", "header.background.color", "label.font.size", "label.font.usecolors",
    "label.use.separation.line", "axis.label.font.size", "axis.ticks.font.size",
    "axis.ticks.font.rotation", "axis.ticks.label.nice", "box.spacing",
    "box.fill.color", "forest.line.type", "forest.line.width", "forest.pch.shift"
  )
  
  # ------------------------------
  # Reactive state
  # ------------------------------
  app_data <- reactiveVal(mock_data)
  
  endpoint_sections <- reactiveVal(list(
    list(
      id = 1,
      title = "Benefits",
      data_source = "mock_benefit",
      axis_num = 1,
      axis_label_col = "estimator",
      split_box_by_col = "endpoint",
      split_axis_by_col = "axis_number",
      num_ticks = 6,
      neutral_pos = 2,
      category_height = 0.05,
      header_cols = list(
        "Endpoint"  = list(source = "endpoint",  width = 0.20, collapse = FALSE, sep_line = TRUE,  size = 1),
        "Treatment" = list(source = "treatment", width = 0.10, collapse = FALSE, sep_line = FALSE,  size = 1),
        "Placebo"   = list(source = "placebo",   width = 0.10, collapse = FALSE, sep_line = TRUE,  size = 1)
      )
    )
  ))
  
  all_data <- reactiveValues(
    mock_benefit = mock_data,
    mock_risk    = mock_data_risks
  )
  
  # ------------------------------
  # Data tab
  # ------------------------------
  observeEvent(input$file1, {
    req(input$file1)
    ext <- tools::file_ext(input$file1$name)
    new_data <- switch(ext,
                       csv = read.csv(input$file1$datapath),
                       rds = readRDS(input$file1$datapath),
                       validate("Invalid file type; Please upload a .csv or .rds file")
    )
    all_data$uploaded <- new_data
    app_data(new_data)
    current_sections <- endpoint_sections()
    if (length(current_sections) > 0) {
      current_sections[[1]]$data_source <- "uploaded"
      endpoint_sections(current_sections)
    }
  })
  
  observeEvent(input$load_mock_benefit, { app_data(mock_data) })
  observeEvent(input$load_mock_risk,    { app_data(mock_data_risks) })
  
  output$contents <- renderDataTable({ app_data() })
  
  # ------------------------------
  # Endpoints tab UI
  # ------------------------------
  output$endpoint_sections_ui <- renderUI({
    sections <- endpoint_sections()
    data_choices <- names(all_data)
    
    if (length(sections) == 0) return(p("No endpoint sections defined. Click 'Add New Endpoint Section' to begin."))
    
    lapply(sections, function(section) {
      id <- section$id
      current_data_source <- section$data_source
      if (is.null(current_data_source) || !current_data_source %in% data_choices) {
        current_data_source <- data_choices[1]
      }
      data_cols <- c("Select column" = "", names(all_data[[current_data_source]]))
      
      header_cols_ui <- if (!is.null(section$header_cols) && length(section$header_cols) > 0) {
        lapply(names(section$header_cols), function(col_name) {
          col_id <- make.unique(gsub("[^A-Za-z0-9]", "_", col_name))
          section_col_id <- paste(id, col_id, sep = "_")
          col_data <- section$header_cols[[col_name]]
          
          fluidRow(
            column(3, p(strong(col_name))),
            column(3, selectInput(paste0("source_",  section_col_id), "Source", choices = data_cols, selected = col_data$source)),
            column(1, numericInput(paste0("width_",   section_col_id), "Width",   value = col_data$width, step = 0.01)),
            column(1, checkboxInput(paste0("collapse_",section_col_id), "Collapse", value = col_data$collapse)),
            column(1, checkboxInput(paste0("sepline_", section_col_id), "Line",     value = col_data$sep_line)),
            column(1, numericInput(paste0("size_",    section_col_id), "Size",    value = col_data$size, step = 0.1)),
            column(2, actionButton(paste0("remove_col_", section_col_id), "Remove", icon = icon("trash"), class = "btn-xs btn-danger", style = "margin-top: 20px;"))
          )
        })
      } else {
        p("No header columns defined for this section.")
      }
      
      wellPanel(
        h4(textInput(paste0("title_", id), "Section Title", value = section$title)),
        fluidRow(
          column(4, selectInput(paste0("data_source_", id),  "Data Source",    choices = data_choices, selected = section$data_source)),
          column(4, selectInput(paste0("split_axis_by_", id), "Split Axis By",  choices = data_cols,    selected = section$split_axis_by_col)),
          column(4, selectInput(paste0("split_box_by_", id),  "Split Box By",   choices = data_cols,    selected = section$split_box_by_col))
        ),
        fluidRow(
          column(4, selectInput(paste0("axis_label_", id),    "Axis Label From", choices = data_cols, selected = section$axis_label_col)),
          column(3, numericInput(paste0("num_ticks_", id),     "# Ticks", value = section$num_ticks, min = 2)),
          column(3, numericInput(paste0("neutral_pos_", id),   "Neutral Position", value = section$neutral_pos, min = 1)),
          column(2, numericInput(paste0("cat_height_", id),    "Cat. Height (npc)", value = section$category_height, step = 0.01))
        ),
        hr(),
        h5("Header Columns"),
        p("Define the columns that appear in the header for this section."),
        header_cols_ui,
        hr(),
        fluidRow(
          column(6, textInput(paste0("new_col_name_", id), "New Column Name", placeholder = "e.g., 'P-Value'")),
          column(6, actionButton(paste0("add_col_", id), "Add Header Column", icon = icon("plus"), class = "btn-success", style = "margin-top: 25px;"))
        ),
        hr(),
        actionButton(paste0("remove_section_", id), "Remove Section", icon = icon("trash"), class = "btn-danger")
      )
    })
  })
  
  # Add new endpoint section
  observeEvent(input$add_endpoint_section, {
    sections <- endpoint_sections()
    new_id <- if (length(sections) > 0) max(sapply(sections, `[[`, "id")) + 1 else 1
    sections[[length(sections) + 1]] <- list(
      id = new_id, title = paste("Endpoint Section", new_id), data_source = "mock_benefit",
      axis_num = 1, axis_label_col = "estimator", split_box_by_col = "endpoint", split_axis_by_col = "axis_number",
      num_ticks = 6, neutral_pos = 1, category_height = 0.05,
      header_cols = list(
        "Endpoint" = list(source = "endpoint", width = 0.2, collapse = FALSE, sep_line = TRUE, size = 1),
        "Treatment" = list(source = "treatment", width = 0.1, collapse = FALSE, sep_line = FALSE, size = 1),
        "Placebo" = list(source = "placebo",  width = 0.1, collapse = FALSE, sep_line = TRUE, size = 1)
      )
    )
    endpoint_sections(sections)
  })
  
  # ------------------------------
  # Guarded add-column observers registry (prevents duplicate observers)
  # ------------------------------
  registered_add_handlers <- reactiveVal(character())
  norm_name <- function(x) tolower(trimws(ifelse(is.null(x), "", as.character(x))))
  
  # ------------------------------
  # Dynamic observers per section
  # ------------------------------
  observe({
    lapply(endpoint_sections(), function(section) {
      id <- section$id
      
      # --- Section property observers ---
      observeEvent(input[[paste0("title_", id)]], ignoreInit = TRUE, {
        current_sections <- endpoint_sections(); idx <- which(sapply(current_sections, `[[`, "id") == id)
        if (length(idx) == 1) { current_sections[[idx]]$title <- input[[paste0("title_", id)]]; endpoint_sections(current_sections) }
      })
      
      observeEvent(input[[paste0("data_source_", id)]], ignoreInit = TRUE, {
        current_sections <- endpoint_sections(); idx <- which(sapply(current_sections, `[[`, "id") == id)
        if (length(idx) == 1) { current_sections[[idx]]$data_source <- input[[paste0("data_source_", id)]]; endpoint_sections(current_sections) }
      })
      
      observeEvent(input[[paste0("split_axis_by_", id)]], ignoreInit = TRUE, {
        current_sections <- endpoint_sections(); idx <- which(sapply(current_sections, `[[`, "id") == id)
        if (length(idx) == 1) { current_sections[[idx]]$split_axis_by_col <- input[[paste0("split_axis_by_", id)]]; endpoint_sections(current_sections) }
      })
      
      observeEvent(input[[paste0("split_box_by_", id)]], ignoreInit = TRUE, {
        current_sections <- endpoint_sections(); idx <- which(sapply(current_sections, `[[`, "id") == id)
        if (length(idx) == 1) { current_sections[[idx]]$split_box_by_col <- input[[paste0("split_box_by_", id)]]; endpoint_sections(current_sections) }
      })
      
      observeEvent(input[[paste0("axis_label_", id)]], ignoreInit = TRUE, {
        current_sections <- endpoint_sections(); idx <- which(sapply(current_sections, `[[`, "id") == id)
        if (length(idx) == 1) { current_sections[[idx]]$axis_label_col <- input[[paste0("axis_label_", id)]]; endpoint_sections(current_sections) }
      })
      
      observeEvent(input[[paste0("num_ticks_", id)]], ignoreInit = TRUE, {
        current_sections <- endpoint_sections(); idx <- which(sapply(current_sections, `[[`, "id") == id)
        if (length(idx) == 1) { current_sections[[idx]]$num_ticks <- input[[paste0("num_ticks_", id)]]; endpoint_sections(current_sections) }
      })
      
      observeEvent(input[[paste0("neutral_pos_", id)]], ignoreInit = TRUE, {
        current_sections <- endpoint_sections(); idx <- which(sapply(current_sections, `[[`, "id") == id)
        if (length(idx) == 1) { current_sections[[idx]]$neutral_pos <- input[[paste0("neutral_pos_", id)]]; endpoint_sections(current_sections) }
      })
      
      observeEvent(input[[paste0("cat_height_", id)]], ignoreInit = TRUE, {
        current_sections <- endpoint_sections(); idx <- which(sapply(current_sections, `[[`, "id") == id)
        if (length(idx) == 1) { current_sections[[idx]]$category_height <- input[[paste0("cat_height_", id)]]; endpoint_sections(current_sections) }
      })
      
      # --- Guarded Add Header Column (bind once per section) ---
      add_key <- paste0("add_col_", id)
      if (!add_key %in% registered_add_handlers()) {
        observeEvent(input[[add_key]], ignoreInit = TRUE, {
          current_sections <- endpoint_sections(); idx <- which(sapply(current_sections, `[[`, "id") == id)
          req(length(idx) == 1)
          new_raw <- input[[paste0("new_col_name_", id)]]
          new_norm <- norm_name(new_raw)
          if (identical(new_norm, "")) { showNotification("Please enter a column name.", type = "message"); return() }
          existing_norm <- norm_name(names(current_sections[[idx]]$header_cols))
          if (new_norm %in% existing_norm) {
            showNotification("Column name already exists in this section.", type = "warning"); return()
          }
          current_sections[[idx]]$header_cols[[new_raw]] <- list(source = "", width = 0.1, collapse = FALSE, sep_line = TRUE, size = 1)
          endpoint_sections(current_sections)
          updateTextInput(session, paste0("new_col_name_", id), value = "")
        })
        registered_add_handlers(c(registered_add_handlers(), add_key))
      }
      
      # --- Existing header cols observers ---
      if (!is.null(section$header_cols)) {
        lapply(names(section$header_cols), function(col_name) {
          col_id <- make.unique(gsub("[^A-Za-z0-9]", "_", col_name))
          section_col_id <- paste(id, col_id, sep = "_")
          
          observeEvent(input[[paste0("source_", section_col_id)]], ignoreInit = TRUE, ignoreNULL = FALSE, {
            current_sections <- endpoint_sections(); idx <- which(sapply(current_sections, `[[`, "id") == id)
            if (length(idx) == 1) { current_sections[[idx]]$header_cols[[col_name]]$source <- input[[paste0("source_", section_col_id)]]; endpoint_sections(current_sections) }
          })
          observeEvent(input[[paste0("width_", section_col_id)]],  ignoreInit = TRUE, {
            current_sections <- endpoint_sections(); idx <- which(sapply(current_sections, `[[`, "id") == id)
            if (length(idx) == 1) { current_sections[[idx]]$header_cols[[col_name]]$width  <- input[[paste0("width_",  section_col_id)]]; endpoint_sections(current_sections) }
          })
          observeEvent(input[[paste0("collapse_", section_col_id)]], ignoreInit = TRUE, {
            current_sections <- endpoint_sections(); idx <- which(sapply(current_sections, `[[`, "id") == id)
            if (length(idx) == 1) { current_sections[[idx]]$header_cols[[col_name]]$collapse <- input[[paste0("collapse_", section_col_id)]]; endpoint_sections(current_sections) }
          })
          observeEvent(input[[paste0("sepline_", section_col_id)]],  ignoreInit = TRUE, {
            current_sections <- endpoint_sections(); idx <- which(sapply(current_sections, `[[`, "id") == id)
            if (length(idx) == 1) { current_sections[[idx]]$header_cols[[col_name]]$sep_line <- input[[paste0("sepline_",  section_col_id)]]; endpoint_sections(current_sections) }
          })
          observeEvent(input[[paste0("size_", section_col_id)]],    ignoreInit = TRUE, {
            current_sections <- endpoint_sections(); idx <- which(sapply(current_sections, `[[`, "id") == id)
            if (length(idx) == 1) { current_sections[[idx]]$header_cols[[col_name]]$size    <- input[[paste0("size_",    section_col_id)]]; endpoint_sections(current_sections) }
          })
          observeEvent(input[[paste0("remove_col_", section_col_id)]], {
            current_sections <- endpoint_sections(); idx <- which(sapply(current_sections, `[[`, "id") == id)
            if (length(idx) == 1) { current_sections[[idx]]$header_cols[[col_name]] <- NULL; endpoint_sections(current_sections) }
          })
        })
      }
      
      # --- Remove section + cleanup registered handler ---
      observeEvent(input[[paste0("remove_section_", id)]], {
        current_sections <- endpoint_sections()
        sections_to_keep <- Filter(function(s) s$id != id, current_sections)
        endpoint_sections(sections_to_keep)
        # cleanup handler key
        add_key <- paste0("add_col_", id)
        registered_add_handlers(setdiff(registered_add_handlers(), add_key))
      })
    })
  })
  
  # ------------------------------
  # Settings tab: Reset buttons
  # ------------------------------
  observe({
    option_names <- names(default_options)
    lapply(option_names, function(opt_name) {
      observeEvent(input[[paste0("reset_", opt_name)]], {
        default_val <- default_options[[opt_name]]
        if (is.numeric(default_val)) updateNumericInput(session, opt_name, value = default_val)
      })
    })
  })
  

  # Convert simple escape sequences typed in textInput to actual characters
  unescape_specials <- function(x) {
    if (is.null(x)) return("")
    x <- as.character(x)
    x <- gsub("\\\\n", "\n", x, perl = TRUE)
    x <- gsub("\\\\t", "\t", x, perl = TRUE)
    x
  }

  # Build a fresh, side-effect-free page_options from current UI inputs
  build_page_options <- function() {
    vals <- sapply(global_setting_ids, function(id) input[[id]], simplify = FALSE)
    opts <- brrr::page_options$new()
    for (nm in names(vals)) {
      v <- vals[[nm]]
      if (is.null(v)) next
      if (nm == "plot_width") {
        try(opts$set_option("plot.width", v), silent = TRUE)
      } else if (nm == "plot_height") {
        try(opts$set_option("plot.height", v), silent = TRUE)
      } else {
        try(opts$set_option(nm, v), silent = TRUE)
      }
    }
    # Ensure PAGE_TOP_MARGIN is exactly the UI value
    if (!is.null(input$PAGE_TOP_MARGIN))
      try(opts$set_option("PAGE_TOP_MARGIN", as.numeric(input$PAGE_TOP_MARGIN)), silent = TRUE)
    opts
  }

  # ------------------------------
  # Plot parameter aggregator
  # ------------------------------
  plot_params <- reactive({
    list(
      endpoints = endpoint_sections(),
      options   = build_page_options(),
      data      = reactiveValuesToList(all_data)
    )
  })
  
  # ------------------------------
  # Drawing and rendering
  # ------------------------------
  draw_br_plot <- function(params) {
    grid.newpage()
    opts <- params$options

    opts <- params$options
    # Clone if available to avoid mutating the original options object
    if (is.list(opts) && !is.null(opts$clone) && is.function(opts$clone)) {
      opts <- opts$clone(deep = TRUE)
    }
    # Force PAGE_TOP_MARGIN to current UI value for this draw
    if (!is.null(input$PAGE_TOP_MARGIN))
      try(opts$set_option("PAGE_TOP_MARGIN", as.numeric(input$PAGE_TOP_MARGIN)), silent = TRUE)

    # Build arrow labels from UI (convert literal `\\n` to real newlines)
    arrow_labels_input <- c(input$header_arrow_label_treatment %||% "",
                            input$header_arrow_label_comparator %||% "")
    if (all(nchar(arrow_labels_input) == 0)) {
      arrow_labels <- c("Favors\nTreatment", "Favors\nPlacebo")
    } else {
      arrow_labels <- vapply(arrow_labels_input, unescape_specials, FUN.VALUE = character(1))
    }

    sections <- params$endpoints
    req(length(sections) > 0)
    
    print(opts)
    
    # Chain by passing the *entire* returned object from plot_br() to the next call
    last_plot_object <- NULL
    
    for (i in seq_along(sections)) {
      section <- sections[[i]]
      cols <- section$header_cols; req(length(cols) > 0)
      columns_specs_vec    <- setNames(sapply(cols, `[[`, "source"), names(cols))
      breaks_widths_vec    <- sapply(names(cols), function(name) {
        col <- cols[[name]]
        ifelse(isFALSE(col$sep_line), -abs(col$width), abs(col$width))
      })
      value_collapse_vec   <- sapply(cols, `[[`, "collapse")
      header_text_size_vec <- sapply(cols, `[[`, "size")
      
      req(section$data_source, section$split_axis_by_col, section$axis_label_col, section$split_box_by_col)
      plot_data <- params$data[[section$data_source]]; req(plot_data)
      
      # Per-section box category height
      opts$set_option('box.category.height', unit(section$category_height, 'npc'))
      
      current_plot_object <- tryCatch({
        plot_br(
          data = plot_data,
          columns_specs = columns_specs_vec,
          breaks_widths = breaks_widths_vec,
          split_axis_by_col = section$split_axis_by_col,
          axis_labels_col = section$axis_label_col,
          split_box_by_col = section$split_box_by_col,
          neutral_pos = section$neutral_pos,
          num_ticks = section$num_ticks,
          value_collapse = value_collapse_vec,
          header_text_size = header_text_size_vec,
          box_group = last_plot_object,
          arrow_labels = arrow_labels,
          options_br = opts
        )
      }, error = function(e) {
        grid.text(paste("Error plotting section:", section$title, "\n", e$message), gp = gpar(col = "red", fontsize = 14))
        NULL
      })
      
      if (!is.null(current_plot_object)) {
        last_plot_object <- current_plot_object
      }
      # continue; do not return early so all sections draw
    }
  }
  
  output$br_plot <- renderPlot({ draw_br_plot(plot_params()) })
  
  output$downloadPlot <- downloadHandler(
    filename = function() paste0("brrr-plot-", Sys.Date(), ".svg"),
    content  = function(file) {
      # Fresh params built from UI at click time
      params_dl <- list(
        endpoints = endpoint_sections(),
        options   = build_page_options(),
        data      = reactiveValuesToList(all_data)
      )
      opts <- params_dl$options

      # Respect explicit plot size from options/UI
      svg_w <- as.numeric(opts$get_option("plot.width"))
      svg_h <- as.numeric(opts$get_option("plot.height"))

      # Keep options/device in sync
      try(opts$set_option("plot.width",  svg_w), silent = TRUE)
      try(opts$set_option("plot.height", svg_h), silent = TRUE)

      # Ensure PAGE_TOP_MARGIN is exactly the UI value at export time
      if (!is.null(input$PAGE_TOP_MARGIN))
        try(opts$set_option("PAGE_TOP_MARGIN", as.numeric(input$PAGE_TOP_MARGIN)), silent = TRUE)

      svglite::svglite(file, width = svg_w, height = svg_h)
      on.exit(dev.off(), add = TRUE)
      draw_br_plot(params_dl)  # uses cloned options internally
    }
  )
  
  # Utility: safely pull a column name that might not exist
  pull_safe <- function(df, col) if (col %in% names(df)) df[[col]] else NULL
  
  
  # Populate description-column choices from the union of columns across all section data sources
  observe({
    secs <- endpoint_sections(); req(length(secs) > 0)
    ds_names <- unique(vapply(secs, function(s) s$data_source, FUN.VALUE = character(1)))
    cols <- unique(unlist(lapply(ds_names, function(nm) names(all_data[[nm]]))))
    cols <- sort(cols[!is.na(cols)])
    sel <- isolate(input$effects_desc_col)
    updateSelectInput(session, "effects_desc_col", choices = c("— none —" = "", cols), selected = if (!is.null(sel)) sel else "")
  })
  
  # Aggregated effects_df across all sections, with optional description column
  effects_df <- reactive({
    secs <- endpoint_sections(); req(length(secs) > 0)
    desc_on  <- isTRUE(input$effects_include_desc)
    desc_col <- input$effects_desc_col %||% ""
    
    rows <- lapply(secs, function(sec) {
      ds_name   <- sec$data_source;      req(ds_name)
      split_box <- sec$split_box_by_col; req(split_box)
      axis_lab  <- sec$axis_label_col;   req(axis_lab)
      dat <- isolate(plot_params())$data[[ds_name]]; req(dat)
      
      value <- if ("value" %in% names(dat)) dat[["value"]] else NULL
      lower <- if ("lower" %in% names(dat)) dat[["lower"]] else NULL
      upper <- if ("upper" %in% names(dat)) dat[["upper"]] else NULL
      est   <- if (axis_lab %in% names(dat)) dat[[axis_lab]] else NA_character_
      endp  <- if (split_box %in% names(dat)) dat[[split_box]] else NA_character_
      
      ci_str <- if (!is.null(lower) && !is.null(upper)) sprintf("[%s, %s]", signif(lower, 3), signif(upper, 3)) else NA_character_
      eff    <- if (!is.null(value)) signif(value, 3) else NA_real_
      
      df <- data.frame(
        Section  = sec$title %||% paste("Section", sec$id),
        Endpoint = endp,
        Estimator= est,
        Effect   = eff,
        CI95     = ci_str,
        stringsAsFactors = FALSE
      )
      if (desc_on) {
        if (nzchar(desc_col) && desc_col %in% names(dat)) {
          df$Description <- as.character(dat[[desc_col]])
        } else {
          df$Description <- ""
        }
      }
      df
    })
    
    out <- dplyr::bind_rows(rows)
    out <- unique(out)
    out <- out[order(out$Section, out$Endpoint, out$Estimator), , drop = FALSE]
    
    # Faux-rowspan effect: put the Section label only on the first row of each group
    out$SectionLabel <- ""
    if (nrow(out) > 0) {
      first_in_group <- !duplicated(out$Section)
      out$SectionLabel[first_in_group] <- out$Section[first_in_group]
    }
    out
  })
  
  # Render gt table
  output$effects_gt <- render_gt({
    d <- effects_df()
    # Columns for display
    show_cols <- c("SectionLabel", "Endpoint",
                   if (isTRUE(input$effects_show_measure)) "Estimator" else NULL,
                   "Effect",
                   if (isTRUE(input$effects_show_ci)) "CI95" else NULL,
                   if (isTRUE(input$effects_include_desc) && nzchar(input$effects_desc_col %||% "")) "Description" else NULL)
    dat <- d[, show_cols, drop = FALSE]
    
    tbl <- gt::gt(dat) |>
      gt::tab_header(
        title = gt::md(input$effects_title %||% "Effects Table"),
        subtitle = if (nzchar(input$effects_subtitle)) gt::md(input$effects_subtitle) else NULL
      )
    
    # Build labels only for columns that exist (handles toggling Estimator/CI95/Description)
    label_lookup <- c(
      SectionLabel = "Section",
      Endpoint     = "Endpoint",
      Estimator    = "Effect measure",
      Effect       = "Effect",
      CI95         = "95% CI",
      Description  = "Description"
    )
    present <- intersect(names(label_lookup), names(dat))
    lab_map <- as.list(unname(label_lookup[present]))
    names(lab_map) <- present
    if (length(lab_map)) {
      tbl <- do.call(gt::cols_label, c(list(tbl), lab_map))
    }
    tbl <- tbl |>
      gt::fmt_missing(columns = tidyselect::everything(), missing_text = "—") |>
      gt::tab_options(table.font.size = gt::px(13), data_row.padding = gt::px(6))
    
    if (nzchar(input$effects_footnote)) {
      tbl <- tbl |> gt::tab_footnote(
        locations = gt::cells_title(groups = "title"),
        footnote  = gt::md(input$effects_footnote)
      )
    }
    
    tbl
  })
  
  
  # Downloads
  output$dl_effects_html <- downloadHandler(
    filename = function() sprintf("effects_table_%s.html", Sys.Date()),
    content = function(file) {
      d <- effects_df()
      export_cols <- c("Section", "Endpoint",
                       if (isTRUE(input$effects_show_measure)) "Estimator" else NULL,
                       "Effect",
                       if (isTRUE(input$effects_show_ci)) "CI95" else NULL,
                       if (isTRUE(input$effects_include_desc) && nzchar(input$effects_desc_col %||% "")) "Description" else NULL)
      dat <- d[ , export_cols, drop = FALSE]
      tbl <- gt(dat) |>
        tab_header(
          title = md(input$effects_title %||% "Effects Table"),
          subtitle = if (nzchar(input$effects_subtitle)) md(input$effects_subtitle) else NULL
        )
      label_lookup <- c(
        Section   = "Section",
        Endpoint  = "Endpoint",
        Estimator = "Effect measure",
        Effect    = "Effect",
        CI95      = "95% CI",
        Description = "Description"
      )
      present <- intersect(names(label_lookup), names(dat))
      lab_map <- as.list(unname(label_lookup[present]))
      names(lab_map) <- present
      if (length(lab_map)) {
        tbl <- do.call(cols_label, c(list(tbl), lab_map))
      }
      gtsave(tbl, file)
    }
  )
  
  output$dl_effects_csv <- downloadHandler(
    filename = function() sprintf("effects_table_%s.csv", Sys.Date()),
    content = function(file) {
      d <- effects_df()
      export_cols <- c("Section", "Endpoint",
                       if (isTRUE(input$effects_show_measure)) "Estimator" else NULL,
                       "Effect",
                       if (isTRUE(input$effects_show_ci)) "CI95" else NULL,
                       if (isTRUE(input$effects_include_desc) && nzchar(input$effects_desc_col %||% "")) "Description" else NULL)
      readr::write_csv(d[ , export_cols, drop = FALSE], file)
    }
  )

}


# --- Run the application ---
shinyApp(ui = ui, server = server)

