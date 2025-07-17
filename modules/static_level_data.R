# modules/static_level_data.R

static_level_data_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "static_level_data",
    fluidRow(
      box(width = 12, status = "info",
        title = "Static Level Data Filters",
        column(2, uiOutput(ns("balance_patch_filter"))),
        column(2, uiOutput(ns("blocker_type_filter"))),
        column(2, uiOutput(ns("feature_introduced_filter"))),
        column(2, uiOutput(ns("move_limit_filter"))),
        column(2, uiOutput(ns("objective_type_filter"))),
        column(2, uiOutput(ns("objective_amount_filter"))),
        column(2, uiOutput(ns("level_number_filter")))
      )
    ),
    fluidRow(
      box(width = 12, status = "primary", DT::dataTableOutput(ns("static_dt_table")))
    )
  )
}

static_level_data_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Helper to get unique values safely
    get_choices <- function(col) {
      if (!is.null(data) && col %in% names(data)) {
        sort(unique(na.omit(data[[col]])))
      } else {
        character(0)
      }
    }

    output$balance_patch_filter <- renderUI({
      selectizeInput(ns("balance_patch"), "Balance Patch", choices = get_choices("balance_patch_version"), multiple = TRUE)
    })
    output$blocker_type_filter <- renderUI({
      selectizeInput(ns("blocker_type"), "Blocker Type", choices = get_choices("blocker_type"), multiple = TRUE)
    })
    output$feature_introduced_filter <- renderUI({
      selectizeInput(ns("feature_introduced"), "Feature Introduced", choices = get_choices("feature_introduced"), multiple = TRUE)
    })
    output$move_limit_filter <- renderUI({
      selectizeInput(ns("move_limit"), "Move Limit", choices = get_choices("move_limit"), multiple = TRUE)
    })
    output$objective_type_filter <- renderUI({
      selectizeInput(ns("objective_type"), "Objective Type", choices = get_choices("objective_type"), multiple = TRUE)
    })
    output$objective_amount_filter <- renderUI({
      selectizeInput(ns("objective_amount"), "Objective Amount", choices = get_choices("objective_amount"), multiple = TRUE)
    })
    output$level_number_filter <- renderUI({
      selectizeInput(ns("level_number"), "Level Number", choices = get_choices("level_number"), multiple = TRUE)
    })

    filtered_data <- reactive({
      df <- data
      req(df)
      if (!is.null(input$balance_patch) && length(input$balance_patch) > 0) {
        df <- df[df$balance_patch_version %in% input$balance_patch, ]
      }
      if (!is.null(input$blocker_type) && length(input$blocker_type) > 0) {
        df <- df[df$blocker_type %in% input$blocker_type, ]
      }
      if (!is.null(input$feature_introduced) && length(input$feature_introduced) > 0) {
        df <- df[df$feature_introduced %in% input$feature_introduced, ]
      }
      if (!is.null(input$move_limit) && length(input$move_limit) > 0) {
        df <- df[df$move_limit %in% input$move_limit, ]
      }
      if (!is.null(input$objective_type) && length(input$objective_type) > 0) {
        df <- df[df$objective_type %in% input$objective_type, ]
      }
      if (!is.null(input$objective_amount) && length(input$objective_amount) > 0) {
        df <- df[df$objective_amount %in% input$objective_amount, ]
      }
      if (!is.null(input$level_number) && length(input$level_number) > 0) {
        df <- df[df$level_number %in% input$level_number, ]
      }
      df
    })

    output$static_dt_table <- DT::renderDataTable({
      df <- filtered_data()
      req(df)
      DT::datatable(df, options = list(pageLength = 50, scrollX = TRUE))
    })
  })
} 