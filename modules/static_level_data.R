# modules/static_level_data.R

static_level_data_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "static_level_data",
    fluidRow(
      box(width = 12, status = "info",
        title = "Static Level Data Filters",
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
      df <- data() # Correctly access reactive data
      if (!is.null(df) && col %in% names(df)) {
        sort(unique(na.omit(df[[col]])))
      } else {
        character(0)
      }
    }

    output$blocker_type_filter <- renderUI({
      selectizeInput(ns("blocker_type"), "Blocker Type", choices = NULL, multiple = TRUE,
                     options = list(placeholder = 'Select a blocker type...'))
    })
    observe({
      updateSelectizeInput(session, "blocker_type", choices = get_choices("blocker_type"), server = TRUE)
    })
    
    output$feature_introduced_filter <- renderUI({
      selectizeInput(ns("feature_introduced"), "Feature Introduced", choices = get_choices("feature_introduced"), multiple = TRUE,
                     options = list(placeholder = 'Select a feature...'))
    })
    output$move_limit_filter <- renderUI({
      selectizeInput(ns("move_limit"), "Move Limit", choices = NULL, multiple = TRUE,
                     options = list(placeholder = 'Select a move limit...'))
    })
    observe({
      updateSelectizeInput(session, "move_limit", choices = get_choices("move_limit"), server = TRUE)
    })
    
    output$objective_type_filter <- renderUI({
      selectizeInput(ns("objective_type"), "Objective Type", choices = get_choices("objective_type"), multiple = TRUE,
                     options = list(placeholder = 'Select an objective type...'))
    })
    output$objective_amount_filter <- renderUI({
      selectizeInput(ns("objective_amount"), "Objective Amount", choices = NULL, multiple = TRUE,
                     options = list(placeholder = 'Select an amount...'))
    })
    observe({
      updateSelectizeInput(session, "objective_amount", choices = get_choices("objective_amount"), server = TRUE)
    })

    output$level_number_filter <- renderUI({
      selectizeInput(ns("level_number"), "Level Number", choices = NULL, multiple = TRUE,
                     options = list(placeholder = 'Select a level number...'))
    })
    observe({
      updateSelectizeInput(session, "level_number", choices = get_choices("level_number"), server = TRUE)
    })

    filtered_data <- reactive({
      df <- data() # Correctly access reactive data
      req(df)
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
      DT::datatable(df, options = list(pageLength = 50, scrollX = TRUE),
                    class = 'compact hover row-border stripe',
                    callback = JS(
                      "$(table.table().node()).css({'font-size': '11px'});",
                      "$(table.table().node()).find('td').css({'padding': '4px 2px'});"
                    ))
    })
  })
} 