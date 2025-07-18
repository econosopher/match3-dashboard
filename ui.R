# ui.R

ui <- fluidPage(
    # Add this to use shinyjs
    shinyjs::useShinyjs(),

    # Password input UI
    div(
        id = "login",
        style = "width: 500px; margin: 0 auto; padding-top: 100px;",
        wellPanel(
            h2("Please log in", class = "text-center", style = "padding-top: 0;"),
            passwordInput("password", "Password:"),
            br(),
            actionButton("login_button", "Log in", class = "btn-primary btn-block")
        )
    ),

    # Main UI (hidden by default)
    shinyjs::hidden(
        div(
            id = "main_content",
            dashboardPage(
                dashboardHeader(title = "Match-3 Balancing"),
                dashboardSidebar(
                    sidebarMenu(
                        id = "tabs",
                        menuItem("Difficulty Analysis", tabName = "difficulty_analysis", icon = icon("chart-line")),
                        menuItem("Core Gameplay", tabName = "core_gameplay", icon = icon("gamepad")),
                        menuItem("Economy Analysis", tabName = "economy_analysis", icon = icon("dollar-sign")),
                        menuItem("Sequence Analysis", tabName = "sequence_analysis", icon = icon("sort-numeric-up")),
                        menuItem("Level Metrics", tabName = "level_metrics", icon = icon("table")),
                        menuItem("Custom Analysis", tabName = "custom_analysis", icon = icon("search")),
                        menuItem("Static Level Data", tabName = "static_level_data", icon = icon("database")),

                        # Add the balance patch filter UI component
                        uiOutput("balance_patch_filter_ui"),
                        uiOutput("difficulty_filter_ui"),
                        uiOutput("level_range_slider")
                    )
                ),
                dashboardBody(
                    tags$head(
                        tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;600;700&display=swap"),
                        tags$style(HTML("
                            body, .content-wrapper, .right-side, .main-sidebar, .main-header .navbar, .main-header .logo, .box, h1, h2, h3, h4, h5, h6, .shiny-input-container, .selectize-input, .selectize-dropdown {
                                font-family: 'Inter', 'Lato', 'Helvetica Neue', Helvetica, Arial, sans-serif !important;
                            }
                            body, .content-wrapper, .right-side, .main-sidebar, .main-header .navbar, .main-header .logo {
                                background-color: #F7F9FB !important;
                                color: #222;
                            }
                            .box, .shiny-input-container, .selectize-input, .selectize-dropdown {
                                 background-color: #FFFFFF !important;
                            }
                            .main-header .logo { font-weight: 700; color: #222 !important; }
                            .main-header .navbar .sidebar-toggle, .main-header .navbar .nav > li > a { color: #222 !important; }
                            .skin-blue .main-sidebar .sidebar .sidebar-menu > li > a,
                            .skin-blue .main-sidebar .sidebar .sidebar-menu .treeview-menu > li > a {
                                color: #222 !important;
                                font-weight: 400;
                            }
                            .skin-blue .main-sidebar .sidebar .sidebar-menu .active > a,
                            .skin-blue .main-sidebar .sidebar .sidebar-menu > li:hover > a,
                            .skin-blue .main-sidebar .sidebar .sidebar-menu .treeview-menu > .active > a,
                            .skin-blue .main-sidebar .sidebar .sidebar-menu .treeview-menu > li:hover > a {
                                background-color: #E3E8EF !important;
                                color: #0072B2 !important;
                                font-weight: 700;
                            }
                            .box {
                                border-top: 3px solid #0072B2;
                                background: #FFFFFF !important;
                            }

                            /* Force the background of the level metrics tab content area to be white */
                            #shiny-tab-level_metrics {
                                background-color: #FFFFFF !important;
                            }

                            .box-body { padding: 0 6px 6px 6px !important; }
                            label[for='file1'], .shiny-input-container label, .irs-single, .irs-bar, .irs-bar-edge, .irs-line { color: #222 !important; font-weight: 600; }
                            .skin-blue .main-sidebar .sidebar .sidebar-menu .treeview > a { color: #fff !important; }
                            .skin-blue .main-sidebar .sidebar .sidebar-menu .treeview > a > i { color: #fff !important; }
                            .skin-blue .main-sidebar .sidebar .sidebar-menu .treeview-menu > li > a { color: #fff !important; }
                            .skin-blue .main-sidebar .sidebar .sidebar-menu > li > a > i.fa-chart-line, .skin-blue .main-sidebar .sidebar .sidebar-menu > li > a:has(.fa-chart-line) { color: #222 !important; }
                            .skin-blue .main-sidebar .sidebar .sidebar-menu > li > a:has(.fa-chart-line) { color: #222 !important; font-weight: 700; }
                        "))
                    ),
                    # --- Only show missing columns warning in body ---
                    uiOutput("csv_check_results"),
                    tabItems(
                        tabItem(tabName = "static_level_data", static_level_data_ui("static_level_data")),
                        tabItem(tabName = "difficulty_analysis", difficulty_analysis_ui("difficulty")),
                        tabItem(tabName = "core_gameplay", core_gameplay_ui("core_gameplay")),
                        tabItem(tabName = "economy_analysis", economy_analysis_ui("economy")),
                        tabItem(tabName = "sequence_analysis", sequence_analysis_ui("sequence")),
                        tabItem(tabName = "level_metrics", level_metrics_table_ui("level_metrics")),
                        tabItem(tabName = "custom_analysis", custom_analysis_ui("custom_analysis"))
                    )
                ),
                skin = "blue"
            )
        )
    )
)
