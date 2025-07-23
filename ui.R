ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      html, body {
        height: 100%;
        margin: 0;
      }

      #page-wrapper {
        display: flex;
        flex-direction: column;
        min-height: 100vh;
      }

      #main-content {
        flex: 1;
        overflow-y: auto;
        padding-bottom: 60px; /* Add enough space to clear the fixed footer */
      }
      
      body.waiting {
      cursor: wait !important;
      }

      .status-bar {
        position: fixed;
        bottom: 0;
        left: 0;
        right: 0;
        background-color: #f0f0f0;
        padding: 0.5rem 1rem;
        font-size: 14px;
        text-align: left;
        border-top: 1px solid #ccc;
        z-index: 1000;
      }
    "))
  ),
  
  tags$script(HTML("
  Shiny.addCustomMessageHandler('toggleCursor', function(state) {
    if (state === 'wait') {
      document.body.classList.add('waiting');
    } else {
      document.body.classList.remove('waiting');
    }
  });
")),
  
  
  div(id = "page-wrapper",
      div(id = "main-content",
          page_navbar(
            useShinyjs(),
            id = "pages",
            theme = my_theme,
            navbar_options = navbar_options(
              class = "bg-primary",
              theme = "dark"
            ),
            
            nav_panel("About", includeHTML("www/about.html")),
            
            nav_menu("Maintenance",
                     nav_panel(value = "KPIMaint", "KPI Source Data",       
                               fluidRow(
                                 column(12, uiOutput("inputs"))
                               )
                     ),
                     nav_panel(value = "BoardMaint", "Programme Board Data", uiOutput("board_inputs"), uiOutput("board_outputs"))
            ),
            
            nav_panel("Update Objectives", uiOutput("objectives_inputs"), uiOutput("objectives_outputs"), value = "objectivespage"),
            nav_panel("Dashboard", uiOutput("dashboard_inputs"), uiOutput("dashboard_outputs"), value = "dashboardspage"),
            
            nav_spacer(),
            
            nav_item(
              tooltip(
                actionLink("help_modal", label = icon("circle-question")),
                "Help",
                placement = "bottom"
              )
            ),
            
            nav_item(
              tooltip(
                tags$a(
                  icon("envelope"),
                  href = "mailto:robert.worth@defra.gov.uk?subject=Plant Health Dashboard Solution Feedback",
                  style = "padding: 0.5rem; font-size: 1.2rem;"
                ),
                "Contact Us",
                placement = "bottom"
              )
            )
          )
      ),
      uiOutput("status_bar")  # Moved outside the navbar
  )
)
