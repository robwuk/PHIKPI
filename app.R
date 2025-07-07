
# Errors
# objective update save not working

################################################################################
# Next Steps
# double click on dashboard update to show detail
# add a count of "objectives without any update" and add a tab
# export dashboard to PPT  - intro, totals & reds/ambers
# "save complete" box on each save
# check column names throughout
# allow user to show/hide complete actions in dashboard

################################################################################
# Useful functions
# Display Objective Maintenance   objectivedata
# Display Board Maintenance       boarddata
# Display Updates                 objectivespage
# Display Dashboard               dashboardspage

################################################################################
# Change History

#setwd("C:/Users/rw000152/OneDrive - Defra/R/PH_KPI_Tracker")

#Load libraries
library(tidyverse)
library(dplyr)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinymanager)
library(shinyWidgets)
library(data.table)   
library(DT)


linebreaks <- function(n){HTML(strrep(br(), n))}

KPISourceFile <- "data/sources.csv"
KPI_source <- read_csv(KPISourceFile)
KPISourceDate <- file.info(KPISourceFile)$mtime

KPI_type <- data.frame(type = c("Objective", "KPI"))
objectiveStatus <- data.frame(type = c("In Progress", "Not Started", "Blocked", "Completed"))

outcomesFile <- "data/outcomes.csv"
outcomes <- read_csv(outcomesFile)

outcomesDate <- file.info(outcomesFile)$mtime

outcomeGroupsFile <- "data/OutcomeGroups.csv"
outcomegroups <- read_csv(outcomeGroupsFile)
outcomeGroupsDate <- file.info(outcomeGroupsFile)$mtime

outcomeDeliverablesFile <- "data/OutcomeDeliverables.csv"
outcomedeliverables <- read_csv(outcomeDeliverablesFile)
outcomesDeliverablesDate <- file.info(outcomeDeliverablesFile)$mtime

objectivesFile <- "data/ObjectivesKPIs.csv"
objectives <- read_csv(objectivesFile)
objectivesDate <- file.info(objectivesFile)$mtime

boardsFile <- "data/BoardData.csv"
boards <- read_csv(boardsFile)
boardsDate <- file.info(boardsFile)$mtime

boardObjectivesFile <- "data/Board_Objective.csv"
boardobjectives <- read_csv(boardObjectivesFile)
boardObjectivesDate <- file.info(boardObjectivesFile)$mtime

objectiveUpdatesFile <- "data/Objective_Updates.csv"
objectiveUpdates <- read_csv(objectiveUpdatesFile)
objectiveUpdatesDate <- file.info(objectiveUpdatesFile)$mtime

boardActionsFile <- "data/BoardActions.csv"
boardActions <- read_csv(boardActionsFile)
boardActionsDate <- file.info(objectiveUpdatesFile)$mtime

objectiveUpdates$Return_to_Green[is.na(objectiveUpdates$Return_to_Green)] <- ""

Global_Source_Name <- ""
Global_Outcome_ID <- ""
Global_Grouping <- ""
Global_Outcome_Desc <- ""
Global_Board_Acronym <- ""
Global_Objective_ID <- ""
Global_Board_date <- ""
objectives_filtered <- ""
board_objective_detail <- ""
latestUpdate <- ""
objectiveUpdates_filtered <- ""
allLeads <- ""
BoardAction_filtered <- ""

#Make user interface
ui <- dashboardPage(
  skin = "green",
  
  dashboardHeader(title = ("Plant & Bee Health KPI Dashboards")),
  
  dashboardSidebar(
    sidebarMenu(
      
      id="pages",
      
      menuItem(
        "About",
        tabName = "aboutpage", icon = icon("leaf", lib = "font-awesome")
      ),
      
      menuItem("Maintenance", icon = icon("gears", lib="font-awesome"),
               menuItem(
                 "KPI Source Data", 
                 tabName = "objectivedata", icon = icon("sliders", lib="font-awesome")
               ),
               
               menuItem(
                 "Programme Board Data", 
                 tabName = "boarddata", icon = icon("chalkboard-user", lib = "font-awesome")
               )
      ),
      
      menuItem(
        "Update Objectives",
        tabName = "objectivespage", icon = icon("pen-to-square", lib = "font-awesome")
      ),
      
      menuItem(
        "Poject Dashboards",
        tabName = "dashboardspage", icon = icon("gauge-simple-high", lib = "font-awesome")
      )
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    fluidRow(
      tabItems(
        tabItem(
          tabName = "aboutpage",
          uiOutput("about")
        ),
        
        tabItem(
          tabName = "objectivedata",
          uiOutput("inputs"),
          uiOutput("outputs")
        ),
        
        tabItem(
          tabName = "boarddata",
          uiOutput("board_inputs"),
          uiOutput("board_outputs")
        ),
        
        tabItem(
          tabName = "objectivespage",
          uiOutput("objectives_inputs"),
          uiOutput("objectives_outputs")
        ),
        
        tabItem(
          tabName = "dashboardspage",
          uiOutput("dashboard_inputs"),
          uiOutput("dashboard_outputs")
        )
      )
    )
  )
)

#Make server function
server <- function(input, output, session) {
  
  ProductOutcomes <- reactive({outcomes})
  ProductSources <- reactive({KPI_source})
  ProductGroups <- reactive({outcomegroups_filtered})
  ProductObjectives <- reactive({objectives_filtered})
  ProductBoards <- reactive({boards})
  ProductBoardObjectives <- reactive({board_objective_detail})
  ProductBoardActions <- reactive({BoardAction_filtered})
  output$source <- DT::renderDataTable(KPI_source, selection = 'single', rownames = FALSE)
  output$board <- DT::renderDataTable(boards, selection = 'single', rownames = FALSE)
  
  # About screen
  output$about <- renderUI({
    tags$div(class="header", style = "align: center",
             HTML("<br><center><h1>Plant Health Boards</h1></center>"),
             
             HTML("<br>Welcome to the dashboard tool for Plant Health Boards. Here you can set up:<br><br>"),
             HTML("<UL>
                    <LI>Objective Sources</LI>
                    <LI>Objective Groups</LI>
                    <LI>Objectives</LI>
                    <LI>Programme Boards</LI>
                    </UL>"),
             HTML("As well as provide updates for objectives and run board dashboards."),
             
             HTML("<br><br><left><h3>FEEDBACK: </h3>if you have any feedback/comments or need support on 
                  about the solution please contact <a href='mailto:robert.worth@defra.gov.uk?subject=Plant Health dashoboarding Solution Feedback'>Robert Worth</a> ")
    )
  })
  
  # KPI Source Data Maintenance Screen
  output$inputs <- renderUI({
    
    column(
      
      width = 12,
      HTML("<br><br><left><h3>KPI Source, Outcome & Group Maintenance</h3>"),
      HTML("<hr>"),
      
      
      box(
        title = "Objective/KPI Source",
        
        width = 4,
        
        fluidRow(column(width = 12,
                        DT::dataTableOutput("source"))
        ),
        fluidRow(
          column(12, align = "center",
                 actionButton("addSource", "Add New Source", icon = icon("plus")))
        )
      ),  
      
      box(
        title = "Source Outcome",
        
        width = 4,
        fluidRow(column(width = 12,  
                        DT::dataTableOutput("outcomes_filtered"))
        ),
        
        fluidRow(column(width = 12, align = "center",
                        actionButton("addOutcome", "Add New Outcome", icon = icon("plus")))
        )
      ),
      
      
      box(
        title = "Outcome Group",
        
        width = 4,
        fluidRow(column(width = 12,  
                        DT::dataTableOutput("outcomegroups_filtered")),
        ),
        
        fluidRow(column(width = 12, align = "center",
                        actionButton("addGroup", "Add New Source Group", icon = icon("plus")))
        )
      ),
      
      column(
        
        width = 12,
        
        box(
          title = "Objectives/KPIs",
          
          width = 12,
          
          fluidRow(column(width = 12,
                          DT::dataTableOutput("objectives_filtered"))
          ),
          fluidRow(
            column(6, align = "center",
                   actionButton("addObjective", "Add New Objective", icon = icon("plus"))),
            column(6, align = "center",
                   actionButton("editObjective", "Edit Objective", icon = icon("pen-to-square")))
          )
        )
      )
    )
    
  })
  
  # Board Maintenance Screen  
  output$board_inputs <- renderUI({
    
    column(
      
      width = 12,
      HTML("<br><br><left><h3>Board Maintenance</h3>"),
      HTML("<hr>"),
      
      
      box(
        title = "Board Data",
        
        width = 12,
        
        fluidRow(column(width = 12,
                        DT::dataTableOutput("board"))
        ),
        fluidRow(
          column(6, align = "center",
                 actionButton("addBoard", "Add New Board", icon = icon("plus"))),
          column(6, align = "center",
                 actionButton("editBoard", "Edit Board", icon = icon("pen-to-square")))
        )
      ),
      
      box(
        title = "Board Objectives",
        
        width = 12,
        
        fluidRow(column(width = 12,
                        DT::dataTableOutput("board_objective_detail"))
        ),
        fluidRow(
          column(6, align = "center",
                 actionButton("addBoardObjective", "Add Objective", icon = icon("plus"))),
          column(6, align = "center",
                 actionButton("removeBoardObjective", "Remove Objective", icon = icon("minus")))
        )
      )
    )
  })
  
  # Objective Update Screen  
  output$objectives_inputs <- renderUI({
    column(
      
      width = 12,
      HTML("<br><br><left><h3>Provide Objective Update</h3>"),
      HTML("<hr>"),
      
      
      box(
        title = "Select Objective",
        
        width = 12,
        
        fluidRow(column(6,
                        selectInput(
                          inputId = 'projectBoard',
                          label = "Select Project Board:",
                          choices = NULL,
                          selected = NULL,
                          multiple = FALSE
                        )
        ),
        column(6,
               selectInput(
                 inputId = 'projectLead',
                 label = "Select Project Lead:",
                 choices = NULL,
                 selected = NULL,
                 multiple = FALSE
               )
        )
        ),
        
        fluidRow(column(12,
                        selectInput(
                          inputId = 'projectObjectives',
                          label = "Select Objective:",
                          choices = NULL,
                          selected = NULL,
                          multiple = FALSE
                        )
        )
        )
      ),
      
      box(
        title = "Latest Update",
        
        width = 12,
        
        fluidRow(column(6,
                        selectInput(
                          inputId = 'RAGStatus',
                          label = "RAG Status:",
                          choices = c("Red", "Amber", "Green", "Complete"),
                          selected = NULL,
                          multiple = FALSE
                        ),
                        column(12,
                               valueBoxOutput("spacer", width = 3),
                               valueBoxOutput("statusBox", width = 6)
                        )                            
                        #)
        ),
        
        column(6,
               dateInput(
                 inputId = "updateDate",
                 label = "Date of Update:",
                 value = Sys.Date(),       # Default value: today
                 format = "dd/mm/yyyy",    # Display format
                 min = as.Date("2024-01-01"),       # Earliest selectable date
                 max = as.Date("2030-12-31"),       # Latest selectable date
                 startview = "month",      # Initial view: "month", "year", or "decade"
                 weekstart = 1             # Week starts on Monday
               )),
        ),
        
        fluidRow(column(6,
                        textAreaInput("latestUpdate", "Update:",
                                      width = "100%", 
                                      height = "200px")),
                 column(6,
                        textAreaInput("returnToGreen", "Return to Green Plan:",
                                      width = "100%", 
                                      height = "200px")),
        ),
        fluidRow(
          column(3, align = "center",
                 actionButton("editObjectiveUpdate", "Edit Update", icon = icon("pen-to-square"))),
          column(3, align = "center",
                 actionButton("addObjectiveUpdate", "Add Update", icon = icon("plus"))),
          column(3, align = "center",
                 actionButton("undoObjectiveUpdate", "Undo Changes", icon = icon("undo"))),
          column(3, align = "center",
                 actionButton("saveObjectiveUpdate", "Save Changes", icon = icon("save")))
        )
      ),
      
      box(
        title = "Previous Updates",
        
        width = 12,
        
        fluidRow(column(width = 12,
                        DT::dataTableOutput("objectiveUpdates_filtered"))
        ),
      )
    )
  })
  
  # Dashboard Screen  
  output$dashboard_inputs <- renderUI({
    column(
      
      width = 12,
      HTML("<br><br><left><h3>Programme Dashboard</h3>"),
      HTML("<hr>"),
      
      
      box(
        title = "Select Board:",
        
        width = 3,
        
        fluidRow(column(12,
                        selectInput(
                          inputId = 'dashBoard',
                          label = "Select Project Board:",
                          choices = NULL,
                          selected = NULL,
                          multiple = FALSE
                        )
        )
        ),
        fluidRow(column(12,
                        dateInput(
                          inputId = "boardDate",
                          label = "Board Date:",
                          value = Sys.Date(),       # Default value: today
                          format = "dd/mm/yyyy",    # Display format
                          min = as.Date("2024-01-01"),       # Earliest selectable date
                          max = as.Date("2030-12-31"),       # Latest selectable date
                          startview = "month",      # Initial view: "month", "year", or "decade"
                          weekstart = 1             # Week starts on Monday
                        )
        )
        )
      ),
      
      
      box(
        title = "Board Overview:",
        width = 9,
        
        fluidRow(
          valueBoxOutput("numberOfActions", width = 2),
          valueBoxOutput("numberOfRisks", width = 2),
          valueBoxOutput("numberOfRed", width = 2),
          valueBoxOutput("numberOfAmber", width = 2),
          valueBoxOutput("numberOfGreen", width = 2),
          valueBoxOutput("numberOfComplete", width = 2)
        )
      ),
      
      tabBox(
        title = "",
        id = "outerTabBox",
        width = 12,
        tabPanel("Actions", 
                 fluidRow(
                   box(
                     width = 12,
                     DT::dataTableOutput("BoardActions")
                   ),
                 ), 
                 fluidRow(
                   column(6, align = "center",
                          actionButton("editBoardAction", "Edit Action", icon = icon("pen-to-square"))),
                   column(6, align = "center",
                          actionButton("addBoardAction", "Add Action", icon = icon("plus")))
                 )
        ),
        tabPanel("Updates",
                 fluidRow(
                   box(
                     width = 12,
                     tabBox(
                       title = "",
                       id="tabs",
                       width=12,
                       tabPanel(tagList(icon("circle-xmark"), "Red"), DT::dataTableOutput("Red_Updates")),
                       tabPanel(tagList(icon("circle-exclamation"), "Amber"), DT::dataTableOutput("Amber_Updates")),
                       tabPanel(tagList(icon("circle-check"), "Green"), DT::dataTableOutput("Green_Updates")),
                       tabPanel(tagList(icon("check-double"), "Complete"), DT::dataTableOutput("Complete_Updates")),
                     )
                   )
                 )
        ),
        tabPanel("Risks", 
                 fluidRow(
                   box(
                     width = 12,
                     DT::dataTableOutput("BoardRisks")
                   ),
                 ), 
                 fluidRow(
                   column(6, align = "center",
                          actionButton("editBoardRisk", "Edit Risk", icon = icon("pen-to-square"))),
                   column(6, align = "center",
                          actionButton("addBoardRisk", "Add Risk", icon = icon("plus")))
                 )
        )
      )
    )
  })
  
  #Update dropdowns
  observeEvent(input$pages,{
    allBoards <- boards
    
    allBoards <- semi_join(allBoards, boardobjectives, by = "Board_Acronym")
    
    allBoards <- setorder(allBoards, Board_Acronym)
    
    if (input$pages == "objectivespage") {
      updateSelectizeInput(
        session, "projectBoard",
        choices = setNames(allBoards$Board_Acronym, allBoards$Board_Name),
        selected = NULL,
        server = TRUE
      )
    }  else if (input$pages == "dashboardspage") {
      updateSelectizeInput(
        session, "dashBoard",
        choices = setNames(allBoards$Board_Acronym, allBoards$Board_Name),
        selected = NULL,
        server = TRUE
      )
    }
  })
  
  
  # Add Source Data
  observeEvent(input$addSource, {
    showModal(modalDialog(
      title = "New Source",
      textInput("user_input", "Enter Source Name:", ""),
      uiOutput("error_message"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("submit_source_data", "Save")
      )
    ))
    output$error_message <- renderUI({
      div(style = "color: red; font-weight: bold;", "")
    })
  })
  
  # Save Add Source Data    
  observeEvent(input$submit_source_data, {
    
    if (input$user_input == "") {
      output$error_message <- renderUI({
        div(style = "color: red; font-weight: bold;", "Source cannot be empty")
      })
    } else { 
      
      if (checkFileDates("KPI")) {
        KPI_source <- read_csv(KPISourceFile)
      }
      
      KPI_duplicate <- filter(KPI_source, source == input$user_input)
      if (nrow(KPI_duplicate) == 0) {
        removeModal()
        KPI_source_new <- data.frame(source = input$user_input)
        KPI_source <- rbind(KPI_source, KPI_source_new)
        file.rename(KPISourceFile, "data/sources_old.csv")
        write.csv(KPI_source, KPISourceFile, row.names = FALSE)
        
      } else {
        output$error_message <- renderUI({
          div(style = "color: red; font-weight: bold;", "This source already exists")
        })
      }
      
      KPI_source <- read_csv(KPISourceFile)
      
      output$source <- DT::renderDataTable(KPI_source, selection = 'single', rownames = FALSE)
    }
  })
  
  # Add Outcome Data
  observeEvent(input$addOutcome, {
    
    showModal(modalDialog(
      
      title = paste("New Outcome for", Global_Source_Name, sep = " "),
      textInput("outcome_number", "Enter Outcome Number:", ""),
      textInput("outcome_desc", "Enter Outcome Description:", ""),
      uiOutput("error_message"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("submit_outcome_data", "Save")
      )
    ))
    output$error_message <- renderUI({
      div(style = "color: red; font-weight: bold;", "")
    })
  })
  
  # Save Add Outcome Data    
  observeEvent(input$submit_outcome_data, {
    if (input$outcome_number == "") {
      output$error_message <- renderUI({
        div(style = "color: red; font-weight: bold;", "Outcome number cannot be empty")
      })
    } else if (input$outcome_desc == "") {
      output$error_message <- renderUI({
        div(style = "color: red; font-weight: bold;", "Outcome description cannot be empty")
      })
    } else {
      
      sourceName <- Global_Source_Name
      
      if (checkFileDates("Outcomes")) {
        outcomes <- read_csv(outcomesFile)
      }
      
      Outcome_duplicate <- filter(outcomes, (Source == Global_Source_Name &
                                               (OutcomeID == input$outcome_number |
                                                  OutcomeDesc == input$outcome_desc)))
      if (nrow(Outcome_duplicate) == 0) {
        removeModal()
        outcomes_new <- data.frame(Source = Global_Source_Name,
                                   OutcomeID = input$outcome_number,
                                   OutcomeDesc = input$outcome_desc
        )
        outcomes <- rbind(outcomes, outcomes_new)
        
        file.rename(outcomesFile, "data/outcomes_old.csv")
        write.csv(outcomes, outcomesFile, row.names = FALSE)
        
      } else{
        output$error_message <- renderUI({
          div(style = "color: red; font-weight: bold;", "This outcome already exists")
        })
      }
    }
    
    outcomes <<- read_csv(outcomesFile)
    outcomes_filtered <- filter(outcomes, Source == Global_Source_Name)
    
    output$outcomes_filtered <- DT::renderDataTable(outcomes_filtered,
                                                    selection = 'single', rownames = FALSE)
  })
  
  # Add Group Data
  observeEvent(input$addGroup, {
    showModal(modalDialog(
      
      title = paste("New Outcome Group for", Global_Source_Name, sep = " "),
      textInput("outcome_group", "Enter Outcome Group Name:", ""),
      uiOutput("error_message"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("submit_group_data", "Save")
      )
    ))
    output$error_message <- renderUI({
      div(style = "color: red; font-weight: bold;", "")
    })
  })
  
  # Save Add Group Data  
  observeEvent(input$submit_group_data, {
    if (input$outcome_group == "") {
      output$error_message <- renderUI({
        div(style = "color: red; font-weight: bold;", "Outcome group cannot be empty")
      })
    } else {
      
      sourceName <- Global_Source_Name
      
      if (checkFileDates("outcomeGroups")) {
        outcomegroups <- read_csv(outcomeGroupsFile)
      }
      
      Group_duplicate <- filter(outcomegroups, (Source == Global_Source_Name &
                                                  OutcomeID == Global_Outcome_ID &
                                                  OutcomeGrouping == input$outcome_group))
      
      if (nrow(Group_duplicate) == 0) {
        removeModal()
        
        group_new <- data.frame(Source = Global_Source_Name,
                                OutcomeID = Global_Outcome_ID,
                                OutcomeGrouping = input$outcome_group
        )
        outcomegroups <- rbind(outcomegroups, group_new)
        
        file.rename(outcomeGroupsFile, "data/OutcomeGroups_old.csv")
        write.csv(outcomegroups, outcomeGroupsFile, row.names = FALSE)
        
        outcomegroups <<- read_csv(outcomeGroupsFile)
        outcomegroups_filtered <<- filter(outcomegroups, Source == Global_Source_Name, OutcomeID == Global_Outcome_ID)
        
        output$outcomegroups_filtered <- DT::renderDataTable(outcomegroups_filtered, selection = 'single', rownames = FALSE)
      } else{
        output$error_message <- renderUI({
          div(style = "color: red; font-weight: bold;", "Outcome group already exists")
        })
      }
    }
  })
  
  # Edit Objective Data
  observeEvent(input$editObjective, {
    ProductObjectives <<- reactive({objectives_filtered})
    
    req(input$objectives_filtered_rows_selected)
    
    selRow <- input$objectives_filtered_rows_selected
    data <- ProductObjectives()[selRow, ] 
    
    showModal(modalDialog(
      
      title = paste("Edit Objective", data$Activity_ID, sep = " "),
      selectInput("objective_type", "Choose an option:", choices = KPI_type, data$Type),
      textAreaInput("objective_desc", "Objective Description:", data$Description, 
                    width = "100%", 
                    height = "200px", 
      ),
      textInput("objective_lead", "Objective Lead Contact:", data$Lead),
      selectInput("Status", "Choose an status:", choices = objectiveStatus, data$Status),
      
      uiOutput("error_message"),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton("submit_objective_edit_data", "Save")
      )
    ))
    
    output$error_message <- renderUI({
      div(style = "color: red; font-weight: bold;", "")
    })
  })
  
  # Save Edit Objective Data    
  observeEvent(input$submit_objective_edit_data, {
    if (input$objective_desc == "") {
      output$error_message <- renderUI({
        div(style = "color: red; font-weight: bold;", "Objective description cannot be empty")
      })
    } else if (input$objective_lead == "") {
      output$error_message <- renderUI({
        div(style = "color: red; font-weight: bold;", "Objective lead cannot be empty. If it is currently unknown please put 'undecided'")
      })
    } else {
      removeModal()
      
      if (checkFileDates("Objectives")) {
        objectives <- read_csv(objectivesFile)
        
        objectives_filtered <<- filter(objectives, Source == Global_Source_Name &
                                         OutcomeArea == Global_Outcome_Desc &
                                         OutcomeSubarea == Global_Grouping
        )
      }
      
      ProductObjectives <- reactive({objectives_filtered})
      
      req(input$objectives_filtered_rows_selected)
      
      selRow <- input$objectives_filtered_rows_selected
      data <- ProductObjectives()[selRow, ] 
      
      objective_edit <- data
      
      objective_edit$Type <- input$objective_type
      objective_edit$Description <- input$objective_desc
      objective_edit$Lead <- input$objective_lead
      objective_edit$Status <- input$Status
      
      
      objectives[objectives$Activity_ID == objective_edit$Activity_ID, ] <- objective_edit
      
      file.rename(objectivesFile, "data/ObjectivesKPIs_old.csv")
      write.csv(objectives, objectivesFile, row.names = FALSE)
      
      objectives <<- read_csv(objectivesFile)
      objectives_filtered <<- filter(objectives, Source == Global_Source_Name, OutcomeArea == Global_Outcome_Desc, OutcomeSubarea == Global_Grouping)
      
      output$objectives_filtered <- DT::renderDataTable(objectives_filtered, selection = 'single', rownames = FALSE)
    }
  })
  
  # Add Objective Data
  observeEvent(input$addObjective, {
    
    showModal(modalDialog(
      
      title = paste("Add Objective for ", Global_Source_Name, "->", Global_Outcome_Desc, "->",  Global_Grouping, sep = ""),
      textInput("objective_ID", "Objective ID:"),
      selectInput("objective_type", "Choose an option:", choices = KPI_type),
      textAreaInput("objective_desc", "Objective Description:", 
                    width = "100%", 
                    height = "200px", 
      ),
      textInput("objective_lead", "Objective Lead Contact:"),
      selectInput("Status", "Choose an status:", choices = objectiveStatus),
      uiOutput("error_message"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("submit_objective_add_data", "Save")
      )
    ))
    
    output$error_message <- renderUI({
      div(style = "color: red; font-weight: bold;", "")
    })
  })
  
  # Save Add Objective Data  
  observeEvent(input$submit_objective_add_data, {
    if (input$objective_ID == "") {
      output$error_message <- renderUI({
        div(style = "color: red; font-weight: bold;", "Objective ID cannot be empty")
      })
    } else if (input$objective_desc == "") {
      output$error_message <- renderUI({
        div(style = "color: red; font-weight: bold;", "Objective description cannot be empty")
      })
    } else if (input$objective_lead == "") {
      output$error_message <- renderUI({
        div(style = "color: red; font-weight: bold;", "Objective lead cannot be empty. If it is currently unknown please put 'undecided'")
      })
    } else {
      
      if (checkFileDates("Objectives")) {
        objectives <- read_csv(objectivesFile)
      }
      
      Objective_duplicate <- filter(objectives, (Source == Global_Source_Name &
                                                   (Number == input$objective_ID | 
                                                      Description == input$objective_desc)))
      
      if (nrow(objectives) == 0) {
        removeModal()
        objectives <- data.frame(Activity_ID = activity_id,
                                 Source = Global_Source_Name,
                                 OutcomeArea = Global_Outcome_Desc,
                                 OutcomeSubarea = Global_Grouping,
                                 Type = input$objective_type,
                                 Number = input$objective_ID,
                                 Description = input$objective_desc,
                                 Lead = input$objective_lead,
                                 Status = input$Status
        )
        
        file.rename(objectivesFile, "data/ObjectivesKPIs_old.csv")
        write.csv(objectives, objectivesFile, row.names = FALSE)
      } else {
        if (nrow(Objective_duplicate) == 0) {
          removeModal()
          activity_id = paste0(Global_Source_Name, input$objective_ID)
          objectives_new <- data.frame(Activity_ID = activity_id,
                                       Source = Global_Source_Name,
                                       OutcomeArea = Global_Outcome_Desc,
                                       OutcomeSubarea = Global_Grouping,
                                       Type = input$objective_type,
                                       Number = input$objective_ID,
                                       Description = input$objective_desc,
                                       Lead = input$objective_lead,
                                       Status = input$Status
          )
          
          objectives <- rbind(objectives, objectives_new)
          
          file.rename(objectivesFile, "data/ObjectivesKPIs_old.csv")
          write.csv(objectives, objectivesFile, row.names = FALSE)
          objectives <<- read_csv(objectivesFile)
          objectives_filtered <<- filter(objectives, Source == Global_Source_Name &
                                           OutcomeArea == Global_Outcome_Desc &
                                           OutcomeSubarea == Global_Grouping
          )
          
          output$objectives_filtered <- DT::renderDataTable(objectives_filtered, selection = 'single', rownames = FALSE)
          
        } else {
          output$error_message <- renderUI({
            div(style = "color: red; font-weight: bold;", "Objective already exists")
          })
        }
      }
    }
  })
  
  # Add Board Data
  observeEvent(input$addBoard, {
    showModal(modalDialog(
      title = "New Board",
      textInput("board_input", "Board Name:", ""),
      textInput("lead_input", "Board Lead Contact:", ""),
      textAreaInput("board_desc", "Board Description:", 
                    width = "100%", 
                    height = "200px", 
      ),
      textInput("acronym_input", "Board Acronym:", ""),
      uiOutput("error_message"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("submit_board_add_data", "Save")
      )
    ))
    
    output$error_message <- renderUI({
      div(style = "color: red; font-weight: bold;", "")
    })
  })
  
  # Save Add Board Data    
  observeEvent(input$submit_board_add_data, {
    if (input$board_input == "") {
      output$error_message <- renderUI({
        div(style = "color: red; font-weight: bold;", "Board name cannot be empty")
      })
    } else if (input$lead_input == "") {
      output$error_message <- renderUI({
        div(style = "color: red; font-weight: bold;", "Board lead cannot be empty. If it is currently unknown please put 'undecided'")
      })
    } else if (input$board_desc == "") {
      output$error_message <- renderUI({
        div(style = "color: red; font-weight: bold;", "Board description cannot be empty")
      })
    } else if (input$acronym_input == "") {
      output$error_message <- renderUI({
        div(style = "color: red; font-weight: bold;", "Board acronym cannot be empty")
      })
    } else {
      if (checkFileDates("boards")) {
        boards <- read_csv(boardsFile)
      }
      
      board_duplicate <- filter(boards, (Board_Name == input$board_input |
                                           Board_Acronym == input$acronym_input))
      
      if (nrow(board_duplicate) == 0) {
        removeModal()
        board_new <- data.frame(Board_Name = input$board_input,
                                Board_Lead = input$lead_input,
                                Board_Description = input$board_desc,
                                Board_Acronym = input$acronym_input)
        boards <- rbind(boards, board_new)
        
        file.rename(boardsFile, "data/BoardData_old.csv")
        write.csv(boards, boardsFile, row.names = FALSE)
        
        boards <<- read_csv(boardsFile)
        output$board <- DT::renderDataTable(boards, selection = 'single', rownames = FALSE)
      } else{
        output$error_message <- renderUI({
          div(style = "color: red; font-weight: bold;", "Board already exists")
        })
      }
    }
  })
  
  # Edit Board Data
  observeEvent(input$editBoard, {
    ProductBoards <<- reactive({boards})
    
    req(input$board_rows_selected)
    
    selRow <- input$board_rows_selected
    data <- ProductBoards()[selRow, ] 
    
    showModal(modalDialog(
      
      title = paste("Edit Board ", data$Board_Name, " (", data$Board_Acronym, ")", sep = ""),
      textInput("board_lead", "Board Lead Contact:", data$Board_Lead),
      textAreaInput("board_desc", "Board Description:", data$Board_Description, 
                    width = "100%", 
                    height = "200px", 
      ),
      uiOutput("error_message"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("submit_board_edit_data", "Save")
      )
    ))
    
    output$error_message <- renderUI({
      div(style = "color: red; font-weight: bold;", "")
    })
  })
  
  # Save Edit Board Data  
  observeEvent(input$submit_board_edit_data, {
    if (input$board_lead == "") {
      output$error_message <- renderUI({
        div(style = "color: red; font-weight: bold;", "Board lead cannot be empty. If it is currently unknown please put 'undecided'")
      })
    } else if (input$board_desc == "") {
      output$error_message <- renderUI({
        div(style = "color: red; font-weight: bold;", "Board description cannot be empty")
      })
    } else {
      removeModal()
      
      if (checkFileDates("boards")) {
        boards <- read_csv(boardsFile)
      }
      
      ProductBoards <- reactive({boards})
      
      req(input$board_rows_selected)
      
      selRow <- input$board_rows_selected
      data <- ProductBoards()[selRow, ] 
      
      board_edit <- data
      board_edit$Board_Lead <- input$board_lead
      board_edit$Board_Description <- input$board_desc
      
      
      boards[boards$Board_Acronym == board_edit$Board_Acronym, ] <- board_edit
      
      file.rename(boardsFile, "data/BoardData_old.csv")
      write.csv(boards, boardsFile, row.names = FALSE)
      
      boards <<- read_csv(boardsFile)
      
      output$board <- DT::renderDataTable(boards, selection = 'single', rownames = FALSE)
    }
  })
  
  # Edit Remove Objective from Board
  observeEvent(input$removeBoardObjective, {
    ProductBoardObjectives <<- reactive({board_objective_detail})
    
    req(input$board_objective_detail_rows_selected)
    
    selRow <- input$board_objective_detail_rows_selected
    data <- ProductBoardObjectives()[selRow, ] 
    
    showModal(modalDialog(
      
      title = paste("Remove Objective", data$ObjectiveID,  "from", data$Board_Acronym, sep = " "),
      tags$p("Are you sure you want to remove this objective from the board?"),
      tags$p("NOTE: you can always re-add the objective at a later date"),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton("submit_board_objective_remove", "Confirm")
      )
    ))
  })
  
  # Save Remove Objective from Board
  observeEvent(input$submit_board_objective_remove, {
    removeModal()
    
    if (checkFileDates("boardObjectives")) {
      boardobjectives <<- read_csv(boardObjectivesFile)
    }
    
    ProductBoardObjectives <<- reactive({board_objective_detail})
    
    req(input$board_objective_detail_rows_selected)
    
    selRow <- input$board_objective_detail_rows_selected
    data <- ProductBoardObjectives()[selRow, ] 
    
    boardobjectives_deleted <- boardobjectives %>% filter(!(Board_Acronym == data$Board_Acronym & ObjectiveID == data$ObjectiveID))
    
    file.rename(boardObjectivesFile, "data/Board_Objective_old.csv")
    write.csv(boardobjectives_deleted, boardObjectivesFile, row.names = FALSE)
    
    boardobjectives <<- read_csv(boardObjectivesFile)
    
    board_objective_detail <<- filter(boardobjectives, Board_Acronym == Global_Board_Acronym)
    
    board_objective_detail <- inner_join(board_objective_detail, objectives, by = c("ObjectiveID" = "Activity_ID"))
    
    board_objective_detail <- board_objective_detail %>% select(ObjectiveID, Source, OutcomeArea,OutcomeSubarea, Type, Description, Lead)
    
    output$board_objective_detail <- DT::renderDataTable(board_objective_detail, selection = 'single', rownames = FALSE)
  })
  
  # Add Objective to Board
  observeEvent(input$addBoardObjective, {
    otherObjectives <- anti_join(objectives, board_objective_detail, by = c("Activity_ID" = "ObjectiveID"))
    
    showModal(modalDialog(
      title = paste("Add Objective to Board", Global_Board_Acronym, sep = " "),
      div(
        radioButtons("selected_objective", "Select an Objective", 
                     choices = setNames(otherObjectives$Activity_ID, paste(otherObjectives$Activity_ID, otherObjectives$Description, sep = ": "))),
        style = "width: 100%;"
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("submit_board_objective_add_data", "Save")
      ),
      tags$style(HTML("
        .modal-dialog { width: 60% !important; } /* Custom modal width */
        .form-group { width: 100%; }
        .shiny-input-container { width: 100% !important; }  /* Ensure radio buttons take full width */
      "))
    ))
  })
  
  # Save Add Objective to Board  
  observeEvent(input$submit_board_objective_add_data, {
    removeModal()
    
    req(input$selected_objective)  # Ensure a row is selected
    
    if (checkFileDates("boardObjectives")) {
      boardobjectives <<- read_csv("Board_Objective.csv")
    }
    
    addBoardObjective <- data.frame(Board_Acronym = Global_Board_Acronym,
                                    ObjectiveID = input$selected_objective)  
    
    boardobjectives <- rbind(boardobjectives, addBoardObjective)
    
    file.rename(boardObjectivesFile, "data/Board_Objective_old.csv")
    write.csv(boardobjectives, boardObjectivesFile, row.names = FALSE)
    
    boardobjectives <<- read_csv(boardObjectivesFile)
    
    board_objective_detail <<- filter(boardobjectives, Board_Acronym == Global_Board_Acronym)
    
    board_objective_detail <- inner_join(board_objective_detail, objectives, by = c("ObjectiveID" = "Activity_ID"))
    
    board_objective_detail <- board_objective_detail %>% select(ObjectiveID, Source, OutcomeArea,OutcomeSubarea, Type, Description, Lead)
    
    output$board_objective_detail <- DT::renderDataTable(board_objective_detail, selection = 'single', rownames = FALSE)
  })
  
  # Edit Update
  observeEvent(input$editObjectiveUpdate, {
    enableEdit()
    disable("updateDate")
  })
  
  # Save Update Changes
  observeEvent(input$saveObjectiveUpdate, {
    if (input$latestUpdate == "") {
      showModal(modalDialog(
        
        title = "Update Objective",
        tags$p("Update decsription cannot be blank."),
        
        footer = tagList(
          modalButton("Continue"),
          actionButton("update_Cancel", "Cancel")
        )
      ))
    } else {
      if (checkFileDates("objectiveUpdates")) {
        objectiveUpdates <- read_csv(objectiveUpdatesFile)
      }
      
      #if (is.na(latestUpdate$RAG)) { #no previous updates for this objective
      disableEdit()
      
      date <- input$updateDate
      parsed_date <- as.Date(date, format = "%Y/%m/%d")
      formatted_date <- format(parsed_date, "%d/%m/%Y")
      
      newUpdate <- data.frame(Board_Acronym = Global_Board_Acronym,
                              ObjectiveID = Global_Objective_ID,
                              Date = formatted_date,
                              RAG = input$RAGStatus,
                              Update = input$latestUpdate,
                              Return_to_Green = input$returnToGreen)
      
      merged <- merge(objectiveUpdates, newUpdate, by = c("Board_Acronym", "ObjectiveID", "Date"), all = TRUE, suffixes = c(".old", ".new"))
      
      merged$RAG <- ifelse(!is.na(merged$RAG.new), merged$RAG.new, merged$RAG.old)
      merged$Update <- ifelse(!is.na(merged$Update.new), merged$Update.new, merged$Update.old)
      merged$Return_to_Green <- ifelse(!is.na(merged$Return_to_Green.new), merged$Return_to_Green.new, merged$Return_to_Green.old)
      
      # Drop the extra columns
      objectiveUpdates <- merged[, c("Board_Acronym", "ObjectiveID",
                                     "Date", "RAG", "Update", 
                                     "Return_to_Green")]
      
      
      
      file.rename(objectiveUpdatesFile, "data/Objective_Updates_old.csv")
      write.csv(objectiveUpdates, objectiveUpdatesFile, row.names = FALSE)
      
      objectiveUpdates <<- read_csv(objectiveUpdatesFile)
      
      objectiveUpdates_filtered <<- filter(objectiveUpdates, (ObjectiveID == Global_Objective_ID & 
                                                                Board_Acronym %like% Global_Board_Acronym))
      
      objectiveUpdates_filtered$Date <<- as.Date(objectiveUpdates_filtered$Date, format ="%d/%m/%Y")
      objectiveUpdates_filtered <<- setorder(objectiveUpdates_filtered, -Date)
      
      latestUpdate <<- objectiveUpdates_filtered[1, , drop = FALSE]
      
      updateSelectInput(session, "RAGStatus", selected = latestUpdate$RAG)
      updateTextInput(session, "latestUpdate", value = latestUpdate$Update)
      updateDateInput(session, "updateDate", value = as.Date(latestUpdate$Date, format ="%d/%m/%Y"))
      updateTextInput(session, "returnToGreen", value = latestUpdate$Return_to_Green)
      
      objectiveUpdates_previous <-objectiveUpdates_filtered[-1, ]
      
      output$objectiveUpdates_filtered <- DT::renderDataTable({datatable(objectiveUpdates_previous, options = list(dom = 't'),
                                                                         selection = 'single', rownames = FALSE) %>%
          formatStyle("RAG",
                      "text-align" = 'center',
                      backgroundColor = styleEqual(
                        c("Red", "Amber", "Green", "Complete"),
                        c('red', 'yellow', 'green', "blue")
                      ),
                      color = styleEqual(
                        c("Red", "Amber", "Green", "Complete"),
                        c('white', 'black', 'white', 'white')
                      )
          )
      })
    } 
  })
  
  # Save Add Objective to Board  
  observeEvent(input$update_Cancel, {
    removeModal()
    disableEdit()
    
    updateSelectInput(session, "RAGStatus", selected = latestUpdate$RAG)
    updateTextInput(session, "latestUpdate", value = latestUpdate$Update)
    updateDateInput(session, "updateDate", value = as.Date(latestUpdate$Date, format ="%d/%m/%Y"))
    updateTextInput(session, "returnToGreen", value = latestUpdate$Return_to_Green)
  })
  
  # Undo Update Changes  
  observeEvent(input$undoObjectiveUpdate, {
    disableEdit()
    
    updateSelectInput(session, "RAGStatus", selected = latestUpdate$RAG)
    updateTextInput(session, "latestUpdate", value = latestUpdate$Update)
    updateDateInput(session, "updateDate", value = as.Date(latestUpdate$Date, format ="%d/%m/%Y"))
    updateTextInput(session, "returnToGreen", value = latestUpdate$Return_to_Green)
  })
  
  # Add Objective Data
  observeEvent(input$addObjectiveUpdate, {
    
    showModal(modalDialog(
      
      title = paste("Add update"),
      selectInput("RAGStatus", "RAG Status:", choices = c("Red", "Amber", "Green", "Complete")),
      dateInput("update_Date", "Date of Update:", value = Sys.Date(), format ="dd/mm/yyyy"),
      textAreaInput("update_Text", "Update:", 
                    width = "100%", 
                    height = "200px", 
      ),
      textAreaInput("return_To_Green", "Return to Green Plan:", 
                    width = "100%", 
                    height = "200px", 
      ),
      uiOutput("error_message"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("submit_objective_add_update", "Save")
      )
    ))
    
    output$error_message <- renderUI({
      div(style = "color: red; font-weight: bold;", "")
    })
  })  
  
  # Save Add Board Data    
  observeEvent(input$submit_objective_add_update, {
    if (!isTruthy(input$update_Date)) {
      output$error_message <- renderUI({
        div(style = "color: red; font-weight: bold;", "Update date cannot be empty")
      })
    } else if (input$update_Text == "") {
      output$error_message <- renderUI({
        div(style = "color: red; font-weight: bold;", "Latest update cannot be empty.")
      }      )
    } else if (isTruthy(latestUpdate$Date)) {
      if (latestUpdate$Date > input$update_Date) { #this update predates previous one
        output$error_message <- renderUI({
          div(style = "color: red; font-weight: bold;", paste0("This update is older than the latest update (", as.Date(latestUpdate$Date, format ="%d/%m/%Y"), ")"))
        })
      } else {
        
        if (checkFileDates("objectiveUpdates")) {
          objectiveUpdates <- read_csv(objectiveUpdatesFile)
        }
        
        removeModal()
        
        date <- input$update_Date
        parsed_date <- as.Date(date, format = "%Y/%m/%d")
        formatted_date <- format(parsed_date, "%d/%m/%Y")
        
        newUpdate <- data.frame(Board_Acronym = Global_Board_Acronym,
                                ObjectiveID = Global_Objective_ID,
                                Date = gsub("-", "/", formatted_date),
                                RAG = input$RAGStatus,
                                Update = input$update_Text,
                                Return_to_Green = input$return_To_Green)
        
        
        objectiveUpdates <- rbind(objectiveUpdates, newUpdate)
        file.rename(objectiveUpdatesFile, "data/Objective_Updates_old.csv")
        write.csv(objectiveUpdates, objectiveUpdatesFile, row.names = FALSE)
        
        objectiveUpdates <<- read_csv(objectiveUpdatesFile)
        
        objectiveUpdates_filtered <<- filter(objectiveUpdates, (ObjectiveID == Global_Objective_ID & 
                                                                  Board_Acronym %like% Global_Board_Acronym))
        
        objectiveUpdates_filtered$Date <<- as.Date(objectiveUpdates_filtered$Date, format ="%d/%m/%Y")
        objectiveUpdates_filtered <<- setorder(objectiveUpdates_filtered, -Date)
        
        latestUpdate <<- objectiveUpdates_filtered[1, , drop = FALSE]
        
        updateSelectInput(session, "RAGStatus", selected = latestUpdate$RAG)
        updateTextInput(session, "latestUpdate", value = latestUpdate$Update)
        updateDateInput(session, "updateDate", value = as.Date(latestUpdate$Date, format ="%d/%m/%Y"))
        updateTextInput(session, "returnToGreen", value = latestUpdate$Return_to_Green)
        
        objectiveUpdates_previous <-objectiveUpdates_filtered[-1, ]
        
        output$objectiveUpdates_filtered <- DT::renderDataTable({datatable(objectiveUpdates_previous, options = list(dom = 't'),
                                                                           selection = 'single', rownames = FALSE) %>%
            formatStyle("RAG",
                        "text-align" = 'center',
                        backgroundColor = styleEqual(
                          c("Red", "Amber", "Green", "Complete"),
                          c('red', 'yellow', 'green', "blue")
                        ),
                        color = styleEqual(
                          c("Red", "Amber", "Green", "Complete"),
                          c('white', 'black', 'white', 'white')
                        )
            )
        })
      }} else {
        
        if (checkFileDates("objectiveUpdates")) {
          objectiveUpdates <- read_csv(objectiveUpdatesFile)
        }
        
        removeModal()
        
        date <- input$update_Date
        parsed_date <- as.Date(date, format = "%Y/%m/%d")
        formatted_date <- format(parsed_date, "%d/%m/%Y")
        
        newUpdate <- data.frame(Board_Acronym = Global_Board_Acronym,
                                ObjectiveID = Global_Objective_ID,
                                Date = gsub("-", "/", formatted_date),
                                RAG = input$RAGStatus,
                                Update = input$update_Text,
                                Return_to_Green = input$return_To_Green)
        
        
        objectiveUpdates <- rbind(objectiveUpdates, newUpdate)
        file.rename(objectiveUpdatesFile, "data/Objective_Updates_old.csv")
        write.csv(objectiveUpdates, objectiveUpdatesFile, row.names = FALSE)
        
        objectiveUpdates <<- read_csv(objectiveUpdatesFile)
        
        objectiveUpdates_filtered <<- filter(objectiveUpdates, (ObjectiveID == Global_Objective_ID & 
                                                                  Board_Acronym %like% Global_Board_Acronym))
        
        objectiveUpdates_filtered$Date <<- as.Date(objectiveUpdates_filtered$Date, format ="%d/%m/%Y")
        objectiveUpdates_filtered <<- setorder(objectiveUpdates_filtered, -Date)
        
        latestUpdate <<- objectiveUpdates_filtered[1, , drop = FALSE]
        
        updateSelectInput(session, "RAGStatus", selected = latestUpdate$RAG)
        updateTextInput(session, "latestUpdate", value = latestUpdate$Update)
        updateDateInput(session, "updateDate", value = as.Date(latestUpdate$Date, format ="%d/%m/%Y"))
        updateTextInput(session, "returnToGreen", value = latestUpdate$Return_to_Green)
        
        objectiveUpdates_previous <-objectiveUpdates_filtered[-1, ]
        
        output$objectiveUpdates_filtered <- DT::renderDataTable({datatable(objectiveUpdates_previous, options = list(dom = 't'),
                                                                           selection = 'single', rownames = FALSE) %>%
            formatStyle("RAG",
                        "text-align" = 'center',
                        backgroundColor = styleEqual(
                          c("Red", "Amber", "Green", "Complete"),
                          c('red', 'yellow', 'green', "blue")
                        ),
                        color = styleEqual(
                          c("Red", "Amber", "Green", "Complete"),
                          c('white', 'black', 'white', 'white')
                        )
            )
        })
      }
  }) 
  
  # Add Objective Data
  observeEvent(input$addBoardAction, {
    
    showModal(modalDialog(
      title = paste("Add Action for ", Global_Board_Acronym),
      dateInput("Action_Date", "Action Creation Date:", value = Sys.Date(), format ="dd/mm/yyyy"),
      textInput("reference", "Action Reference", ""),
      textAreaInput("action_Text", "Action:", 
                    width = "100%", 
                    height = "200px", 
      ),
      textInput("Action_Owner", "Action Owner", ""),
      selectInput("actionStatus", "Action Status:", choices = objectiveStatus),
      textAreaInput("action_Update", "Update:", 
                    width = "100%", 
                    height = "200px", 
      ),
      uiOutput("error_message"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("submit_action_add_data", "Save")
      )
    ))
    
    output$error_message <- renderUI({
      div(style = "color: red; font-weight: bold;", "")
    })
  })
  
  # Save Add Objective Data  
  observeEvent(input$submit_action_add_data, {
    if (!isTruthy(input$Action_Date)) {
      output$error_message <- renderUI({
        div(style = "color: red; font-weight: bold;", "Action date cannot be empty")
      })
    } else if (input$reference == "") {
      output$error_message <- renderUI({
        div(style = "color: red; font-weight: bold;", "Reference cannot be empty.")
      }      )
    } else if (input$action_Text == "") {
      output$error_message <- renderUI({
        div(style = "color: red; font-weight: bold;", "Action description cannot be empty.")
      }      )
    } else if (input$Action_Owner == "") {
      output$error_message <- renderUI({
        div(style = "color: red; font-weight: bold;", "Action owner cannot be empty. If it is currently unknown please put 'undecided'")
      })
    } else {
      Action_duplicate <- filter(BoardAction_filtered, Reference == input$reference)
      if (nrow(Action_duplicate > 0)) {
        output$error_message <- renderUI({
          div(style = "color: red; font-weight: bold;", "An action with this reference already exists")
        })
      } else {
        
        if (checkFileDates("actionUpdates")) {
          boardActions <- read_csv(boardActionsFile)
        }
        
        removeModal()
        
        date <- input$Action_Date
        parsed_date <- as.Date(date, format = "%Y/%m/%d")
        formatted_date <- format(parsed_date, "%d/%m/%Y")
        
        newAction <- data.frame(Board_Acronym = Global_Board_Acronym,
                                Reference = input$reference,
                                CreatedDate = gsub("-", "/", formatted_date),
                                ActionDesc = input$action_Text,
                                ActionOwner = input$Action_Owner,
                                Status = input$actionStatus,
                                Update = input$action_Update)
        
        
        boardActions <- rbind(boardActions, newAction)
        file.rename(boardActionsFile, "data/BoardActions_old.csv")
        write.csv(boardActions, boardActionsFile, row.names = FALSE)
        
        boardActions <<- read_csv(boardActionsFile)
        
        BoardAction_filtered <<- filter(boardActions, 
                                        (Board_Acronym == Global_Board_Acronym)) %>%
          select(-Board_Acronym)
        
        BoardAction_open <- filter(BoardAction_filtered, 
                                   !(Status == "Complete"))
        
        output$numberOfActions <-renderValueBox({
          value <- nrow(BoardAction_open)
          valueBox(
            value = value,
            subtitle = "No. Open Actions",
            color = "black"
          )
        })
        
        output$BoardActions <- DT::renderDataTable({datatable(BoardAction_filtered, rownames = FALSE, selection = 'single', width = "100%")})
      }
    }
  })
  
  observeEvent(input$editBoardAction, {
    ProductBoardActions <<- reactive({BoardAction_filtered})
    
    req(input$BoardActions_rows_selected)
    
    selRow <- input$BoardActions_rows_selected
    data <- ProductBoardActions()[selRow, ] 
    
    
    showModal(modalDialog(
      title = paste("Edit", Global_Board_Acronym, "action", data$Reference, sep = " "),
      #dateInput("Action_Date", "Action Creation Date:", value = Sys.Date(), format ="dd/mm/yyyy"),
      textAreaInput("action_Text", "Action:", 
                    data$ActionDesc,
                    width = "100%", 
                    height = "200px"
      ),
      textInput("Action_Owner", "Action Owner", data$ActionOwner),
      selectInput("actionStatus", "Action Status:", choices = objectiveStatus,
                  data$Status),
      textAreaInput("action_Update", "Update:", data$Update,
                    width = "100%", 
                    height = "200px", 
      ),
      
      uiOutput("error_message"),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton("submit_action_edit_data", "Save")
      )
    ))
    
    output$error_message <- renderUI({
      div(style = "color: red; font-weight: bold;", "")
    })
  })
  
  # Save Edit Objective Data    
  observeEvent(input$submit_action_edit_data, {
    if (input$action_Text == "") {
      output$error_message <- renderUI({
        div(style = "color: red; font-weight: bold;", "Action description cannot be empty.")
      }      )
    } else if (input$Action_Owner == "") {
      output$error_message <- renderUI({
        div(style = "color: red; font-weight: bold;", "Action owner cannot be empty. If it is currently unknown please put 'undecided'")
      })
    } else {
      removeModal()
      
      if (checkFileDates("actionUpdates")) {
        boardActions <- read_csv(boardActionsFile)
      }
      
      BoardAction_filtered1 <- filter(boardActions, 
                                      (Board_Acronym == Global_Board_Acronym))
      
      ProductBoardActions <<- reactive({BoardAction_filtered1})
      
      req(input$BoardActions_rows_selected)
      
      selRow <- input$BoardActions_rows_selected
      data <- ProductBoardActions()[selRow, ] 
      
      action_edit <- data
      
      action_edit$ActionDesc <- input$action_Text
      action_edit$ActionOwner <- input$Action_Owner
      action_edit$Status <- input$actionStatus
      action_edit$Update <- input$action_Update
      
      boardActions[boardActions$Board_Acronym == Global_Board_Acronym &
                     boardActions$Reference == action_edit$Reference, ] <- action_edit
      
      file.rename(boardActionsFile, "data/BoardActions_old.csv")
      write.csv(boardActions, boardActionsFile, row.names = FALSE)
      
      boardActions <<- read_csv(boardActionsFile)
      
      BoardAction_filtered <<- filter(boardActions, 
                                      (Board_Acronym == Global_Board_Acronym)) %>%
        select(-Board_Acronym)
      
      BoardAction_open <- filter(BoardAction_filtered, 
                                 !(Status == "Complete"))
      
      output$numberOfActions <-renderValueBox({
        value <- nrow(BoardAction_open)
        valueBox(
          value = value,
          subtitle = "No. Open Actions",
          color = "black"
        )
      })
      
      output$BoardActions <- DT::renderDataTable({datatable(BoardAction_filtered, rownames = FALSE, selection = 'single', width = "100%")})
    }
  })
  
  # Source On-Click
  observe({
    req(input$source_rows_selected)
    
    selRow <- input$source_rows_selected
    data <- ProductSources()[selRow, ] 
    
    selRow = 0
    
    Global_Source_Name <<- data$source
    
    outcomes_filtered <- filter(outcomes, Source == data$source)
    
    output$outcomes_filtered <- DT::renderDataTable(outcomes_filtered, selection = 'single', rownames = FALSE)
    
  })
  
  # Outcomes On-Click
  observe({
    req(input$outcomes_filtered_rows_selected)
    
    selRow <- input$outcomes_filtered_rows_selected
    data <- ProductOutcomes()[selRow, ] 
    
    selRow = 0
    
    Global_Outcome_ID <<- data$OutcomeID
    Global_Outcome_Desc <<- data$OutcomeDesc
    
    outcomegroups_filtered <<- filter(outcomegroups, OutcomeID == Global_Outcome_ID & Source == Global_Source_Name)
    
    # ProductGroups <<- outcomegroups_filtered
    
    output$outcomegroups_filtered <- DT::renderDataTable(outcomegroups_filtered, selection = 'single', rownames = FALSE)
    
  })
  
  # Outcome Groups On-Click
  observe({
    req(input$outcomegroups_filtered_rows_selected)
    
    
    ProductGroups <- outcomegroups_filtered
    selRow <- input$outcomegroups_filtered_rows_selected
    data <- outcomegroups_filtered[selRow, ] 
    selRow = 0
    
    Global_Grouping <<- data$OutcomeGrouping
    
    objectives_filtered <<- filter(objectives, Source == Global_Source_Name &
                                     OutcomeArea == Global_Outcome_Desc &
                                     OutcomeSubarea == Global_Grouping
    )
    
    output$objectives_filtered <- DT::renderDataTable(objectives_filtered, selection = 'single', rownames = FALSE)
    
  })
  
  # Board Rows On-Click
  observe({
    req(input$board_rows_selected)
    
    ProductBoards <<- boards
    selRow <- input$board_rows_selected
    data <- boards[selRow, ] 
    selRow = 0
    
    Global_Board_Acronym <<- data$Board_Acronym
    
    board_objective_detail <<- filter(boardobjectives, Board_Acronym == data$Board_Acronym)
    
    board_objective_detail <- inner_join(board_objective_detail, objectives, by = c("ObjectiveID" = "Activity_ID"))
    
    board_objective_detail <- board_objective_detail %>% select(ObjectiveID, Source, OutcomeArea,OutcomeSubarea, Type, Description, Lead, Status)
    
    output$board_objective_detail <- DT::renderDataTable(board_objective_detail, selection = 'single', rownames = FALSE)
  })
  
  # Objective Update Screen - select board
  observeEvent(input$projectBoard, {
    disable("RAGStatus")
    disable("latestUpdate")
    disable("updateDate")
    disable("returnToGreen")
    disable("saveObjectiveUpdate")
    disable("undoObjectiveUpdate")
    
    Global_Board_Acronym <<- input$projectBoard
    
    board_objective_detail <- inner_join(boardobjectives, objectives, by = c("ObjectiveID" = "Activity_ID"))
    
    boardObjectivesLeads <- filter(board_objective_detail, board_objective_detail$Board_Acronym %like% input$projectBoard)
    
    allLeads <<- boardObjectivesLeads
    allLeads <<- setorder(allLeads, Lead)
    
    updateSelectInput(
      session,
      "projectLead",
      choices = allLeads$Lead
    )
  })
  
  # Objective Update Screen - select lead
  observeEvent(input$projectLead, {
    projectLead = input$projectLead
    
    board_objective_detail <- inner_join(boardobjectives, objectives, by = c("ObjectiveID" = "Activity_ID"))
    
    if (projectLead == "") {
      boardObjectiveList <- data.frame(Board_Acronym = "",
                                       ObjectiveID = "",
                                       Date = gsub("-", "/", Sys.Date()),
                                       RAG = "",
                                       Update = "",
                                       Return_to_Green = "")
      updateSelectInput(
        session,
        "projectObjectives",
        choices = ""
      )
    } else {
      boardObjectiveList <- filter(board_objective_detail, board_objective_detail$Lead %like% projectLead)
      updateSelectInput(
        session,
        "projectObjectives",
        choices = setNames(boardObjectiveList$ObjectiveID, boardObjectiveList$Description)
      )
    }
    
    
  })
  
  # Objective Update Screen - select objective
  observeEvent(input$projectObjectives, {
    if (input$projectObjectives == "") {
      updateSelectInput(session, "RAGStatus", selected = "")
      updateTextInput(session, "latestUpdate", value = "")
      updateDateInput(session, "updateDate", value = "")
      updateTextInput(session, "returnToGreen", value = "")
    } else {
      Global_Objective_ID <<- input$projectObjectives
      
      objectiveUpdates_filtered <<- filter(objectiveUpdates, (ObjectiveID == input$projectObjectives & 
                                                                Board_Acronym %like% Global_Board_Acronym))
      
      objectiveUpdates_filtered$Date <<- as.Date(objectiveUpdates_filtered$Date, format ="%d/%m/%Y")
      objectiveUpdates_filtered <<- setorder(objectiveUpdates_filtered, -Date)
      
      latestUpdate <<- objectiveUpdates_filtered[1, , drop = FALSE]
      
      updateSelectInput(session, "RAGStatus", selected = latestUpdate$RAG)
      updateTextInput(session, "latestUpdate", value = latestUpdate$Update)
      updateDateInput(session, "updateDate", value = as.Date(latestUpdate$Date, format ="%d/%m/%Y"))
      updateTextInput(session, "returnToGreen", value = latestUpdate$Return_to_Green)
      
      objectiveUpdates_previous <- objectiveUpdates_filtered[-1, ]
      
      output$objectiveUpdates_filtered <- DT::renderDataTable({datatable(objectiveUpdates_previous, options = list(dom = 't'),
                                                                         selection = 'single', rownames = FALSE) %>%
          formatStyle("RAG",
                      "text-align" = 'center',
                      backgroundColor = styleEqual(
                        c("Red", "Amber", "Green", "Complete"),
                        c('red', 'yellow', 'green', "blue")
                      ),
                      color = styleEqual(
                        c("Red", "Amber", "Green", "Complete"),
                        c('white', 'black', 'white', 'white')
                      )
          )
      })
      
    }
  })
  
  # Dashboard Screen - select board
  observeEvent(input$dashBoard, {
    Global_Board_Acronym <<- input$dashBoard
    Global_Board_Date <<- input$boardDate
    updateDashboard()
  })
  
  observeEvent(input$boardDate, {
    Global_Board_Acronym <<- input$dashBoard
    Global_Board_Date <<- input$boardDate
    updateDashboard()
  })
  
  # Function to align columns
  align_columns <- function(df_list) {
    all_cols <- unique(unlist(lapply(df_list, names)))
    
    lapply(df_list, function(df) {
      missing_cols <- setdiff(all_cols, names(df))
      df[missing_cols] <- NA  # Fill missing with NA
      df[all_cols]  # Reorder columns
    })
  }
  
  # Update Dashobad Page
  updateDashboard <- function() {
    boardObjectiveUpdates <- objectiveUpdates
    boardObjectiveUpdates$Date <- as.Date(boardObjectiveUpdates$Date, format ="%d/%m/%Y")
    boardObjectiveUpdates <- filter(boardObjectiveUpdates, (Board_Acronym == Global_Board_Acronym &
                                                              Date <= Global_Board_Date))
    
    boardObjectiveUpdates <- inner_join(boardObjectiveUpdates, objectives, by = c("ObjectiveID" = "Activity_ID"))
    
    boardObjectiveUpdates <- boardObjectiveUpdates[, c("OutcomeArea",
                                                       "OutcomeSubarea",
                                                       "ObjectiveID",
                                                       "Description",
                                                       "Lead",
                                                       "Status",
                                                       "Date",
                                                       "RAG",
                                                       "Update",
                                                       "Return_to_Green"
    )]
    
    latestUpdates <- boardObjectiveUpdates %>%
      arrange(ObjectiveID, desc(Date)) %>%   # Sort by group and latest date
      group_by(ObjectiveID) %>%              # Group by the column of interest
      slice(1) %>%                     # Take the latest row per group
      mutate(Date = format(Date, "%d/%m/%Y")) %>%
      ungroup()
    
    previousUpdate <- boardObjectiveUpdates %>%
      arrange(ObjectiveID, desc(Date)) %>%   # Sort by group and latest date
      group_by(ObjectiveID) %>%              # Group by the column of interest
      slice(2) %>%                     # Take the latest row per group
      mutate(Date = format(Date, "%d/%m/%Y")) %>%
      ungroup()
    
    if (nrow(latestUpdates > 0)) {
      latestUpdates <- latestUpdates %>%
        left_join(select(previousUpdate, ObjectiveID, Date, RAG), by = "ObjectiveID")
      latestUpdates <- latestUpdates %>% rename (
        Date = Date.x,
        RAG = RAG.x,
        PreviousDate = Date.y,
        PreviousRAG = RAG.y
      )
    } else {
      latestUpdates$PreviousDate = ""
      latestUpdates$PreviousRAG = ""
    }
    
    Red_Updates <- filter(latestUpdates, (RAG == "Red"))
    Amber_Updates <- filter(latestUpdates, (RAG == "Amber"))
    Green_Updates <- filter(latestUpdates, (RAG == "Green"))
    Complete_Updates <- filter(latestUpdates, (RAG == "Complete"))
    
    output$numberOfRed <- renderValueBox({
      value <- nrow(Red_Updates)
      valueBox(
        value = value,
        subtitle = "No. Red",
        color = "red"
      )
    })
    
    output$numberOfAmber <-renderValueBox({
      value <- nrow(Amber_Updates)
      valueBox(
        value = value,
        subtitle = "No. Amber",
        color = "yellow"
      )
    })
    
    output$numberOfGreen <- renderValueBox({
      value <- nrow(Green_Updates)
      valueBox(
        value = value,
        subtitle = "No. Green",
        color = "green"
      )
    })
    
    output$numberOfComplete <- renderValueBox({
      value <- nrow(Complete_Updates)
      valueBox(
        value = value,
        subtitle = "No. Complete",
        color = "blue"
      )
    })
    
    output$Red_Updates <- DT::renderDataTable({datatable(Red_Updates, rownames = FALSE, options = list(
      dom = 't',
      pageLength = 5,
      columnDefs = list(
        list(width = '100px', targets = 0:1),
        list(width = '75px', targets = 2),
        list(width = '120px', targets = 3),
        list(width = '100px', targets = 4),
        list(width = '75px', targets = 5:7),
        list(width = '100px', targets = 8:9),
        list(width = '75px', targets = 10:11)
      ), selection = 'single', width = "100%")) %>%
        formatStyle(columns = c("RAG", "PreviousRAG"),
                    "text-align" = 'center',
                    backgroundColor = styleEqual(
                      c("Red", "Amber", "Green", "Complete"),
                      c('red', 'yellow', 'green', "blue")
                    ),
                    color = styleEqual(
                      c("Red", "Amber", "Green", "Complete"),
                      c('white', 'black', 'white', 'white')
                    )
        )
    })
    
    output$Amber_Updates <- DT::renderDataTable({datatable(Amber_Updates, rownames = FALSE, options = list(
      dom = 't',
      pageLength = 5,
      columnDefs = list(
        list(width = '100px', targets = 0:1),
        list(width = '75px', targets = 2),
        list(width = '120px', targets = 3),
        list(width = '100px', targets = 4),
        list(width = '75px', targets = 5:7),
        list(width = '100px', targets = 8:9),
        list(width = '75px', targets = 10:11)
      ), selection = 'single', width = "100%"))  %>%
        formatStyle(columns = c("RAG", "PreviousRAG"),
                    "text-align" = 'center',
                    backgroundColor = styleEqual(
                      c("Red", "Amber", "Green", "Complete"),
                      c('red', 'yellow', 'green', "blue")
                    ),
                    color = styleEqual(
                      c("Red", "Amber", "Green", "Complete"),
                      c('white', 'black', 'white', 'white')
                    )
        )
    })
    
    output$Green_Updates <- DT::renderDataTable({datatable(Green_Updates, rownames = FALSE,  options = list(
      dom = 't',
      pageLength = 5,
      columnDefs = list(
        list(width = '100px', targets = 0:1),
        list(width = '75px', targets = 2),
        list(width = '120px', targets = 3),
        list(width = '100px', targets = 4),
        list(width = '75px', targets = 5:7),
        list(width = '100px', targets = 8:9),
        list(width = '75px', targets = 10:11)
      ), selection = 'single', width = "100%"))  %>%
        formatStyle(columns = c("RAG", "PreviousRAG"),
                    "text-align" = 'center',
                    backgroundColor = styleEqual(
                      c("Red", "Amber", "Green", "Complete"),
                      c('red', 'yellow', 'green', "blue")
                    ),
                    color = styleEqual(
                      c("Red", "Amber", "Green", "Complete"),
                      c('white', 'black', 'white', 'white')
                    )
        )
    })
    
    output$Complete_Updates <- DT::renderDataTable({datatable(Complete_Updates, rownames = FALSE, options = list(
      dom = 't',
      pageLength = 5,
      columnDefs = list(
        list(width = '100px', targets = 0:1),
        list(width = '75px', targets = 2),
        list(width = '120px', targets = 3),
        list(width = '100px', targets = 4),
        list(width = '75px', targets = 5:7),
        list(width = '100px', targets = 8:9),
        list(width = '75px', targets = 10:11)
      ), selection = 'single', width = "100%"))  %>%
        formatStyle(columns = c("RAG", "PreviousRAG"),
                    "text-align" = 'center',
                    backgroundColor = styleEqual(
                      c("Red", "Amber", "Green", "Complete"),
                      c('red', 'yellow', 'green', "blue")
                    ),
                    color = styleEqual(
                      c("Red", "Amber", "Green", "Complete"),
                      c('white', 'black', 'white', 'white')
                    )
        )
    })
    
    BoardAction_filtered <<- filter(boardActions, 
                                    (Board_Acronym == Global_Board_Acronym)) %>%
      select(-Board_Acronym)
    
    BoardAction_open <- filter(BoardAction_filtered, 
                               !(Status == "Complete"))
    
    output$numberOfActions <-renderValueBox({
      value <- nrow(BoardAction_open)
      valueBox(
        value = value,
        subtitle = "No. Open Actions",
        color = "black"
      )
    })
    
    output$BoardActions <- DT::renderDataTable({datatable(BoardAction_filtered, rownames = FALSE, selection = 'single', width = "100%")})
  }
  
  observeEvent(input$Red_Updates_cell_dblclick, {
    #info <- input$mytable_cell_dblclick
    #row <- info$row
    #col <- info$col
    #value <- iris[row, col]
    
    showModal(modalDialog(
      title = paste("Hello"),
      paste("You double-clicked on:"),
      easyClose = TRUE
    ))
  })
  
  output$statusBox <- renderValueBox({
    status_color <- switch(input$RAGStatus,
                           "Green" = "green",
                           "Amber" = "yellow",
                           "Red" = "red",
                           "Complete" = "blue",
                           "white")  # default fallback
    valueBox(
      value = "",
      subtitle = "",
      color = status_color  # must be one of "green", "yellow", "red", "aqua", etc.
    )
  })
  
  
  checkFileDates <- function(dataType) {
    
    if (dataType == "KPI") {
      return(KPISourceDate < file.info(KPISourceFile)$mtime)
    } else if (dataType == "Outcomes") {
      return(outcomesDate < file.info(outcomesFile)$mtime)
    } else if (dataType == "outcomeGroups") {
      return(outcomeGroupsDate < file.info(outcomeGroupsFile)$mtime)
    } else if (dataType == "Objectives") {
      return(objectivesDate < file.info(objectivesFile)$mtime)
    } else if (dataType == "boards") {
      return(boardsDate < file.info(boardsFile)$mtime)
    } else if (dataType == "boardObjectives") {
      return(boardObjectivesDate < file.info(boardObjectivesFile)$mtime)
    } else if (dataType == "objectiveUpdates") {
      return(objectiveUpdatesDate < file.info(objectiveUpdatesFile)$mtime)
    } else if (dataType == "actionUpdates") {
      return(boardActionsDate < file.info(boardActionsFile)$mtime)
    }
    
  }
  
  disableEdit <- function() {
    disable("RAGStatus")
    disable("latestUpdate")
    disable("updateDate")
    disable("returnToGreen")
    disable("saveObjectiveUpdate")
    disable("undoObjectiveUpdate")
    enable("editObjectiveUpdate")
    enable("addObjectiveUpdate")
  }
  
  enableEdit <- function() {
    enable("RAGStatus")
    enable("latestUpdate")
    enable("updateDate")
    enable("returnToGreen")
    enable("saveObjectiveUpdate")
    enable("undoObjectiveUpdate")
    disable("editObjectiveUpdate")
    disable("addObjectiveUpdate")
  }
  
}

#Run app
shinyApp(ui, server)


#NEXT STEPS
