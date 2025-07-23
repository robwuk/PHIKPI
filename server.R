#Make server function
server <- function(input, output, session) {
  
  ################################################################################
  #### Reactive variables
  ################################################################################
  
  ProductOutcomes <- reactive({outcomes})
  ProductSources <- reactive({KPI_source})
  ProductGroups <- reactive({outcomegroups_filtered})
  ProductObjectives <- reactive({objectives_filtered})
  ProductBoards <- reactive({boards})
  ProductBoardObjectives <- reactive({board_objective_detail})
  ProductBoardActions <- reactive({BoardAction_filtered})
  output$source <- DT::renderDataTable(KPI_source, 
                                       colnames = c("Source"),
                                       selection = 'single', rownames = FALSE)
  output$board <- DT::renderDataTable(boards, 
                                      colnames = c("Name", "Lead",
                                                   "Description", "Acronym"),
                                      selection = 'single', rownames = FALSE)
  RedRow <- reactive({Red_Updates})
  AmberRow <- reactive({Amber_Updates})
  GreenRow <- reactive({Green_Updates})
  CompleteRow <- reactive({Complete_Updates})
  WithoutRow <- reactive({Without_Updates})
  status_msg <- reactiveVal("Ready")
  
  ################################################################################
  #### Output functions
  ################################################################################
  
  # About screen
  output$about <- renderUI({
    includeHTML("about.html")
  })
  
  # KPI Source Data Maintenance Screen
  output$inputs <- renderUI({
    
    createBox <- function(title, tableId, buttonId, buttonLabel) {
      box(
        title = title,
        width = 4,
        DT::dataTableOutput(tableId),
        div(align = "center",
            actionButton(buttonId, buttonLabel, class = "btn-primary", icon = icon("plus")))
      )
    }
    
    tagList(
      br(), br(),
      HTML("<h3>KPI Source, Outcome & Group Maintenance</h3>"),
      tags$hr(),
      
      fluidRow(
        createBox("Objective/KPI Source", "source", "addSource", "Add New Source"),
        createBox("Source Outcome", "outcomes_filtered", "addOutcome", "Add New Outcome"),
        createBox("Outcome Group", "outcomegroups_filtered", "addGroup", "Add New Source Group")
      ),
      
      box(
        title = "Objectives/KPIs",
        width = 12,
        DT::dataTableOutput("objectives_filtered"),
        fluidRow(
          column(6, align = "center",
                 actionButton("addObjective", "Add New Objective", class = "btn-primary", icon = icon("plus"))),
          column(6, align = "center",
                 actionButton("editObjective", "Edit Objective", class = "btn-primary", icon = icon("pen-to-square")))
        )
      )
    )
  })
  
  
  # Board Maintenance Screen  
  output$board_inputs <- renderUI({
    
    createBoardBox <- function(title, tableId, buttons) {
      box(
        title = title,
        width = 12,
        DT::dataTableOutput(tableId),
        fluidRow(
          lapply(buttons, function(btn) {
            column(6, align = "center", btn)
          })
        )
      )
    }
    
    tagList(
      br(), br(),
      HTML("<h3>Board Maintenance</h3>"),
      tags$hr(),
      
      createBoardBox(
        "Board Data",
        "board",
        list(
          actionButton("addBoard", "Add New Board", class = "btn-primary", icon = icon("plus")),
          actionButton("editBoard", "Edit Board", class = "btn-primary", icon = icon("pen-to-square"))
        )
      ),
      
      createBoardBox(
        "Board Objectives",
        "board_objective_detail",
        list(
          actionButton("addBoardObjective", "Add Objective", class = "btn-primary", icon = icon("plus")),
          actionButton("removeBoardObjective", "Remove Objective", class = "btn-primary", icon = icon("minus"))
        )
      )
    )
  })
  
  
  
  output$objectives_inputs <- renderUI({
    
    # Helper to create selectInput columns
    selectCol <- function(id, label, choices = NULL, width = 6) {
      column(width,
             selectInput(inputId = id, label = label, choices = choices, selected = NULL, multiple = FALSE))
    }
    
    # Helper to create textAreaInput columns
    textAreaCol <- function(id, label, width = 3) {
      column(width,
             textAreaInput(inputId = id, label = label, width = "100%", height = "200px"))
    }
    
    tagList(
      br(), br(),
      HTML("<h3>Provide Objective Update</h3>"),
      tags$hr(),
      
      # Objective selection box
      box(
        width = 12,
        fluidRow(
          selectCol("projectBoard", "Select Project Board:"),
          selectCol("projectLead", "Select Project Lead:")
        ),
        fluidRow(
          column(12,
                 selectInput("projectObjectives", "Select Objective:", choices = NULL, selected = NULL, multiple = FALSE, width = "100%"))
        )
      ),
      
      # Latest update box
      box(
        title = "Latest Update",
        width = 12,
        fluidRow(
          column(3,
                 selectInput("RAGStatus", "RAG Status:", choices = c("Red", "Amber", "Green", "Complete")),
                 valueBoxOutput("spacer", width = 3),
                 uiOutput("statusBox", width = 6)
          ),
          column(3,
                 dateInput("updateDate", "Date of Update:", value = Sys.Date(),
                           format = "dd/mm/yyyy", min = as.Date("2024-01-01"),
                           max = as.Date("2030-12-31"), startview = "month", weekstart = 1)
          )
        ),
        fluidRow(
          column(12,
                 textAreaInput("latestUpdate", "Update:", width = "100%", height = "200px"))
        ),
        fluidRow(
          textAreaCol("returnToGreen", "Mitigating Actions:"),
          textAreaCol("risksIssues", "Risks/issues",),
          textAreaCol("decisionsRequired", "Decisions Required:"),
          textAreaCol("forwardLook", "Forward Look:")
        ),
        fluidRow(
          column(3, align = "center", actionButton("editObjectiveUpdate", "Edit Update", class = "btn-primary", icon = icon("pen-to-square"))),
          column(3, align = "center", actionButton("addObjectiveUpdate", "Add Update", class = "btn-primary", icon = icon("plus"))),
          column(3, align = "center", actionButton("undoObjectiveUpdate", "Undo Changes", class = "btn-primary", icon = icon("undo"))),
          column(3, align = "center", actionButton("saveObjectiveUpdate", "Save Changes", class = "btn-primary", icon = icon("save")))
        )
      ),
      
      # Previous updates box
      box(
        title = "Previous Updates",
        width = 12,
        DT::dataTableOutput("objectiveUpdates_filtered")
      )
    )
  })
  
  
  output$dashboard_inputs <- renderUI({
    
    # Helper for value boxes
    createValueBox <- function(title_text, output_id, bg, fg = "white") {
      column(
        width = 2,
        value_box(
          title = tags$div(title_text, style = "font-size: 16px; font-weight: normal;"),
          height = "120px",
          value = uiOutput(output_id),
          theme = value_box_theme(bg = bg, fg = fg)
        )
      )
    }
    
    tagList(
      br(), br(),
      HTML("<h3>Dashboard</h3>"),
      tags$hr(),
      
      fluidRow(
        box(
          title = "Select Board:",
          width = 3,
          selectInput("dashBoard", "Select Project Board:", choices = NULL, selected = NULL, width = "100%"),
          dateInput("boardDate", "Board Date:", value = Sys.Date(), format = "dd/mm/yyyy",
                    min = as.Date("2024-01-01"), max = as.Date("2030-12-31"),
                    startview = "month", weekstart = 1, width = "100%"),
          div(align = "center",
              downloadButton("generatePPT", "Create PowerPoint", class = "btn-primary", icon = icon("pen-to-square"))
          ),
          style = "margin-bottom: 20px;"
        ),
        
        box(
          title = "Board Overview:",
          width = 9,
          fluidRow(
            createValueBox("# Actions", "numberOfActions", gov_black),
            createValueBox("# Without Update", "numberWithoutUpdate", gov_black),
            createValueBox("# Red", "numberOfRed", gov_red),
            createValueBox("# Amber", "numberOfAmber", gov_yellow, fg = "black"),
            createValueBox("# Green", "numberOfGreen", gov_green),
            createValueBox("# Complete", "numberOfComplete", gov_blue)
          ),
          style = "margin-bottom: 20px;"
        )
      ),
      
      navset_card_tab(
        id = "outerTabBox",
        
        nav_panel("Actions",
                  layout_columns(
                    card(
                      full_screen = TRUE,
                      card_header("Board Actions"),
                      card_body(DTOutput("BoardActions"))
                    )
                  ),
                  layout_columns(
                    column(6, align = "center",
                           actionButton("editBoardAction", "Edit Action", class = "btn-primary", icon = icon("pen-to-square"))),
                    column(6, align = "center",
                           actionButton("addBoardAction", "Add Action", class = "btn-primary", icon = icon("plus")))
                  )
        ),
        
        nav_panel("Updates",
                  card(
                    full_screen = TRUE,
                    card_header("Updates by Status"),
                    navset_card_tab(
                      id = "tabs",
                      nav_panel(tagList(icon("circle-xmark"), "Red"), DTOutput("Red_Updates")),
                      nav_panel(tagList(icon("circle-exclamation"), "Amber"), DTOutput("Amber_Updates")),
                      nav_panel(tagList(icon("circle-check"), "Green"), DTOutput("Green_Updates")),
                      nav_panel(tagList(icon("check-double"), "Complete"), DTOutput("Complete_Updates")),
                      nav_panel(tagList(icon("question"), "No Updates"), DTOutput("Without_Updates"))
                    )
                  )
        )
      )
    )
  })
  
  
  ################################################################################
  #### Observe functions
  ################################################################################
  
  #Update dropdowns
  observeEvent(input$pages, {
    allBoards <- boards %>%
      semi_join(boardobjectives, by = "Board_Acronym") %>%
      setorder(Board_Acronym)
    
    print(input$pages)
    
    input_map <- list(
      objectivespage = "projectBoard",
      dashboardspage = "dashBoard"
    )
    
    input_id <- input_map[[input$pages]]
    
    if (!is.null(input_id)) {
      updateSelectizeInput(
        session, input_id,
        choices = setNames(allBoards$Board_Acronym, allBoards$Board_Name),
        selected = NULL,
        server = TRUE
      )
    }
  })
  
  
  
  # Add Source Data
  # Show modal and initialize error message
  # Show modal
  observeEvent(input$addSource, {
    showModal(
      modalDialog(
        title = "New Source",
        tooltip(
          textInput("user_input", "Enter Source Name:", ""),
          "Please enter the source name",
          placement = "right"
        ),
        uiOutput("error_message"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("submit_source_data", "Save")
        )
      )
    )
    
    output$error_message <- renderUI({ modalErrorUI("") })  # Clear error
  })
  
  # Validate and save
  observeEvent(input$submit_source_data, {
    new_source <- trimws(input$user_input)
    
    if (new_source == "") {
      output$error_message <- renderUI({ modalErrorUI("Source cannot be empty") })
      return()
    }
    
    if (checkFileDates("KPI")) {
      KPI_source <- read_csv(KPISourceFile)
    }
    
    if (new_source %in% KPI_source$source) {
      output$error_message <- renderUI({ modalErrorUI("This source already exists") })
      return()
    }
    
    KPI_source <- rbind(KPI_source, data.frame(source = new_source))
    file.rename(KPISourceFile, "data/sources_old.csv")
    write.csv(KPI_source, KPISourceFile, row.names = FALSE)
    
    KPI_source <- read_csv(KPISourceFile)
    output$source <- DT::renderDataTable(KPI_source, 
                                         colnames = c("Source"),
                                         selection = 'single', rownames = FALSE)
    
    removeModal()
    dataSaved()
  })
  
  
  
  
  # Add Outcome Data
  observeEvent(input$addOutcome, {
    output$error_message <- renderUI({ modalErrorUI("") })  # Clear error
    
    showModal(
      modalDialog(
        title = paste("New Outcome for", Global_Source_Name),
        tooltip(
          textInput("outcome_number", "Enter Outcome Number:", ""),
          "Enter the reference number for this outcome",
          placement = "right"
        ),
        tooltip(
          textInput("outcome_desc", "Enter Outcome Description:", ""),
          "Enter the description of the outcome",
          placement = "right"
        ),
        uiOutput("error_message"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("submit_outcome_data", "Save")
        )
      )
    )
  })
  
  
  # Save Add Outcome Data    
  
  observeEvent(input$submit_outcome_data, {
    number <- trimws(input$outcome_number)
    desc <- trimws(input$outcome_desc)
    
    # Validate inputs
    if (number == "") {
      output$error_message <- renderUI({ modalErrorUI("Outcome number cannot be empty") })
      return()
    }
    if (desc == "") {
      output$error_message <- renderUI({ modalErrorUI("Outcome description cannot be empty") })
      return()
    }
    
    # Load latest data if needed
    if (checkFileDates("Outcomes")) {
      outcomes <- read_csv(outcomesFile)
    }
    
    # Check for duplicates
    is_duplicate <- outcomes %>%
      filter(Source == Global_Source_Name & 
               (OutcomeID == number | OutcomeDesc == desc)) %>%
      nrow() > 0
    
    if (is_duplicate) {
      output$error_message <- renderUI({ modalErrorUI("This outcome already exists") })
      return()
    }
    
    # Save new outcome
    outcomes_new <- data.frame(Source = Global_Source_Name,
                               OutcomeID = number,
                               OutcomeDesc = desc)
    outcomes <- rbind(outcomes, outcomes_new)
    
    file.rename(outcomesFile, "data/outcomes_old.csv")
    write.csv(outcomes, outcomesFile, row.names = FALSE)
    
    # Refresh data and UI
    outcomes <<- read_csv(outcomesFile)
    outcomes_filtered <- filter(outcomes, Source == Global_Source_Name)
    
    output$outcomes_filtered <- DT::renderDataTable(outcomes_filtered,
                                                    colnames = c("Source", "ID", "Description"),
                                                    selection = 'single', rownames = FALSE)
    
    removeModal()
    dataSaved()
  })
  
  # Add Group Data
  observeEvent(input$addGroup, {
    output$error_message <- renderUI({ modalErrorUI("") })  # Clear error
    
    showModal(modalDialog(
      title = paste("New Outcome Group for", Global_Source_Name, sep = " "),
      tooltip(
        textInput("outcome_group", "Enter Outcome Group Name:", ""),
        "Enter the name of the outcome group",
        placement = "right"
      ),
      uiOutput("error_message"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("submit_group_data", "Save")
      )
    ))
  })
  
  # Save Add Group Data  
  observeEvent(input$submit_group_data, {
    new_group <- input$outcome_group
    if (new_group == "") {
      output$error_message <- renderUI({ modalErrorUI("Outcome group cannot be empty") })
      return()
    } 
    
    sourceName <- Global_Source_Name
    
    if (checkFileDates("outcomeGroups")) {
      outcomegroups <- read_csv(outcomeGroupsFile)
    }
    
    # Check for duplicates
    is_duplicate <- outcomegroups %>%
      filter(Source == Global_Source_Name &
               OutcomeID == Global_Outcome_ID &
               OutcomeGrouping == input$outcome_group) %>%
      nrow() > 0
    
    if (is_duplicate) {
      output$error_message <- renderUI({ modalErrorUI("This outcome group already exists") })
      return()
    }
    
    group_new <- data.frame(Source = Global_Source_Name,
                            OutcomeID = Global_Outcome_ID,
                            OutcomeGrouping = input$outcome_group
    )
    outcomegroups <- rbind(outcomegroups, group_new)
    
    file.rename(outcomeGroupsFile, "data/OutcomeGroups_old.csv")
    write.csv(outcomegroups, outcomeGroupsFile, row.names = FALSE)
    
    outcomegroups <<- read_csv(outcomeGroupsFile)
    outcomegroups_filtered <<- filter(outcomegroups, Source == Global_Source_Name, OutcomeID == Global_Outcome_ID)
    
    output$outcomegroups_filtered <- DT::renderDataTable(outcomegroups_filtered, 
                                                         colnames = c("Source", "ID", "Description"),
                                                         selection = 'single', rownames = FALSE)
    
    removeModal()
    
    dataSaved()
  })
  
  # Edit Objective Data
  observeEvent(input$editObjective, {
    req(input$objectives_filtered_rows_selected)
    
    data <- objectives_filtered[input$objectives_filtered_rows_selected, ]
    
    output$error_message <- renderUI({ modalErrorUI("") })  # Clear error
    
    showModal(
      modalDialog(
        title = paste("Edit Objective", data$Activity_ID),
        selectInput("objective_type", "Choose an option:", choices = KPI_type, selected = data$Type),
        textAreaInput("objective_desc", "Objective Description:", value = data$Description, width = "100%", height = "200px"),
        textInput("objective_lead", "Objective Lead Contact:", value = data$Lead),
        selectInput("Status", "Choose a status:", choices = objectiveStatus, selected = data$Status),
        uiOutput("error_message"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("submit_objective_edit_data", "Save")
        )
      )
    )
  })
  
  # Save Edit Objective Data    
  observeEvent(input$submit_objective_edit_data, {
    desc <- trimws(input$objective_desc)
    lead <- trimws(input$objective_lead)
    
    if (desc == "") {
      output$error_message <- renderUI({ modalErrorUI("Objective description cannot be empty") })
      return()
    }
    
    if (lead == "") {
      output$error_message <- renderUI({ modalErrorUI("Objective lead cannot be empty. If it is currently unknown please put 'undecided'") })
      return()
    }
    
    # Reload data if needed
    if (checkFileDates("Objectives")) {
      objectives <- read_csv(objectivesFile)
    }
    
    # Update filtered data
    objectives_filtered <<- filter(objectives,
                                   Source == Global_Source_Name,
                                   OutcomeArea == Global_Outcome_Desc,
                                   OutcomeSubarea == Global_Grouping)
    
    req(input$objectives_filtered_rows_selected)
    selRow <- input$objectives_filtered_rows_selected
    data <- objectives_filtered[selRow, ]
    
    # Update the selected row
    objectives[objectives$Activity_ID == data$Activity_ID, ] <- data.frame(
      Activity_ID = data$Activity_ID,
      Source = data$Source,
      OutcomeArea = data$OutcomeArea,
      OutcomeSubarea = data$OutcomeSubarea,
      Type = input$objective_type,
      Number = data$Number,
      Description = desc,
      Lead = lead,
      Status = input$Status,
      stringsAsFactors = FALSE
    )
    
    # Save updated data
    file.rename(objectivesFile, "data/ObjectivesKPIs_old.csv")
    write.csv(objectives, objectivesFile, row.names = FALSE)
    
    # Refresh data and UI
    objectives <<- read_csv(objectivesFile)
    objectives_filtered <<- filter(objectives,
                                   Source == Global_Source_Name,
                                   OutcomeArea == Global_Outcome_Desc,
                                   OutcomeSubarea == Global_Grouping)
    
    output$objectives_filtered <- DT::renderDataTable(objectives_filtered, 
                                                      colnames = c("ID", "Source", "Outcome",
                                                                   "Group", "Type", "Ref #",
                                                                   "Description", "Lead", "Status"),
                                                      selection = 'single', rownames = FALSE)
    
    removeModal()
    dataSaved()
  })
  
  
  # Add Objective Data
  observeEvent(input$addObjective, {
    
    output$error_message <- renderUI({ modalErrorUI("") })  # Clear error
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
          
          output$objectives_filtered <- DT::renderDataTable(objectives_filtered,
                                                            colnames = c("ID", "Source", "Outcome",
                                                                         "Group", "Type", "Ref #",
                                                                         "Description", "Lead", "Status"),
                                                            selection = 'single', rownames = FALSE)
          
          dataSaved()
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
        output$board <- DT::renderDataTable(boards, 
                                            colnames = c("Name", "Lead",
                                                         "Description", "Acronym"),
                                            selection = 'single', rownames = FALSE)
        
        dataSaved()
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
      
      output$board <- DT::renderDataTable(boards, 
                                          colnames = c("Name", "Lead",
                                                       "Description", "Acronym"),
                                          selection = 'single', rownames = FALSE)
      
      dataSaved()
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
    
    board_objective_detail <- board_objective_detail %>% select(ObjectiveID, Source, OutcomeArea,OutcomeSubarea, Type, Description, Lead, Status)
    
    output$board_objective_detail <- DT::renderDataTable(board_objective_detail, 
                                                         colnames = c("ID", "Source", "Outcome",
                                                                      "Group", "Type",
                                                                      "Description", "Lead", "Status"),
                                                         selection = 'single', rownames = FALSE)
    dataSaved()
  })
  
  # Add Objective to Board
  observeEvent(input$addBoardObjective, {
    otherObjectives <<- anti_join(objectives, board_objective_detail, by = c("Activity_ID" = "ObjectiveID"))
    
    # Show modal with table
    showModal(modalDialog(
      title = paste("Add Objective to Board", Global_Board_Acronym),
      div(
        DT::dataTableOutput("objective_table"),
        style = "width: 100%;"
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("submit_board_objective_add_data", "Save")
      ),
      tags$style(HTML("
      .modal-dialog { width: 80% !important; }
      .form-group { width: 100%; }
      .shiny-input-container { width: 100% !important; }
    "))
    ))
    
    # Render the table
    output$objective_table <- DT::renderDataTable({
      DT::datatable(
        otherObjectives[, c("Activity_ID", "Description")],
        rownames = FALSE,
        colnames = c("Objective ID", "Objective Description"),
        selection = "multiple",  # 🔹 Allow multiple rows
        options = list(pageLength = 5)
      )
    })
    
    
  })
  
  #Save add board objectives
  observeEvent(input$submit_board_objective_add_data, {
    removeModal()
    
    req(input$objective_table_rows_selected)  # Ensure rows are selected
    
    selected_indices <- input$objective_table_rows_selected
    selected_objectives <- otherObjectives[selected_indices, "Activity_ID", drop = FALSE]
    
    if (checkFileDates("boardObjectives")) {
      boardobjectives <<- read_csv(boardObjectivesFile)
    }
    
    # Create new entries for each selected objective
    new_entries <- data.frame(
      Board_Acronym = Global_Board_Acronym,
      ObjectiveID = selected_objectives$Activity_ID
    )
    
    boardobjectives <- rbind(boardobjectives, new_entries)
    
    file.rename(boardObjectivesFile, "data/Board_Objective_old.csv")
    write.csv(boardobjectives, boardObjectivesFile, row.names = FALSE)
    
    boardobjectives <<- read_csv(boardObjectivesFile)
    
    board_objective_detail <<- filter(boardobjectives, Board_Acronym == Global_Board_Acronym)
    board_objective_detail <- inner_join(board_objective_detail, objectives, by = c("ObjectiveID" = "Activity_ID"))
    board_objective_detail <- board_objective_detail %>%
      select(ObjectiveID, Source, OutcomeArea, OutcomeSubarea, Type, Description, Lead, Status)
    
    output$board_objective_detail <- DT::renderDataTable(
      board_objective_detail,
      colnames = c("ID", "Source", "Outcome", "Group", "Type", "Description", "Lead", "Status"),
      selection = 'single',
      rownames = FALSE
    )
    
    dataSaved()
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
      
      disableEdit()
      
      date <- input$updateDate
      parsed_date <- as.Date(date, format = "%Y/%m/%d")
      formatted_date <- format(parsed_date, "%d/%m/%Y")
      
      newUpdate <- data.frame(Board_Acronym = Global_Board_Acronym,
                              ObjectiveID = Global_Objective_ID,
                              Date = formatted_date,
                              RAG = input$RAGStatus,
                              Update = input$latestUpdate,
                              Mitigating_Actions = input$returnToGreen,
                              Decisions_Required = input$decisionsRequired,
                              Forward_Look = input$forwardLook,
                              Risks_Issues = input$risksIssues)
      
      merged <- merge(objectiveUpdates, newUpdate, by = c("Board_Acronym", "ObjectiveID", "Date"), all = TRUE, suffixes = c(".old", ".new"))
      
      merged$RAG <- ifelse(!is.na(merged$RAG.new), merged$RAG.new, merged$RAG.old)
      merged$Update <- ifelse(!is.na(merged$Update.new), merged$Update.new, merged$Update.old)
      merged$Mitigating_Actions <- ifelse(!is.na(merged$Mitigating_Actions.new), merged$Mitigating_Actions.new, merged$Mitigating_Actions.old)
      merged$Decisions_Required <- ifelse(!is.na(merged$Decisions_Required.new), merged$Decisions_Required.new, merged$Decisions_Required.old)
      merged$Forward_Look <- ifelse(!is.na(merged$Forward_Look.new), merged$Forward_Look.new, merged$Forward_Look.old)
      merged$Risks_Issues <- ifelse(!is.na(merged$Risks_Issues.new), merged$Risks_Issues.new, merged$Risks_Issues.old)
      
      # Drop the extra columns
      objectiveUpdates <- merged[, c("Board_Acronym", "ObjectiveID",
                                     "Date", "RAG", "Update", 
                                     "Mitigating_Actions",
                                     "Decisions_Required",
                                     "Forward_Look",
                                     "Risks_Issues")]
      
      
      
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
      updateTextInput(session, "returnToGreen", value = latestUpdate$Mitigating_Actions)
      updateTextInput(session, "risksIssues", value = latestUpdate$Risks_Issues)
      updateTextInput(session, "decisionsRequired", value = latestUpdate$Decisions_Required)
      updateTextInput(session, "forwardLook", value = latestUpdate$Forward_Look)
      
      objectiveUpdates_previous <-objectiveUpdates_filtered[-1, ]
      
      output$objectiveUpdates_filtered <- DT::renderDataTable({datatable(objectiveUpdates_previous, 
                                                                         colnames = c("Board", "ID", "Date",
                                                                                      "RAG", "Update", "Risks/issues",
                                                                                      "Mitigating Actions", "Decisions Required", "Forward Look"),
                                                                         options = list(dom = 'p'),
                                                                         selection = 'single', rownames = FALSE) %>%
          formatStyle("RAG",
                      "text-align" = 'center',
                      backgroundColor = styleEqual(
                        c("Red", "Amber", "Green", "Complete"),
                        c(gov_red, gov_yellow, 'green', "blue")
                      ),
                      color = styleEqual(
                        c("Red", "Amber", "Green", "Complete"),
                        c('white', 'black', 'white', 'white')
                      )
          )
      })
      
      dataSaved()
    } 
  })
  
  # Save Add Objective to Board  
  observeEvent(input$update_Cancel, {
    removeModal()
    disableEdit()
    
    updateSelectInput(session, "RAGStatus", selected = latestUpdate$RAG)
    updateTextInput(session, "latestUpdate", value = latestUpdate$Update)
    updateDateInput(session, "updateDate", value = as.Date(latestUpdate$Date, format ="%d/%m/%Y"))
    updateTextInput(session, "returnToGreen", value = latestUpdate$Mitigating_Actions)
    updateTextInput(session, "risksIssues", value = latestUpdate$Risks_Issues)
    updateTextInput(session, "decisionsRequired", value = latestUpdate$Decisions_Required)
    updateTextInput(session, "forwardLook", value = latestUpdate$Forward_Look)
  })
  
  # Undo Update Changes  
  observeEvent(input$undoObjectiveUpdate, {
    disableEdit()
    
    updateSelectInput(session, "RAGStatus", selected = latestUpdate$RAG)
    updateTextInput(session, "latestUpdate", value = latestUpdate$Update)
    updateDateInput(session, "updateDate", value = as.Date(latestUpdate$Date, format ="%d/%m/%Y"))
    updateTextInput(session, "returnToGreen", value = latestUpdate$Mitigating_Actions)
    updateTextInput(session, "risksIssues", value = latestUpdate$Risks_Issues)
    updateTextInput(session, "decisionsRequired", value = latestUpdate$Decisions_Required)
    updateTextInput(session, "forwardLook", value = latestUpdate$Forward_Look)
  })
  
  # Add Objective Data
  observeEvent(input$addObjectiveUpdate, {
    
    showModal(modalDialog(
      
      title = paste("Add update"),
      selectInput("addRAGStatus", "RAG Status:", choices = c("Red", "Amber", "Green", "Complete")),
      dateInput("update_Date", "Date of Update:", value = Sys.Date(), format ="dd/mm/yyyy"),
      textAreaInput("update_Text", "Update:", 
                    width = "100%", 
                    height = "200px", 
      ),
      fluidRow(
        column(6, 
               textAreaInput("risks_Issues", "Risks/Issues:", 
                             width = "100%", 
                             height = "200px", 
               ),
               textAreaInput("Decisions_Required", "Decisions Required:", 
                             width = "100%", 
                             height = "200px", 
               )
        ),
        column(6, 
               textAreaInput("Mitigating_Actions", "Mitigating Actions:", 
                             width = "100%", 
                             height = "200px", 
               ),
               textAreaInput("Forward_Look", "Forward Look:", 
                             width = "100%", 
                             height = "200px", 
               )
        )
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
        enable("editObjectiveUpdate")
        
        print(input$RAGStatus)
        
        date <- input$update_Date
        parsed_date <- as.Date(date, format = "%Y/%m/%d")
        formatted_date <- format(parsed_date, "%d/%m/%Y")
        
        newUpdate <- data.frame(Board_Acronym = Global_Board_Acronym,
                                ObjectiveID = Global_Objective_ID,
                                Date = gsub("-", "/", formatted_date),
                                RAG = input$addRAGStatus,
                                Update = input$update_Text,
                                Risks_Issues = input$risks_Issues,
                                Mitigating_Actions = input$Mitigating_Actions,
                                Decisions_Required = input$Decisions_Required,
                                Forward_Look = input$Forward_Look)
        
        
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
        updateTextInput(session, "returnToGreen", value = latestUpdate$Mitigating_Actions)
        updateTextInput(session, "risksIssues", value = latestUpdate$Risks_Issues)
        updateTextInput(session, "decisionsRequired", value = latestUpdate$Decisions_Required)
        updateTextInput(session, "forwardLook", value = latestUpdate$Forward_Look)
        
        objectiveUpdates_previous <-objectiveUpdates_filtered[-1, ]
        
        output$objectiveUpdates_filtered <- DT::renderDataTable({datatable(objectiveUpdates_previous, 
                                                                           colnames = c("Board", "ID", "Date",
                                                                                        "RAG", "Update", "Risks/issues",
                                                                                        "Mitigating Actions", "Decisions Required", "Forward Look"),
                                                                           options = list(dom = 'p'),
                                                                           selection = 'single', rownames = FALSE) %>%
            formatStyle("RAG",
                        "text-align" = 'center',
                        backgroundColor = styleEqual(
                          c("Red", "Amber", "Green", "Complete"),
                          c(gov_red, gov_yellow, 'green', "blue")
                        ),
                        color = styleEqual(
                          c("Red", "Amber", "Green", "Complete"),
                          c('white', 'black', 'white', 'white')
                        )
            )
        })
        
        dataSaved()
      }} else {
        
        if (checkFileDates("objectiveUpdates")) {
          objectiveUpdates <- read_csv(objectiveUpdatesFile)
        }
        
        
        date <- input$update_Date
        parsed_date <- as.Date(date, format = "%Y/%m/%d")
        formatted_date <- format(parsed_date, "%d/%m/%Y")
        
        enable("editObjectiveUpdate")
        print(input$addRAGStatus)
        
        newUpdate <- data.frame(Board_Acronym = Global_Board_Acronym,
                                ObjectiveID = Global_Objective_ID,
                                Date = gsub("-", "/", formatted_date),
                                RAG = input$addRAGStatus,
                                Update = input$update_Text,
                                Risks_Issues = input$risks_Issues,
                                Mitigating_Actions = input$Mitigating_Actions,
                                Decisions_Required = input$Decisions_Required,
                                Forward_Look = input$Forward_Look)
        
        removeModal()
        
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
        updateTextInput(session, "returnToGreen", value = latestUpdate$Mitigating_Actions)
        updateTextInput(session, "risksIssues", value = latestUpdate$Risks_Issues)
        updateTextInput(session, "decisionsRequired", value = latestUpdate$Decisions_Required)
        updateTextInput(session, "forwardLook", value = latestUpdate$Forward_Look)
        
        objectiveUpdates_previous <-objectiveUpdates_filtered[-1, ]
        
        output$objectiveUpdates_filtered <- DT::renderDataTable({datatable(objectiveUpdates_previous, 
                                                                           colnames = c("Board", "ID", "Date",
                                                                                        "RAG", "Update", "Risks/issues",
                                                                                        "Mitigating Actions", "Decisions Required", "Forward Look"),
                                                                           options = list(dom = 'p'),
                                                                           selection = 'single', rownames = FALSE) %>%
            formatStyle("RAG",
                        "text-align" = 'center',
                        backgroundColor = styleEqual(
                          c("Red", "Amber", "Green", "Complete"),
                          c(gov_red, gov_yellow, 'green', "blue")
                        ),
                        color = styleEqual(
                          c("Red", "Amber", "Green", "Complete"),
                          c('white', 'black', 'white', 'white')
                        )
            )
        })
        
        dataSaved()
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
        
        output$numberOfActions <-renderUI({
          value <- nrow(BoardAction_open)
          tags$h2(
            paste0(value), 
            style = "font-size: 36px;"
          )
        })
        
        output$BoardActions <- DT::renderDataTable({datatable(BoardAction_filtered, 
                                                              colnames = c("Reference", "Date Created", "Description",
                                                                           "Owner", "Status", "Update"),
                                                              rownames = FALSE, selection = 'single', width = "100%")})
        
        dataSaved()
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
      
      output$numberOfActions <-renderUI({
        value <- nrow(BoardAction_open)
        tags$h2(
          paste0(value),
          style = "font-size: 36px;"
        )
      })
      
      output$BoardActions <- DT::renderDataTable({datatable(BoardAction_filtered, 
                                                            colnames = c("Reference", "Date Created", "Description",
                                                                         "Owner", "Status", "Update"),
                                                            rownames = FALSE, selection = 'single', width = "100%")})
      
      dataSaved()
    }
  })
  
  # Red_Updates On-Click
  observe({
    req(input$Red_Updates_rows_selected)
    
    selRow <- input$Red_Updates_rows_selected
    data <- RedRow()[selRow, ] 
    
    col_names <- c(
      OutcomeArea = "Outcome",
      OutcomeSubarea = "Group",
      OnjectiveID = "ID",
      Description = "Description",
      Lead = "Lead",
      Status = "Status",
      Date = "Date",
      RAG = "RAG",
      Update = "Update",
      Risks_Issues = "Risks/Issues",
      Mitigating_Actions = "Mitigating Actions",
      Decisions_Required = "Decisions Required",
      Forward_Look = "Forward Look",
      PreviousDate = "Prev. Date",
      PreviousRAG = "Prev. RAG"
    )
    
    row_ui <- lapply(names(data), function(col_name) {
      tags$p(
        tags$strong(paste0(col_names[col_name], ": ")),
        as.character(data[[col_name]])
      )
    })
    
    showModal(modalDialog(
      
      title = "Update Detail",
      do.call(tagList, row_ui),
      easyClose = TRUE
      
    ))
  })
  
  # Amber_Updates On-Click
  observe({
    req(input$Amber_Updates_rows_selected)
    
    selRow <- input$Amber_Updates_rows_selected
    data <- AmberRow()[selRow, ] 
    
    col_names <- c(
      OutcomeArea = "Outcome",
      OutcomeSubarea = "Group",
      OnjectiveID = "ID",
      Description = "Description",
      Lead = "Lead",
      Status = "Status",
      Date = "Date",
      RAG = "RAG",
      Update = "Update",
      Risks_Issues = "Risks/Issues",
      Mitigating_Actions = "Mitigating Actions",
      Decisions_Required = "Decisions Required",
      Forward_Look = "Forward Look",
      PreviousDate = "Prev. Date",
      PreviousRAG = "Prev. RAG"
    )
    
    row_ui <- lapply(names(data), function(col_name) {
      tags$p(
        tags$strong(paste0(col_names[col_name], ": ")),
        as.character(data[[col_name]])
      )
    })
    
    showModal(modalDialog(
      
      title = "Update Detail",
      do.call(tagList, row_ui),
      easyClose = TRUE
      
    ))
  })
  
  # Green_Updates On-Click
  observe({
    req(input$Green_Updates_rows_selected)
    
    selRow <- input$Green_Updates_rows_selected
    data <- GreenRow()[selRow, ] 
    
    col_names <- c(
      OutcomeArea = "Outcome",
      OutcomeSubarea = "Group",
      OnjectiveID = "ID",
      Description = "Description",
      Lead = "Lead",
      Status = "Status",
      Date = "Date",
      RAG = "RAG",
      Update = "Update",
      Risks_Issues = "Risks/Issues",
      Mitigating_Actions = "Mitigating Actions",
      Decisions_Required = "Decisions Required",
      Forward_Look = "Forward Look",
      PreviousDate = "Prev. Date",
      PreviousRAG = "Prev. RAG"
    )
    
    row_ui <- lapply(names(data), function(col_name) {
      tags$p(
        tags$strong(paste0(col_names[col_name], ": ")),
        as.character(data[[col_name]])
      )
    })
    
    showModal(modalDialog(
      
      title = "Update Detail",
      do.call(tagList, row_ui),
      easyClose = TRUE
      
    ))
  })
  
  # Complete_Updates On-Click
  observe({
    req(input$Complete_Updates_rows_selected)
    
    selRow <- input$Complete_Updates_rows_selected
    data <- CompleteRow()[selRow, ] 
    
    col_names <- c(
      OutcomeArea = "Outcome",
      OutcomeSubarea = "Group",
      OnjectiveID = "ID",
      Description = "Description",
      Lead = "Lead",
      Status = "Status",
      Date = "Date",
      RAG = "RAG",
      Update = "Update",
      Risks_Issues = "Risks/Issues",
      Mitigating_Actions = "Mitigating Actions",
      Decisions_Required = "Decisions Required",
      Forward_Look = "Forward Look",
      PreviousDate = "Prev. Date",
      PreviousRAG = "Prev. RAG"
    )
    
    row_ui <- lapply(names(data), function(col_name) {
      tags$p(
        tags$strong(paste0(col_names[col_name], ": ")),
        as.character(data[[col_name]])
      )
    })
    
    showModal(modalDialog(
      
      title = "Update Detail",
      do.call(tagList, row_ui),
      easyClose = TRUE
      
    ))
  })
  
  # Without_Updates On-Click
  observe({
    req(input$Without_Updates_rows_selected)
    
    selRow <- input$Without_Updates_rows_selected
    data <- WithoutRow()[selRow, ] 
    
    col_names <- c(
      Activity_ID = "ID",
      Source = "Source",
      OutcomeArea = "Outcome",
      OutcomeSubarea = "Group",
      Type = "Type",
      Number = "Number",
      Description = "Description",
      Lead = "Lead",
      Status = "Status")
    
    row_ui <- lapply(names(data), function(col_name) {
      tags$p(
        tags$strong(paste0(col_names[col_name], ": ")),
        as.character(data[[col_name]])
      )
    })
    
    showModal(modalDialog(
      
      title = "Update Detail",
      do.call(tagList, row_ui),
      easyClose = TRUE
      
    ))
  })
  
  # Source On-Click
  observe({
    req(input$source_rows_selected)
    
    selRow <- input$source_rows_selected
    data <- ProductSources()[selRow, ] 
    
    selRow = 0
    
    Global_Source_Name <<- data$source
    
    outcomes_filtered <- filter(outcomes, Source == data$source)
    
    output$outcomes_filtered <- DT::renderDataTable(outcomes_filtered, 
                                                    colnames = c("Source", "ID", "Description"),
                                                    selection = 'single', rownames = FALSE)
    
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
    
    output$outcomegroups_filtered <- DT::renderDataTable(outcomegroups_filtered, 
                                                         colnames = c("Source", "ID", "Description"),
                                                         selection = 'single', rownames = FALSE)
    
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
    
    output$objectives_filtered <- DT::renderDataTable(objectives_filtered, 
                                                      colnames = c("ID", "Source", "Outcome",
                                                                   "Group", "Type", "Ref #",
                                                                   "Description", "Lead", "Status"),
                                                      selection = 'single', rownames = FALSE)
    
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
    
    output$board_objective_detail <- DT::renderDataTable(board_objective_detail,
                                                         colnames = c("ID", "Source", "Outcome",
                                                                      "Group", "Type",
                                                                      "Description", "Lead", "Status"),
                                                         selection = 'single', rownames = FALSE)
  })
  
  # Objective Update Screen - select board
  observeEvent(input$projectBoard, {
    disable("RAGStatus")
    disable("latestUpdate")
    disable("updateDate")
    disable("returnToGreen")
    disable("risksIssues")
    disable("decisionsRequired")
    disable("forwardLook")
    disable("decisionsRequired")
    disable("forwardLook")
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
      updateTextInput(session, "risksIssues", value = "")
      updateTextInput(session, "decisionsRequired", value = "")
      updateTextInput(session, "forwardLook", value = "")
    } else {
      Global_Objective_ID <<- input$projectObjectives
      
      objectiveUpdates_filtered <<- filter(objectiveUpdates, (ObjectiveID == input$projectObjectives & 
                                                                Board_Acronym %like% Global_Board_Acronym))
      
      objectiveUpdates_filtered$Date <<- as.Date(objectiveUpdates_filtered$Date, format ="%d/%m/%Y")
      objectiveUpdates_filtered <<- setorder(objectiveUpdates_filtered, -Date)
      
      if (nrow(objectiveUpdates_filtered) == 0) {
        disable("editObjectiveUpdate")
      } else {
        enable("editObjectiveUpdate")
      }
      
      latestUpdate <<- objectiveUpdates_filtered[1, , drop = FALSE]
      
      updateSelectInput(session, "RAGStatus", selected = latestUpdate$RAG)
      updateTextInput(session, "latestUpdate", value = latestUpdate$Update)
      updateDateInput(session, "updateDate", value = as.Date(latestUpdate$Date, format ="%d/%m/%Y"))
      updateTextInput(session, "returnToGreen", value = latestUpdate$Mitigating_Actions)
      updateTextInput(session, "risksIssues", value = latestUpdate$Risks_Issues)
      updateTextInput(session, "decisionsRequired", value = latestUpdate$Decisions_Required)
      updateTextInput(session, "forwardLook", value = latestUpdate$Forward_Look)
      
      objectiveUpdates_previous <- objectiveUpdates_filtered[-1, ]
      
      output$objectiveUpdates_filtered <- DT::renderDataTable({datatable(objectiveUpdates_previous, 
                                                                         colnames = c("Board", "ID", "Date",
                                                                                      "RAG", "Update", "Risks/issues",
                                                                                      "Mitigating Actions", "Decisions Required", "Forward Look"),
                                                                         options = list(dom = 'p'),
                                                                         selection = 'single', rownames = FALSE) %>%
          formatStyle("RAG",
                      "text-align" = 'center',
                      backgroundColor = styleEqual(
                        c("Red", "Amber", "Green", "Complete"),
                        c(gov_red, gov_yellow, 'green', "blue")
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
    print(boards)
    Global_Board_Acronym <<- input$dashBoard
    Board_filtered <- filter(boards, Board_Acronym == Global_Board_Acronym)
    Global_Board_Name <<- Board_filtered$Board_Name
    print(Global_Board_Name)
    Global_Board_Date <<- input$boardDate
    updateDashboard()
  })
  
  # Dashboard Screen - select board date
  observeEvent(input$boardDate, {
    Global_Board_Acronym <<- input$dashBoard
    Board_filtered <- filter(boards, Board_Acronym == Global_Board_Acronym)
    Global_Board_Name <<- Board_filtered$Board_Name
    Global_Board_Date <<- input$boardDate
    updateDashboard()
  })
  
  observeEvent(input$help_modal, {
    
    if (input$pages == "KPIMaint") {
      showModal(modalDialog(
        title = "KPI Source, Outcome & Group Maintenance",
        includeHTML("www/help_source.html"),
        easyClose = TRUE
      ))
    } else if (input$pages == "objectivespage") {
      showModal(modalDialog(
        title = "Objective Update",
        includeHTML("www/help_update.html"),
        easyClose = TRUE
      ))
    } else if (input$pages == "BoardMaint") {
      showModal(modalDialog(
        title = "Board Maintenance",
        includeHTML("www/help_board.html"),
        easyClose = TRUE
      ))
    }  else if (input$pages == "dashboardspage") {
      showModal(modalDialog(
        title = "Dashboard",
        includeHTML("www/help_dashboard.html"),
        easyClose = TRUE
      ))
    }
    
  })
  
  #Helper function: Update Dashboard Page
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
                                                       "Risks_Issues",
                                                       "Mitigating_Actions",
                                                       "Decisions_Required",
                                                       "Forward_Look"
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
    
    Red_Updates <<- filter(latestUpdates, (RAG == "Red"))
    Amber_Updates <<- filter(latestUpdates, (RAG == "Amber"))
    Green_Updates <<- filter(latestUpdates, (RAG == "Green"))
    Complete_Updates <<- filter(latestUpdates, (RAG == "Complete"))
    
    output$numberOfRed <-renderUI({
      value <- nrow(Red_Updates)
      tags$h2(
        paste0(value), 
        style = "font-size: 36px;"
      )
    })
    
    output$numberOfAmber <-renderUI({
      value <- nrow(Amber_Updates)
      tags$h2(
        paste0(value), 
        style = "font-size: 36px;"
      )
    })
    
    output$numberOfGreen <-renderUI({
      value <- nrow(Green_Updates)
      tags$h2(
        paste0(value), 
        style = "font-size: 36px;"
      )
    })
    
    output$numberOfComplete <-renderUI({
      value <- nrow(Complete_Updates)
      tags$h2(
        paste0(value), 
        style = "font-size: 36px;"
      )
    })
    
    output$Red_Updates <<- DT::renderDT({datatable(Red_Updates, 
                                                   colnames = c("Outcome", "Group", "ID",
                                                                "Description", "Lead", "Status",
                                                                "Date", "RAG", "Update",
                                                                "Risks/Issues", "Mitigating Actions",
                                                                "Decisions Required", "Forward Look",
                                                                "Prev. Date", "Prev. RAG"),
                                                   rownames = FALSE, options = list(
                                                     dom = 'p',
                                                     scrollX = TRUE,
                                                     pageLength = 5,
                                                     columnDefs = list(
                                                       list(width = '60px', targets = c(2, 5:7, 13:14)),
                                                       list(width = '120px', targets = c(0:1, 3, 4, 8:12)),
                                                       list(
                                                         targets = c(3, 8:12), # Column index (0-based)
                                                         render = JS(
                                                           "function(data, type, row, meta) {",
                                                           "  if (type !== 'display') return data;",
                                                           "  if (data === null || data === undefined || data === '') return '';",
                                                           "  return data.length > 40 ? ",
                                                           "    '<span title=\"' + data + '\">' + data.substr(0, 40) + '...</span>' : data;",
                                                           "}"
                                                         ))
                                                     ),
                                                     selection = 'single', width = "100%")) %>%
        formatStyle(columns = c("RAG", "PreviousRAG"),
                    "text-align" = 'center',
                    backgroundColor = styleEqual(
                      c("Red", "Amber", "Green", "Complete"),
                      c(gov_red, gov_yellow, gov_green, gov_blue)
                    ),
                    color = styleEqual(
                      c("Red", "Amber", "Green", "Complete"),
                      c('white', gov_black, 'white', 'white')
                    )
        )
    })
    
    output$Amber_Updates <<- DT::renderDT({datatable(Amber_Updates, 
                                                     colnames = c("Outcome", "Group", "ID",
                                                                  "Description", "Lead", "Status",
                                                                  "Date", "RAG", "Update",
                                                                  "Risks/Issues", "Mitigating Actions",
                                                                  "Decisions Required", "Forward Look",
                                                                  "Prev. Date", "Prev. RAG"),
                                                     rownames = FALSE, options = list(
                                                       dom = 'p',
                                                       scrollX = TRUE,
                                                       pageLength = 5,
                                                       columnDefs = list(
                                                         list(width = '60px', targets = c(2, 5:7, 13:14)),
                                                         list(width = '120px', targets = c(0:1, 3, 4, 8:12)),
                                                         list(
                                                           targets = c(3, 8:12), # Column index (0-based)
                                                           render = JS(
                                                             "function(data, type, row, meta) {",
                                                             "  if (type !== 'display') return data;",
                                                             "  if (data === null || data === undefined || data === '') return '';",
                                                             "  return data.length > 40 ? ",
                                                             "    '<span title=\"' + data + '\">' + data.substr(0, 40) + '...</span>' : data;",
                                                             "}"
                                                           ))
                                                       ),
                                                       selection = 'single', width = "100%"))  %>%
        formatStyle(columns = c("RAG", "PreviousRAG"),
                    "text-align" = 'center',
                    backgroundColor = styleEqual(
                      c("Red", "Amber", "Green", "Complete"),
                      c(gov_red, gov_yellow, gov_green, gov_blue)
                    ),
                    color = styleEqual(
                      c("Red", "Amber", "Green", "Complete"),
                      c('white', gov_black, 'white', 'white')
                    )
        )
    })
    
    output$Green_Updates <<- DT::renderDT({datatable(Green_Updates, 
                                                     colnames = c("Outcome", "Group", "ID",
                                                                  "Description", "Lead", "Status",
                                                                  "Date", "RAG", "Update",
                                                                  "Risks/Issues", "Mitigating Actions",
                                                                  "Decisions Required", "Forward Look",
                                                                  "Prev. Date", "Prev. RAG"),
                                                     rownames = FALSE,  options = list(
                                                       dom = 'p',
                                                       scrollX = TRUE,
                                                       pageLength = 5,
                                                       columnDefs = list(
                                                         list(width = '60px', targets = c(2, 5:7, 13:14)),
                                                         list(width = '120px', targets = c(0:1, 3, 4, 8:12)),
                                                         list(
                                                           targets = c(3, 8:12), # Column index (0-based)
                                                           render = JS(
                                                             "function(data, type, row, meta) {",
                                                             "  if (type !== 'display') return data;",
                                                             "  if (data === null || data === undefined || data === '') return '';",
                                                             "  return data.length > 40 ? ",
                                                             "    '<span title=\"' + data + '\">' + data.substr(0, 40) + '...</span>' : data;",
                                                             "}"
                                                           ))
                                                       ),
                                                       selection = 'single', width = "100%"))  %>%
        formatStyle(columns = c("RAG", "PreviousRAG"),
                    "text-align" = 'center',
                    backgroundColor = styleEqual(
                      c("Red", "Amber", "Green", "Complete"),
                      c(gov_red, gov_yellow, 'green', "blue")
                    ),
                    color = styleEqual(
                      c("Red", "Amber", "Green", "Complete"),
                      c('white', 'black', 'white', 'white')
                    )
        )
    })
    
    output$Complete_Updates <<- DT::renderDT({datatable(Complete_Updates, 
                                                        colnames = c("Outcome", "Group", "ID",
                                                                     "Description", "Lead", "Status",
                                                                     "Date", "RAG", "Update",
                                                                     "Risks/Issues", "Mitigating Actions",
                                                                     "Decisions Required", "Forward Look",
                                                                     "Prev. Date", "Prev. RAG"),
                                                        rownames = FALSE, options = list(
                                                          dom = 'p',
                                                          scrollX = TRUE,
                                                          pageLength = 5,
                                                          columnDefs = list(
                                                            list(width = '60px', targets = c(2, 5:7, 13:14)),
                                                            list(width = '120px', targets = c(0:1, 3, 4, 8:12)),
                                                            list(
                                                              targets = c(3, 8:12), # Column index (0-based)
                                                              render = JS(
                                                                "function(data, type, row, meta) {",
                                                                "  if (type !== 'display') return data;",
                                                                "  if (data === null || data === undefined || data === '') return '';",
                                                                "  return data.length > 40 ? ",
                                                                "    '<span title=\"' + data + '\">' + data.substr(0, 40) + '...</span>' : data;",
                                                                "}"
                                                              ))
                                                          ),
                                                          selection = 'single', width = "100%"))  %>%
        formatStyle(columns = c("RAG", "PreviousRAG"),
                    "text-align" = 'center',
                    backgroundColor = styleEqual(
                      c("Red", "Amber", "Green", "Complete"),
                      c(gov_red, gov_yellow, 'green', "blue")
                    ),
                    color = styleEqual(
                      c("Red", "Amber", "Green", "Complete"),
                      c('white', 'black', 'white', 'white')
                    )
        )
    })
    
    Without_Updates <<- anti_join(objectives, boardObjectiveUpdates, by = c("Activity_ID" = "ObjectiveID"))
    Without_Updates <<- inner_join(Without_Updates, boardobjectives, by = c("Activity_ID" = "ObjectiveID")) 
    Without_Updates <<- filter(Without_Updates, (Board_Acronym == Global_Board_Acronym))
    Without_Updates <<- Without_Updates[, -ncol(Without_Updates)]
    
    print(nrow(Without_Updates))
    
    output$Without_Updates <<- DT::renderDT({datatable(Without_Updates, 
                                                       colnames = c("ID", "Source", "Outcome", 
                                                                    "Group", "Type",
                                                                    "Number", "Description", "Lead",
                                                                    "Status"),
                                                       rownames = FALSE, options = list(
                                                         dom = 'p',
                                                         pageLength = 5,
                                                         columnDefs = list(
                                                           list(width = '60px', targets = c(0:2, 4:5, 8)),
                                                           list(width = '120px', targets = c(3, 6:7)),
                                                           list(
                                                             targets = 6, # Column index (0-based)
                                                             render = JS(
                                                               "function(data, type, row, meta) {",
                                                               "  if (type !== 'display') return data;",
                                                               "  if (data === null || data === undefined || data === '') return '';",
                                                               "  return data.length > 40 ? ",
                                                               "    '<span title=\"' + data + '\">' + data.substr(0, 40) + '...</span>' : data;",
                                                               "}"
                                                             ))
                                                         )),
                                                       selection = 'single', width = "100%")
    })
    
    output$numberWithoutUpdate <-renderUI({
      value <- nrow(Without_Updates)
      tags$h2(
        paste0(value), 
        style = "font-size: 36px;"
      )
    })
    
    BoardAction_filtered <<- filter(boardActions, 
                                    (Board_Acronym == Global_Board_Acronym)) %>%
      select(-Board_Acronym)
    
    BoardAction_open <- filter(BoardAction_filtered, 
                               !(Status == "Complete"))
    
    output$numberOfActions <-renderUI({
      value <- nrow(BoardAction_open)
      tags$h2(
        paste0(value), 
        style = "font-size: 36px;"
      )
    })
    
    output$BoardActions <- DT::renderDataTable({datatable(BoardAction_filtered,
                                                          colnames = c("Reference", "Date Created", "Description",
                                                                       "Owner", "Status", "Update"),
                                                          rownames = FALSE, selection = 'single', width = "100%")})
  }
  
  #Helper function: render RAG Status colour
  output$statusBox <- renderUI({
    status_color <- switch(input$RAGStatus,
                           "Green" = gov_green,
                           "Amber" = gov_yellow,
                           "Red" = gov_red,
                           "Complete" = gov_blue,
                           "white")  # default fallback
    #div(
    #style = "overflow-y: hidden !important;",
    
    value_box(
      title = "",
      value = "",
      theme = value_box_theme(bg = status_color, fg = "#FFFFFF"),
      height = "60px" 
    )
    #)
  })
  
  #Helper function: render status bar text
  output$status_bar <- renderUI({
    div(class = "status-bar",
        status_msg())
  })
  
  #Helper function: Create dashboard powerpoint slide
  output$generatePPT <- downloadHandler(
    filename = function() {
      paste0("Board_Summary_", Sys.Date(), ".pptx")
    },
    content = function(file) {
      showPageSpinner(
        caption = "Generating your PowerPoint… please wait",
        size = 0.8
      )
      
      # Start presentation
      ppt <- read_pptx(path = "standard_template.pptx")
      
      print(layout_properties(ppt, layout = "Title and Content", master = "Office Theme"))
      
      board_name <- filter(boards, Board_Acronym == Global_Board_Acronym)
      
      # Add RAG status summary slide
      ppt <- add_slide(ppt, layout = "Title Slide", master = "Office Theme")
      
      ppt <- ph_with(ppt, board_name$Board_Name,
                     location = ph_location_type("ctrTitle"))
      
      ppt <- ph_with(ppt, format(Global_Board_Date, "%d/%m/%Y"),
                     location = ph_location_type("subTitle"))
      
      ppt <- remove_slide(ppt, index = 1)
      
      
      ppt <- add_slide(ppt, layout = "Blank", master = "Office Theme")
      
      ppt <- ph_with(ppt,
                     value = block_list(fpar(ftext("RAG Summary", fp_text(font.size = 44, bold = FALSE)))),
                     location = ph_location(left = .92, top = .40, width = 11.5, height = 1.45))
      
      n_red <- nrow(Red_Updates)
      n_amber <- nrow(Amber_Updates)
      n_green <- nrow(Green_Updates)
      n_complete <- nrow(Complete_Updates)
      
      # Create RAG status ggplot
      rag_data <- data.frame(
        Status = factor(c("Red", "Amber", "Green", "Complete"), levels = c("Red", "Amber", "Green", "Complete")),
        Count = c(n_red, n_amber, n_green, n_complete),
        Color = c(gov_red, gov_yellow, gov_green, gov_blue)
      )
      
      rag_plot <- ggplot(rag_data, aes(x = 1, y = 1, fill = Color)) +
        geom_tile(width = 0.9, height = 0.9, color = "black") +
        geom_text(
          aes(label = paste(Status, "\n", Count)),
          color = "white",
          size = 6,
          fontface = "bold"
        ) +
        scale_fill_identity() +
        facet_wrap(~Status, ncol = 2) +
        theme_void() +
        theme(strip.text = element_blank())
      
      
      
      
      # Add RAG status plot to slide
      ppt <- ph_with(ppt, value = rag_plot, 
                     location = ph_location(left = .92, top = 2, width = 11.5, height = 4.76))
      
      # Actions slide
      
      ppt <- add_slide(ppt, layout = "Blank", master = "Office Theme")
      ppt <- ph_with(ppt,
                     value = block_list(fpar(ftext("Actions", fp_text(font.size = 44, bold = FALSE))
                     )),
                     location = ph_location(left = .92, top = .40, width = 11.5, height = 1.45))  
      
      if (nrow(BoardAction_filtered) > 0) {
        actions <- BoardAction_filtered[] %>%
          flextable() %>%
          bold(part = "header") %>%
          fontsize(size = 10, part = "all") %>%
          bg(i = ~ (seq_len(nrow(BoardAction_filtered)) %% 2 == 1), part = "body", bg = "#FFFFFF") %>%
          bg(i = ~ (seq_len(nrow(BoardAction_filtered)) %% 2 == 0), part = "body", bg = "#F0F0F0") %>%
          bg(part = "header", bg = "#156082") %>%
          color(part = "header", color = "white") %>%
          fix_border_issues() %>% 
          set_header_labels(
            Reference = "Reference",
            CreatedDate = "Date Created",
            ActionDesc = "Description",
            ActionOwner = "Owner",
            Status = "Status",
            Update = "Update"
          )
        
        actions <- actions %>%
          width(j = 1, width = 1.2) %>%
          width(j = 2, width = 1.2) %>%
          width(j = 3, width = 3.8) %>%
          width(j = 4, width = 1.7) %>%
          width(j = 5, width = 1.2) %>%
          width(j = 6, width = 3.8) %>%
          set_table_properties(layout = "fixed")
        
        ppt <- ph_with(ppt, value = actions, location = ph_location(left = 0.2, top = 2, width = 8.75, height = 1.5))
      } else {
        title2 <- block_list(fpar(ftext("There are no actions for this board", fp_text(font.size = 14, bold = FALSE))))
        ppt <- ph_with(ppt, value = title2, location = ph_location(left = 0.2, top = 2, width = 8.75, height = 1.5))
      }
      
      render_outcomes <- function(data, ppt) {
        for (i in 1:nrow(data)) {
          title1 <- ftext(paste0("Outcome ", data$ObjectiveID[i], ": "), fp_text(font.size = 44, bold = FALSE))
          title2 <- ftext(data$Description[i], fp_text(font.size = 24, bold = FALSE))
          
          title <- block_list(fpar(title1, title2))
          
          make_table <- function(cols) {
            data[i, cols, drop = FALSE] %>%
              flextable() %>%
              bold(part = "header") %>%
              fontsize(size = 10, part = "all") %>%
              bg(part = "body", bg = "#FFFFFF") %>%
              bg(part = "header", bg = "#156082") %>%
              color(part = "header", color = "white") %>%
              fix_border_issues()
          }
          
          topTable <- make_table(c("OutcomeArea", "OutcomeSubarea", "Lead", "Status")) %>% 
            set_header_labels(
              OutcomeArea = "Outcome",
              OutcomeSubarea = "Group",
              Lead = "Lead",
              Status = "Status"
            )
          
          
          midTable <- make_table(c("Date", "RAG", "PreviousDate", "PreviousRAG")) %>%
            bg(j = "RAG", i = ~RAG == "Red", bg = gov_red) %>%
            bg(j = "RAG", i = ~RAG == "Amber", bg = gov_yellow) %>%
            bg(j = "RAG", i = ~RAG == "Green", bg = gov_green) %>%
            bg(j = "PreviousRAG", i = ~PreviousRAG == "Red", bg = gov_red) %>%
            bg(j = "PreviousRAG", i = ~PreviousRAG == "Amber", bg = gov_yellow) %>%
            bg(j = "PreviousRAG", i = ~PreviousRAG == "Green", bg = gov_green)  %>% 
            set_header_labels(
              Date = "Date",
              RAG = "RAG",
              PreviousDate = "Prev. Date",
              PreviousRAG = "Prev. RAG"
            )
          
          bottomTable <- make_table(c("Update", "Risks_Issues", "Mitigating_Actions", "Decisions_Required", "Forward_Look"))  %>% 
            set_header_labels(
              Update = "Update",
              Risks_Issues = "Risks/Issues",
              Mitigating_Actions = "Mitigating Actions",
              Decisions_Required = "Decisions Required",
              Forward_Look= "Forward Look"
            )
          
          topTable <- topTable  %>%
            width(j = 1, width = 2.5) %>%
            width(j = 2, width = 2.5) %>%
            width(j = 3, width = 2.0) %>%
            width(j = 4, width = 1) %>%
            set_table_properties(layout = "fixed")
          
          midTable <-  midTable %>%
            width(j = 1, width = 1.2) %>%
            width(j = 2, width = 1.2) %>%
            width(j = 3, width = 1.2) %>%
            width(j = 4, width = 1.2) %>%
            set_table_properties(layout = "fixed")
          
          bottomTable <- bottomTable %>%
            width(j = 1, width = 2.6) %>%
            width(j = 2, width = 2.6) %>%
            width(j = 3, width = 2.6) %>%
            width(j = 4, width = 2.6) %>%
            width(j = 5, width = 2.5) %>%
            set_table_properties(layout = "fixed")
          
          ppt <- add_slide(ppt, layout = "Blank", master = "Office Theme")
          ppt <- ph_with(ppt,
                         value = title,
                         location = ph_location(left = .92, top = .40, width = 11.5, height = 1.45))
          
          # ppt <- ph_with(ppt, value = title, location = ph_location_type(type = "title"))
          ppt <- ph_with(ppt, value = topTable, location = ph_location(left = 0.2, top = 2, width = 8.75, height = 1.5))
          ppt <- ph_with(ppt, value = midTable, location = ph_location(left = 8.3, top = 2, width = 8.75, height = 1.5))
          ppt <- ph_with(ppt, value = bottomTable, location = ph_location(left = 0.2, top = 2.8, width = 18, height = 2))
        }
        
        return(ppt)
      }
      
      if (!is.null(Red_Updates) && nrow(Red_Updates) > 0) {
        ppt <- render_outcomes(Red_Updates, ppt)
      }
      if (!is.null(Amber_Updates) && nrow(Amber_Updates) > 0) {
        ppt <- render_outcomes(Amber_Updates, ppt)
      }
      
      # Export the final presentation
      print(ppt, target = file)
      
      hidePageSpinner()
      
    }
  )
  
  #Helper function: render data saved timestamp
  dataSaved <- function() {
    status_msg(paste("Data saved at", Sys.time()))
    
    delay(4000, status_msg("Ready"))
    
  }
  
}