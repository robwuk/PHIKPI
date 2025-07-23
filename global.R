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
library(officer)
library(bslib)
library(rmarkdown)
library(flextable)
library(ggplot2)
library(gridExtra)
library(grid)
library(ggtext)
library(shinycssloaders)


linebreaks <- function(n){HTML(strrep(br(), n))}

#Internal tables
KPI_type <- data.frame(type = c("Objective", "KPI"))
objectiveStatus <- data.frame(type = c("In Progress", "Not Started", "Blocked", "Completed"))

#Load data
KPISourceFile <- "data/sources.csv"
KPI_source <- read_csv(KPISourceFile)
KPISourceDate <- file.info(KPISourceFile)$mtime

outcomesFile <- "data/outcomes.csv"
outcomes <- read_csv(outcomesFile)
outcomesDate <- file.info(outcomesFile)$mtime

outcomeGroupsFile <- "data/OutcomeGroups.csv"
outcomegroups <- read_csv(outcomeGroupsFile)
outcomeGroupsDate <- file.info(outcomeGroupsFile)$mtime

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

objectiveUpdates$Mitigating_Actions[is.na(objectiveUpdates$Mitigating_Actions)] <- ""

#Initialise global variables
Global_Source_Name <- ""
Global_Outcome_ID <- ""
Global_Grouping <- ""
Global_Outcome_Desc <- ""
Global_Board_Acronym <- ""
Global_Objective_ID <- ""
Global_Board_date <- ""
Global_Board_Name <- ""
objectives_filtered <- ""
board_objective_detail <- ""
latestUpdate <- ""
objectiveUpdates_filtered <- ""
allLeads <- ""
BoardAction_filtered <- ""
Red_Updates <- ""
Amber_Updates <- ""
Green_Updates <- ""
Complete_Updates <- ""
Without_Updates <- ""
otherObjectives <- ""

#Set default colours
gov_red <- 	"#d4351c"
gov_yellow <- "#ffdd00"
gov_green <- "#00703c"
gov_blue <- "#1d70b8"
gov_black <- "#0b0c0c"
gov_dark_grey <- "#505a5f"
gov_mid_grey <- "#b1b4b6"
            
          #Set theme
          my_theme <- bs_theme(
            bg = "#FFFFFF",       # light background
            fg = gov_black,       # Light text
            primary = gov_black,  # Buttons like actionButton(..., class = "btn-primary")
            success = gov_green,  # btn-success
            danger = gov_red    # btn-danger
          ) %>% bs_add_rules("
  .table.dataTable {
    border: 1px solid #b1b4b6;
    border-collapse: collapse;
  }
  
  .bslib-value-box {
    overflow-y: hidden !important;
  }

  .bslib-value-box .html-fill-container,
  .bslib-value-box .html-fill-item {
    overflow-y: hidden !important;
  }
  
  /* Unselected tab background */
  .nav-tabs .nav-link {
    background-color: 	#f3f2f1;
    color: 	#0b0c0c;
  }

  /* Selected (active) tab background */
  .nav-tabs .nav-link.active {
    background-color: #0b0c0c;
    color: #FFFFFF;
    font-weight: bold;
  }

  /* Optional: hover effect */
  .nav-tabs .nav-link:hover {
    background-color: #505a5f;
  }
  
  .btn-primary:hover {
    background-color: #505a5f !important;
    color: #FFFFFF !important;
  }

")
  
 ################################################################################
 #### Helper Functions
 ################################################################################
          
 # Helper function: to align columns
# align_columns <- function(df_list) {
#  all_cols <- unique(unlist(lapply(df_list, names)))
#            
#  lapply(df_list, function(df) {
#    missing_cols <- setdiff(all_cols, names(df))
#    df[missing_cols] <- NA  # Fill missing with NA
#    df[all_cols]  # Reorder columns
#  })
#}  



#Helper function: check if file has changed since it was loaded  
checkFileDates <- function(dataType) {
  date_map <- list(
    KPI = KPISourceDate,
    Outcomes = outcomesDate,
    outcomeGroups = outcomeGroupsDate,
    Objectives = objectivesDate,
    boards = boardsDate,
    boardObjectives = boardObjectivesDate,
    objectiveUpdates = objectiveUpdatesDate,
    actionUpdates = boardActionsDate
  )
  
  file_map <- list(
    KPI = KPISourceFile,
    Outcomes = outcomesFile,
    outcomeGroups = outcomeGroupsFile,
    Objectives = objectivesFile,
    boards = boardsFile,
    boardObjectives = boardObjectivesFile,
    objectiveUpdates = objectiveUpdatesFile,
    actionUpdates = boardActionsFile
  )
  
  if (!dataType %in% names(date_map)) return(FALSE)
  
  return(date_map[[dataType]] < file.info(file_map[[dataType]])$mtime)
}

#Helper function: disable latest update fields
disableEdit <- function() {
  to_disable <- c("RAGStatus", "latestUpdate", "updateDate", "returnToGreen",
                 "risksIssues", "decisionsRequired", "forwardLook",
                 "saveObjectiveUpdate", "undoObjectiveUpdate")
  
  to_enable <- c("editObjectiveUpdate", "addObjectiveUpdate")
  
  lapply(to_enable, enable)
  lapply(to_disable, disable)
}

#Helper function: enable latest update fields
enableEdit <- function() {
  to_enable <- c("RAGStatus", "latestUpdate", "updateDate", "returnToGreen",
                 "risksIssues", "decisionsRequired", "forwardLook",
                 "saveObjectiveUpdate", "undoObjectiveUpdate")
  
  to_disable <- c("editObjectiveUpdate", "addObjectiveUpdate")
  
  lapply(to_enable, enable)
  lapply(to_disable, disable)
}

#Helper function: update error message when saving data
modalErrorUI <- function(message) {
  div(style = "color: red; font-weight: bold;", message)
}

