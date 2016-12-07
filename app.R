############################################################################################################################
#### ----------------------------------------------------------------------------------------------------------------- #####
#### --------------------------------- Ascendance: A New Paradigm in Time Management --------------------------------- #####
#### ----------------------------------------------------------------------------------------------------------------- #####
############################################################################################################################

#' Ascendance is the evolution of TimeProg, a predecessor program, with many more degrees of flexibility and a coherent
#' central structure. This is the program's source code, containing the functions essential for its operation, integrated
#' into a Shiny application for interactive usage.

#' To run this application for the first time you will need to uncomment the code below and run it 
#' (make sure each package installs completely before installing the next, and don't forget to re-comment
#' the code when you've finished running it). Then you will need to create the following folder 
#' structure somewhere in your computer: 'Ascendance > App > Setup' and replace the 'filepaths' object 
#' below with the path to the first folder in that sequence (don't forget the ending slash). Additionally, 
#' you will need to change the name of the R file, currently being saved to the 'paths' object below the 
#' sourcing code, from 'A_Johann.R' to 'A_[your name].R'. Once all this is done, just hit the 'Run App' button
#' at the top of the screen, and you will be launched into the setup process.

#' IMPORTANT NOTES CONCERNING THE SETUP PROCESS: At the current time, there are numerous inconveniences you will
#' have to suffer to use this program. Firstly, do not click the "back" button; you will probably lose your data, 
#' or other terrible things will happen. Second, do not hit 'Submit' or 'Go' buttons more than once; much of the 
#' time, you will not recieve feedback that you cliked a button, but the action specified by that button will still
#' occur. Think carefully before you click 'Next'. Read all of the text in each of the modules, as they have been
#' updated to inform you about how to avoid problems.
#' 
# install.packages("RColorBrewer"); install.packages("ggplot2"); install.packages("devtools")
# require(devtools); install_github("rstudio/shiny");
# install.packages("shinydashboard"); install.packages("shinyjs"); install.packages("magrittr"); 
# install.packages("lubridate"); install.packages("tibble")

require(RColorBrewer)
require(ggplot2)
require(shinydashboard)
require(shinyjs)
require(magrittr)
require(lubridate)
require(devtools)
require(tibble)

require(shiny)

# filepaths <- "D:/R/Ascendance/"
# filepaths <- "C:/Users/Johann/Documents/Ascendance/"
filepaths <- "/home/alan/Documents/Ascendance/"

source(paste0(filepaths, "App/Setup/setup_focusAreas.R"))
source(paste0(filepaths, "App/Setup/setup_categories.R"))
source(paste0(filepaths, "App/Setup/setup_focusPeriod.R"))
source(paste0(filepaths, "App/Setup/setup_taskSchedule.R"))
source(paste0(filepaths, "App/AssessmentModule.R"))
source(paste0(filepaths, "App/InputModule.R"))

#  "A_Johann.R"
path <- paste0(filepaths, "A_Johann6.R")

options(shiny.reactlog=TRUE)

#' GenFunc

nameObject <- function(name, object, Envir = .GlobalEnv) {
  nm <- name
  v <- object
  assign(nm,v, envir = Envir)
}

tic <- function(gcFirst = TRUE, type=c("elapsed", "user.self", "sys.self")) {
  type <- match.arg(type)
  assign(".type", type, envir=baseenv())
  if(gcFirst) gc(FALSE)
  tic <- proc.time()[type]         
  assign(".tic", tic, envir=baseenv())
  invisible(tic)
}

toc <- function() {
  type <- get(".type", envir=baseenv())
  toc <- proc.time()[type]
  tic <- get(".tic", envir=baseenv())
  print(toc - tic)
  invisible(toc)
}

#'

#' InputFunc

build_slider <- function(id, label, mxmi=c(0,1), val= runif(1, min = 0, max = 1)){
  sliderInput(id,
              label = label,
              min = mxmi[1],
              max = mxmi[2],
              value = val, 
              ticks = FALSE, 
              round = FALSE)
}

build_calendar <- function(id, label) {
  dateRangeInput(id, label = label)
}

build_time <- function(id, label) {
  textInput(id, label = label, value = "0")        
}

build_focus_input <- function(focusAreas) {
  fluidRow(h3(paste(focusAreas)))          
}

build_goals <- function(id, label, value) {
  textInput(id, label = label, value)        
}

pieChart_output <- function(id) {
  
  column(3, plotOutput(paste("pie_", id, sep = "")))
  
}

colfunc <- colorRampPalette(c("slategray1", "deepskyblue4"))

pieChart_render <- function(id, groups) {
  
  output[[paste("pie_", id, sep = "")]] <- renderPlot({
    
    weights <- rep(1, length(groups))/length(groups)
    
    dfT <- data.frame(subjects = groups, value = weights)
    
    ggplot(dfT, aes(x = "", y = value, fill = groups)) + geom_bar(width = 2, stat = "identity") +
      coord_polar("y", start=0) + scale_fill_manual(values = colfunc(length(groups))) + 
      theme(axis.title.x = element_blank()) + theme(axis.title.y = element_blank())
    
  })
  
}


priorityOutput <- function(inputlist) {
  df <- data.frame(
    FocusArea = rv$afg,
    value = inputlist
  )
}

category_observer <- function(id) {
  
  observeEvent(input$submitcat, {
    categories <- unlist(strsplit(unlist(strsplit(input[[paste("focusAreas", id, sep = "_")]], ",")), " "))
    categories <- categories[-which(categories == "")]
    
    A$afg[[paste("focusAreas", id, sep = "_")]] <- categories
    
    rv$afg[[paste("focusAreas", id, sep = "_")]] <- categories
    
  })
}

if (file.exists(path) == TRUE) {
  
  load(path, envir = .GlobalEnv)
  
  header <- dashboardHeader(title = "Ascendance (R)")
  
  sidebar <- dashboardSidebar( sidebarMenuOutput("menu") )
  
  body <- dashboardBody({
    
    tabItems(
      
      tabItem(tabName = "assessment_tab",
              h2(""),
              uiOutput("uiAssess")),
      
      tabItem(tabName = "input_tab",
              h2(""),
              uiOutput("uiInput"))
              
      )
    
    
    
  })
  
  ui <- dashboardPage(header, sidebar, body)
  
  server <- function(input, output) {
    
    output$menu <- renderMenu({
      
      sidebarMenu(
        menuItem("Assess", tabName = "assessment_tab", icon = icon("area-chart")),
        menuItem("Manage", tabName = "main_tab", icon = icon("cog"),
                 menuSubItem("Focus areas", tabName = "Mfocus"),         
                 menuSubItem("Ascendance Options", tabName = "Mao")
        ),
        
        menuItem("Input", tabName = "input_tab", icon = icon("archive")),
        menuItem("Predict", tabName = "prediction_tab", icon = icon("line-chart")),
        menuItem("Plan", tabName = "planning_tab", icon = icon("calendar")),
        menuItem("View reports", tabName = "report_tab", icon = icon("file"))
      )
      
    })
    
    callModule(assess, "assess", Path = path); callModule(Input, "input", Path = path)
    
    output$uiAssess <- renderUI({
      
      assessUI("assess")
      
    })
    
    output$uiInput <- renderUI({
      
      InputUI("input")
      
    })
    
    
  }
  
  shinyApp(ui, server)
  
} else {
  
  A <<- new.env()
  # load(path, envir = .GlobalEnv)
  
  header <- dashboardHeader(title = "Ascendance")
  
  sidebar <- dashboardSidebar(disable = TRUE, sidebarMenuOutput("menu"))
  
  body <- dashboardBody(
    
    fluidRow(uiOutput("UI")), 
    
    fluidRow(
      
      column(width = 1, actionButton("Back", "Back")), 
      
      column(width = 1, actionButton("Next", "Next"))
      
    ) 
    
  )
  
  ui <- dashboardPage(header, sidebar, body)
  
  server <- function(input, output) {
    
    tut <- reactiveValues()
    tut$stage <- 1
    
    observe(priority = 1, {
      
      if (tut$stage == 0) {
        
        callModule(setup_intro, "intro")
        
        output$UI <- renderUI({
          
          setup_introUI("intro")
          
        })
        
      } else if (tut$stage == 1) {
        
        callModule(setup_focusAreas, "focusAreas")
        
        output$UI <- renderUI({
          
          setup_focusAreasInput("focusAreas")
          
        })
        
      } else if (tut$stage == 2) {
        
        save(A, file = path)
        
        callModule(setup_categories, "categories")
        
        output$UI <- renderUI({
          
          setup_categoriesUI("categories")
          
        })
        
      } else if (tut$stage == 3) {
        
        save(A, file = path)
        
        yes <- reactive({input$Next})
        
        callModule(setup_focusPeriod, "focusperiod", yes)
        
        output$UI <- renderUI({
          
          setup_focusPeriodUI("focusperiod")
          
        })
        
      } else if (tut$stage == 4) {
        
        save(A, file = path)
        
        yes <- reactive({input$Next})
        
        callModule(setup_taskSchedule, "taskSchedule", yes, Path = path)
        
        output$UI <- renderUI({
          
          setup_taskScheduleUI("taskSchedule")
          
        })
        
      } else if (tut$stage == 5) {
        
        save(A, file = path)
        
        stopApp()
        
      }
      
    })
    
    observeEvent(input$Next, priority = 2, {
      
      if (tut$stage < 5) {
        
        tut$stage <- tut$stage + 1
        
      }
      
    })
    
    observeEvent(input$Back, {
      
      if (tut$stage >= 0) {
        
        tut$stage <- tut$stage - 1
        
      }
      
    })
    
    
  }
  
  shinyApp(ui, server)
  
}

# dropdownMenu(type = "tasks", badgeStatus = "success",
#             taskItem(value = 90, color = "green",
#                      "Documentation"
#             ),
#             taskItem(value = 17, color = "aqua",
#                      "Project X"
#             ),
#             taskItem(value = 75, color = "yellow",
#                      "Server deployment"
#             ),
#             taskItem(value = 80, color = "red",
#                      "Overall project"
#             )
#)
