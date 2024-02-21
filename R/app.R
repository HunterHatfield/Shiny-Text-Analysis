
##### DASBOARD APP ######

# Using renv::restore() & lockfile in .Rprofile
# Shiny options set in .Rprofile

# library call on all req packages
library(berryFunctions)
library(dplyr)
library(DT)
library(finalfit)
library(forcats)
library(ggfortify)
library(ggplot2)
library(hunspell)
library(janitor)
library(kableExtra)
library(MASS)
library(modelsummary)
library(nortest)
library(openxlsx)
library(plotly)
library(purrr)
library(quarto)
library(readr)
library(reshape2)
library(rmarkdown)
library(rstatix)
library(scales)
library(schoRsch)
library(shiny)
library(shinyalert)
library(shinycssloaders)
library(shinydashboard)
library(shinyFiles)
library(shinyjs)
library(sjmisc)
library(sjPlot)
library(stringr)
library(tidyr)
library(tidytext)
library(tm)
library(waiter)
library(wordcloud2)
library(xgxr)

source("R/utils.R")

#-----------------------#
#-- textApp function  --#
#-----------------------#

textApp <- function(){
  
  #  Source all relevant files first
  
  ###### Main UI ###########################
  ui <- dashboardPage(skin = "black",
                      
        dashboardHeader(title = "Text Analysis",  
                        tags$li(class = "dropdown", 
                                actionButton("refresh_app", NULL,
                                             icon = 
                                               icon("rotate-right"),
                                             class = "btn-top-reload")),
                        tags$li(class = "dropdown", 
                                actionButton("exit_app", NULL,
                                             # icon = 
                                             #   icon("person-from-portal"),
                                             icon = 
                                               icon("door-open"),
                                             class = "btn-top-exit"))
                        ),
        
                      dashboardSidebar(
                        sidebarMenu(id = "sidebar",
                                    
                                    menuItem("  Home", 
                                             tabName = "home", 
                                             icon = icon("house")
                                    ),
                                    menuItem("  Import data", 
                                             tabName = "textSelectorTab",
                                             icon = icon("file-import")
                                    ),
                                    menuItem("  Text preparation", 
                                             tabName = "textPrepTab", 
                                             icon = icon("filter")
                                    ),
                                    menuItem("  Visualise", 
                                             tabName = "textFreqTab",
                                             icon = icon("chart-column")
                                    ),
                                    menuItem("Search/Concordance (under construction)",
                                             tabName = "concordanceTab",
                                             icon = icon("dashboard")
                                    ),
                                    menuItem("Statistical Analysis", 
                                             tabName = "statsTab",
                                             icon = icon("calculator")
                                    ),
                                    menuItem("Report", 
                                             tabName = "reportingTab",
                                             icon = icon("scroll")
                                    )
                                    
                        ), # end sidebar menu
                        
                        column(12,
                               hr(class = "hr-blank")
                        )
                        
                        
                      ), # end dashboard sidebar
                      
                      dashboardBody(
                        
                        # Linking to external style sheets without package
                        # tags$link(rel = "stylesheet", type="text/css",
                        #           href="./www/styles.css"),
                        # tags$link(rel = "stylesheet", type="text/css",
                        #           href="./www/button-styles.css"),
                        # tags$link(rel = "stylesheet", type="text/css",
                        #           href="./www/error-styles.css"),
                        
                        # Linking to external style sheet in package
                        includeCSS("./www/styles.css"),
                        includeCSS("./www/button-styles.css"),
                        includeCSS("./www/error-styles.css"),
                        
                        tabItems(
                          # Inserting inner UI items into the tabs 
                          tabItem(tabName = "home", 
                                  homeUI("home")
                          ),
                          
                          tabItem(tabName = "textSelectorTab",
                                  textSelectorUI("textSelector",  
                                                 label = "Choose text file(s):")
                          ),
                          
                          # Text preparation UI
                          tabItem(tabName = "textPrepTab", 
                                  textPrepUI("textPrep")
                          ),
                          
                          # Text frequency UI
                          tabItem(tabName = "textFreqTab",
                                  # textFreqUI("textFreq")
                                  reportingUI("reporting")
                          ),
                          
                          # Concordance UI
                          # tabItem(tabName = "concordanceTab",
                          #         # concordanceUI("concordance")
                          #         ),
                          
                          # Stat UI
                          tabItem(tabName = "statsTab",
                                  statsUI("stats")
                          ),
                          tabItem(tabName = "reportingTab",
                                  reportMakerUI("reportMaker")
                          )
                        )
                      ) # end dashboard body
  )
  
  ###### Main server ##########
  server <- function(input, output, session) {
    
    # Create reactive value list within server - 
    # results in reactive value list private to each 
    # user session
    rv <- shiny::reactiveValues() # creating reactive values list
    
    # Inner module servers
    homeAttempt <- try(homeServer("home", parent = session))
    if("try-error" %in% class(homeAttempt)){
      shinyalert(
        title = "The home tab's server call returned a fatal error. ",
        text = "Refresh your app and ensure all files sourced properly.",
        size = "xs", 
        closeOnEsc = TRUE, closeOnClickOutside = TRUE,
        html = FALSE, type = "info",
        showConfirmButton = TRUE, showCancelButton = FALSE,
        confirmButtonText = "Dismiss",
        confirmButtonCol = "#4169E1",
        timer = 0, imageUrl = "", animation = TRUE
      )
      return()
    }
    textSelectorAttempt <- try(textSelectorServer("textSelector", 
                                                  rv = rv))
    if("try-error" %in% class(textSelectorAttempt)){
      shinyalert(
        title = "The text selector tab's server call returned a fatal error. ",
        text = "Reload the app and ensure all files sourced properly.",
        size = "xs", 
        closeOnEsc = TRUE, closeOnClickOutside = TRUE,
        html = FALSE, type = "info",
        showConfirmButton = TRUE, showCancelButton = FALSE,
        confirmButtonText = "Dismiss",
        confirmButtonCol = "#4169E1",
        timer = 0, imageUrl = "", animation = TRUE
      )
      return()
    }
    # textFreqAttempt <- try(textFreqServer("textFreq", rv = rv))
    textPrepAttempt <- try(textPrepServer("textPrep", rv = rv))
    if("try-error" %in% class(textPrepAttempt)){
      shinyalert(
        title = "The text preparation tab's server call returned a fatal error. ",
        text = "Reload the app and ensure all files sourced properly.",
        size = "xs", 
        closeOnEsc = TRUE, closeOnClickOutside = TRUE,
        html = FALSE, type = "info",
        showConfirmButton = TRUE, showCancelButton = FALSE,
        confirmButtonText = "Dismiss",
        confirmButtonCol = "#4169E1",
        timer = 0, imageUrl = "", animation = TRUE
      )
      return()
    }
    reportingServerAttempt <- try(reportingServer("reporting",
                                                  rv = rv))
    if("try-error" %in% class(reportingServerAttempt)){
      shinyalert(
        title = "Reporting tab error",
        text = "There was a fatal error in the server function of the reporting module. Refresh your app and try again.",
        size = "xs",
        closeOnEsc = TRUE, closeOnClickOutside = TRUE,
        html = FALSE, type = "info",
        showConfirmButton = TRUE, showCancelButton = FALSE,
        confirmButtonText = "Dismiss",
        confirmButtonCol = "#4169E1",
        timer = 0, imageUrl = "", animation = TRUE
      )
      return()
    }
    statsServerAttempt <- try(statsServer("stats", rv = rv))
    if("try-error" %in% class(statsServerAttempt)){
      shinyalert(
        title = "The statistics tab's server call returned a fatal error. ",
        text = "Reload the app and ensure all files sourced properly.",
        size = "xs", 
        closeOnEsc = TRUE, closeOnClickOutside = TRUE,
        html = FALSE, type = "info",
        showConfirmButton = TRUE, showCancelButton = FALSE,
        confirmButtonText = "Dismiss",
        confirmButtonCol = "#4169E1",
        timer = 0, imageUrl = "", animation = TRUE
      )
      return()
    }
    reportMakerAttempt <- try(reportMakerServer("reportMaker", rv = rv))
    if("try-error" %in% class(reportMakerAttempt)){
      shinyalert(
        title = "The reporting tab's server call returned a fatal error. ",
        text = "Reload the app and ensure all files sourced properly.",
        size = "xs", 
        closeOnEsc = TRUE, closeOnClickOutside = TRUE,
        html = FALSE, type = "info",
        showConfirmButton = TRUE, showCancelButton = FALSE,
        confirmButtonText = "Dismiss",
        confirmButtonCol = "#4169E1",
        timer = 0, imageUrl = "", animation = TRUE
      )
      return()
    }
    

    # Function run on session end
    # onSessionEnded(function() {
    #   # If appState$refreshed is FALSE, quit
    #     if (!interactive()) {
    #   # if(session$isClosed()){
    #       cat("Thank you for using our Text Analysis App. 
    #           Exited sucessfully.")
    #       stopApp()
    #     }
    # })
    
    
    observeEvent(input$refresh_app, {
      refresh()
    })
    
    observeEvent(input$exit_app, {
      cat("\n 
          ------------------------##----------------------------
          Thank you for using our Text Analysis App. 
          Exited sucessfully. 
          To restart the app, run the 'runTextApp()' function.
          ------------------------##----------------------------
          \n ")
      stopApp()
    })
  
    
  } # end server
  
  # Run the application 
  shinyApp(ui = ui, server = server)
  
}

# textApp()

