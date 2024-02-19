
##### DASBOARD APP ######
# library(berryFunctions)
# library(devtools)
# library(dplyr)
# library(DT)
# library(finalfit)
# library(forcats)
# library(ggfortify)
# library(ggplot2)
# library(hunspell)
# library(janitor)
# library(kableExtra)
# library(MASS)
# library(modelsummary)
# library(nortest)
# library(openxlsx)
# library(plotly)
# library(purrr)
# library(quarto)
# library(readr)
# library(reshape2)
# library(rmarkdown)
# library(rstatix)
# library(scales)
# library(schoRsch)
# library(shiny)
# library(shinyalert)
# library(shinycssloaders)
# library(shinydashboard)
# library(shinyFiles)
# library(shinyjs)
# library(sjmisc)
# library(sjPlot)
# library(stringr)
# library(tidyr)
# library(tidytext)
# library(tm)
# library(waiter)
# library(wordcloud2)
# library(xgxr)


# Now using renv::restore() & lockfile to manage packages
if(!require('pacman'))install.packages('pacman')
# library(pacman)
# pacman::p_load(
#   berryFunctions, # for adding in rows easily
#   devtools, dplyr, DT,
#   finalfit, forcats,
#   ggfortify,ggplot2,
#   hunspell,
#   janitor,
#   kableExtra,
#   MASS, modelsummary,
#   nortest,
#   openxlsx,
#   plotly, purrr,
#   quarto,
#   readr, reshape2, rmarkdown, rstatix,
#   scales, schoRsch,
#   shiny, shinyalert, shinycssloaders,
#   shinydashboard, shinyFiles, shinyjs,
#   sjmisc, sjPlot, stringr,
#   tidyr, tidytext, tm,
#   waiter, wordcloud2,
#   xgxr # For the corr plot
#   )

# # Setting shiny options
# # file upload limit to 500MB (0.5GB) (override 5MB limit)
# options(shiny.maxRequestSize = 500*1024^2)
# # options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))
# options(htmlwidgets.TOJSON_ARGS = NULL)
# # ensures that missing values are represented consistently as the string 'string' in the JSON representation of the DataTable
# options(DT.TOJSON_ARGS = list(na = 'string'))
# options(spinner.type = 7, spinner.color = "royalblue")
# 
# # Sourcing non-reactive utility functions
# 
# source("R/utils.R")
# # Sourcing each .R file
# source("R/home.R", local = T)
# source("R/upload.R", local = T)
# source("R/csv.R", local = T)
# source("R/gutenbergR.R", local = T)
# source("R/rvest.R", local = T)
# source("R/twitteR.R", local = T)
# source("R/secondaryUpload.R", local = T)
# source("R/textSelector.R", local = T)
# source("R/stats.R", local = T)
# source("R/stopwords.R", local = T)
# source("R/tokenize.R", local = T)
# source("R/textPrep.R", local = T)
# source("R/reporting.R", local = T)
# # source("textFrequency.R", local = T)
# source("R/reportMaker.R", local = T)
# source("R/wordCloud.R", local = T)



textApp <- function(){
  
  ############# Main UI ###########################
  ui <- dashboardPage(skin = "black",
                      
                      dashboardHeader(title = "Text Analysis"),
                      
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
                               hr(class = "hr-blank"),
                               hr(class = "hr-blank"),
                               hr(class = "hr-blank"),
                               hr(class = "hr-blank"),
                               
                               h4("Dev notes:", style = "color: lightgray;"),
                               em("I've added <<__>> around blurbs which could be improved on Let me know if you have any suggestions for them.", style = "color: lightgray;"),
                               hr(class = "hr-blank"),
                               em("I tested out editing the home tab's text to having 4 actions (import, visualise, analyse, report), but it makes the landing page look too complicated & users would have to scroll to read all the steps and get to the start button. I think having a clear and concise 3 calls to action (import, analyse/visualise/?, report) gives a good overview of the app and memorable steps for the landing page. But what to call step 2... analyse, visualise or something else? I guess analyse includes visualise/search/concordance/stats stuff...", style = "color: lightgray;")
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
  
  ############# Main server ###########################
  server <- function(input, output, session) {
    
    
    # Create reactive value list within server - 
    # results in reactive value list private to each 
    # user session
    rv <- shiny::reactiveValues() # creating reactive values list
    print("reactive value list init")
    
    # Inner module servers
    homeAttempt <- try(homeServer("home", parent = session))
    if("try-error" %in% class(homeAttempt)){
      shinyalert(
        title = "Hey bull! Stop running around in my china shop!",
        text = "The home tab's backend server returned a fatal error. Refresh your app or navigate to another tab.",
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
        title = "The text selector server broke!",
        text = "No more file uploads for you >:(",
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
    # if("try-error" %in% class(textFreqAttempt)){
    #   shinyalert(
    #     title = "The text frequency server broke!",
    #     text = "No graphs for you :(",
    #     size = "xs", 
    #     closeOnEsc = TRUE, closeOnClickOutside = TRUE,
    #     html = FALSE, type = "info",
    #     showConfirmButton = TRUE, showCancelButton = FALSE,
    #     confirmButtonText = "Dismiss",
    #     confirmButtonCol = "#4169E1",
    #     timer = 0, imageUrl = "", animation = TRUE
    #   )
    #   return()
    # }
    
    
    textPrepAttempt <- try(textPrepServer("textPrep", rv = rv))
    if("try-error" %in% class(textPrepAttempt)){
      shinyalert(
        title = "The text preparation server broke!",
        text = "It's so not preppy in here :/",
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
    
    
    # testServer("test", rv = rv)
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
        title = "The statistical analysis tab just broke!",
        text = "Statistical insights? Never heard of her!",
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
        title = "The reporting tab just broke!",
        text = "How will you ever share your passion for text analysis?!",
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
    
    
    onStop(function() {
      cat("\n Session stopped.")
      
      cat("\n rv list:")
      print(rv)
    })
    
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)
  
}

textApp()

