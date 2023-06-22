
##### DASBOARD APP ######

# devtools::load_all() - automatically saves all open files, source()s every file in R/, loads all datasets in data/ then puts your cursor in the console
# run the actual app: textApp() in the console

# usethis::edit_r_profile() - to edit Rprofile (code that runs every time opening R)
# Call usethis::use_description() to create a description file
# Remove any calls to source(), since load_all() automatically sources all .Rfiles in R/.

# Need to eventually all library()  with usethis::use_package("name")
# usethis::use_package("shiny")
# usethis::use_package("DT")
# usethis::use_package("shinyFiles")
# usethis::use_package("readr")
# usethis::use_package("dplyr")
# usethis::use_package("shinydashboard")
# usethis::use_package("purrr")
# usethis::use_package("tidytext")
# usethis::use_package("devtools")
# usethis::use_package("openxlsx")

# install.packages("sjmisc")
library(sjmisc)

library(shiny)
library(shinyalert)
library(DT)
library(shinyFiles)
library(dplyr)
library(shinydashboard)
library(devtools)
library(tidytext)
library(purrr)
library(readr)
library(openxlsx)
library(forcats)
library(scales)
library(xgxr)
library(tidyr)
library(MASS)
library(janitor)
library(shinyjs)
library(kableExtra)
library(modelsummary)
library(sjPlot)
library(lme4)
library(plotly)
library(reshape2)
library(ggfortify)
library(nortest)
library(rstatix)
library(schoRsch)
library(hunspell)

library(rhandsontable)

# Setting shiny options
# file upload limit to 10MB (override 5MB limit)
options(shiny.maxRequestSize = 10*1024^2)

rv <- shiny::reactiveValues() # creating reactive values list
report_rv <- shiny::reactiveValues() # creating rv list for the report
mini_rv <- shiny::reactiveValues()
stats_report_rv <- shiny::reactiveValues()

textApp <- function(...){
  
  ############# Main UI ###########################
  ui <- dashboardPage(skin = "black",

    dashboardHeader(title = "Text Analysis"),
    
    dashboardSidebar(
      sidebarMenu(id = "sidebar",
                  
        menuItem("Home", 
                 tabName = "home", 
                 icon = icon("house")
        ),
        menuItem("Text Selector", 
                 tabName = "textSelectorTab",
                 icon = icon("file-import")
        ),
        menuItem("Text Preparation", 
                 tabName = "textPrepTab", 
                 icon = icon("filter")
        ),
        menuItem("Text Frequency", 
                 tabName = "textFreqTab",
                 icon = icon("chart-column")
        ),
        # menuItem("Search/Concordance", 
        #          tabName = "concordanceTab",
        #          icon = icon("dashboard")
        # ),
        menuItem("Statistical Analysis", 
                 tabName = "statsTab",
                 icon = icon("calculator")
        ),
       menuItem("Reporting", 
                tabName = "reportingTab",
                icon = icon("scroll")
        )
       
      ) # end sidebar menu
    ), # end dashboard sidebar
    
    dashboardBody(
      
      # Linking to external style sheet
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
                  textFreqUI("textFreq")
                ),
        
        # Concordance UI
        # tabItem(tabName = "concordanceTab",
        #         # concordanceUI("concordance")
        #         ),
        
        # Stat UI
        tabItem(tabName = "statsTab",
                statsUI("stats")
        ),
        # Stat UI
        tabItem(tabName = "reportingTab",
                reportMakerUI("reportMaker")
        )
      )
    ) # end dashboard body
  )

  ############# Main server ###########################
  server <- function(input, output, session) {
    
    # inner module servers
    homeServer("home", rv=rv, parent = session)
    
    textSelectorServer("textSelector", rv = rv)
    
    textFreqServer("textFreq", rv = rv)
    textPrepServer("textPrep", rv = rv)
    
    statsServer("stats", rv = rv)
    
    reportingServer("reporting", rv = rv, report_rv = report_rv)
    reportMakerServer("reportMaker", rv = rv, report_rv = report_rv)

  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)
}

