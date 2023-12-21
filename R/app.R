
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

if(!require('pacman'))install.packages('pacman')
library(pacman)

pacman::p_load(
  berryFunctions, # for adding in rows easily
  devtools,
  dplyr,
  DT,
  finalfit,
  forcats,
  ggfortify,
  ggplot2,
  hunspell,
  janitor,
  kableExtra,
  MASS,
  modelsummary,
  nortest,
  openxlsx,
  plotly,
  purrr,
  quarto,
  readr,
  reshape2,
  rmarkdown,
  rstatix,
  scales, # For the corr plot
  schoRsch,
  shiny,
  shinyalert,
  shinydashboard,
  shinyFiles,
  shinyjs,
  sjmisc,
  sjPlot,
  stringr,
  tidyr,
  tidytext,
  tm,
  wordcloud2,
  xgxr # For the corr plot
  )

# Setting shiny options
# file upload limit to 10MB (override 5MB limit)
options(shiny.maxRequestSize = 10*1024^2)

# ensures that missing values are represented consistently as the string 'string' in the JSON representation of the DataTable
# options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))
options(htmlwidgets.TOJSON_ARGS = NULL)
options(DT.TOJSON_ARGS = list(na = 'string'))


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
                  
        menuItem("  Home", 
                 tabName = "home", 
                 icon = icon("house")
        ),
        menuItem("  Text Selector", 
                 tabName = "textSelectorTab",
                 icon = icon("file-import")
        ),
        menuItem("  Text Preparation", 
                 tabName = "textPrepTab", 
                 icon = icon("filter")
        ),
        menuItem("  Text Frequency", 
                 tabName = "textFreqTab",
                 icon = icon("chart-column")
        ),
        menuItem("Search/Concordance (in progress...)",
                 tabName = "concordanceTab",
                 icon = icon("dashboard")
        ),
        menuItem("Statistical Analysis", 
                 tabName = "statsTab",
                 icon = icon("calculator")
        ),
       menuItem("Reporting (editing...)", 
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
        )
        # tabItem(tabName = "reportingTab",
        #         reportMakerUI("reportMaker")
        # )
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
    
    statsServerAttempt <- try(statsServer("stats", rv = rv))
    if("try-error" %in% class(statsServerAttempt)){
      shinyalert(
        title = "Stats tab broke",
        text = "Congrats! You just broke the statistics tab.",
        size = "xs", 
        closeOnEsc = TRUE, closeOnClickOutside = TRUE,
        html = FALSE, type = "info",
        showConfirmButton = TRUE, showCancelButton = FALSE,
        confirmButtonText = "Dismiss",
        confirmButtonCol = "#4169E1",
        timer = 0, imageUrl = "", animation = TRUE
      )
    } else {
      statsServer("stats", rv = rv)
    }
    
    reportMakerServer("reportMaker", rv = rv, report_rv = report_rv)
    
    onStop(function() {
      cat("\n This will run on session stop")
      outputDir <- "R"
      
      cat("\n rv list:")
      print(rv)
      cat("\n report_rv list:")
      print(report_rv)
    })
    

  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)
}

