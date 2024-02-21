source("renv/activate.R")
if (!requireNamespace("renv")) install.packages("renv")
renv::restore(prompt = FALSE)

# Setting shiny options
# file upload limit to 500MB (0.5GB) (override 5MB limit)
options(shiny.maxRequestSize = 500*1024^2)
# options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))
options(htmlwidgets.TOJSON_ARGS = NULL)
# ensures that missing values are represented consistently as the string 'string' in the JSON representation of the DataTable
options(DT.TOJSON_ARGS = list(na = 'string'))
options(spinner.type = 7, spinner.color = "royalblue")


# library(devtools)
# devtools::load_all()

source("R/utils.R")
source("R/app.R")
source("R/home.R", local = T)
source("R/upload.R", local = T)
source("R/csv.R", local = T)
source("R/gutenbergR.R", local = T)
source("R/rvest.R", local = T)
source("R/twitteR.R", local = T)
source("R/secondaryUpload.R", local = T)
source("R/textSelector.R", local = T)
source("R/stats.R", local = T)
source("R/stopwords.R", local = T)
source("R/tokenize.R", local = T)
source("R/textPrep.R", local = T)
source("R/reporting.R", local = T)
# source("textFrequency.R", local = T)
source("R/reportMaker.R", local = T)
source("R/wordCloud.R", local = T)

cat("\n 
      ----------------------------##--------------------------------
        Kia ora.
        Thank you for downloading our Text Analysis App. 
    
        To run the app, execute  the 'runTextApp()' function in the 
        console line just below this message.
        
        Refresh the app by reloading your browser page or by hitting 
        the 'Reload' icon in the app's header.
        
        Exit the app by clicking the 'Exit' icon in the app's header,    
        or by clicking the small red 'Stop' button at the top right of
        the R console window.
        
        For troubleshooting advice,visit our GitHub: 
        https://github.com/HunterHatfield/Shiny-Text-Analysis/ 
        
        Happy analysing!
    -------------------------------##-------------------------------- 
    \n"
)
