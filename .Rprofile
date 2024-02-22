# source("renv/activate.R")
# if (!requireNamespace("renv")) install.packages("renv")
# renv::restore(prompt = FALSE)

source("R/utils.R")

# Setting shiny options
# file upload limit to 500MB (0.5GB) (override 5MB limit)
options(shiny.maxRequestSize = 500*1024^2)
# options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))
options(htmlwidgets.TOJSON_ARGS = NULL)
# ensures that missing values are represented consistently as the string 'string' in the JSON representation of the DataTable
options(DT.TOJSON_ARGS = list(na = 'string'))
options(spinner.type = 7, spinner.color = "royalblue")

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
