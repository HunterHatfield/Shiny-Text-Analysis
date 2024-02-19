source("renv/activate.R")
if (!requireNamespace("renv")) install.packages("renv")
renv::restore(clean = T)

# library cal on all req packages
library(berryFunctions)
library(devtools)
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

if(!require('pacman'))install.packages('pacman')

# Setting shiny options
# file upload limit to 500MB (0.5GB) (override 5MB limit)
options(shiny.maxRequestSize = 500*1024^2)
# options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))
options(htmlwidgets.TOJSON_ARGS = NULL)
# ensures that missing values are represented consistently as the string 'string' in the JSON representation of the DataTable
options(DT.TOJSON_ARGS = list(na = 'string'))
options(spinner.type = 7, spinner.color = "royalblue")

# Sourcing non-reactive utility functions

source("R/utils.R")
# Sourcing each .R file
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
source("R/app.R")
