# 

# Merged text frequency and report modules - made no sense for 
# separation. Did create module for word cloud to separate out
# word cloud breaking solution. See app_files/wordCloud.R.


# #### TEXT FREQUENCY UI #####
# textFreqUI <- function(id){
#   ns <- NS(id)
#   tagList(
#     useShinyjs(),
# 
#     reportingUI(ns("reporting"))
# 
#   )
# }
# 
# 
# #### TEXT FREQ SERVER ####
# textFreqServer <- function(id, rv = NULL){
#   moduleServer(id, function(input, output, session){
#     
#     
#     # ######################
#     # ## Reporting module ##
#     # ######################
#     # reportingServerAttempt <- try(reportingServer("reporting", rv = rv))
#     # if("try-error" %in% class(reportingServerAttempt)){
#     #   shinyalert(
#     #     title = "Reporting tab error",
#     #     text = "There was a fatal error in the server function of the reporting module. Refresh your app and try again.",
#     #     size = "xs",
#     #     closeOnEsc = TRUE, closeOnClickOutside = TRUE,
#     #     html = FALSE, type = "info",
#     #     showConfirmButton = TRUE, showCancelButton = FALSE,
#     #     confirmButtonText = "Dismiss",
#     #     confirmButtonCol = "#4169E1",
#     #     timer = 0, imageUrl = "", animation = TRUE
#     #   )
#     #   return()
#     # }
#     
#   }
#   )
# }
# 
# 
# 
