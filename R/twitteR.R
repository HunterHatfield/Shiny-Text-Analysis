# ######## twitteR method UI & server ############
# 
# # Vignette:
# # https://cran.r-project.org/web/packages/rtweet/vignettes/rtweet.html
# 
# 
# twitteRUI <- function(id, label = "Enter a twitter link:"){
#   ns <- NS(id)
#   tagList(
#     
#     h2(textOutput(ns("title1"))),
#     p(textOutput(ns("subtitle1"))),
#     
#     # Drop down menu to select single tweet, multiple or user
#     selectInput(inputId = "tweetSelect", 
#                 label = "Choose the scale of your analysis:",
#                 choices = c("Single Tweet" = "singleTweet", 
#                             "Multiple Tweets" = "multipleTweets", 
#                             "Entire Twitter Account" = "entireAccount")
#     ),
#     
#     # Textbox to enter tweets URL
#     textInput(inputId = "tweetInput", 
#               label = label,
#               value = " "),
#     
#     # Sumbit button to submit url
#     actionButton(inputId = "submitTweet", label = "Submit"),
#     
#     # Output of what user has entered
#     h2(textOutput(ns("title2"))),
#     em(textOutput(ns("subtitle2"))),
#     # tableOutput(ns("files")),
#     # tableOutput(ns("contents")),
#     
#     # Source
#     hr(),
#     em(textOutput(ns("source"))),
#   )
# }
# 
# twitteRServer <- function(id, rv = rv){
#   moduleServer(
#     id, 
#     function(input, output, session, rv){
#       
#       output$title1 <- renderText({
#         paste("Twitter Scraping")
#       })
#       
#       output$subtitle1 <- renderText({
#         paste("Scrape tweets for text data with the twitteR package. Generate text for analysis from a single tweet, or scrape an entire Twitter account for text from said user's tweets...")
#       })
# 
#       title2 <- reactive({
#         "Tweet(s) Summary"
#       })
#       output$title2 <- renderText({
#         paste(title2())
#       })
#       
#       subtitle2 <- reactive({
#         if(is.null(input$tweetInput)){
#           "No tweets uploaded."
#         } else {
#           "...Below is a summary of tweets..."
#         }
#       })
#       output$subtitle2 <- renderText({
#         paste(subtitle2())
#       })
#       
#       
#       observeEvent(input$tweetURL, {
#         print(input$tweetURL)
#       } )
#       
#       
#       output$source <- renderText({
#         paste("Source: Gentry, Jeff. twitteR: R Based Twitter Client. http://geoffjentry.hexdump.org/twitteR.pdf, https://github.com/geoffjentry/twitteR")
#       })
#     }
#   )
# }
