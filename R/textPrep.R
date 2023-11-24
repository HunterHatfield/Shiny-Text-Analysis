# Text preparation tab

# Copied from old textPrep.R to edit stop-removal, tokenisation and stemming
# to be done on content_prepared instead of content_stop_rm, and have written
# functions to do actions instead of all in little modules

# library(tidytext)
# library(stringr)
# 
# data(stop_words)
# rv$is_stop_removed <- FALSE
# 
# ##### STOPWORDS MODULE UI ####
# stopWordsUI <- function(id) {
#   ns <- NS(id)
#   tagList(
#     h3("Stop-word selection"),
#     
#     # Select a dataset to alter
#     selectInput(ns("data_stop_rm"), 
#                 label = "Select a dataset to alter:", 
#                 choices = list("N/A" = "na")),
#     
#     # Select a column to remove stop-words from
#     selectInput(ns("col_name_stop_rm"), 
#                 label = "Select a column to remove stop-words from:", 
#                 choices = list("N/A" = "na")),
# 
#     # checkbox input for including default stop words 
#     checkboxInput(ns("default_check"),
#       label = "Include default stop-word list"
#     ),
# 
#     # textbox input for extra stop words
#     textInput(ns("added_words"),
#       label = "Add extra stop-word(s) separated by commas and/or spaces:",
#       value = NULL,
#       placeholder = "e.g. apple, banana carrot"
#     ),
#     
#     fluidRow(
#       column(6, {
#         actionButton(ns("submit_stop_words"),
#           label = "Submit",
#           class = "btn-success"
#         )
#       }),
#       column(6, {
#         actionButton(ns("undo_stop_words"),
#           label = "Undo",
#           class = "btn-danger"
#         )
#       })
#     ),
#     conditionalPanel(
#       condition = "input.default_check == 'true'",
#       p(textOutput(ns("default_caption")))
#     ),
#     hr(),
#     h3("Submitted stop-words"),
#     p(textOutput(ns("stop_word_display"))),
# 
#     em(textOutput(ns("no_stop_words_caption"))),
#     
#     ##### num_stops is what?
#     conditionalPanel(
#       condition = paste0(
#         "output['", ns("num_stops"),
#         "'] == '0' "
#       ),
#       em(textOutput("No stop-words submitted."))
#     ),
#     
#     DT::dataTableOutput(ns("stop_words_table")),
#     
#     uiOutput(ns("download_stop_words"))
#   )
# }
# 
# #### STOPWORDS SERVER ####
# stopWordsServer <- function(id, stop_word_list = stop_word_list, rv = rv) {
#   moduleServer(id, function(input, output, session) {
#     # Rendering that no stop words are submitted if nothing submitted in table
#     # output$no_stop_words_caption <- renderText({
#     #
#     #   req(rv$content)
#     #
#     #   print("stop words:")
#     #   print(length(rv$stop_words_final$word))
#     #
#     #   if(rv$is_stop_removed){
#     #     NULL
#     #   } else {
#     #     "No stop-words submitted."
#     #   }
#     # })
# 
#     output$num_stops <- renderText(length(rv$stop_words_final$word))
#     
#     ############################
#     ## Creating dataset chooser
#     ############################
#     # Always update possible dataset list with current available subsets
#     # so using observe()
#     observe({
#       # Create list of possible datasets to select from
#       req(rv$content_primary)
#       potential_sets_stop_rm <- list("Primary data")
#       
#       if(!is.null(rv$subset_one)){
#         potential_sets_stop_rm[length(potential_sets_stop_rm) + 1] <- "Subset one"
#       }
#       if(!is.null(rv$subset_two)){
#         potential_sets_stop_rm[length(potential_sets_stop_rm) + 1] <- "Subset two"
#       }
#       
#       # updating select input list to contain datasets available
#       updateSelectInput(session, "data_stop_rm",
#                         choices = potential_sets_stop_rm,
#                         selected= potential_sets_stop_rm[1])
#     }) # end observe
# 
#     
#     ############################
#     ## Creating column chooser #
#     ############################
#     # When a datasets chosen to perform removal on, need to load which 
#     # columns available in that dataset
#     observe({
#       req(rv$content_primary$content_prepared)
#       
#       # creating list of available columns
#       # subsets' content_prepared is initialized on save of subsets
#       colnames <- switch(input$data_stop_rm, 
#                          "Primary data" = 
#                            colnames(rv$content_primary$content_prepared), 
#                          "Subset one" = colnames(rv$subset_one$content_prepared), 
#                          "Subset two" = colnames(rv$subset_two$content_prepared)
#                          )
#       # updating select input list for choosing column to remove stops from
#       updateSelectInput(session, "col_name_stop_rm",
#                         choices = colnames[-1],
#                         selected= colnames[2])
#     })
#     
#     # Rendering whatever dataset selected in datatable display
#     observeEvent(input$data_stop_rm, {
#       req(rv$content_primary$content_prepared)
#       rv$content_prepared_display_2 <- 
#         switch(input$data_stop_rm, 
#                "Primary data" =  rv$content_primary$content_prepared, 
#                "Subset one" = rv$subset_one$content_prepared, 
#                "Subset two" = rv$subset_two$content_prepared
#       )
#     })
#     
#     
#     # When submit button clicked...
#     # Creating final stop words tibble depending on various input cases
#     # If user wants to include defaults & added words, combine defaults and
#     # added words
#     # If user wants to include only added words, return only added words tibble
#     # Otherwise if user only wants defaults, return only these
#     observeEvent(input$submit_stop_words, {
#       # Creating added stop words tibble. If user has entered something (input
#       # isn't null), create tibble and unnest words, then return them.
#       # Otherwise return NULL
#       added_stop_words <- reactive({
#         if (nchar(input$added_words) != 0) { # if there are added words
#           added_words <- tibble(input = input$added_words)
#           added_words <- added_words %>%
#             unnest_tokens(word, input, token = "words") %>%
#             filter(!grepl(" ", word, fixed = TRUE))
#             # filtering out "words" which are just a blank space
#           return(added_words)
#         } else {
#            return(NULL)
#         }
#       })
# 
#       rv$added_stop_words <- added_stop_words()
# 
#       # Creating default stop words tibble.
#       # When user chooses to include default stop-words, default list obtained
#       # from tidytext library and saved if not NULL is returned
#       # data(stop_words) loaded at top of file on start of app
#       # could export stop_words to a csv which is imported if default chosen
#       default_stop_words <- reactive({
#         stop_words %>% dplyr::select(word)
#       })
#       rv$default_stop_words <- default_stop_words()
# 
#       # Creating final stop words tibble collecting together added & defaults
#       stop_words_final <- reactive({
#         if (input$default_check) {
#           if (!is.null(rv$added_stop_words)) { # both added & default words 
# 
#             # combining default and added word tibbles
#             default_and_added <- rbind(
#               rv$added_stop_words,
#               rv$default_stop_words
#             )
#             return(default_and_added)
#             
#           } else { # if just default words to be included
#             return(rv$default_stop_words)
#           }
#           
#         } else if(!is.null(rv$added_stop_words)) { # added words but not defaults
#             
#           return(rv$added_stop_words)
#           
#           } else { # if no defaults or added words to be included
#             return(NULL)
#           }
#       }) # end reactive stop_words_final creation
# 
#       rv$stop_words_final <- stop_words_final()
# 
#       # Printing added stop-words(s)
#       output$stop_word_display <-
#         renderText({
#           if (nchar(input$added_words) != 0) { # if user added stop-words
#             paste(c("You have added a total of ", nrow(rv$stop_words_final),
#               " stop-word(s)."
#             ), sep = ",")
#           } else if (input$default_check) { # if user chooses default stop-words
#             paste(c("You have added a default list of ",
#               nrow(rv$default_stop_words), " stop-words."), sep = ",")
#           } else if (is.null(rv$stop_words_final) || length(rv$stop_words_final$word) == 0) {
#             return(NULL)
#           }
#         })
# 
#       req(rv$stop_words_final)
# 
#       # Displaying stop-words added
#       output$stop_words_table <- DT::renderDataTable(
#         rv$stop_words_final,
#         # callback = JS("$('div.dwnld').append($('download_stop_words_tsv'));"),
#         options = list(
#           paging = TRUE,
#           pageLength = 5,
#           scrollX = TRUE,
#           scrollY = TRUE,
#           dom = "frtipB", # buttons render below DT
#           buttons = list("csv", "excel")
#         ),
#         escape = FALSE,
#         extensions = "Buttons",
#         selection = "none",
#         rownames = FALSE # don't show row numbers
#       )
# 
#       # Render UI to download stop words table as csv or tsv
#       output$download_stop_words <-
#         renderUI({
#           ns <- NS(id)
#           tagList(
#             fluidRow(
#               column(4, {
#                 downloadButton(ns("download_stop_words_csv"),
#                   label = "Download .csv"
#                 )
#               }),
#               column(4, {
#                 downloadButton(ns("download_stop_words_csv"),
#                   label = "Download .tsv"
#                 )
#               }),
#             ) # end fluid row
#           )
#         })
# 
#       
#       ######################
#       ## Stop word removal #
#       ######################
#       req(rv$content_primary)
#       
#       # For selected dataset, create pre-stop-removed dataset in case of undo, 
#       # and call on utils.R function remove_stop_words() to return dataset with
#       # stop-words removed. 
#       if(input$data_stop_rm == "Primary data"){
#         
#         # Saving pre-stop-removal 
#         rv$content_primary$content_pre_stop_rm <- rv$content_primary$content_prepared
#         
#         print("Starting stop-word removal...")
#         # Use try-catch to run function and throw error if doesn't work
#         # Hardcoding column for now
#         content_stop_rm <- remove_stop_words(
#                                 data = rv$content_primary$content_prepared,
#                                 col_name = input$col_name_stop_rm,
#                                 stop_words = rv$stop_words_final, 
#                                 is_tokenised = rv$content_primary$is_tokenised)
#         
#         print("Used remove_stop_words function to remove stop words:")
#         print(content_stop_rm)
#         
#         rv$content_primary$content_stop_rm <- content_stop_rm
#         rv$content_primary$content_prepared <- content_stop_rm
#         rv$content_primary$is_stop_rm <- TRUE
#         rv$content_prepared_display_2 <- rv$content_primary$content_stop_rm
#         
#       } else if(input$data_stop_rm == "Subset one"){
#         
#         # Saving pre-stop-removal data
#         rv$subset_one$content_pre_stop_rm <- rv$subset_one$content_prepared
#         
#         content_stop_rm <- remove_stop_words(
#                             data = rv$subset_one$data,
#                             col_name = input$col_name_stop_rm,
#                             stop_words = rv$stop_words_final, 
#                             is_tokenised = rv$subset_one$is_tokenised)
#         
#         print("Used remove_stop_words function to remove stop words:")
#         print(content_stop_rm)
#         
#         rv$subset_one$content_stop_rm <- content_stop_rm
#         rv$subset_one$content_prepared <- content_stop_rm
#         rv$subset_one$is_stop_rm <- TRUE
#         rv$content_prepared_display_2 <- rv$subset_one$content_stop_rm
#         
#       } else if(input$data_stop_rm == "Subset two"){
#         
#         # Saving pre-stop-removal data
#         rv$subset_one$content_pre_stop_rm <- rv$subset_one$content_prepared
#         
#         content_stop_rm <- remove_stop_words(
#           data = rv$subset_two$data,
#           col_name = input$col_name_stop_rm,
#           stop_words = rv$stop_words_final, 
#           is_tokenised = rv$subset_two$is_tokenised)
#         
#         rv$subset_two$content_stop_rm <- content_stop_rm
#         rv$subset_two$content_prepared <- content_stop_rm
#         rv$subset_two$is_stop_rm <- TRUE
#         rv$content_prepared_display_2 <- rv$subset_two$content_stop_rm
#       }
# 
#       # # Setting content_parameterised & prepared to new data w stop removed
#       # rv$content_parameterised <- rv$content_stop_rm
#       # rv$content_prepared <- rv$content_stop_rm
#       # rv$is_tokenised <- FALSE
#       # rv$is_stop_removed <- TRUE ##### Check
#       
#       
#       #########################
#       # Downloading stop-words
#       #########################
#       # Rendering download buttons with downloadHandler()
#       output$download_stop_words_csv <- downloadHandler(
#         filename = function() {
#           paste("Stop_word_list.csv")
#         },
#         content = function(file) {
#           write_delim(as.data.frame(rv$stop_words_final), file,
#             delim = ",")
#         }
#       )
# 
#       # Download handler to download tsv
#       output$download_stop_words_tsv <- downloadHandler(
#         filename = function() {
#           paste("Stop_word_list.tsv")
#         },
#         content = function(file) {
#           write_tsv(as.data.frame(rv$stop_words_final), file)
#         }
#       )
#     }) # end submit stop-words
# 
# 
#     ########################
#     ### Undo stop-words ####
#     ########################
#     # When undo button clicked, reset reactive values
#     # Also indicate that tokenisation is undone since we
#     # revert to original content
#     observeEvent(input$undo_stop_words, {
#       
#       rv$content_stop_rm <- rv$content
#       rv$content_parameterised <- rv$content
#       rv$content_prepared_diplay <- rv$content
#       rv$added_stop_words <- NULL
#       rv$default_stop_words <- NULL
#       rv$stop_words_final <- NULL
# 
#       rv$is_tokenised <- FALSE
#       rv$is_stop_removed <- FALSE
#       
#       # Setting content_stop_rm back to pre-stop-removed data in selected dataset
#       # Also setting display table to display undone data
#       if(input$data_stop_rm == "Primary data"){
#         rv$content_primary$content_prepared <- 
#               rv$content_primary$content_pre_stop_rm
#         rv$content_primary$content_stop_rm <- NULL
#         rv$content_primary$is_stop_rm <- FALSE
#         rv$content_prepared_display_2 <- rv$content_primary$content_prepared
#         
#         
#       } else if(input$data_stop_rm == "Subset one"){
#         rv$subset_one$content_prepared <- rv$subset_one$content_pre_stop_rm
#         rv$subset_one$content_stop_rm <- NULL
#         rv$subset_one$is_stop_rm <- FALSE
#         rv$content_prepared_display_2 <- rv$subset_one$content_prepared
#         
#       } else if(input$data_stop_rm == "Subset two"){
#         rv$subset_two$content_prepared <- rv$subset_two$content_pre_stop_rm
#         rv$subset_two$content_stop_rm <- NULL
#         rv$subset_two$is_stop_rm <- FALSE
#         rv$content_prepared_display_2 <- rv$subset_one$content_prepared
#       }
# 
#       # using a list of rv's to delete each w for loop if exists
#       to_reset <- c(
#         "default_check", "stop_word_check",
#         "added_words", "submit_stop_words"
#       )
#       for (i in 1:length(to_reset)) {
#         if (!is.null(to_reset[i])) {
#           shinyjs::reset(to_reset[i])
#         }
#       }
#     })
#   })
# }


# #### TOKENISATION MODULE UI ####
# tokenizeUI <- function(id) {
#   ns <- NS(id)
#   tagList(
#     
#     h3("Tokenisation"),
#     
#     selectInput(ns("data_to_tokenise"), 
#                 label = "Select a dataset:", 
#                 choices = list("N/A" = "na")),
#     
#     selectInput(ns("col_name_to_tokenise"), 
#                 label = "Select a column to tokenise:", 
#                 choices = list("N/A" = "na")),
#     
#     selectInput(ns("token"),
#       label = "Choose a token to split text data on:",
#       choices = c(
#         "Words" = "words",
#         "Bi-grams" = "bigrams",
#         "n-grams" = "ngrams",
#         # "Sentences" = "sentences",
#         "Other" = "other"
#       ),
#       selected = "Words"
#     ),
#     conditionalPanel(
#       condition = "input.tokens == 'ngrams'",
#       ns = ns,
#       # uiOutput(ns("ngramsUI")) # was using renderUI in server to make UI for this
#       numericInput(ns("n_grams"), label = "Enter n-grams:", value = 2)
#     ),
#     conditionalPanel(
#       condition = "input.tokens == 'other'",
#       ns = ns,
#       # make mini UI for regex token
#       textInput(ns("other_token"), "Specify a custom token:")
#     ),
#     fluidRow(
#       column(6, {
#         actionButton(ns("submit_tokenise"),
#           label = "Tokenise",
#           class = "btn-success"
#         )
#       }),
#       column(6, {
#         actionButton(ns("revert_tokenise"),
#           label = "Revert",
#           class = "btn-danger"
#         )
#       })
#     ),
#   )
# }
# 
# #### TOKENISATION SERVER ####
# tokenizeServer <- function(id, rv = rv) {
#   moduleServer(id, function(input, output, session) {
#     
#     # rv$is_tokenised <- FALSE
# 
#     #####################
#     ## Dataset chooser ##
#     #####################
#     # Always update possible dataset list with current available subsets
#     observe({
#       # Create list of possible datasets to select from
#       req(rv$content_primary)
#       potential_sets_tokenise <- list("Primary data")
#       print("Made potential sets list:")
#       print(potential_sets_tokenise)
#       
#       if(!is.null(rv$subset_one)){
#         potential_sets_tokenise[length(potential_sets_tokenise) + 1] <- "Subset one"
#       }
#       if(!is.null(rv$subset_two)){
#         potential_sets_tokenise[length(potential_sets_tokenise) + 1] <- "Subset two"
#       }
#       
#       # updating select input list to contain datasets available
#       updateSelectInput(session, "data_to_tokenise",
#                         choices = potential_sets_tokenise,
#                         selected= potential_sets_tokenise[1])
#     })
#     
#     ###################
#     ## Column chooser #
#     ###################
#     # When a dataset is chosen to perform tokenisation on, need to load which 
#     # columns available in that dataset
#     observe({
#       req(rv$content_primary$content_prepared)
#       
#       # creating list of available columns
#       # subsets' content_prepared is initialized on save of subsets
#       colnames <- switch(input$data_to_tokenise, 
#                          "Primary data" = 
#                            colnames(rv$content_primary$content_prepared), 
#                          "Subset one" = colnames(rv$subset_one$content_prepared), 
#                          "Subset two" = colnames(rv$subset_two$content_prepared)
#       )
#       # updating select input list for choosing column to remove stops from
#       updateSelectInput(session, "col_name_to_tokenise",
#                         choices = colnames[-1],
#                         selected= colnames[2])
#     })
#     
#     # Rendering whatever dataset selected in datatable display
#     observeEvent(input$data_to_tokenise, {
#       req(rv$content_primary$content_prepared)
#       rv$content_prepared_display_2 <- 
#         switch(input$data_to_tokenise, 
#                "Primary data" =  rv$content_primary$content_prepared, 
#                "Subset one" = rv$subset_one$content_prepared, 
#                "Subset two" = rv$subset_two$content_prepared
#         )
#     })
# 
# 
#     #####################
#     ### Tokenisation ####
#     #####################
#     # When "tokenise" button clicked...
#     observeEvent(input$submit_tokenise, {
#       req(rv$content_primary) # always require something submitted
#       
#       # For selected dataset, create pre-stop-removed dataset in case of undo, 
#       # and then tokenise with tokenise_data() function in utils.R
#       if(input$data_to_tokenise == "Primary data"){
#         
#         # Save copy of content_prepared in case of undo
#         rv$content_primary$content_pre_tokenise <- 
#             rv$content_primary$content_prepared
#         
#         # Use tokenise_data function, passing in data_to_tokenise, 
#         # col_name_to_tokenise, input$token and if input$token = "other"
#         # pass in custom_token = input$other_token
#         content_tokenised <- tokenise_data(rv$content_primary$content_prepared, 
#                                            col_name = input$col_name_to_tokenise, 
#                                            token = input$token, 
#                                            custom_token = input$other_token, 
#                                            n_grams = input$n_grams)
#         
#         # Set tokenised data to rv$content_primary$content_prepared and 
#         # set is_tokenised = TRUE
#         rv$content_primary$content_prepared <- content_tokenised
#         rv$content_primary$is_tokenised <- TRUE
#         
#         print("Tokenised, rv$content_primary$content_prepared display:")
#         print(rv$content_primary$content_prepared)
#         
#       } else if(input$data_to_tokenise == "Subset one"){
#         
#         # Save copy of content_prepared in case of undo
#         rv$subset_one$content_pre_tokenise <- rv$subset_one$content_prepared
#         
#         # Tokenising
#         content_tokenised <- tokenise_data(rv$subset_one$content_prepared, 
#                                            col_name = input$col_name_to_tokenise, 
#                                            token = input$token, 
#                                            custom_token = input$other_token, 
#                                            n_grams = input$n_grams)
#         
#         rv$subset_one$content_prepared <- content_tokenised
#         rv$subset_one$is_tokenised <- TRUE
#         
#         
#       } else if(input$data_to_tokenise == "Subset two"){
#         
#         # Save copy of content_prepared in case of undo
#         rv$subset_two$content_pre_tokenise <- rv$subset_two$content_prepared
#         
#         # Tokenising
#         content_tokenised <- tokenise_data(rv$subset_two$content_prepared, 
#                                            col_name = input$col_name_to_tokenise, 
#                                            token = input$token, 
#                                            custom_token = input$other_token, 
#                                            n_grams = input$n_grams)
#         
#         rv$subset_two$content_prepared <- content_tokenised
#         rv$subset_two$is_tokenised <- TRUE
# 
#       }
#       
#       # Tokenising content tibble. Output column is selected token, input is
#       # Contents columns, tokenising by user-selected token
#       # content_tokenised <- reactive({
#         # # if token is easily handled, just unnest w input$token
#         # if (input$token == "words" || input$token == "sentences") {
#         #   rv$content_stop_rm %>%
#         #     unnest_tokens(Token, Contents, token = input$token)
#         # } else if (input$token == "bigrams") {
#         #   rv$content_stop_rm %>%
#         #     unnest_ngrams(Token, Contents, n = 2)
#         # } else if (input$token == "ngrams") {
#         #   rv$content_stop_rm %>%
#         #     unnest_ngrams(Token, Contents, n = input$n_grams)
#         # } else if (input$token == "other") {
#         #   # unnest on the user inputted regex
#         #   rv$content_stop_rm %>%
#         #     unnest_regex(Token, Contents,
#         #       pattern = tolower(input$other_token))
#         # }
#       # }) # end assigning reactive content_tokenised
# 
#       # rv$content_parameterised <- content_tokenised() %>%
#       #   relocate(Token, .after = ID)
# 
#       rv$content_prepared <- rv$content_parameterised
#       rv$is_tokenised <- TRUE
#       
#     }) # end observeEvent tokenise button
# 
# 
#     
#     #####################
#     ## Revert tokenise ##
#     #####################
#     # When revert tokens button is clicked set content_prepared back 
#     # to pre_tokenised data saved
#     observeEvent(input$revert_tokenise, {
# 
#       if (rv$is_tokenised) {
#         rv$content_prepared <- rv$content_stop_rm
#       }
#       rv$is_tokenised <- FALSE
#       
#       # For selected dataset, create pre-stop-removed dataset in case of undo, 
#       # and then tokenise with tokenise_data() function in utils.R
#       if(input$data_to_tokenise == "Primary data"){
#         rv$content_primary$content_prepared <- 
#           rv$content_primary$content_pre_tokenise
#         rv$content_primary$is_tokenised <- FALSE
#         
#       } else if(input$data_to_tokenise == "Subset one"){
#         # Save copy of content_prepared in case of undo
#         rv$subset_one$content_prepared <- rv$subset_one$content_pre_tokenise 
#         rv$subset_one$is_tokenised <- FALSE
# 
#       } else if(input$data_to_tokenise == "Subset two"){
#         # Save copy of content_prepared in case of undo
#         rv$subset_two$content_prepared <- rv$subset_two$content_pre_tokenise
#         rv$subset_two$is_tokenised <- FALSE
#       }
#     
#       shinyjs::reset(input$token)
#     }) # end revert tokenise button
#   })
# }





#### TEXT PREP UI #####
textPrepUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      wellPanel(
        h1("02 | Text Preparation"),
        em("Include stop-words, tokenise, and/or filter your text data below."),
      ),
      hr(),
      fluidRow(

        # Box for data editing
        box(
          title = "Data editing",
          status = "primary",
          solidHeader = T,
          collapsible = T,
          width = 12,
          fluidRow(
            column(
              4,
              h3("Mutate data"),
              p("Edit your text data by double-clicking on the table and/or add a column using the mutating tools."),

              # Radio buttons to render which type of mutation controls
              radioButtons(ns("mutate_option"),
                label = "Choose a mutate method:",
                choices = list(
                  "Add new column" = "mutate_new",
                  "Update existing column" = "mutate_update"
                )
              ),
              wellPanel(

                # Conditional panel renders either new col name box or dropdown box of
                # existing columns depending on the mutate method option selected
                conditionalPanel(
                  paste0(
                    "input['", ns("mutate_option"),
                    "'] == 'mutate_new' "
                  ),
                  uiOutput(ns("mutate_new_UI"))
                ),
                conditionalPanel(
                  paste0(
                    "input['", ns("mutate_option"),
                    "'] == 'mutate_update' "
                  ),
                  uiOutput(ns("mutate_update_UI"))
                ),

                # Conditional panel for simple or advanced mutate options
                conditionalPanel(
                  paste0(
                    "input['", ns("is_advanced_mutate"),
                    "'] == true "
                  ),
                  uiOutput(ns("advanced_mutate_UI"))
                ),
                conditionalPanel(
                  paste0(
                    "input['", ns("is_advanced_mutate"),
                    "'] == false "
                  ),
                  textInput(ns("mutate_to_insert_simple"),
                    label =
                      "Value to insert:",
                    placeholder = "e.g. 1, apple, TRUE"
                  ),
                ),
                checkboxInput(
                  ns("is_advanced_mutate"),
                  "Advanced mutate"
                ),
                fluidRow(
                  column(
                    width = 6,
                    actionButton(ns("submit_mutate"),
                      label = "Mutate",
                      class = "btn-primary"
                    )
                  ),
                  column(
                    width = 6,
                    actionButton(ns("undo_mutate"),
                      label = "Undo",
                      class = "btn-danger"
                    )
                  )
                ),
              ), # end mutating options well panel

              hr(),
              
              # Filtering options 
              h3("Filter data"),
              p("Use the interactive datatable to filter attributes of your text data. Save your filtered data to use in further analyses."),
              
                wellPanel(
                  fluidRow(
                  column(
                    width = 6,
                    actionButton(ns("submit_filters"),
                                 label = "Filter",
                                 class = "btn-primary"
                    )
                  ),
                  column(
                    width = 6,
                    actionButton(ns("undo_filters"),
                                 label = "Undo filters",
                                 class = "btn-danger"
                    )
                  ),
                ) # end fluidRow
                
              ),# end well panel
              
            ), # end col 1

            column(
              8,
              # Display DT to filter data
              h4(textOutput(ns("parameterised_subtitle1")) %>%
                tagAppendAttributes(style = "background-color: crimson;
                            color: white;")),
              wellPanel(
                # DT::dataTableOutput(ns("content_prepared_DT")),
                DT::DTOutput(ns("content_prepared_DT")),
              ),
              fluidRow(
                column(
                  width = 6,
                  hr()
                ),
                column(
                  width = 3,
                  actionButton(ns("add_content_row"),
                    label = "Add row",
                    class = "btn-warning"
                  )
                ),
                column(
                  width = 3,
                  actionButton(ns("remove_content_row"),
                    label = "Delete row",
                    class = "btn-danger"
                  )
                )
              ), # end fluid row
            ), # end col 2
          ), # end fluid row

          hr(),
          fluidRow(
            
            column(5,
                     br(),
            ), # end column 
            
            column(7,
                   wellPanel(
                     # Subsetting options
                     h3("Subset data"),
                     p("Save your filtered and/or mutated subset of data for comparison."),
                     
                     fluidRow(
                       column(width = 6,
                              actionButton(ns("save_subset_one"),
                                           label = "Save in Slot I",
                                           class = "btn-success"),
                       ),
                       column(width = 6,
                              actionButton(ns("save_subset_two"),
                                           label = "Save in Slot II",
                                           class = "btn-success"),
                       ),
                     ), # end subsetting fluid row
                   ), # end well panel
              
            ) # end column
            
          ), # end fluid row
          
        ), # end box
      ), # end outer fluid row


      fluidRow(
        column(
          5,
          tabBox(
            side = "left",
            selected = "Stop-words",
            width = 12,
            tabPanel(
              title = "Stop-words",
              checkboxInput("stop_word_check",
                label = HTML("<p style='color:black;'>
                                            Include stop-word(s) </p>")
              ),
              wellPanel(
                conditionalPanel(
                  condition = "input.stop_word_check == true",
                  stopWordsUI(ns("stopwords"))
                ),
                conditionalPanel(
                  condition = "input.stop_word_check == false",
                  em("No stop-word(s) included.")
                )
              ) # end well panel
            ), # end tab panel

            tabPanel(
              title = "Tokenisation",
              wellPanel(
                tokenizeUI(ns("tokenize"))
              ) # end well panel
            ), # end tab panel

            tabPanel(
              title = "Stemming",
              wellPanel(
                h3("Stemming"),
                p("Performing stemming of the current tokenised data."),
                em("Note: data must be tokenised to perform stemming."),
                
                selectInput(ns("data_to_stem"), 
                            label = "Select a dataset:", 
                            choices = list("N/A" = "na")),
                
                selectInput(ns("col_name_to_stem"), 
                            label = "Select a column to perform stemming on:", 
                            choices = list("N/A" = "na")),
                
                fluidRow(
                  column(
                    width = 6,
                    actionButton(ns("submit_stemming"),
                      label = "Submit",
                      class = "btn-success"
                    ),
                  ),
                  column(
                    width = 6,
                    actionButton(ns("undo_stemming"),
                      label = "Undo",
                      class = "btn-danger"
                    ),
                  )
                ),
              ),
            ), # end stemming tabpanel
          ), # end tab box

          box(
            title = "Checkpoint",
            status = "success",
            solidHeader = T,
            collapsible = T,
            width = 12,
            h3("Download your data"),
            p("Save a copy of your parameterised data to your own
                  device."),
            fluidRow(
              column(4, {
                downloadButton(ns("download_parameterised_csv"),
                  label = "Save .csv"
                )
              }),
              column(4, {
                downloadButton(ns("download_parameterised_tsv"),
                  label = "Save .tsv"
                )
              }),
              column(4, {
                p()
              })
            ),
          ), # end box
        ), # end col 1

        column(
          7,
          tabBox(
            side = "right",
            width = 12,
            tabPanel(
              title = " ",
              wellPanel(
                h2("Updated data"),
                DT::dataTableOutput(ns("content_prepared_DT_2"))
              ) # end well panel
            ), # end tab panel
          ) # end tab box
        ) # end col 2
      ), # end outer fluid row
    ) # end fluid page
  )
}


#### TEXT PREP SERVER ####
textPrepServer <- function(id, rv = rv) {
  moduleServer(
    id,
    function(input, output, session) {
      stopWordsServer("stopwords", rv = rv)
      tokenizeServer("tokenize", rv = rv)

      # if no files submitted, print none found. Else print nothing.
      output$parameterised_subtitle1 <- renderText({
        validate(
          need(rv$content_primary$data, "Submit files in the Text Selector Tab to continue")
        )
        return(NULL)
      })
      
      ##### Probably not necessary - on initial run of server the content_prepared is rendered
      ##### i guess
      observe({
        rv$content_prepared_display_2 <- rv$content_primary$content_prepared
      })
      

      # Don't need to enforce tokenising
      # output$parameterised_subtitle2 <- renderText({
      #   # If content has not been parameterised, say so
      #   # If it has, return nothing
      #   validate(
      #     need(rv$content_parameterised,
      #          "Select stop-words and/or tokenise your data.")
      #   )
      #   return(NULL)
      # })

      # Rendering table of data to display
      # if no stop-words/tokenising performed, show content
      # in observe event submit stop-words: if stop-words submitted, render content_stop_rm
      # in observe event submit tokenise: if tokenised, render content_tokenised
      observe({
        req(rv$content_primary$data)

        # if content not filtered or mutated, set content_prepared to OG data
        # is_filtered and is_mutated set on initialisation of content_primary data list
        
        ##### Instead of this, have initialised $content_prepared as $data when subsets created
        if (!(rv$content_primary$is_filtered) && !(rv$content_primary$is_mutated)) {
          print("content_edited & content_prepared set to content in text prep server")
          rv$content_primary$content_edited <- rv$content_primary$data
          rv$content_primary$content_prepared <- rv$content_primary$data
        }
      })

      # Creating datatable of content parameterised at top of page
      output$content_prepared_DT <- DT::renderDataTable(
        rv$content_primary$content_prepared,
        filter = "top",
        server = TRUE,
        options = list(
          paging = TRUE,
          pageLength = 7,
          scrollX = TRUE,
          scrollY = TRUE,
          dom = "frtip",
          columnDefs =
            list(
              list(
                targets = 1,
                render = JS(
                  "function(data, type, row, meta) {",
                  "return type === 'display' && data.length > 100 ?",
                  "'<span title=\"' + data + '\">' + data
                        .substr(0, 100) + '...</span>' : data;", "}"
                )
              )
            )
        ),
        selection = "single",
        rownames = FALSE,
        editable = TRUE
      )


      ###########################
      #### Editing table ########
      ###########################

      # Observe event for each time a cell is edited, so it is saved
      # When edit occurs, subset content_prepared with user-entered value
      # in the datatable display. Result is that the data in server gets
      # updated whenever user makes edits on front end
      # R col indexes start at 1 whilst DT at 0, so col + 1 is used
      observeEvent(input$content_prepared_DT_cell_edit, {
        print("event observed: content_prepared_DT_cell_edit")

        rv$content_primary$content_prepared[input$content_prepared_DT_cell_edit$row, input$content_prepared_DT_cell_edit$col + 1] <<- input$content_prepared_DT_cell_edit$value

        print("content_prepared after edits made:")
        print(rv$content_primary$content_prepared)
      })

      # Observe event for save button - when save clicked, save current content_prepared
      # state
      observeEvent(input$save_content_edits, {
        req(rv$content_primary$content_prepared)

        print("saving content clicked, not sure what to do yet tho")

        # rv$content_prepared <- temp[input[["content_prepared_DT"]], ]
      })

      
      ###########################
      ###### Mutating UI ########
      ###########################
      # Creating ui for new column mutation 
      # UI for simple add new column
      output$mutate_new_UI <- renderUI({
        ns <- NS(id)
        tagList(
          textInput(ns("mutate_new_col_name"),
                    label = "Choose a name for your new column:",
                    value = "new_column"
          ),
        ) # end tagList of renderUI
      }) # end renderUI
      
      # UI for simple update column
      output$mutate_update_UI <- renderUI({
        ns <- NS(id)
        tagList(
          varSelectInput(ns("mutate_col_to_update"),
                         label = "Choose a column to update:",
                         rv$content_primary$content_prepared,
                         selected = 2,
                         multiple = FALSE
          ),
        ) # end tagList of renderUI
      }) # end renderUI
      
      # UI for advanced mutate
      output$advanced_mutate_UI <- renderUI({
        ns <- NS(id)
        tagList(
          p("Specify condition and values to insert"),
          # if <mutate_advanced_condition_col> <mutate_advanced condition>
          # <mutate_advanced_condition_input> then fill with
          varSelectInput(ns("mutate_advanced_condition_col"),
                         label = "If:",
                         rv$content_primary$content_prepared,
                         selected = 2,
                         multiple = FALSE
          ),
          fluidRow(
            column(
              width = 6,
              selectInput(ns("mutate_advanced_condition"),
                          label = NULL,
                          choices = list(
                            "is equal to (numeric)" = "is equal to (numeric)",
                            "is less than" = "is less than",
                            "is greater than" = "is greater than",
                            "matches string" = "matches string",
                            "contains" = "contains"
                          )
              )
            ),
            column(
              width = 6,
              # Render text input if 'contains' condition selected
              conditionalPanel(
                
                # if condition is contains OR is matches
                condition = paste0("input['", ns("mutate_advanced_condition"),
                                   "'] == 'contains' || 
                                   input['", ns("mutate_advanced_condition"), 
                                   "'] == 'matches string'"),
                textInput(ns("mutate_advanced_condition_text_input"),
                          label = NULL,
                          value = NULL,
                          placeholder = "e.g. shiny"
                ),
              ),
              # Render numeric input if equals/less than/greater than condition
              # is selected
              conditionalPanel(
                condition = paste0("input['", ns("mutate_advanced_condition"),
                                   "'] == 'is equal to (numeric)' || 
                                   input['", ns("mutate_advanced_condition"), 
                                   "'] == 'is less than' || input['", 
                                   ns("mutate_advanced_condition"), "'] ==
                                   'is greater than'"),
                numericInput(ns("mutate_advanced_condition_numeric_input"),
                             label = NULL,
                             value = 1
                ),
              ),
            )
          ),
          fluidRow(
            column(
              width = 6,
              textInput(ns("mutate_advanced_equals_true"),
                        label = "Then fill with:",
                        value = "TRUE"
              ),
            ),
            column(
              width = 6,
              textInput(ns("mutate_advanced_equals_false"),
                        label = "Else fill with:",
                        value = NULL,
                        placeholder = "(optional)"
              ),
            )
          ),
        ) # end tagList of renderUI
      }) # end renderUI

      ###########################
      ###### Mutating data ######
      ###########################

      observeEvent(input$submit_mutate, {
        req(rv$content_primary$content_edited) # is content if no editing yet, or mutating result
        req(rv$content_primary$content_prepared) # is either content/result of filtering/mutating
        rv$content_primary$pre_mutated_temp <- rv$content_primary$content_prepared # saving temp in case of undo

        
        # Saving what is in data table, so user can mutate and add column values
        # to just filtered data rows if they wish as opposed to every row
        in_datatable <- 
          rv$content_primary$content_prepared[input[["content_prepared_DT_rows_all"]], ]

        # Add new column selected
        if (input$mutate_option == "mutate_new") {
          
          # First ensuring column name isn't replicated
          if(input$mutate_new_col_name %in% colnames(rv$content_primary$content_prepared)){
            shinyalert(
              title = "Mutate failed: column name error",
              text = "Column names must be unique. \n \n Ensure your new column name is unique to all other column names. \n \n To mutate an existing column, select the 'Update existing column' button.",
              size = "xs", 
              closeOnEsc = TRUE, closeOnClickOutside = TRUE,
              html = FALSE, type = "warning",
              showConfirmButton = TRUE, showCancelButton = FALSE,
              confirmButtonText = "Dismiss",
              confirmButtonCol = "#4169E1",
              timer = 0, imageUrl = "", animation = TRUE
            )
            
            return()
            
          }
          
          # Basic add column selected
          if (input$is_advanced_mutate == FALSE) {
            # If the content_prepared ID column is in the
            # data table selected, add the value to insert, otherwise NA
            rv$content_primary$content_mutated <- rv$content_primary$content_prepared %>%
              mutate(
                "{input$mutate_new_col_name}" :=
                  if_else(.$ID %in% in_datatable$ID,
                          input$mutate_to_insert_simple, NA
                  )
              )
            
            rv$content_primary$is_mutated <- TRUE
            
          } else { # Where advanced mutate is selected:

            print("Advanced mutate new selected")
            
            # Running advanced_mutate from utils.R, parsing in user inputs
            res <- advanced_mutate(
              rv$content_primary$content_prepared,
              in_datatable,
              input$mutate_option,
              input$mutate_new_col_name,
              mutate_col_to_update = NULL,
              input$mutate_advanced_condition_col,
              input$mutate_advanced_condition,
              input$mutate_advanced_condition_numeric_input,
              input$mutate_advanced_condition_text_input,
              input$mutate_advanced_equals_true,
              input$mutate_advanced_equals_false
            )
            
            print("Resulting added column:")
            print(res)
            
            rv$content_primary$content_mutated <- res
            
            rv$content_primary$is_mutated <- TRUE
            
          }
          
        } # END mutate new logic

        # START update mutate logic
        # Where basic update column is selected:
        else if (input$mutate_option == "mutate_update") {

          if (input$is_advanced_mutate == FALSE) {
            
            # If row is in datatable, insert the value specified,
            # else input what is originally content_prep but not in datatable 
            # If datatable has been filtered but filters not submitted, update 
            # is performed on the displayed rows and other rows are left alone. 
            # If filters have been selected and submitted, update is performed 
            # on all rows
            for (i in 1:nrow(rv$content_primary$content_prepared[input$mutate_col_to_update])) {
              if (rv$content_primary$content_prepared$ID[i] %in% in_datatable$ID[i]) {
                rv$content_primary$content_prepared[[input$mutate_col_to_update]][[i]] <-
                  input$mutate_to_insert_simple
              }
            }
            rv$content_primary$content_mutated <- rv$content_primary$content_prepared

            rv$content_primary$is_mutated <- TRUE
            
            # Where advanced update column is selected:
          } else {
            print("Advanced mutate update selected")

            res <- advanced_mutate(
                      rv$content_primary$content_prepared,
                      in_datatable,
                      input$mutate_option,
                      mutate_new_col_name = NULL,
                      input$mutate_col_to_update,
                      input$mutate_advanced_condition_col,
                      input$mutate_advanced_condition,
                      input$mutate_advanced_condition_numeric_input,
                      input$mutate_advanced_condition_text_input,
                      input$mutate_advanced_equals_true,
                      input$mutate_advanced_equals_false
                    )
            
            print("Resulting updated column:")
            print(res)

            rv$content_primary$content_mutated <- res

            rv$content_primary$is_mutated <- TRUE
          }
        }

        # Setting content_edited and content_prepared to the
        # new mutated content
        req(rv$content_primary$content_mutated)
        rv$content_primary$content_edited <- rv$content_primary$content_mutated
        rv$content_primary$content_prepared <- rv$content_primary$content_mutated

        print("Content prepared after mutating in text prep tab:")
        print(rv$content_primary$content_prepared)
      })


      ## Undo mutations:
      ## If content has been mutated, revert to pre_mutated_temp
      observeEvent(input$undo_mutate, {
        req(rv$content_primary$is_mutated)

        # rv$content_parameterised <- rv$pre_mutated_temp # revert to original
        rv$content_primary$content_prepared <- rv$content_primary$pre_mutated_temp
        rv$content_primary$content_edited <- rv$content_primary$pre_mutated_temp

        rv$content_primary$is_mutated <- FALSE
      })


      ###########################
      ###### Filtering data #####
      ###########################

      # When submit filters clicked, subset content_stats by filtered rows
      observeEvent(input$submit_filters, {
        req(rv$content_primary$content_prepared)

        # Creating temporary storage hold
        rv$content_primary$pre_filtered_temp <- rv$content_primary$content_prepared

        # Subsetting content_stats by filters
        rv$content_primary$content_prepared <-
          rv$content_primary$pre_filtered_temp[input[["content_prepared_DT_rows_all"]], ]

        rv$content_primary$is_filtered <- TRUE
      })

      # When undo filters button clicked, revert content_stats to original
      observeEvent(input$undo_filters, {
        req(rv$content_primary$is_filtered)
        req(rv$content_primary$pre_filtered_temp) # require original reactive value

        rv$content_primary$content_parameterised <- rv$content_primary$pre_filtered_temp # revert to original
        rv$content_primary$content_prepared <- rv$content_primary$content_parameterised

        rv$content_primary$is_filtered <- FALSE
      })

      ###########################
      #### Subsetting data ######
      ###########################
      
      observeEvent(input$save_subset_one, {
        
        # require content_prepared to be present (i.e. required submitted data)
        req(rv$content_primary$content_prepared)
        
        # Saving whatever is currently in the mutated/filtered/edited datatable
        in_datatable <- rv$content_primary$content_prepared[input[["content_prepared_DT_rows_all"]], ]
        
        # initializing reactive value list of data and characteristics
        subset_one <- shiny::reactiveValues(data = in_datatable, 
                                           is_stop_rm = F, 
                                           is_tokenised = F, 
                                           content_prepared = in_datatable)
        
        # saving data and characteristics list to main rv list
        rv$subset_one <- subset_one
        
      })
      
      observeEvent(input$save_subset_two, {
        
        # require content_prepared to be present (i.e. required submitted data)
        req(rv$content_primary$content_prepared)
        
        # Saving whatever is currently in the mutated/filtered/edited datatable
        in_datatable <- rv$content_primary$content_prepared[input[["content_prepared_DT_rows_all"]], ]
        
        # initializing reactive value list of data and characteristics
        subset_two <- shiny::reactiveValues(data = in_datatable, 
                                            is_stop_rm = F, 
                                            is_tokenised = F, 
                                            content_prepared = in_datatable)
        
        # saving data and characteristics list to main rv list
        rv$subset_two <- subset_two
        
        print("Printing subset two:")
        print(rv$subset_two$data)
        
      })
      


      ###########################
      #### Downloading data #####
      ###########################
      # Rendering a download button with downloadHandler()
      output$download_parameterised_csv <- downloadHandler(
        filename = function() {
          paste("Parameterised_Content.csv")
        },
        content = function(file) {
          write_delim(as.data.frame(rv$content_primary$content_parameterised), file,
            delim = ","
          )
        }
      )

      output$download_parameterised_tsv <- downloadHandler(
        filename = function() {
          paste("Parameterised_Content.tsv")
        },
        content = function(file) {
          write_tsv(as.data.frame(rv$content_primary$content_parameterised), file)
        }
      )


      # Creating datatable 2 of content parameterised after stop word removal
      output$content_prepared_DT_2 <- DT::renderDataTable(
        # rv$content_primary$content_prepared,
        rv$content_prepared_display_2,
        options = list(
          paging = TRUE,
          pageLength = 7,
          scrollX = TRUE,
          scrollY = TRUE,
          dom = "rtip",
          columnDefs =
            list(
              list(
                targets = 1,
                render = JS(
                  "function(data, type, row, meta) {",
                  "return type === 'display' && data.length > 100 ?",
                  "'<span title=\"' + data + '\">' + data
                        .substr(0, 100) + '...</span>' : data;", "}"
                )
              )
            )
        ),
        selection = "none",
        rownames = FALSE,
      )


      ###########################
      ####### Stemming ##########
      ###########################
      # Again need to select dataset, choose column to stem
      # Need to enforce that column selected is tokenised before stemming
      
      ## Dataset chooser 
      # Always update possible dataset list with current available subsets
      observe({
        # Create list of possible datasets to select from
        req(rv$content_primary)
        potential_sets_tokenise <- list("Primary data")
        
        if(!is.null(rv$subset_one)){
          potential_sets_tokenise[length(potential_sets_tokenise) + 1] <- "Subset one"
        }
        if(!is.null(rv$subset_two)){
          potential_sets_tokenise[length(potential_sets_tokenise) + 1] <- "Subset two"
        }
        
        # updating select input list to contain datasets available
        updateSelectInput(session, "data_to_stem",
                          choices = potential_sets_tokenise,
                          selected= potential_sets_tokenise[1])
      })
      
      ## Column chooser 
      # When a dataset is chosen to perform stemming on, need to load which 
      # columns available in that dataset
      observe({
        req(rv$content_primary$content_prepared)
        
        # creating list of available columns
        # subsets' content_prepared is initialized on save of subsets
        colnames <- switch(input$data_to_stem, 
                           "Primary data" = 
                             colnames(rv$content_primary$content_prepared), 
                           "Subset one" = colnames(rv$subset_one$content_prepared), 
                           "Subset two" = colnames(rv$subset_two$content_prepared)
        )

        updateSelectInput(session, "col_name_to_stem",
                          choices = colnames[-1],
                          selected= colnames[2])
      })
      
      # Rendering whatever dataset selected in datatable display
      observeEvent(input$data_to_stem, {
        req(rv$content_primary$content_prepared)
        rv$content_prepared_display_2 <- 
          switch(input$data_to_stem, 
                 "Primary data" =  rv$content_primary$content_prepared, 
                 "Subset one" = rv$subset_one$content_prepared, 
                 "Subset two" = rv$subset_two$content_prepared
          )
      })

      ## Actually stemming data
      observeEvent(input$submit_stemming, {
        
        # First check if selected column is tokenised
        # or does it still work if not tokenised...?
        
        req(rv$content_primary) # always require something submitted
        
        # For selected dataset, create pre-stemmed dataset in case of undo, 
        # and then stem with stem_data() function in utils.R
        if(input$data_to_stem == "Primary data"){
          
          # Save copy of content_prepared in case of undo
          rv$content_primary$content_pre_stemmed <- rv$content_primary$content_prepared
          
          # Use stem_data function to stem
          content_stemmed <- stem_data(rv$content_primary$content_prepared, 
                                             col_name = input$col_name_to_stem)
          
          # Set stemmed data to rv$content_primary$content_prepared and 
          # set is_stemmed = TRUE
          rv$content_primary$content_prepared <- content_stemmed
          rv$content_primary$is_stemmed <- TRUE
          
          print("Stemmed, rv$content_primary$content_prepared display:")
          print(rv$content_primary$content_prepared)
          
        } else if(input$data_to_stem == "Subset one"){
          
          req(rv$subset_one$content_prepared)
          
          # Save copy of content_prepared in case of undo
          rv$subset_one$content_pre_stemmed <- rv$subset_one$content_prepared
          
          # Stemming
          content_stemmed <- stem_data(rv$subset_one$content_prepared, 
                                             col_name = input$col_name_to_stem)
          
          rv$subset_one$content_prepared <- content_stemmed
          rv$subset_one$is_stemmed <- TRUE
          
        } else if(input$data_to_stem == "Subset two"){
          
          req(rv$subset_two$content_prepared)
          
          # Save copy of content_prepared in case of undo
          rv$subset_two$content_pre_stemmed <- rv$subset_one$content_prepared
          
          # Stemming
          content_stemmed <- stem_data(rv$subset_two$content_prepared, 
                                           col_name = input$col_name_to_stem)
          
          rv$subset_two$content_prepared <- content_stemmed
          rv$subset_two$is_stemmed <- TRUE
        }

      }) # end observe event submit stemming

      # Undo stemming - set chosen dataset back to pre-stemmed save
      observeEvent(input$undo_stemming, {
        
        if(input$data_to_stem == "Primary data"){
          
          req(rv$content_primary$content_pre_stemmed)
          rv$content_primary$content_prepared <- rv$content_primary$content_pre_stemmed
          rv$content_primary$is_stemmed <- FALSE
          
        } else if(input$data_to_stem == "Subset one"){
          
          req(rv$subset_one$content_pre_stemmed)
          rv$subset_one$content_prepared <- rv$subset_one$content_pre_stemmed
          rv$subset_one$is_stemmed <- FALSE
          
        } else if(input$data_to_stem == "Subset two"){
          
          req(rv$subset_two$content_pre_stemmed)
          rv$subset_two$content_prepared <- rv$subset_two$content_pre_stemmed
          rv$subset_two$is_stemmed <- FALSE
        }
      }) # end observe event undo stemming
      
      
    }
  )
} # end text preparation server logic
