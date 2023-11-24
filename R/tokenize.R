
#############################
#### Tokenisation module ####
#############################

# Handles tokenisation of text.
# Requires initial dataset to tokenise on, rv$content_primary$content_prepared,
# provided in a 2D reactive value list, rv. 
# Depends on 1 function, tokenise_data(), in utils.R

#### TOKENISATION MODULE UI ####
tokenizeUI <- function(id) {
  ns <- NS(id)
  tagList(
    
    h3("Tokenisation"),
    
    selectInput(ns("data_to_tokenise"), 
                label = "Select a dataset:", 
                choices = list("N/A" = "na")),
    
    selectInput(ns("col_name_to_tokenise"), 
                label = "Select a column to tokenise:", 
                choices = list("N/A" = "na")),
    
    selectInput(ns("token"),
                label = "Choose a token to split text data on:",
                choices = c(
                  "Words" = "words",
                  "Bi-grams" = "bigrams",
                  "n-grams" = "ngrams",
                  # "Sentences" = "sentences",
                  "Other" = "other"
                ),
                selected = "Words"
    ),
    conditionalPanel(
      condition = "input.tokens == 'ngrams'",
      ns = ns,
      # uiOutput(ns("ngramsUI")) # was using renderUI in server to make UI for this
      numericInput(ns("n_grams"), label = "Enter n-grams:", value = 2)
    ),
    conditionalPanel(
      condition = "input.tokens == 'other'",
      ns = ns,
      # make mini UI for regex token
      textInput(ns("other_token"), "Specify a custom token:")
    ),
    fluidRow(
      column(6, {
        actionButton(ns("submit_tokenise"),
                     label = "Tokenise",
                     class = "btn-success"
        )
      }),
      column(6, {
        actionButton(ns("revert_tokenise"),
                     label = "Revert",
                     class = "btn-danger"
        )
      })
    ),
  )
}

#### TOKENISATION SERVER ####
tokenizeServer <- function(id, rv = rv) {
  moduleServer(id, function(input, output, session) {
    
    # rv$is_tokenised <- FALSE
    
    #####################
    ## Dataset chooser ##
    #####################
    # Always update possible dataset list with current available subsets
    observe({
      # Create list of possible datasets to select from
      req(rv$content_primary)
      potential_sets_tokenise <- list("Primary data")
      print("Made potential sets list:")
      print(potential_sets_tokenise)
      
      if(!is.null(rv$subset_one)){
        potential_sets_tokenise[length(potential_sets_tokenise) + 1] <- "Subset one"
      }
      if(!is.null(rv$subset_two)){
        potential_sets_tokenise[length(potential_sets_tokenise) + 1] <- "Subset two"
      }
      
      # updating select input list to contain datasets available
      updateSelectInput(session, "data_to_tokenise",
                        choices = potential_sets_tokenise,
                        selected= potential_sets_tokenise[1])
    })
    
    ###################
    ## Column chooser #
    ###################
    # When a dataset is chosen to perform tokenisation on, need to load which 
    # columns available in that dataset
    observe({
      req(rv$content_primary$content_prepared)
      
      # creating list of available columns
      # subsets' content_prepared is initialized on save of subsets
      colnames <- switch(input$data_to_tokenise, 
                         "Primary data" = 
                           colnames(rv$content_primary$content_prepared), 
                         "Subset one" = colnames(rv$subset_one$content_prepared), 
                         "Subset two" = colnames(rv$subset_two$content_prepared)
      )
      # updating select input list for choosing column to remove stops from
      updateSelectInput(session, "col_name_to_tokenise",
                        choices = colnames[-1],
                        selected= colnames[2])
    })
    
    # Rendering whatever dataset selected in datatable display
    observeEvent(input$data_to_tokenise, {
      req(rv$content_primary$content_prepared)
      rv$content_prepared_display_2 <- 
        switch(input$data_to_tokenise, 
               "Primary data" =  rv$content_primary$content_prepared, 
               "Subset one" = rv$subset_one$content_prepared, 
               "Subset two" = rv$subset_two$content_prepared
        )
    })
    
    
    #####################
    ### Tokenisation ####
    #####################
    # When "tokenise" button clicked...
    observeEvent(input$submit_tokenise, {
      req(rv$content_primary) # always require something submitted
      
      # For selected dataset, create pre-stop-removed dataset in case of undo, 
      # and then tokenise with tokenise_data() function in utils.R
      if(input$data_to_tokenise == "Primary data"){
        
        # Save copy of content_prepared in case of undo
        rv$content_primary$content_pre_tokenise <- 
          rv$content_primary$content_prepared
        
        # Use tokenise_data function, passing in data_to_tokenise, 
        # col_name_to_tokenise, input$token and if input$token = "other"
        # pass in custom_token = input$other_token
        content_tokenised <- tokenise_data(rv$content_primary$content_prepared, 
                                           col_name = input$col_name_to_tokenise, 
                                           token = input$token, 
                                           custom_token = input$other_token, 
                                           n_grams = input$n_grams)
        
        # Set tokenised data to rv$content_primary$content_prepared and 
        # set is_tokenised = TRUE
        rv$content_primary$content_prepared <- content_tokenised
        rv$content_primary$is_tokenised <- TRUE
        
        print("Tokenised, rv$content_primary$content_prepared display:")
        print(rv$content_primary$content_prepared)
        
      } else if(input$data_to_tokenise == "Subset one"){
        
        # Save copy of content_prepared in case of undo
        rv$subset_one$content_pre_tokenise <- rv$subset_one$content_prepared
        
        # Tokenising
        content_tokenised <- tokenise_data(rv$subset_one$content_prepared, 
                                           col_name = input$col_name_to_tokenise, 
                                           token = input$token, 
                                           custom_token = input$other_token, 
                                           n_grams = input$n_grams)
        
        rv$subset_one$content_prepared <- content_tokenised
        rv$subset_one$is_tokenised <- TRUE
        
        
      } else if(input$data_to_tokenise == "Subset two"){
        
        # Save copy of content_prepared in case of undo
        rv$subset_two$content_pre_tokenise <- rv$subset_two$content_prepared
        
        # Tokenising
        content_tokenised <- tokenise_data(rv$subset_two$content_prepared, 
                                           col_name = input$col_name_to_tokenise, 
                                           token = input$token, 
                                           custom_token = input$other_token, 
                                           n_grams = input$n_grams)
        
        rv$subset_two$content_prepared <- content_tokenised
        rv$subset_two$is_tokenised <- TRUE
        
      }
      
      # Tokenising content tibble. Output column is selected token, input is
      # Contents columns, tokenising by user-selected token
      # content_tokenised <- reactive({
      # # if token is easily handled, just unnest w input$token
      # if (input$token == "words" || input$token == "sentences") {
      #   rv$content_stop_rm %>%
      #     unnest_tokens(Token, Contents, token = input$token)
      # } else if (input$token == "bigrams") {
      #   rv$content_stop_rm %>%
      #     unnest_ngrams(Token, Contents, n = 2)
      # } else if (input$token == "ngrams") {
      #   rv$content_stop_rm %>%
      #     unnest_ngrams(Token, Contents, n = input$n_grams)
      # } else if (input$token == "other") {
      #   # unnest on the user inputted regex
      #   rv$content_stop_rm %>%
      #     unnest_regex(Token, Contents,
      #       pattern = tolower(input$other_token))
      # }
      # }) # end assigning reactive content_tokenised
      
      # rv$content_parameterised <- content_tokenised() %>%
      #   relocate(Token, .after = ID)
      
      rv$content_prepared <- rv$content_parameterised
      rv$is_tokenised <- TRUE
      
    }) # end observeEvent tokenise button
    
    
    
    #####################
    ## Revert tokenise ##
    #####################
    # When revert tokens button is clicked set content_prepared back 
    # to pre_tokenised data saved
    observeEvent(input$revert_tokenise, {
      
      if (rv$is_tokenised) {
        rv$content_prepared <- rv$content_stop_rm
      }
      rv$is_tokenised <- FALSE
      
      # For selected dataset, create pre-stop-removed dataset in case of undo, 
      # and then tokenise with tokenise_data() function in utils.R
      if(input$data_to_tokenise == "Primary data"){
        rv$content_primary$content_prepared <- 
          rv$content_primary$content_pre_tokenise
        rv$content_primary$is_tokenised <- FALSE
        
      } else if(input$data_to_tokenise == "Subset one"){
        # Save copy of content_prepared in case of undo
        rv$subset_one$content_prepared <- rv$subset_one$content_pre_tokenise 
        rv$subset_one$is_tokenised <- FALSE
        
      } else if(input$data_to_tokenise == "Subset two"){
        # Save copy of content_prepared in case of undo
        rv$subset_two$content_prepared <- rv$subset_two$content_pre_tokenise
        rv$subset_two$is_tokenised <- FALSE
      }
      
      shinyjs::reset(input$token)
    }) # end revert tokenise button
  })
}
