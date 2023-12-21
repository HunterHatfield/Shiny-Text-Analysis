
#############################
#### Tokenisation module ####
#############################

# Handles Tokenisation of text.
# Requires initial dataset to tokenise on, rv$content_primary$content_prepared,
# provided in a 2D reactive value list, rv. 
# Depends on 1 function, tokenise_data(), in utils.R

#### Tokenisation MODULE UI ####
tokeniseUI <- function(id) {
  ns <- NS(id)
  tagList(
    
    h3("Tokenisation"),
    p("Tokenisation breaks text into smaller units, or tokens, like words or bi-grams. This enables us to produce visualisations and efficiently analyse token frequencies  within text data."),
    tags$a(href="https://neptune.ai/blog/tokenization-in-nlp", 
           "Learn more about tokenisation here"),
    
    hr(),
    
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
      condition = "input.token == 'ngrams'",
      ns = ns,
      numericInput(ns("n_grams"), label = "Enter n-grams:", value = 2)
    ),
    conditionalPanel(
      condition = "input.token == 'other'",
      ns = ns,
      # make mini UI for regex token
      textInput(ns("other_token"), "Specify a custom token:")
    ),
    fluidRow(
      column(6, {
        actionButton(ns("tokenise_trigger"),
                     label = "Tokenise...",
                     class = "btn-success"
        )
      }),
      column(6, {
        actionButton(ns("revert_tokenise"),
                     label = "Undo",
                     class = "btn-danger"
        )
      })
    ),
  )
}

#### tokenisation SERVER ####
tokeniseServer <- function(id, rv = rv) {
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
    
    
    ### Tokenise button triggers modal
    # set the session namespace and return modelDilog() content 
    show_tokenise_modal <- function(){
      
      ns <- session$ns
      modalDialog(
        size = "l",
        h2("Confirm tokenisation?"),
        p("Clicking submit will break up text data into specified tokens."),
        p("Tokenising from smaller to larger tokens where rows must be combined will not preserve additional columns added."),
        hr(class = "hr-blank"),
        fluidRow(
          column(12, 
                 actionButton(ns("submit_tokenise"), label = "Submit tokenise",
                              class = "btn-success"),
          )
        ), # end fluid row
      )
    } # end show_subset_modal() function
    
    observeEvent(input$tokenise_trigger, {
      req(rv$content_primary$content_prepared)
      
      showModal(show_tokenise_modal())
    })
    
    #####################
    ### Tokenisation ####
    #####################
    tokenise_alert <- function(){ 
      shinyalert(
      title = "Tokenisation failed: invalid selection",
      text = "Attempts to convert data from smaller to larger token forms will fail if data is not reverted to paragraph form first. \n \n To convert data from e.g. word tokens to bi-grams, revert text data to paragraphs first then tokenise.",
      size = "s", 
      closeOnEsc = TRUE, closeOnClickOutside = TRUE,
      html = FALSE, type = "warning",
      showConfirmButton = TRUE, showCancelButton = FALSE,
      confirmButtonText = "Dismiss",
      confirmButtonCol = "#4169E1",
      timer = 0, imageUrl = "", animation = TRUE
    )
    }
    
    # When "tokenise" button clicked...
    observeEvent(input$submit_tokenise, {
      removeModal()
      req(rv$content_primary) # always require something submitted
      
      # For selected dataset, create pre-stop-removed dataset in case of undo, 
      # and then tokenise with tokenise_data() function in utils.R
      if(input$data_to_tokenise == "Primary data"){
        
        # initializing list of already tokenised cols
        if(is.null(rv$content_primary$tokenised_col_names)){
          rv$content_primary$tokenised_col_names <- list()
        }
        
        # Generating alert is column is already tokenised
        if({input$col_name_to_tokenise} %in% rv$content_primary$tokenised_col_names){
          shinyalert(
            title = "Column already tokenised",
            text = "Undo tokenisation on this column with the 'Undo' button before re-tokenising.",
            size = "s", 
            closeOnEsc = TRUE, closeOnClickOutside = TRUE,
            html = FALSE, type = "warning",
            showConfirmButton = TRUE, showCancelButton = FALSE,
            confirmButtonText = "Dismiss",
            confirmButtonCol = "#4169E1",
            timer = 0, imageUrl = "", animation = TRUE
          )
          
          return()
        }
        
        # Save copy of content_prepared in case of undo
        rv$content_primary$content_pre_tokenise <- 
          rv$content_primary$content_prepared
        
        # Use tokenise_data function, passing in data_to_tokenise, 
        # col_name_to_tokenise, input$token and if input$token = "other"
        # pass in custom_token = input$other_token
        content_tokenised <- try(tokenise_data(rv$content_primary$content_prepared, 
                                           col_name = input$col_name_to_tokenise, 
                                           token = input$token, 
                                           custom_token = input$other_token, 
                                           n_grams = input$n_grams))
        
        # Check that tokenisation didn't return all NAs or an error
        # need to split up to check if produces try-error first then check
        # if column is NA, since can't subset to find a column if produces error
        if("try-error" %in% class(content_tokenised)){
          print(" try error occurred")
          tokenise_alert()
          return()
        } else if(all(is.na(content_tokenised[[input$col_name_to_tokenise]]))){
          print("all NA occurred")
          tokenise_alert()
          return()
        }
        
        # Set tokenised data to rv$content_primary$content_prepared and 
        # set is_tokenised = TRUE
        rv$content_primary$content_prepared <- content_tokenised
        rv$content_primary$is_tokenised <- TRUE
        
        # Adding col_name to tokenised columns list
        rv$content_primary$tokenised_col_names <- append(
          rv$content_primary$tokenised_col_names, 
          {input$col_name_to_tokenise})
        
        # Updating display table with tokenised content
        rv$content_prepared_display_2 <- content_tokenised
        
      } else if(input$data_to_tokenise == "Subset one"){
        
        # initializing list of already tokenised cols
        if(is.null(rv$subset_one$tokenised_col_names)){
          rv$subset_one$tokenised_col_names <- list()
        }
        
        if(input$col_name_to_tokenise %in% rv$subset_one$tokenised_col_names){
          
          shinyalert(
            title = "Column already tokenised",
            text = "Undo tokenisation on this column with the 'Revert' button before re-tokenising.",
            size = "s", 
            closeOnEsc = TRUE, closeOnClickOutside = TRUE,
            html = FALSE, type = "warning",
            showConfirmButton = TRUE, showCancelButton = FALSE,
            confirmButtonText = "Dismiss",
            confirmButtonCol = "#4169E1",
            timer = 0, imageUrl = "", animation = TRUE
          )
          
          return()
        }
        
        # Save copy of content_prepared in case of undo
        rv$subset_one$content_pre_tokenise <- rv$subset_one$content_prepared
        
        # Tokenising
        content_tokenised <- tokenise_data(rv$subset_one$content_prepared, 
                                           col_name = input$col_name_to_tokenise, 
                                           token = input$token, 
                                           custom_token = input$other_token, 
                                           n_grams = input$n_grams)
        
        # need to split up to check if produces try-error first then check
        # if column is NA, since can't subset to find a column if produces error
        if("try-error" %in% class(content_tokenised)){
          print(" try error occurred")
          tokenise_alert()
          return()
          
        } else if(all(is.na(content_tokenised[[input$col_name_to_tokenise]]))){
          print("all NA occurred")
          tokenise_alert()
          return()
        }
        
        rv$subset_one$content_prepared <- content_tokenised
        rv$subset_one$is_tokenised <- TRUE
        
        rv$subset_one$tokenised_col_names<- append(
            rv$subset_one$tokenised_col_names, 
            {input$col_name_to_tokenise})
        
        # Updating display table with tokenised content
        rv$content_prepared_display_2 <- content_tokenised
        
      } else if(input$data_to_tokenise == "Subset two"){
        
        # initializing list of already tokenised cols
        if(is.null(rv$subset_two$tokenised_col_names)){
          rv$subset_two$tokenised_col_names <- list()
        }
        
        if(input$col_name_to_tokenise %in% rv$subset_two$tokenised_col_names){
          
          shinyalert(
            title = "Column already tokenised",
            text = "Undo tokenisation on this column with the 'Revert' button before re-tokenising.",
            size = "s", 
            closeOnEsc = TRUE, closeOnClickOutside = TRUE,
            html = FALSE, type = "warning",
            showConfirmButton = TRUE, showCancelButton = FALSE,
            confirmButtonText = "Dismiss",
            confirmButtonCol = "#4169E1",
            timer = 0, imageUrl = "", animation = TRUE
          )
          
          return()
        }
        
        # Save copy of content_prepared in case of undo
        rv$subset_two$content_pre_tokenise <- rv$subset_two$content_prepared
        
        # Tokenising
        content_tokenised <- tokenise_data(rv$subset_two$content_prepared, 
                                           col_name = input$col_name_to_tokenise, 
                                           token = input$token, 
                                           custom_token = input$other_token, 
                                           n_grams = input$n_grams)
        
        # Check that tokenisation didn't return all NAs or an error
        # need to split up to check if produces try-error first then check
        # if column is NA, since can't subset to find a column if produces error
        if("try-error" %in% class(content_tokenised)){
          print(" try error occurred")
          tokenise_alert()
          return()
          
        } else if(all(is.na(content_tokenised[[input$col_name_to_tokenise]]))){
          print("all NA occurred")
          tokenise_alert()
          return()
        }
        
        rv$subset_two$content_prepared <- content_tokenised
        rv$subset_two$is_tokenised <- TRUE
        rv$subset_two$tokenised_col_names <- append(
          rv$subset_two$tokenised_col_names, 
          {input$col_name_to_tokenise})
        # Updating display table with tokenised content
        rv$content_prepared_display_2 <- content_tokenised
        
      }
      
      print("after tokenise error got to here")
      
      
    }) # end observeEvent tokenise button
    
    
    #####################
    ## Revert tokenise ##
    #####################
    # When revert tokens button is clicked set content_prepared back 
    # to pre_tokenised data saved
    observeEvent(input$revert_tokenise, {
      
      # For selected dataset, create pre-stop-removed dataset in case of undo, 
      # and then tokenise with tokenise_data() function in utils.R
      if(input$data_to_tokenise == "Primary data"){
        req(rv$content_primary$content_pre_tokenise)
        req(rv$content_primary$tokenised_col_names)
        
        rv$content_primary$content_prepared <- 
          rv$content_primary$content_pre_tokenise
        rv$content_primary$is_tokenised <- FALSE
        
        # Undoing tokenise goes back one step from the last thing tokenised,
        # thus removing last column added to tokenised col names list
        rv$content_primary$tokenised_col_names <- 
          rv$content_primary$tokenised_col_names[-length(
            rv$content_primary$tokenised_col_names
          )]
        
        rv$content_prepared_display_2 <- rv$content_primary$content_prepared
        
      } else if(input$data_to_tokenise == "Subset one"){
        req(rv$subset_one$content_pre_tokenise)
        req(rv$subset_one$tokenised_col_names)
        
        # Save copy of content_prepared in case of undo
        rv$subset_one$content_prepared <- rv$subset_one$content_pre_tokenise 
        rv$subset_one$is_tokenised <- FALSE
        
        rv$subset_one$tokenised_col_names <- 
          rv$subset_one$tokenised_col_names[-length(
            rv$subset_one$tokenised_col_names
          )]
        
        rv$content_prepared_display_2 <- rv$subset_one$content_prepared
        
      } else if(input$data_to_tokenise == "Subset two"){
        req(rv$subset_two$content_pre_tokenise)
        req(rv$subset_two$tokenised_col_names)
        
        # Save copy of content_prepared in case of undo
        rv$subset_two$content_prepared <- rv$subset_two$content_pre_tokenise
        rv$subset_two$is_tokenised <- FALSE
        
        rv$subset_two$tokenised_col_names <- 
          rv$subset_two$tokenised_col_names[-length(
            rv$subset_two$tokenised_col_names
          )]
        
        rv$content_prepared_display_2 <- rv$subset_two$content_prepared
        
      }
      
      shinyjs::reset(input$token)
    }) # end revert tokenise button
  })
}
