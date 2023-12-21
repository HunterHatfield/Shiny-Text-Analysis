
##################################
#### Stop word removal module ####
##################################

# Handles stop word import, gathering and removal.
# Requires initial dataset to remove stop words on, rv$content_primary$content_prepared,
# provided in a 2D reactive value list, rv. 
# Depends on 1 function, remove_stop_words(), in utils.R

data(stop_words)
rv$is_stop_removed <- FALSE

##### STOPWORDS MODULE UI ####
stopWordsUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Stop-word selection"),
    p("Stop-words, like 'the' and 'and,' are common but lack meaningful content. Removing them helps us to focus on essential words, making text analysis more efficient and accurate."),
    tags$a(href="https://smltar.com/stopwords", 
           "Learn more about stop-words here"),
    
    hr(), 
    
    # Select a dataset to alter
    selectInput(ns("data_stop_rm"), 
                label = "Select a dataset to alter:", 
                choices = list("N/A" = "na")),
    
    # Select a column to remove stop-words from
    selectInput(ns("col_name_stop_rm"), 
                label = "Select a column to remove stop-words from:", 
                choices = list("N/A" = "na")),
    
    # checkbox input for including default stop words 
    checkboxInput(ns("default_check"),
                  label = "Include default stop-word list"
    ),
    
    # textbox input for extra stop words
    textInput(ns("added_words"),
              label = "Add extra stop-word(s) separated by commas and/or spaces:",
              value = NULL,
              placeholder = "e.g. apple, banana carrot"
    ),
    
    fluidRow(
      column(6, {
        actionButton(ns("stop_word_trigger"),
                     label = "Submit...",
                     class = "btn-success"
        )
      }),
      column(6, {
        actionButton(ns("undo_stop_words"),
                     label = "Undo",
                     class = "btn-danger"
        )
      })
    ),
    conditionalPanel(
      condition = "input.default_check == 'true'",
      p(textOutput(ns("default_caption")))
    ),
    hr(),
    h3("Submitted stop-words"),
    p("Default English stop-words are sourced from the tidytext package in R."),
    tags$a(href="https://rdrr.io/cran/tidytext/man/stop_words.html", 
           "View tidytext docs here"),
    wellPanel(
      p(textOutput(ns("stop_word_display"))),
      DT::dataTableOutput(ns("stop_words_table")),
      hr(class = "hr-blank")
    ),
    
    
    
  )
}

#### STOPWORDS SERVER ####
stopWordsServer <- function(id, stop_word_list = stop_word_list, rv = rv) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      req(rv$content_primary$data)
      updateSelectInput(session, "test",
                        choices = c("updated"))
    })
    
    output$num_stops <- renderText(length(rv$stop_words_final$word))
    
    ############################
    ## Creating dataset chooser
    ############################
    # Always update possible dataset list with current available subsets
    # so using observe()
    observe({
      # Create list of possible datasets to select from
      req(rv$content_primary)
      potential_sets_stop_rm <- list("Primary data")
      
      if(!is.null(rv$subset_one)){
        potential_sets_stop_rm[length(potential_sets_stop_rm) + 1] <- "Subset one"
      }
      if(!is.null(rv$subset_two)){
        potential_sets_stop_rm[length(potential_sets_stop_rm) + 1] <- "Subset two"
      }
      
      # updating select input list to contain datasets available
      updateSelectInput(session, "data_stop_rm",
                        choices = potential_sets_stop_rm,
                        selected= potential_sets_stop_rm[1])
    }) # end observe
    
    
    ############################
    ## Creating column chooser #
    ############################
    # When a datasets chosen to perform removal on, need to load which 
    # columns available in that dataset
    observe({
      req(rv$content_primary$content_prepared)
      
      # creating list of available columns
      # subsets' content_prepared is initialized on save of subsets
      colnames <- switch(input$data_stop_rm, 
                         "Primary data" = 
                           colnames(rv$content_primary$content_prepared), 
                         "Subset one" = colnames(rv$subset_one$content_prepared), 
                         "Subset two" = colnames(rv$subset_two$content_prepared)
      )
      # updating select input list for choosing column to remove stops from
      updateSelectInput(session, "col_name_stop_rm",
                        choices = colnames[-1],
                        selected= colnames[2])
    })
    
    # Rendering whatever dataset selected in datatable display
    observeEvent(input$data_stop_rm, {
      req(rv$content_primary$content_prepared)
      rv$content_prepared_display_2 <- 
        switch(input$data_stop_rm, 
               "Primary data" =  rv$content_primary$content_prepared, 
               "Subset one" = rv$subset_one$content_prepared, 
               "Subset two" = rv$subset_two$content_prepared
        )
    })
    
    ### Stop-word removal triggers modal
    # set the session namespace and return modelDilog() content 
    show_stop_word_modal <- function(){
      
      ns <- session$ns
      modalDialog(
        size = "l",
        h2("Confirm stop-word removal?"),
        p("Clicking submit will remove stop-words from the selected text data."),
        p("Stop-word removal will not preserve any existing tokenisation. Therefore, it is strongly recommended to remove stop-words on non-tokenised text data."),
        fluidRow(
          column(12,
                 imageOutput(ns("stop_word_info"), height = 'auto'),
                 ),
        ),
        hr(class = "hr-blank"),
        fluidRow(
          column(12, 
                 actionButton(ns("submit_stop_words"), label = "Submit stop-words",
                              class = "btn-success"),
          )
        ), # end fluid row
      )
    } # end show_subset_modal() function
    
    observeEvent(input$stop_word_trigger, {
      req(rv$content_primary$content_prepared)
      
      showModal(show_stop_word_modal())
    })
    
    # Rendering stop-word info image
    output$stop_word_info <- renderImage({
      filename <- './www/stop_word_info.png'
      # Return a list containing the filename and alt text
      list(src = filename,
           alt = paste("Stop-word information"),
           width = 850
      )
    }, deleteFile = FALSE)
    
    
    # When submit button clicked...
    # Creating final stop words tibble depending on various input cases
    # If user wants to include defaults & added words, combine defaults and
    # added words
    # If user wants to include only added words, return only added words tibble
    # Otherwise if user only wants defaults, return only these
    observeEvent(input$submit_stop_words, {
      
      removeModal() # remove stop-word info modal
      
      # Creating added stop words tibble. If user has entered something (input
      # isn't null), create tibble and unnest words, then return them.
      # Otherwise return NULL
      added_stop_words <- reactive({
        if (nchar(input$added_words) != 0) { # if there are added words
          added_words <- tibble(input = input$added_words)
          added_words <- added_words %>%
            unnest_tokens(word, input, token = "words") %>%
            filter(!grepl(" ", word, fixed = TRUE))
          # filtering out "words" which are just a blank space
          return(added_words)
        } else {
          return(NULL)
        }
      })
      
      rv$added_stop_words <- added_stop_words()
      
      # Creating default stop words tibble.
      # When user chooses to include default stop-words, default list obtained
      # from tidytext library and saved if not NULL is returned
      # data(stop_words) loaded at top of file on start of app
      # could export stop_words to a csv which is imported if default chosen
      default_stop_words <- reactive({
        stop_words %>% dplyr::select(word)
      })
      rv$default_stop_words <- default_stop_words()
      
      # Creating final stop words tibble collecting together added & defaults
      stop_words_final <- reactive({
        if (input$default_check) {
          if (!is.null(rv$added_stop_words)) { # both added & default words 
            # combining default and added word tibbles
            default_and_added <- rbind(
              rv$added_stop_words,
              rv$default_stop_words
            )
            return(default_and_added)
          } else { # if just default words to be included
            return(rv$default_stop_words)
          }
        } else if(!is.null(rv$added_stop_words)) { # added words but not defaults
          return(rv$added_stop_words)
        } else { # if no defaults or added words to be included
          return(NULL)
        }
      }) # end reactive stop_words_final creation
      
      rv$stop_words_final <- stop_words_final()
      
      # Printing added stop-words(s)
      output$stop_word_display <-
        renderText({
          if (nchar(input$added_words) != 0) { # if user added stop-words
            paste(c("You have added a total of ", nrow(rv$stop_words_final),
                    " stop-word(s)."
            ), sep = ",")
          } else if (input$default_check) { # if user chooses default stop-words
            paste(c("You have added a default list of ",
                    nrow(rv$default_stop_words), " stop-words."), sep = ",")
          }
        })
      
      ######################
      ## Stop word removal #
      ######################
      req(rv$content_primary$content_prepared)
      
      stop_word_alert <- function(){
        shinyalert(
          title = "Stop-word removal failed: data error",
          text = "Stop-words could not be removed due to tokenisation. \n \n Ensure the column selected either tokenised by words or not at all.",
          size = "xs", 
          closeOnEsc = TRUE, closeOnClickOutside = TRUE,
          html = FALSE, type = "info",
          showConfirmButton = TRUE, showCancelButton = FALSE,
          confirmButtonText = "Dismiss",
          confirmButtonCol = "#4169E1",
          timer = 0, imageUrl = "", animation = TRUE
        )
      }
      
      # For selected dataset, create pre-stop-removed dataset in case of undo, 
      # and call on utils.R function remove_stop_words() to return dataset with
      # stop-words removed. 
      if(input$data_stop_rm == "Primary data"){
        
        # Saving pre-stop-removal 
        rv$content_primary$content_pre_stop_rm <- rv$content_primary$content_prepared

        # Use try-catch to run function and throw error if doesn't work
        content_stop_rm <- try(
          remove_stop_words(
          data = rv$content_primary$content_prepared,
          col_name = input$col_name_stop_rm,
          stop_words = rv$stop_words_final, 
          is_tokenised = rv$content_primary$is_tokenised)
        )
        
        if("try-error" %in% class(content_stop_rm)){
          stop_word_alert()
          
          return()
        }
        
        rv$content_primary$content_stop_rm <- content_stop_rm
        rv$content_primary$content_prepared <- content_stop_rm
        rv$content_primary$is_stop_rm <- TRUE
        rv$content_prepared_display_2 <- rv$content_primary$content_stop_rm
        
      } else if(input$data_stop_rm == "Subset one"){
        
        # Saving pre-stop-removal data
        rv$subset_one$content_pre_stop_rm <- rv$subset_one$content_prepared
        
        content_stop_rm <- try(remove_stop_words(
          data = rv$subset_one$data,
          col_name = input$col_name_stop_rm,
          stop_words = rv$stop_words_final, 
          is_tokenised = rv$subset_one$is_tokenised))
        
        if("try-error" %in% class(content_stop_rm)){
          stop_word_alert()
          return()
        }
        
        
        rv$subset_one$content_stop_rm <- content_stop_rm
        rv$subset_one$content_prepared <- content_stop_rm
        rv$subset_one$is_stop_rm <- TRUE
        rv$content_prepared_display_2 <- rv$subset_one$content_stop_rm
        
      } else if(input$data_stop_rm == "Subset two"){
        
        # Saving pre-stop-removal data
        rv$subset_one$content_pre_stop_rm <- rv$subset_one$content_prepared
        
        content_stop_rm <- try(remove_stop_words(
          data = rv$subset_two$data,
          col_name = input$col_name_stop_rm,
          stop_words = rv$stop_words_final, 
          is_tokenised = rv$subset_two$is_tokenised))
        
        if("try-error" %in% class(content_stop_rm)){
          stop_word_alert()
          return()
        }
        
        rv$subset_two$content_stop_rm <- content_stop_rm
        rv$subset_two$content_prepared <- content_stop_rm
        rv$subset_two$is_stop_rm <- TRUE
        rv$content_prepared_display_2 <- rv$subset_two$content_stop_rm
      }
      
    }) # end submit stop-words
    
    # Displaying stop-words added
    output$stop_words_table <- DT::renderDataTable({
      
      validate(need(rv$stop_words_final, "No stop-words submitted."))
      
      DT::datatable(
        rv$stop_words_final,
        options = list(
          paging = TRUE,
          pageLength = 3,
          scrollX = TRUE,
          scrollY = TRUE,
          dom = "frtip"
        ),
        escape = FALSE,
        selection = "none",
        rownames = FALSE # don't show row numbers
      )
      
    }) # end render datatable
    
    
    ########################
    ### Undo stop-words ####
    ########################
    # When undo button clicked, reset reactive values
    # Also indicate that tokenisation is undone since we
    # revert to original content
    observeEvent(input$undo_stop_words, {
      
      req(isTruthy(rv$content_primary$content_pre_stop_rm) ||
            isTruthy(rv$subset_one$content_pre_stop_rm) ||
            isTruthy(rv$subset_two$content_pre_stop_rm))
      
      # require stop-words to actually be submitted
      req(isTruthy(rv$content_primary$is_stop_rm) ||
            isTruthy(rv$subset_one$is_stop_rm) ||
            isTruthy(rv$subset_two$is_stop_rm))

      req(rv$stop_words_final)
      rv$added_stop_words <- NULL
      rv$default_stop_words <- NULL
      rv$stop_words_final <- NULL
      
      rv$is_stop_removed <- FALSE
      
      # Setting content_stop_rm back to pre-stop-removed data in selected dataset
      # Also setting display table to display undone data
      if(input$data_stop_rm == "Primary data"){
        rv$content_primary$content_prepared <- 
          rv$content_primary$content_pre_stop_rm
        rv$content_primary$content_stop_rm <- NULL
        rv$content_primary$is_stop_rm <- FALSE
        rv$content_prepared_display_2 <- rv$content_primary$content_prepared
        
      } else if(input$data_stop_rm == "Subset one"){
        rv$subset_one$content_prepared <- rv$subset_one$content_pre_stop_rm
        rv$subset_one$content_stop_rm <- NULL
        rv$subset_one$is_stop_rm <- FALSE
        rv$content_prepared_display_2 <- rv$subset_one$content_prepared
        
      } else if(input$data_stop_rm == "Subset two"){
        rv$subset_two$content_prepared <- rv$subset_two$content_pre_stop_rm
        rv$subset_two$content_stop_rm <- NULL
        rv$subset_two$is_stop_rm <- FALSE
        rv$content_prepared_display_2 <- rv$subset_one$content_prepared
      }
      
      # using a list of rv's to delete each w for loop if exists
      to_reset <- c(
        "default_check", "added_words", "submit_stop_words"
      )
      for (i in 1:length(to_reset)) {
        if (!is.null(to_reset[i])) {
          shinyjs::reset(to_reset[i])
        }
      }
    })
  })
}