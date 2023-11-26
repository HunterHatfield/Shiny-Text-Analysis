
##################################
#### Stop word removal module ####
##################################

# Handles stop word import, gathering and removal.
# Requires initial dataset to remove stop words on, rv$content_primary$content_prepared,
# provided in a 2D reactive value list, rv. 
# Depends on 1 function, remove_stop_words(), in utils.R

library(tidytext)

data(stop_words)
rv$is_stop_removed <- FALSE

##### STOPWORDS MODULE UI ####
stopWordsUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Stop-word selection"),
    
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
        actionButton(ns("submit_stop_words"),
                     label = "Submit",
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
    p(textOutput(ns("stop_word_display"))),
    
    em(textOutput(ns("no_stop_words_caption"))),
    
    ##### num_stops is what?
    conditionalPanel(
      condition = paste0(
        "output['", ns("num_stops"),
        "'] == '0' "
      ),
      em(textOutput("No stop-words submitted."))
    ),
    
    DT::dataTableOutput(ns("stop_words_table")),
    
    uiOutput(ns("download_stop_words"))
  )
}

#### STOPWORDS SERVER ####
stopWordsServer <- function(id, stop_word_list = stop_word_list, rv = rv) {
  moduleServer(id, function(input, output, session) {
    # Rendering that no stop words are submitted if nothing submitted in table
    # output$no_stop_words_caption <- renderText({
    #
    #   req(rv$content)
    #
    #   print("stop words:")
    #   print(length(rv$stop_words_final$word))
    #
    #   if(rv$is_stop_removed){
    #     NULL
    #   } else {
    #     "No stop-words submitted."
    #   }
    # })
    
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
    
    
    # When submit button clicked...
    # Creating final stop words tibble depending on various input cases
    # If user wants to include defaults & added words, combine defaults and
    # added words
    # If user wants to include only added words, return only added words tibble
    # Otherwise if user only wants defaults, return only these
    observeEvent(input$submit_stop_words, {
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
          } else if (is.null(rv$stop_words_final) || length(rv$stop_words_final$word) == 0) {
            return(NULL)
          }
        })
      
      req(rv$stop_words_final)
      
      # Displaying stop-words added
      output$stop_words_table <- DT::renderDataTable(
        rv$stop_words_final,
        # callback = JS("$('div.dwnld').append($('download_stop_words_tsv'));"),
        options = list(
          paging = TRUE,
          pageLength = 5,
          scrollX = TRUE,
          scrollY = TRUE,
          dom = "frtipB", # buttons render below DT
          buttons = list("csv", "excel")
        ),
        escape = FALSE,
        extensions = "Buttons",
        selection = "none",
        rownames = FALSE # don't show row numbers
      )
      
      # Render UI to download stop words table as csv or tsv
      output$download_stop_words <-
        renderUI({
          ns <- NS(id)
          tagList(
            fluidRow(
              column(4, {
                downloadButton(ns("download_stop_words_csv"),
                               label = "Download .csv"
                )
              }),
              column(4, {
                downloadButton(ns("download_stop_words_csv"),
                               label = "Download .tsv"
                )
              }),
            ) # end fluid row
          )
        })
      
      
      ######################
      ## Stop word removal #
      ######################
      req(rv$content_primary)
      
      # For selected dataset, create pre-stop-removed dataset in case of undo, 
      # and call on utils.R function remove_stop_words() to return dataset with
      # stop-words removed. 
      if(input$data_stop_rm == "Primary data"){
        
        # Saving pre-stop-removal 
        rv$content_primary$content_pre_stop_rm <- rv$content_primary$content_prepared
        
        print("Starting stop-word removal...")
        # Use try-catch to run function and throw error if doesn't work
        # Hardcoding column for now
        content_stop_rm <- remove_stop_words(
          data = rv$content_primary$content_prepared,
          col_name = input$col_name_stop_rm,
          stop_words = rv$stop_words_final, 
          is_tokenised = rv$content_primary$is_tokenised)
        
        print("Used remove_stop_words function to remove stop words:")
        print(content_stop_rm)
        
        rv$content_primary$content_stop_rm <- content_stop_rm
        rv$content_primary$content_prepared <- content_stop_rm
        rv$content_primary$is_stop_rm <- TRUE
        rv$content_prepared_display_2 <- rv$content_primary$content_stop_rm
        
      } else if(input$data_stop_rm == "Subset one"){
        
        # Saving pre-stop-removal data
        rv$subset_one$content_pre_stop_rm <- rv$subset_one$content_prepared
        
        content_stop_rm <- remove_stop_words(
          data = rv$subset_one$data,
          col_name = input$col_name_stop_rm,
          stop_words = rv$stop_words_final, 
          is_tokenised = rv$subset_one$is_tokenised)
        
        print("Used remove_stop_words function to remove stop words:")
        print(content_stop_rm)
        
        rv$subset_one$content_stop_rm <- content_stop_rm
        rv$subset_one$content_prepared <- content_stop_rm
        rv$subset_one$is_stop_rm <- TRUE
        rv$content_prepared_display_2 <- rv$subset_one$content_stop_rm
        
      } else if(input$data_stop_rm == "Subset two"){
        
        # Saving pre-stop-removal data
        rv$subset_one$content_pre_stop_rm <- rv$subset_one$content_prepared
        
        content_stop_rm <- remove_stop_words(
          data = rv$subset_two$data,
          col_name = input$col_name_stop_rm,
          stop_words = rv$stop_words_final, 
          is_tokenised = rv$subset_two$is_tokenised)
        
        rv$subset_two$content_stop_rm <- content_stop_rm
        rv$subset_two$content_prepared <- content_stop_rm
        rv$subset_two$is_stop_rm <- TRUE
        rv$content_prepared_display_2 <- rv$subset_two$content_stop_rm
      }
      
      # # Setting content_parameterised & prepared to new data w stop removed
      # rv$content_parameterised <- rv$content_stop_rm
      # rv$content_prepared <- rv$content_stop_rm
      # rv$is_tokenised <- FALSE
      # rv$is_stop_removed <- TRUE ##### Check
      
      
      #########################
      # Downloading stop-words
      #########################
      # Rendering download buttons with downloadHandler()
      output$download_stop_words_csv <- downloadHandler(
        filename = function() {
          paste("Stop_word_list.csv")
        },
        content = function(file) {
          write_delim(as.data.frame(rv$stop_words_final), file,
                      delim = ",")
        }
      )
      
      # Download handler to download tsv
      output$download_stop_words_tsv <- downloadHandler(
        filename = function() {
          paste("Stop_word_list.tsv")
        },
        content = function(file) {
          write_tsv(as.data.frame(rv$stop_words_final), file)
        }
      )
    }) # end submit stop-words
    
    
    ########################
    ### Undo stop-words ####
    ########################
    # When undo button clicked, reset reactive values
    # Also indicate that tokenisation is undone since we
    # revert to original content
    observeEvent(input$undo_stop_words, {
      
      rv$content_stop_rm <- rv$content
      rv$content_parameterised <- rv$content
      rv$content_prepared_diplay <- rv$content
      rv$added_stop_words <- NULL
      rv$default_stop_words <- NULL
      rv$stop_words_final <- NULL
      
      rv$is_tokenised <- FALSE
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
        "default_check", "stop_word_check",
        "added_words", "submit_stop_words"
      )
      for (i in 1:length(to_reset)) {
        if (!is.null(to_reset[i])) {
          shinyjs::reset(to_reset[i])
        }
      }
    })
  })
}