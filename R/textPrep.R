
# Text preparation tab

library(tidytext)
library(stringr)

data(stop_words)

rv$is_stop_removed <- FALSE


##### STOPWORDS MODULE UI ####
stopWordsUI <- function(id, label = "Choose your stop-word(s):"){
  ns <- NS(id)
  tagList(
    
    h3("Stop-word selection"),
    
    # checkbox input for including stop words 
    checkboxInput(ns("default_check"),
                  label = " Use default stop-words list "
    ),
    
    # textbox input for stop words
    textInput(ns("added_words"), 
              label = "Add extra stop-word(s) separated by commas and/or spaces",
              value = NULL, 
              placeholder = "e.g. apple, banana carrot"),
    
    fluidRow(
      column(6, {
        actionButton(ns("submit_stop_words"), 
                     label = "Submit", 
                     class ="btn-success")
      }),
      column(6, {
        actionButton(ns("clear_stop_words"), 
                     label = "Clear", 
                     class ="btn-danger")
      })
    ),
    
    conditionalPanel(
      condition = "input.default_check == 'true'",
      p(textOutput(ns("default_caption")))
    ),
    
    hr(),
    h3("Submitted stop-words"),
    p(textOutput(ns("stop_word_display"))),
    
    # conditionalPanel(
    #   condition = "input.submit_",
    #   p(textOutput(ns("default_caption")))
    # ),
    
    em(textOutput(ns("no_stop_words_caption"))),
    
    conditionalPanel(
      condition = paste0("output['", ns("num_stops"), 
                         "'] == '0' "),
      
      em(textOutput("No stop-words submitted."))
    ),
    
    DT::dataTableOutput(ns("stop_words_table")),
    
    uiOutput(ns("download_stop_words"))
  )
}

#### STOPWORDS SERVER ####
stopWordsServer <- function(id, stop_word_list = stop_word_list, rv = rv){
  moduleServer(id, function(input, output, session){
    
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
    #   
    #   
    # })
    
    output$num_stops <- renderText(length(rv$stop_words_final$word))
    
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
        if(nchar(input$added_words) != 0){ # if there are added words
          added_words <- tibble(input = input$added_words) 
          added_words <- added_words %>% 
            unnest_tokens(word, input, token = "words")  %>%
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
      default_stop_words <- reactive({
        default_words <- stop_words %>%
          dplyr::select(word)
        return(default_words)
      })
      
      rv$default_stop_words <- default_stop_words()
      
      # Creating final stop words tibble - collecting together added & defaults
      stop_words_final <- reactive({
        if(input$default_check){ 
          
          if(!is.null(rv$added_stop_words)){ # both added and default words included
            
            # combining default and added word tibbles
            default_and_added <- rbind(rv$added_stop_words,
                                       rv$default_stop_words) 
            
            return(default_and_added) 
            
          } else { # if just default words to be included
            return(rv$default_stop_words) 
          }
          
        } else {
          if(!is.null(rv$added_stop_words)){ # added words included but not defaults
            return(rv$added_stop_words) 
            
          } else { # if no defaults or added words to be included
            return(NULL) 
          }
        }
      })
      
      rv$stop_words_final <- stop_words_final() 
      
      
      # Printing added stop-words(s)
      output$stop_word_display <- 
        renderText({
          if(nchar(input$added_words) != 0){ # if user added stop-words
            paste(c("You have added a total of ", nrow(rv$stop_words_final),
                    " stop-word(s)."), sep = ",")
          } else if(input$default_check){ # if user chooses default stop-words
            paste(c("You have added a default list of ", 
                    nrow(rv$default_stop_words), 
                    " stop-words."), sep = ",")
          } else if(is.null(rv$stop_words_final) || length(rv$stop_words_final$word) == 0){
            return(NULL)
          }
        })
      
      req(rv$stop_words_final)
      
      # Displaying stop-words added
      output$stop_words_table <- DT::renderDataTable(
        rv$stop_words_final,
        # callback = JS("$('div.dwnld').append($('download_stop_words_tsv'));"),
        options = list(paging = TRUE,
                       pageLength = 5,
                       scrollX = TRUE,
                       scrollY = TRUE,
                       dom = 'frtip', # buttons render below DT
                       buttons = list(
                         "csv",
                         "excel"
                        )
        ),
        escape = FALSE,
        # extensions = 'Buttons',
        selection = 'none',
        rownames = FALSE  # don't show row numbers
      )
      
      # Render UI to download stop words table as csv or tsv
      output$download_stop_words <- 
        renderUI({
          ns <- NS(id)
          tagList(
            fluidRow(
              column(4, {
                downloadButton(ns("download_stop_words_csv"),
                               label = "Download .csv")
              }),
              column(4, {
                downloadButton(ns("download_stop_words_csv"),
                               label = "Download .tsv")
              }),
            ) # end fluid row
          )
        })
      
      # Removing stop words from content by first unnesting 
      # into words, then anti-joining with the stop words final tibble. 
      # Then grouped by ID to stitch words back together.
      # Doing separately to other columns in content so they are not lost in grouping
      req(rv$content)
      
      rv$content_stop_rm <- rv$content # create a copy to modify
      
      # Removing stop-words with an anti-join on Contents column
      # Then grouping by ID and collapsing back into paragraphs ready 
      # for user to choose their own tokenisation method
      # This subset of content is just ID and Contents cols
      content_cols_stop_rm <- rv$content_stop_rm %>%
        dplyr::select(ID, Contents) %>%
        unnest_tokens(word, Contents, token = "words") %>%
        anti_join(rv$stop_words_final, by = "word") %>%
        group_by(ID) %>%
        summarize(Contents = str_c(word, collapse = " ")) %>%
        ungroup()
      
      # Creating subset of rest of cols in submitted content
      # Deselecting Contents to instead append stop-removed Contents next
      content_other_cols <- rv$content_stop_rm %>%
        dplyr::select(-Contents)
      
      # Then joining stop-removed col and rest of cols
      content_stop_rm <- full_join(content_cols_stop_rm, content_other_cols, by = 'ID')
      
      rv$content_stop_rm <- content_stop_rm # saving in rv list
      
      # Setting content_parameterised & prepared to new data w stop removed
      rv$content_parameterised <- rv$content_stop_rm
      rv$content_prepared <- rv$content_stop_rm
      
      print("stop-words removed, rv$content_prepared:")
      print(rv$content_prepared)
      
      # To indicate data has not been tokenised yet, for when new data is uploaded
      rv$is_tokenised <- FALSE
      # To indicate stop words have been removed
      rv$is_stop_removed <- TRUE
      
      
      # Rendering download buttons with downloadHandler()
      output$download_stop_words_csv <- downloadHandler(
        filename = function() {
          paste("Stop_word_list.csv")
        },
        content = function(file){
          write_delim(as.data.frame(rv$stop_words_final), file,
                      delim = ",")
        }
      )
      
      # Download handler to download tsv
      output$download_stop_words_tsv <- downloadHandler(
        filename = function() {
          paste("Stop_word_list.tsv")
        }, 
        content = function(file){
          write_tsv(as.data.frame(rv$stop_words_final), file)
        }
      )
      
    }) # end submit stop-words
    
    
    
    # When clear button clicked, reset reactive values
    # Also indicate that tokenisation is undone since we 
    # revert to original content
    observeEvent(input$clear_stop_words, {
      
      rv$content_stop_rm <- rv$content
      rv$content_parameterised <- rv$content
      rv$content_prepared_diplay <- rv$content
      rv$added_stop_words <- NULL
      rv$default_stop_words <- NULL
      rv$stop_words_final <- NULL
      
      rv$is_tokenised <- FALSE
      rv$is_stop_removed <- FALSE
      
      # using a list of rv's to delete each w for loop if exists
      to_reset <- c("default_check", "stop_word_check", 
                    "added_words", "submit_stop_words")
      for(i in 1:length(to_reset)){
        if(!is.null(to_reset[i])){
          shinyjs::reset(to_reset[i])
        }
      }
    })
    
  }
  )
}


#### TOKENISATION MODULE UI ####
tokenizeUI <- function(id){
  ns <- NS(id)
  tagList(
    
    selectInput(ns("tokens"), label= "Choose a method of tokenization:", 
                choices = c("Words" = "words", 
                            "Bi-grams"= "bigrams", 
                            "n-grams" = "ngrams",
                            # "Sentences" = "sentences", 
                            "Other" = "other"), 
                selected = "Words"),
    
    conditionalPanel(
      condition = "input.tokens == 'ngrams'",
      ns = ns,
      # condition = "1 == 1",
      # uiOutput(ns("ngramsUI")) # was using renderUI in server to make UI for this
      numericInput(ns("n_grams"), label = "Enter n-grams:", value = 2)
    ),
    conditionalPanel(
      condition = "input.tokens == 'other'",
      ns=ns,
      # make mini UI for regex token
      textInput(ns("other_token"), "Specify a custom token:")
    ),
    
    fluidRow(
      column(6, {
        actionButton(ns("submit_tokenise"), 
                     label = "Tokenise", 
                     class ="btn-success")
      }),
      column(6, {
        actionButton(ns("revert_tokenise"), 
                     label = "Revert", 
                     class ="btn-danger")
      })
    ),
  )
}

#### TOKENISATION SERVER ####
tokenizeServer <- function(id, rv = rv){
  moduleServer(id, function(input, output, session){
    
    rv$is_tokenised <- FALSE
    
    # Generating string representing selected tokenisation method
    token <- reactive({
      rv$token <- input$tokens # saving rv$token so can use in other modules too
      
      return(input$tokens)
    })
    
    observe({
      rv$other_token <- input$other_token
    })
    
    n_grams <- reactive({
      rv$n_grams <- input$n_grams # saving rv$n_grams so can use in other modules too
      return(input$n_grams)
    })
    
    # Specifying n for ngrams as 2 if user chooses bigrams as token, 
    # or otherwise a value specified by the user.
    n_for_grams <- reactive({
      if(token() == "ngrams"){
        return(n_grams())
      } else return(2)
    })
    
    
    # Tokenised tibble should only be rendered after "tokenise" button observed
    observeEvent(input$submit_tokenise, {
      
      req(rv$content)
      # Tokenising content tibble. Output column is selected token, input is 
      # Contents columns, tokenising by user-selected token
      content_tokenised <- reactive({ 
        
        validate(need(rv$is_stop_removed, NULL)) 
        
        if(!rv$is_stop_removed){ 
          
          req(rv$content)
          # if content_stop_rm is null then no stop-words have been submitted,
          # so first set content_stop_rm <- content
          rv$content_stop_rm <- rv$content
          print("set content_stop_rm to content in tokenise server")
        }
        
        # if token is easily handled, just unnest w token() 
        if(token() == "words" || token() == "sentences"){
          rv$content_stop_rm %>% 
            unnest_tokens(Token, Contents, token = token()) 
          
        } else if(token() == "bigrams" || token() == "ngrams"){ 
          rv$content_stop_rm %>% 
            unnest_ngrams(Token, Contents, n = n_for_grams())
          
        } else if(token() == "other"){
          rv$content_stop_rm %>%
            unnest_regex(Token, Contents, 
                         pattern = tolower(rv$other_token))
          # unnest on the user inputted regex
        }
        
      })
      
      
      rv$content_parameterised <- content_tokenised() %>%
        relocate(Token, .after = ID)
      
      rv$content_prepared <- rv$content_parameterised
      print("Tokenised, content_prepared display:")
      print(rv$content_prepared)
      rv$is_tokenised <- TRUE
      
    }) # end observeEvent tokenise button
    
    
    # When revert tokens button is clicked...
    observeEvent(input$revert_tokenise, {
      
      # undo tokenisation by setting content_parameterised back to content_stop_rm
      
      if(rv$is_tokenised){
        print("reverting tokenise")
        rv$content_prepared <- rv$content_stop_rm
      }
      
      rv$is_tokenised <- FALSE
      
      shinyjs::reset(input$tokens)
      
    }) # end revert tokenise button
    
  })
}



#### TEXT PREP UI #####
textPrepUI <- function(id){
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
        box(title = "Data editing", 
            status = "primary", 
            solidHeader = T, 
            collapsible = T,
            width = 12,
         
            fluidRow(
            
              column(4, 
                   
                   h3("Mutate data"), 
                   p("Edit your text data by double-clicking on the table and/or add a column using the mutating tools."),
                   
                   textInput(ns("mutate_colname"), 
                            label = "Choose a name for your new column:",
                             value = "new_column"),
                   
                   textInput(ns("mutate_to_insert"), 
                             label = "Input value(s) to populate the new column:",
                             placeholder = "e.g. 1, apple, TRUE"),
                   
                   fluidRow(
                     column(width = 6, 
                            actionButton(ns("submit_mutate"), 
                                         label = "Mutate", 
                                         class = "btn-primary")
                     ),
                     column(width = 6, 
                            actionButton(ns("undo_mutate"), 
                                         label = "Undo", 
                                         class = "btn-danger")
                     )
                   ),
                   
                   hr(),
                   
                   h3("Filter data"),
                   p("Use the interactive datatable to filter attributes of your text data. Save your filtered data to use in further analyses."),
                   
                   fluidRow(
                     column(width = 6, 
                            actionButton(ns("submit_filters"), 
                                         label = "Filter", 
                                         class = "btn-primary")
                     ),
                     column(width = 6, 
                            actionButton(ns("undo_filters"), 
                                         label = "Undo filters", 
                                         class = "btn-danger")
                     )
                   ),
                   ), # end col 1
            
            column(8, 
                   # Display DT to filter data
                   h4(textOutput(ns("parameterised_subtitle1")) %>%
                        tagAppendAttributes(style = "background-color: crimson;
                            color: white;")
                   ),
                   
                   DT::dataTableOutput(ns("content_prepared_DT")),
                   
                   ), # end col 2
            ), # end fluid row
            
            # Subsetting options
            # h4("Subset data"), 
            # p("Save the filtered subset of data for comparison."),
            # 
            # fluidRow(
            #   column(width = 6, 
            #          actionButton(ns("save_subset_one"), 
            #                       label = "Save as subset I", 
            #                       class = "btn-success"),
            #   ), 
            #   column(width = 6, 
            #          actionButton(ns("save_subset_two"), 
            #                       label = "Save as subset II", 
            #                       class = "btn-success"),
            #   ),
            # ), # end fluid row
            

          

          ), # end box
               
        
      ), # end outer fluid row
      
      
      fluidRow(
        column(5,

           tabBox( side = "left", 
                   selected = "Stop-words", 
                   width = 12, 
                   
                   
                   tabPanel(title = "Stop-words",
                            
                            checkboxInput("stop_word_check",
                                          label = HTML("<p style='color:black;'> 
                                            Include stop-word(s) </p>")), 
                            
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
                   
                   tabPanel(title = "Tokenisation",
                            
                        wellPanel(
                          
                          tokenizeUI(ns("tokenize"))
                          
                        ) # end well panel
                   ), # end tab panel
                   
                 ), # end tab box

               box(title = "Checkpoint", 
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
                                      label = "Save .csv")
                     }),
                     column(4, {
                       downloadButton(ns("download_parameterised_tsv"), 
                                      label = "Save .tsv")
                     }),
                     column(4,{
                       p()
                     })
                   ),
               ), # end box
               
        ), # end col 1
        
      ), # end outer fluid row
      
      fluidRow(
        
       box(title = "Search/Concordance", 
           status = "primary", 
           solidHeader = T, 
           collapsible = T,
           width = 12,
           
           h3("Search/Concordance"), 
           
           fluidRow(
             column(width = 6, 
                      wellPanel(
                        h4("Stemming"),
                        p("Specify stems to search for within text data."),
                        
                        hr(), 
                        textInput(ns("stem_input"), 
                                  label = "Enter stem", 
                                  placeholder = "e.g. run"),
                        
                        hr(),
                        
                        selectInput(ns("stem_group_by_input"), 
                                    label = "(Optional) Choose a variable to group frequencies by)", 
                                    choices = 
                                      list("Choices" = 
                                             "N/A")
                        ),
                        
                        actionButton(ns("submit_stem"), 
                                     label = "Submit", 
                                     class = "btn-success"),
                        
                        hr(),
                      ),
                    
                    ),
             
             column(width = 6,  
                    wellPanel(
                      DT::dataTableOutput(ns("test"))
                    ),
                    
                    ) # end column
             
           ) # end fluid row
           
       ), # end box
              
      ), # end fluid row
      
    ) # end fluid page
  )
}


#### TEXT PREP SERVER ####
textPrepServer <- function(id, rv = rv){
  moduleServer(
    id, 
    function(input, output, session){
      
      stopWordsServer("stopwords", rv = rv)
      tokenizeServer("tokenize", rv = rv)
      
      # if no files submitted, print none found. Else print nothing.
      output$parameterised_subtitle1 <- renderText({
        validate(
          need(rv$content, "Submit files in the Text Selector Tab to continue")
        )
        return(NULL)
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
      
      
      rv$is_content_filtered <- FALSE
      rv$is_content_mutated <- FALSE
      
      # # Creating handsontable to be editable for editing section
      # output$content_edited <- renderRHandsontable({
      #   rhandsontable(rv$content_prepared,
      #                 height = 500, 
      #                 stretchH = "all", 
      #                 useTypes = FALSE) %>%
      #     hot_cols(fixedColumnsLeft = 1, 
      #              columnSorting = TRUE) %>%
      #     hot_context_menu(
      #       customOpts = list(
      #         search = list(name = "Search",
      #                       callback = htmlwidgets::JS(
      #                         "function (key, options) {
      #                    var srch = prompt('Search criteria');
      # 
      #                    this.search.query(srch);
      #                    this.render();
      #                  }"))))
      #   })
      
      
      ###########################
      ###### Mutating data ###### 
      ###########################
      
      observeEvent(input$submit_mutate, {
        
        req(rv$content_edited) # is either content if no editing yet, or
        # is the result of mutating and/or filtering
        req(rv$content_prepared)
        
        # Saving temp in case user wants to undo changes
        rv$pre_mutated_temp <- rv$content_prepared
        
        # Saving what is in data table, so 
        # user can mutate and add column values to just filtered
        # data rows if they wish as opposed to every row
        in_datatable <- rv$content_prepared[input[["content_prepared_DT_rows_all"]], ]
          # rv$content_edited[input[["content_prepared_DT_rows_all"]], ]
        
        # Mutating content: if the content_edited ID column is in the 
        # data table selected, add the value to insert, otherwise null
        rv$content_mutated <- rv$content_prepared %>% # rv$content_edited %>% 
          mutate(user_added_col = 
                   if_else(.$ID %in% in_datatable$ID, input$mutate_to_insert, NULL))
        
        # Renaming column to user inputted name
        colnames(rv$content_mutated)[which(names(rv$content_mutated) == "user_added_column")] <- input$mutate_colname
        
        rv$is_content_mutated <- TRUE
        
        # Setting content_edited and content_prepared to the new mutated content
        rv$content_edited <- rv$content_mutated
        rv$content_prepared <- rv$content_mutated
        
        print("Content prepared after mutating in text prep tab:")
        print(rv$content_prepared)
      })
      
      
      ###########################
      ###### Filtering data #####
      ###########################
      
      # When submit filters clicked, subset content_stats by filtered rows
      observeEvent(input$submit_filters, {
        
        req(rv$content_prepared)
        
        # Creating temporary storage hold 
        rv$pre_filtered_temp <- rv$content_prepared

        # Subsetting content_stats by filters
        rv$content_prepared <- 
          rv$pre_filtered_temp[input[["content_prepared_DT_rows_all"]], ]
        
        rv$is_content_filtered <- TRUE
        
      })
      
      # When undo filters button clicked, revert content_stats to original
      observeEvent(input$undo_filters, {
        
        req(rv$is_content_filtered)
        req(rv$pre_filtered_temp) # require original reactive value
        
        rv$content_parameterised <- rv$pre_filtered_temp # revert to original 
        rv$content_prepared <- rv$content_parameterised
        
        rv$is_content_filtered <- FALSE
      })
      
      # Rendering table of data to display
      # if no stop-words/tokenising performed, show content
      # in observe event submit stop-words: if stop-words submitted, render content_stop_rm
      # in observe event submit tokenise: if tokenised, render content_tokenised
      observe({
        
        req(rv$content)
        # if neither stop-words or tokenisation performed yet,
        # render rv$content
        # if(!(rv$is_stop_removed) && !(rv$is_tokenised)){
        #   print("content_prepared set to content in text prep server")
        #   rv$content_prepared <- rv$content
        # }
        
        # if content not filtered or mutated, set content_prepared <- content
        if(!(rv$is_content_filtered) && !(rv$is_content_mutated)){
          print("content_edited & content_prepared set to content in text prep server")
          rv$content_edited <- rv$content
          rv$content_prepared <- rv$content
        }
        
      })
      
      # Creating datatable of content parameterised
      output$content_prepared_DT <- DT::renderDataTable(
        rv$content_prepared,
        filter = 'top',
        # server = TRUE,
        options = list(
          paging = TRUE,
          pageLength = 7,
          scrollX = TRUE,
          scrollY = TRUE,
          dom = 'frtip',
          columnDefs =
            list(
              list(targets = 1,
                   render = JS("function(data, type, row, meta) {",
                               "return type === 'display' && data.length > 100 ?",
                               "'<span title=\"' + data + '\">' + data
                        .substr(0, 100) + '...</span>' : data;","}"
                   )
              )
            )
        ),
        selection = 'none',
        rownames = FALSE,
        editable = 'column'
      )
      
      
      # Rendering a download button with downloadHandler()
      output$download_parameterised_csv <- downloadHandler(
        filename = function() {
          paste("Parameterised_Content.csv")
        }, 
        content = function(file){
          write_delim(as.data.frame(rv$content_parameterised), file, 
                      delim = ",")
        }
      )
      
      output$download_parameterised_tsv <- downloadHandler(
        filename = function() {
          paste("Parameterised_Content.tsv")
        }, 
        content = function(file){
          write_tsv(as.data.frame(rv$content_parameterised), file)
        }
      )
      
      
      # Stemming 
      observeEvent(input$submit_stem, {
        
        content_stemmed <- reactive({
          
          req(rv$content)
          req(rv$is_tokenised)
          
          stem <- input$stem_input
          
          stemmed <- rv$content_prepared %>%
            mutate(stem = hunspell_stem(stem)) # %>% 
            # unnest(token) %>%
            # group_by(ID) %>%
            # count(stem)
          
          stemmed
          
        })
        
        output$test <- DT::renderDataTable(
          content_stemmed(),
          options = list(
            paging = TRUE,
            pageLength = 5,
            scrollX = TRUE,
            scrollY = TRUE,
            dom = 'frtip',
            columnDefs =
              list(
                list(targets = 1,
                     render = JS("function(data, type, row, meta) {",
                                 "return type === 'display' && data.length > 100 ?",
                                 "'<span title=\"' + data + '\">' + data
                        .substr(0, 100) + '...</span>' : data;","}"
                     )
                )
              )
          ),
          selection = 'none',
          rownames = FALSE
        )
        
      })
      
      # To update select group by variable
      stem_choices <- reactive({
        
        if(!is.null(rv$content_prepared)){
          return(colnames(rv$content_prepared))
        }
        
        return("N/A")
      })
      
      observe({
        
        rv$stem_choices <- stem_choices()
        
        updateSelectInput(session, "stem_group_by_input",
                          choices = rv$stem_choices,
                          selected= rv$stem_choices[1])
      })
      
      
      
      # output$test <- renderPrint(rv$content_prepared)
      
      

    }
  )
} # end text preparation server logic

