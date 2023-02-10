
# Text frequency reporting tab

library(tidytext)
library(stringr)

data(stop_words)

rv$is_stop_removed <- FALSE

##### STOPWORDS MODULE UI ####
stopwordsUI <- function(id, label = "Choose your stop-word(s):"){
  ns <- NS(id)
  tagList(
    
    h3("Stop-words selection"),
    
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
    em(textOutput(ns("no_stop_words_caption"))),
    DT::dataTableOutput(ns("stop_words_table")),
    hr(), 
    
    fluidRow(
      column(4, {
        downloadButton(ns("download_stop_words_csv"), 
                       label= "Save .csv")
      }), 
      column(4, {
        downloadButton(ns("download_stop_words_tsv"), 
                       label = "Save .tsv")
      }),
      column(4,{
        p()
      })
    )
  )
}

#### STOPWORDS SERVER ####
stopWordsServer <- function(id, stop_word_list = stop_word_list, rv = rv){
  moduleServer(id, function(input, output, session){
    
    
    
    # output$default_caption <- renderText({
    #   "Default stop-words included in stop-word list."
    # })
    
    output$no_stop_words_caption <- renderText({
      validate(
        need(rv$stop_words_final, "No stop-words submitted.")
      )
      return(NULL)
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
        if(nchar(input$added_words) != 0){ # if there are added words
          added_words <- tibble(input = input$added_words) 
          added_words <- added_words %>% 
            unnest_tokens(word, input, token = "words")  %>%
            filter(!grepl(" ", word, fixed = TRUE)) # filtering out "words" which are just a blank space
          
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
          } else if(is.null(rv$stop_words_final) || nrow(rv$stop_words_final) == 0){
            return(NULL)
          }
        })
      
      req(rv$stop_words_final)
      
      # If stop words have been submitted
      if(!is.null(rv$stop_words_final) && nrow(rv$stop_words_final) > 0){
        rv$isStopRemoved <- TRUE
      }
      
      # Displaying stop-words added
      output$stop_words_table <- DT::renderDataTable(
        rv$stop_words_final,
        options = list(paging = TRUE,
                       pageLength = 5,
                       scrollX = TRUE,
                       scrollY = TRUE,
                       dom = 'frtip' # buttons render below DT
        ),
        escape = FALSE,
        selection = 'none',
        rownames = FALSE  # don't show row numbers
      )
      
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
      rv$content_stop_rm <- full_join(content_cols_stop_rm, content_other_cols, by = 'ID')

      
      # To indicate data has not been tokenised yet, for when new data is uploaded
      rv$is_tokenised <- FALSE
      # To indicate stop words have been removed
      rv$is_stop_removed <- TRUE
      
      rv$content_parameterised <- rv$content_stop_rm
      
    }) # end submit stop-words
    
    
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
    
    output$download_stop_words_tsv <- downloadHandler(
      filename = function() {
        paste("Stop_word_list.tsv")
      }, 
      content = function(file){
        write_tsv(as.data.frame(rv$stop_words_final), file)
      }
    )
    

    # When clear button clicked, clear reactive values
    # Also indicate that tokenisation is undone since we 
    # revert to original content
    observeEvent(input$clear_stop_words, {
      
      rv$content_stop_rm <- rv$content
      rv$content_parameterised <- rv$content
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
    
    # For testing
    #verbatimTextOutput(ns("test")),
    
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
      
      rv$is_tokenised <- TRUE
      
    }) # end observeEvent tokenise button
    
    
    # When revert tokens button is clicked...
    observeEvent(input$revert_tokenise, {
      
      # undo tokenisation by setting content_parameterised back to content_stop_rm
      rv$content_parameterised <- rv$content_stop_rm
      rv$is_tokenised <- FALSE
      
      shinyjs::reset(input$tokens)
      
    }) # end revert tokenise button
    
  })
}

#### TEXT FREQUENCY UI #####
textFreqUI <- function(id){
  ns <- NS(id)
  tagList(
    
    fluidPage(

      wellPanel(
        h1("02 | Parameterise"),
        em("Include stop-words, tokenise, and/or filter your text data below."),
      ),
      hr(),
      
      fluidRow(
        column(5,
               
          fluidRow(
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
                           stopwordsUI(ns("stopwords"))
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
            
          ), #end fluid row
            
            box(title = "Checkpoint", 
                status = "success", 
                solidHeader = T, 
                collapsible = T,
                width = 12,
                
                h2("Download your data"),
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
          
        column(7, 
           box(title = " ", 
               status = "primary", 
               solidHeader = T, 
               collapsible = T,
               width = 12, # width is relative to to column
               
               # Display tokenised text data
               h2("Parameterised Text Data"),
               p("Your parameterised text data with stop-words removed and 
              tokensiation complete where specified."),
              hr(),
              
              h4(textOutput(ns("parameterised_subtitle1")) %>%
                   tagAppendAttributes(style = "background-color: crimson;
                                              color: white;")
                 ),
              
              h4(textOutput(ns("parameterised_subtitle2")) %>%
                   tagAppendAttributes(style = "background-color: crimson;
                                              color: white;")
              ) ,
              
              DT::dataTableOutput(ns("content_parameterised")),
           ),
   
        ), # end col 2
        
      ), # end outer fluid row
    
      hr(),
      
      fluidRow(
        wellPanel(
          reportingUI("reporting"),
        )
      ) # end fluid row 
    ) # end fluid page
  )
}


#### TEXT FREQ SERVER ####
textFreqServer <- function(id, rv = rv){
  moduleServer(
    id, 
    function(input, output, session){
      
      stopWordsServer("stopwords", rv = rv)
      tokenizeServer("tokenize", rv = rv)
      reportingServer("reporting", rv = rv)
      
      
      # if no files submitted, print none found. Else print nothing.
      output$parameterised_subtitle1 <- renderText({
        
        # If content isn't present, render text to say so
        # If it is present, show nothing
        validate(
          need(rv$content, "Submit files in the Text Selector Tab to continue")
        )
        return(NULL)
      })
      
      # if no files submitted, print none found. Else print nothing.
      output$parameterised_subtitle2 <- renderText({
        
        # If content has not been parameterised, say so
        # If it has, return nothing
        validate(
          need(rv$content_parameterised, 
               "Select stop-words and/or tokenise your data on the left.")
        )
        return(NULL)
      })

      
      # Creating datatable of content parameterised
      output$content_parameterised <- DT::renderDataTable(
        rv$content_parameterised,
        options = list(
          paging = TRUE,
          pageLength = 10,
          scrollX = TRUE,
          scrollY = TRUE,
          dom = 'frtip',
          # rowCallback = JS(rowCallback),
          columnDefs =
            list(#list(targets = '_all', className = 'dt-left'),
              list(targets = 1,
                   render = JS("function(data, type, row, meta) {",
                               "return type === 'display' && data.length > 100 ?",
                               "'<span title=\"' + data + '\">' + data
                          .substr(0, 100) + '...</span>' : data;","}"
                   )
              )
            )
        ),
        selection = 'single',
        rownames = FALSE
      )
      

      # Rendering a download button with downloadHandler()
      # No point in taking user input to name file since downloader can change
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
      
        
      onStop(function() {
        cat("This will run on session stop")
        outputDir <- "R"

        print(report_rv)
      })
    }
  )
}
