
# Module for parameter selection

# loadData <- function() {
#   contents <- read_csv("content.csv")
#   # contents <- read.csv("bigrams_overall.csv")
#   assign('content', contents , envir = .GlobalEnv)
#   data <- content
#   data
# }

library(tidytext)
library(stringr)

data(stop_words)

##### STOPWORDS MODULE UI ####
stopwordsUI <- function(id, label = "Choose your stop-word(s):"){
  ns <- NS(id)
  tagList(
    
    # checkbox input for including stop words 
    checkboxInput(ns("default_check"),
                  label = HTML("<p style='color:black;'> Use default stop-word(s) </p>")
    ),
    
    # textbox input for stop words
    textInput(ns("added_words"), label = "Enter stop-word(s) separated by commas and/or spaces (e.g. apple, banana, carrot...).", value = NULL),
    
    fluidRow(
      column(3, {
        actionButton(ns("submit_stop_words"), 
                     label = "Submit stop-word(s)", 
                     class ="btn-submit")
      }),
      column(5, offset = 1, {
        actionButton(ns("clear_stop_words"), 
                     label = "Clear stop-word(s)", 
                     class ="btn-danger")
      })
    ),
    
    conditionalPanel(
      condition = "input.default_check == 'true'",
      p(textOutput(ns("default_caption")))
    ),
    
    h3("Stop-words entered"),

    p(textOutput(ns("stop_word_display"))),

    DT::dataTableOutput(ns("stop_words_table")),
    
    hr(),
    h2("Updated data:"),
    em("Download a copy of your updated data by selecting a format below:"),
    p(),
    DT::dataTableOutput(ns("content_stop_rm_display"))
  )
}

#### STOPWORDS SERVER ####
stopWordsServer <- function(id, stop_word_list = stop_word_list, rv = rv){
  moduleServer(id, function(input, output, session){
    
    output$default_caption <- renderText({
      "Default stop-words included in stop-word list."
    })
    
    # When submit button clicked...
    # Creating final stop words tibble depending on various input cases
    # If user wants to include defaults & added words, combine defaults and added words
    # If user wants to include only added words, return only added words tibble
    # Otherwise if user only wants defaults, return only these
    observeEvent(input$submit_stop_words, {
      
      # Creating added stop words tibble. If user has entered something (input isn't null), 
      # create tibble and unnest words, then return them. Otherwise return NULL.
      
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
      
      # Creating default stop words tibble. 
      # When user chooses to include default stop-words, default list obtained from tidytext library and 
      # saved if not NULL is returned
      default_stop_words <- reactive({
        default_words <- stop_words %>%
          select(word)
        return(default_words)
      })
     
      # Creating final stop words tibble - collected relevant words and globally assigned.
      stop_words_final <<- reactive({
        if(input$default_check){ 
          
          if(!is.null(added_stop_words())){ # if both added and default words to be included
            
            # adding additional stop words to stop_words tibble
            # default_and_added <- full_join(default_stop_words(),
            #                              added_stop_words(), by = word)
            
            # combining default and added word tibbles
            default_and_added <- rbind(added_stop_words(), 
                                       default_stop_words()) 
            
            return(default_and_added) 
            
          } else { # if just default words to be included
            return(default_stop_words()) 
          }
          
        } else {
          if(!is.null(added_stop_words())){ # if just added words to be included but not defaults
            return(added_stop_words()) 
            
          } else { # if no defaults or added words to be included
            return(NULL) 
          }
        }
      })
      
      # Printing added stop-words(s)
     output$stop_word_display <- renderText({
       
       # paste(c("You have added ", nrow(added_stop_words()), 
       #         " stop-word(s) to a default list of ", 
       #         nrow(default_stop_words()), 
       #              " stop-words"), sep = ",")
       
       if(nchar(input$added_words) != 0){ # if user added stop-words
         paste(c("You have added ", nrow(added_stop_words()),
                 " stop-word(s)."), sep = ",")
       } else if(input$default_check){ # if user chooses default stop-words
         paste(c("You have added a default list of ", 
                 nrow(default_stop_words()), 
                 " stop-words."), sep = ",")
       }
       
     })
     
     # plain table
     headerCallbackRemoveHeaderFooter <- c(
       "function(thead, data, start, end, display){",
       "  $('th', thead).css('display', 'none');",
       "}"
     )
     
     # Displaying stop-words used
     output$stop_words_table <- DT::renderDataTable(
       stop_words_final(),
       options = list(paging = TRUE, 
                      # pageLength = 5, 
                      scrollX = TRUE, 
                      scrollY = TRUE, 
                      columnDefs = list(list(targets = '_all',
                                             render = JS(
                                               "function(data, type, row, meta) {",
                                               "return type === 'display' && data.length > 50 ?",
                                               "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
                                               "}"))),
                      dom = 'Bfrtip',
                      # allow user to download
                      buttons = c('csv', 'excel')
                      # assign specific options to columns
                      # could set all columns (targets = '_all') to be 
                      # centre-aligned (className = 'dt-center')
                      ),
       escape = FALSE,
       # in combo with buttons option, allows user to download
       extensions = "Buttons",
       selection = 'single', 
       # width= 500,
       rownames = FALSE  # don't show row numbers
     )
     
     # Removing stop words from content by first unnesting into words, then anti-joining with the stop words final tibble. 
     
     
     # Then grouped by ID to stitch words back together.
     content_stop_rm <<- reactive({
       submitted_content() %>%
         unnest_tokens(word, Contents, token = "words") %>%
         anti_join(stop_words_final(), by = "word") %>%
         group_by(ID) %>%
         summarize(Contents = str_c(word, collapse = " ")) %>%
         ungroup()
     })
     
     # Rendering data table to display updated data
     output$content_stop_rm_display <- DT::renderDataTable(
       content_stop_rm(), 
       options = list(paging = TRUE, 
                      pageLength = 5, 
                      scrollX = TRUE, 
                      scrollY = TRUE, 
                      autoWidth = TRUE,
                      # use client-side processing
                      # server = FALSE, 
                      dom = 'Bfrtip',
                      # allow user to download
                      buttons = c('csv', 'excel'),
                      # assign specific options to columns,
                      # here making ... if long text cell
                      # could set all columns (targets = '_all') to be 
                      # centre-aligned (className = 'dt-center')
                      columnDefs = list(list(targets = '_all',
                                             render = JS(
                                               "function(data, type, row, meta) {",
                                               "return type === 'display' && data.length > 200 ?",
                                               "'<span title=\"' + data + '\">' + data.substr(0, 200) + '...</span>' : data;",
                                               "}")))),
       # used for security
       # The default, escape = TRUE, escapes the HTML elements and 
       # displays the underlying HTML code - this is because unexpected 
       # HTML entities can potentially be a security issue.
       escape = FALSE,
       # in combo with buttons option, allows user to download
       extensions = "Buttons",
       # select a single column, accessed through 
       # input$tableID_rows_selected in the server
       selection = 'single', 
       # filter rows
       # filter = 'bottom', 
       rownames = FALSE  # don't show row numbers
     )
     })
    
    
    # When clear button clicked
    observeEvent(input$clear_stop_words, {
      # replace stop word table list with null
      # uncheck include stop words
      # bring back original table
      
    })
    }
  )
}

#### PARAMETER UI #####
parameterUI <- function(id, label = "Choose parameter(s):"){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        h3("Parameter Selection"), 
        em("Select parameters to include in your analysis."),
        checkboxInput("stop_word_check",
           label = HTML("<p style='color:black;'> Include stop-word(s) </p>")
        ),
        hr(),
        
        wellPanel(
          # Displaying original raw data from upload method
          p("(used to contain original data, shifting to first page instead)")
          # h2("Original raw data:"),
          # DT::dataTableOutput(ns("original_content_display")),
          # hr()
        )
      ),
      
      mainPanel(
        wellPanel(
          h2("Stop-word Selection"),
          
          conditionalPanel(
            condition = "input.stop_word_check == true",
              stopwordsUI(ns("stopwords"))
          ),
          conditionalPanel(
            condition = "input.stop_word_check == false", 
            em("No stop-word(s) included.")
          ),
          
          # Not having this in ns() breaks conditional panel
          # textOutput(ns("stop_word_display"))
          # tableOutput(ns("content")),
        ),
        wellPanel(
          h2("Text Tokenization"),
          selectInput(ns("tokens"), label= "Choose a method of tokenization:", 
                      choices = c("Words" = "words", 
                                  "Bi-grams"= "bigrams", 
                                  "n-grams" = "ngrams",
                                  "Sentences" = "sentences", 
                                  "Paragraphs" = "paragraphs",
                                  "Other" = "other"), 
                      selected = "Words"),
          conditionalPanel(
            condition = "input.tokens == 'words'",
            ns=ns,
            p("Token selected: Words")
          ),
          conditionalPanel(
            condition = "input.tokens == 'bigrams'",
            ns=ns,
            p("Token selected: Bi-grams")
          ),
          conditionalPanel(
            condition = "input.tokens == 'ngrams'",
            ns = ns,
            # condition = "1 == 1",
            # uiOutput(ns("ngramsUI")) # was using renderUI in server to make UI for this
            numericInput(ns("n_grams"), label = "Enter n-grams:", value = 2)
          ),
          conditionalPanel(
            condition = "input.tokens == 'sentences'",
            ns=ns,
            p("Token selected: Sentences")
          ),
          conditionalPanel(
            condition = "input.tokens == 'paragraphs'",
            ns=ns,
            p("Token selected: Paragraphs. Assumes unnesting on \"PPPP\".")
          ),
          conditionalPanel(
            condition = "input.tokens == 'other'",
            ns=ns,
            # make mini UI for regex token
            textInput(ns("other_token"), "Specify a custom token:")
          ),
          
          actionButton(ns("submit_tokenise"), 
                       label = "Tokenise", 
                       class ="btn-success")
          
        ),
        wellPanel(

          # Test tokens
          verbatimTextOutput(ns("test_token")), 
          verbatimTextOutput(ns("test_n")), 
          
          # Display tokenised text data
          h2("Updated data"),
          em("Download a copy of your tokenised data by selecting a format below:"),
          p(),
          DT::dataTableOutput(ns("content_tokenised")),
          
          
          # DT::dataTableOutput(ns("content_stop_rm_display"))
        
        ),
        
        # Saving data
        # Button added to save data to be passed onto the reporting tab
        # Button added to save a personal copy of the updated data
        wellPanel(
          h2("Save your parameterised data"),
          p("Enter a name to save your final paramaterised data and/or download a personal copy."),
        
          # text input to name data
          textInput(ns("content_updated_name"), label = "Input file name excluding spaces or file extensions:", value = "e.g. text_data, myData"),
          
          fluidRow(
            column(3, {
              actionButton(ns("save_content_parameterised"), 
                           label = "Save data", 
                           class ="btn-success")
            }),
            column(5, offset = 0, {
              actionButton(ns("download_content_parameterised"), 
                           label = "Download a copy", 
                           class ="btn-submit")
            })
          )
        )
      )
    )
  )
}


#### PARAMETER SERVER ####
parameterServer <- function(id, rv){
  moduleServer(
    id, 
    function(input, output, session){
      
      # content_from_server <- uploadServer("get_content")
      
      output$content_stop_rm <- DT::renderDataTable(content_stop_rm())
      
      stopWordsServer("stopwords", rv)


      # Tokenisation stuff -----------------------------------------------
      
      # Generating string representing selected tokenisation method
      token <- reactive({
        input$tokens
      })
      
      n_grams <- reactive({
        return(input$n_grams)
      })
      
      # Specifying n for ngrams as 2 if user chooses bigrams as token, or otherwise
      # a value specified by the user.
      n_for_grams <- reactive({
        if(token() == "ngrams"){
          return(n_grams())
          #input$n_grams
        } else return(2)
      })
      
      # Test stuff to just print tokens, helpful for future addition of token options.
      # output$test_token <- reactive({
      #   token()
      # })
      # 
      # output$test_n <- reactive({
      #   n_for_grams()
      # })
      # 
      
      # Tokenised tibble should only be rendered after "tokenise" button observed
      observeEvent(input$submit_tokenise, {
        
        # Tokenising content tibble. Output column is selected token, input is 
        # Contents columns, tokenising by user-selected token
        content_tokenised <- reactive({ 
          
          # if token is easily handled, just unnest w token() 
          if(token() == "words" || token() == "sentences"){
            # tokenising content_stop_rm() or could do content()
            content_stop_rm() %>%
              unnest_tokens(Token, Contents, token = token()) 
            
          } else if(token() == "bigrams" || token() == "ngrams"){ 
            content_stop_rm() %>%
              unnest_ngrams(Token, Contents, n = n_for_grams())
              
              # works with this: unnest_ngrams(Token, Contents, n = 3)
            
          } else if(token() == "other"){
            content_stop_rm()
            # unnest on the user inputted regex
            
          } else if(token() == "paragraphs"){
            content_stop_rm() %>%
              unnest_regex(Token, Contents, pattern = "pppp")
            # unnest on the PPPP
          }
        })
        
        # Datatable display rendering for tokenised data
        output$content_tokenised <- DT::renderDataTable(
          content_tokenised(), 
          options = list(paging = TRUE, 
                         # pageLength = 5, 
                         # scrollX = TRUE, 
                         # scrollY = TRUE, 
                         # autoWidth = TRUE,
                         
                         dom = 'Bfrtip',
                         # allow user to download
                         buttons = c('csv', 'excel')
          ),
          escape = FALSE,
          # in combo with buttons option, allows user to download
          extensions = "Buttons",
          selection = 'single', 
          # filter rows
          # filter = 'bottom', 
          rownames = FALSE  # don't show row numbers
        )
        
        
      }) # end observeEvent tokenise button
        
      
      # output$updated_content <- DT::renderDataTable(content())
      # Using datatable from DT package for file output table
      output$original_content <- DT::renderDataTable(
        submitted_content(), 
        options = list(paging = TRUE, 
                       pageLength = 5, 
                       scrollX = TRUE, 
                       scrollY = TRUE, 
                       # autoWidth = TRUE,
                       # use client-side processing
                       # server = FALSE, 
                       columnDefs = list(list(targets = '_all',
                                              render = JS(
                                                "function(data, type, row, meta) {",
                                                "return type === 'display' && data.length > 50 ?",
                                                "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
                                                "}")))),
        # used for security
        # The default, escape = TRUE, escapes the HTML elements and 
        # displays the underlying HTML code - this is because unexpected 
        # HTML entities can potentially be a security issue.
        escape = FALSE,
        # select a single column, accessed through 
        # input$tableID_rows_selected in the server
        selection = 'single', 
        rownames = FALSE  # don't show row numbers
      )

      # Saving data on click
      
      observeEvent(input$save_content_parameterised, {
        
        inputted_file_name <- reactive({input$content_updated_name})
        
        # saving the file name as what was entered by user
        final_file_name <- reactive({
          
          return(paste(inputted_file_name(), "csv", sep = "."))
          
        })
        
        outputDir <- "R"
        write.csv(
          x = content_tokenised(),
          file = file.path(outputDir, "test_content_updated.csv"), 
          row.names = FALSE, quote = FALSE
        )
        
      })

    }
  )
}
