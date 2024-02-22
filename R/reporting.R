
############################################################
#  Reporting module for vis
############################################################
waiting_screen_reporting <- tagList(
  spin_flower(),
  h4("Rendering visualisations...")
)

reportingUI <- function(id){
  ns <- NS(id)
  tagList(
    useShinyjs(),
    useWaiter(),
      
      #### Header panel ####
      wellPanel(
        h1("03 | Visualisation"),
        p("Visualize your text data with ease. Generate plots to summarise token frequencies of your corpus, compare document token frequencies, explore Zipf's Law, and gain insights into TF-IDF scores. Empower your analysis with clear, informative visualizations tailored to your text data exploration needs.")
      ),
    
      hr(class = "hr-blank"),
      
      ### Data selector ####
      fluidRow(
        box(title = NULL,
            status = "primary",
            solidHeader = T, collapsible = T,
            width = 3,
            # Data selection
            h3("Dataset selector"),
            selectInput(ns("data_to_visualise"),
                        label = "Select a dataset to visualise:",
                        choices = list("N/A" = "na")),
            selectInput(ns("col_name_to_visualise"),
                        label = "Choose which column of tokenised text to use in visualisations:", 
                        choices = list("N/A" = "na")),
            actionButton(ns("confirm_data_to_visualise"),
                         label = "Confirm",
                         class = "btn-success"),
        ),
        
        box(title = NULL,
            status = "primary",
            solidHeader = T,
            collapsible = T,
            width = 9, # width is relative to to column
            
            # Display tokenised text data
            h2("Prepared Text Data"),
            p("The selected dataset below will be used for visualisations. Use the Dataset selector to select a different dataset to visualise at any time."),
            hr(),
            DT::dataTableOutput(ns("content_to_visualise_DT")) %>%
              withSpinner(),
            hr(class = "hr-blank"),
        ), # end box
      ), # end fluid row
      
      ### Summary stats ####
      fluidRow(
        box(title = " ",
            status = "success",
            collapsible = T,
            solidHeader = T,
            width = 3,
            h3("Quick summary"),
            textOutput(ns("stats1")) %>%
              withSpinner(),
            p(textOutput(ns("stats2"))),
            p(textOutput(ns("max_min_IDs"))),
        ),
        
      #### Word cloud ####
      box(title = "Word cloud", status = "primary",
          solidHeader = T,collapsible = T, width = 6,
          
          # Module to render wordcloud with function which fixes it
          wordCloudUI(ns("wordcloud"))
      ),
      
      
      #### Generating quick report #####
      box(title = " ", status = "success",
          solidHeader = T, collapsible = T,
          width = 3,
          h3("Quick report"),
          p("Generate a quick report of your visualisations."),
          em("Note: Reports are available to download only after visualisations are rendered. Generate a fully customised report in the reporting tab."),
          hr(class = "hr-blank"),
          downloadButton(ns("download_quick_report"),
                         label = "Generate report")
        )
      ), # end fluid row
      
      #### Token frequency plot of entire corpus ####
      fluidRow(
        # Token frequency customizing
        box(title = " ",
            status = "primary",
            solidHeader = T,
            collapsible = T,
            width = 4,
            
            h3("Token frequency"),
            p("Explore the most frequent tokens in your corpus with this token frequency plot. Quickly identify the top tokens based on frequency, which is the number of times a token appears in the imported corpus."),
            hr(),
            numericInput(ns("min_freq"),
                         label = "Number of most frequent tokens to display:",
                         value = 20,
                         min = 1),
            textInput(ns("token_freq_plot_title"),
                      label = "Figure title:",
                      value = "Most common tokens by frequency"),
            # textInput(ns("token_freq_plot_caption"),
            #           label = "Customise plot caption",
            #           value = "Figure 1: Frequency of most common tokens across the corpus."),
            
            hr(),
            
            h4("Download figure"),
            
            fluidRow(
              column(width = 4, {
                numericInput(ns("token_freq_plot_dpi"),
                             label = "DPI",
                             value = 72)
              }),
              column(width = 4, {
                numericInput(ns("token_freq_plot_h"),
                             label = "H (in)",
                             value = 29.16)
              }),
              column(width = 4, {
                numericInput(ns("token_freq_plot_w"),
                             label = "W (in)",
                             value = 29.16)
              })
            ),
            
            downloadButton(ns("download_token_freq_plot"),
                           label= "Save .png")
            
        ), # end info box
        
        # Word frequency plot box
        box(title = "Token frequency plot",
            status = "primary",
            solidHeader = T,
            collapsible = T,
            width = 8,
            
            # plotOutput(ns("word_freq")),
            plotlyOutput(ns("word_freq_plotly")) %>%
              withSpinner()
            
        ),
      ), # end fluid row
      
      #### Zipfs law vis. ####
      fluidRow(
        box(title = " ",
            status = "primary",
            solidHeader = T,
            collapsible = T,
            width = 5,
            
            h3("Illustrating Zipf's Law"),
            p("Zipf's law states that the frequency of a word appearing is inversely proportional to its rank."),
            imageOutput(ns("zipfs_law"), height = '70px'),
            p("This inverse proportional relationship can be visualised by plotting these values on log scales.  Each line represents a text document in the current corpus."),
            tags$a(href="https://www.tidytextmining.com/tfidf.html#zipfs-law",
                   "Learn more about Zipf's Law"),
            hr(),
            
            textInput(ns("zipfs_plot_title"),
                      label = "Plot title:",
                      value = "Zipf's law (rank vs. term frequency)"),
            
            # textInput(ns("zipfs_plot_caption"),
            #           label = "Customise plot caption",
            #           value = "Figure 3: A demonstration of Zipf's Law:  plotting term frequency against term rank on log-log scales."),
            
            hr(),
            h4("Download figure"),
            fluidRow(
              column(width = 4, {
                numericInput(ns("zipfs_plot_dpi"),
                             label = "DPI",
                             value = 72)
              }),
              column(width = 4, {
                numericInput(ns("zipfs_plot_h"),
                             label = "H (in)",
                             value = 29.16)
              }),
              column(width = 4, {
                numericInput(ns("zipfs_plot_w"),
                             label = "W (in)",
                             value = 29.16)
              })
            ),
            
            downloadButton(ns("download_zipfs_plot"),
                           label= "Save .png")
        ),
        
        box(title = "Zipf's law",
            status = "primary",
            solidHeader = T,
            collapsible = T,
            width = 7,
            
            # plotOutput(ns("zipfs_plot")),
            plotlyOutput(ns("zipfs_plotly")) %>%
              withSpinner()
        ),
      ), # end fluid row
      
      #### Tf-idf vis. ####
      fluidRow(
        box(title = " ",
            status = "primary",
            solidHeader = T,
            collapsible = T,
            width = 4,
            
            h3("Tf-idf"),
            p("Term frequency-inverse document frequency (tf-idf) is a method used to quantify which words are important to a document."),
            p("Tf-idf is the product of a term's frequency and the inverse document frequency, producing a statistic which measures how important a term is to a document."),
            imageOutput(ns("idf_eqn"), height = '90px'),
            p("The method aims to decrease weighting of common terms and increase weighting of terms with low frequencies. "),
            em("For the most accurate results, remove any stop-words from your data and tokenise by words. ", style = "color: crimson;"),
            br(),
            tags$a(href="https://www.tidytextmining.com/tfidf.html#tfidf",
                   "Learn more about tf-idf"),
            
            hr(),
            # Checkboxes for tf-idf
            # selectInput(ns("checkbox_content_ID"),
            #             label = "Choose texts to perform tf-idf on",
            #             choices = list("N/A" ="na")),
            # selectInput(ns("checkbox_content_ID2"),
            #             label = "(Optional) Pick another text to compare to",
            #             choices = list("N/A" ="na")),
            uiOutput(ns("tf_idf_ID_selectors")),
            
            textInput(ns("tf_idf_plot_title"),
                      label = "Plot title:",
                      value = "Highest tf-idf tokens of selected documents"),
            
            
            em("Download this figure via Plotly controls, found by hovering on the top right of the figure."),
            
            # fluidRow(
            #   column(width = 4, {
            #     numericInput(ns("tf_idf_plot_dpi"),
            #                  label = "DPI",
            #                  value = 72)
            #   }),
            #   column(width = 4, {
            #     numericInput(ns("tf_idf_plot_h"),
            #                  label = "H (in)",
            #                  value = 29.16)
            #   }),
            #   column(width = 4, {
            #     numericInput(ns("tf_idf_plot_w"),
            #                  label = "W (in)",
            #                  value = 29.16)
            #   })
            # ),
            # downloadButton(ns("download_tf_idf_plot"),
            #                label= "Save .png")
        ),
        
        box(title = "Tf-idf",
            status = "primary",
            solidHeader = T,
            collapsible = T,
            width = 8,
            
            # plotOutput(ns("tf_idf_plot")),
            plotlyOutput(ns("tf_idf_plotly")) %>%
              withSpinner(),
            hr(class = "hr-blank", style = "height: 400px;"),
            
        ),
      ), # end fluid row
      
      #### Comp freq plot single vs. corpus frequency ####
      fluidRow(
        box(title = " ",
            status = "primary",
            solidHeader = T,
            collapsible = T,
            width = 4,
            
            h3("Token frequency comparison"),
            p("Explore textual distinctions within a corpus by plotting the token proportions of an individual document against the combined token proportions of the rest of the corpus—tokens near the central line indicate similar usage, while those distant reveal variations in proportions."),
            p("Tokens closer to the line have similar frequencies in both sets of texts."),
            hr(),
            uiOutput(ns("comp_freq_single_ID")),
            hr(),
            
            h4("Pearson's correlation test"),
            p("Pearson's product-moment correlation test allows us to quantify how similar texts are."),
            p(textOutput(ns("corr_test_summary"))) %>%
               withSpinner(),
            
            hr(),
            
            h4("Download figure"),
            fluidRow(
              column(width = 4, {
                numericInput(ns("comp_freq_plot_dpi"),
                             label = "DPI",
                             value = 72)
              }),
              column(width = 4, {
                numericInput(ns("comp_freq_plot_h"),
                             label = "H (in)",
                             value = 29.16)
              }),
              column(width = 4, {
                numericInput(ns("comp_freq_plot_w"),
                             label = "W (in)",
                             value = 29.16)
              }),
            ), # end fluid row
            downloadButton(ns("download_comp_freq_plot"),
                           label= "Save .png")
        ), # end info box
        
        box(title = "Token frequency comparison",
            status = "primary",
            solidHeader = T,
            collapsible = T,
            width = 8,
            # plotOutput(ns("comp_freq")),
            plotlyOutput(ns("comp_freq_plotly")) %>%
              withSpinner(),
        ), # end box
      ), # end fluid row
      
      ### Interactive datatable ####
      fluidRow(
        box(title = " ",
            status = "primary",
            solidHeader = T,
            collapsible = T,
            width = 12,
            
            h2("Interactive Datatable"),
            p("Explore your tf-idf data by filtering, sorting, and searching with the buttons and boxes below."),
            em("Note: Actions performed on the datatable below will not affect your prepared data. If you wish to fine-tune your data for analysis and reporting, visit the 'Text Preparation' tab.", style = "color: gray;"),
            hr(class = "hr-blank"),
            DT::dataTableOutput(ns("interact_DT")) %>%
              withSpinner(),
            hr(class = "hr-blank"),
        ),
      ), # end fluid row datatable
      

  ) # end tagList
  
} # end UI function


########################
## Reporting server ####
########################
reportingServer <- function(id, rv = NULL){
  moduleServer(id, function(input, output, session){
    
    
    #######################
    ### Dataset chooser ###
    #######################
    observe({
      # Create list of possible datasets to select from
      req(rv$content_primary)
      potential_sets_visualise <- list("Primary data")
      if(!is.null(rv$subset_one)){
        potential_sets_visualise[length(potential_sets_visualise) + 1] <- "Subset one"
      }
      if(!is.null(rv$subset_two)){
        potential_sets_visualise[length(potential_sets_visualise) + 1] <- "Subset two"
      }
      updateSelectInput(session, "data_to_visualise",
                        choices = potential_sets_visualise,
                        selected= potential_sets_visualise[1])
    })
    
    # Always update possible dataset list with current available subsets
    observe({
      req(rv$content_primary$content_prepared)
      req(input$data_to_visualise)
      
      # Rendering whatever dataset selected in datatable display. Needs to be
      # rendered as diff sets selected before confirm button clicked
      rv$content_to_visualise_DT <-
        switch(input$data_to_visualise,
               "Primary data" =  rv$content_primary$content_prepared,
               "Subset one" = rv$subset_one$content_prepared,
               "Subset two" = rv$subset_two$content_prepared)
      
      ###################
      ## Column chooser #
      ###################
      # When a dataset is chosen to visualise on, need to load which
      # columns available in that dataset
      # (subsets' content_prepared is initialized on save of subsets)
      rv$content_to_visualise_colnames <- switch(input$data_to_visualise,
                                                 "Primary data" =
                                                   colnames(rv$content_primary$content_prepared),
                                                 "Subset one" = colnames(rv$subset_one$content_prepared),
                                                 "Subset two" = colnames(rv$subset_two$content_prepared)
      )
      updateSelectInput(session, "col_name_to_visualise",
                        choices = rv$content_to_visualise_colnames[-1],
                        selected= rv$content_to_visualise_colnames[2])
    })
    
    ########################################
    ## Validate content data to visualise ##
    ########################################
    observeEvent(input$confirm_data_to_visualise, {
      
      waiter_show(html = waiting_screen_reporting, color = "royalblue")
      
      # Require something to be upload (content_primary) and inputs selected
      req(rv$content_primary)
      req(input$data_to_visualise)
      req(input$col_name_to_visualise)
      
      # Double check the selected column exists in the dataset chosen
      req(input$col_name_to_visualise %in% rv$content_to_visualise_colnames)
      
      # Verifying the dataset is convertible to chars and has 1-3 words/tokens per row
      # Have rv$content_to_visualise_DT from switch before, checking this
      req(rv$content_to_visualise_DT)
      
      # Attempt to convert the selected column to characters
      col_to_visualise_char_attempt <- try(
        as.character(rv$content_to_visualise_DT[[input$col_name_to_visualise]])
      )
      
      # If conversion to characters failed, produce alert and return
      # else assign the converted column to data to visualise
      if("try-error" %in% class(col_to_visualise_char_attempt)){
        shinyalert(
          title = "Selection invalid: data invalid",
          text = "The contents of the selected column could not be converted to strings in order to visualise. \n \n Ensure your selected column contains strings of text/word tokens.",
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
      rv$content_to_visualise_DT[[input$col_name_to_visualise]] <-
        col_to_visualise_char_attempt
      
      
      # Validating the supplied data to visualise is tokenised enough, see utils.R
      is_tokenised <- tokenised_enough(rv$content_to_visualise_DT,
                                       input$col_name_to_visualise)
      if(!is_tokenised){
        shinyalert(
          title = "Too many tokens per row",
          text = "Data should contain no more than 3 words per row to produce visualisations. \n \n Use the Tokenise tool in the Text Preparation tab to split up your text into 1-3 words per row.",
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
      
      # Creating list for dataset created and its characteristics
      content_to_visualise <- shiny::reactiveValues(
        data = rv$content_to_visualise_DT,
        col_to_visualise = input$col_name_to_visualise
      )
      rv$content_to_visualise <- content_to_visualise
      
      # At this point we have dataset's column to visualise which is tokenised enough
      # Extracting from data to visualise just the ID and column of interest
      # was rv$content_to_visualise_DT being manipulated
      req(rv$content_to_visualise$data[['ID']])
      rv$content_to_visualise$plotting_data <- rv$content_to_visualise$data %>%
        select("ID", {input$col_name_to_visualise}) %>%
        rename(Token = {input$col_name_to_visualise})
      
      # Rendering choices_ID, num_files, total_tokens
      req(rv$content_to_visualise$plotting_data[['ID']])
      # Must assign any calculations as reactive or they won't update properly
      # Getting unique IDs of each file uploaded for the dropdown menu choices
      # choices_ID <- unique(rv$content_to_visualise$plotting_data$ID)
      rv$content_to_visualise$choices_ID <- 
        unique(rv$content_to_visualise$plotting_data$ID)
      
      # Saving number of documents as number of unique IDs
      # num_files <- length(rv$content_to_visualise$choices_ID)
      rv$content_to_visualise$num_files <- length(rv$content_to_visualise$choices_ID)
      
      # Saving total tokens to visualise - since data is tokenised, total tokens
      # is just the number of rows in the data
      rv$content_to_visualise$total_tokens <- nrow(rv$content_to_visualise$data)
      
      # Calculating counts of each token in overall corpus i.e. the
      # number of each token in the overall corpus
      # For example, 'the' occurs 1000 times total across all docs
      req(rv$content_to_visualise$plotting_data[['Token']])
      rv$content_to_visualise$token_counts_corpus <-
        rv$content_to_visualise$plotting_data %>%
        count(Token, sort = T) %>%
        mutate(Token = reorder(Token, n))
      # e.g.
      # Token     n
      # <fct> <int>
      # 1 the   35828
      # 2 and   28293
      # 3 to    20322
      
      # Calculating counts of each token within each ID, i.e. the
      # frequency of each token within a particular document
      rv$content_to_visualise$token_counts_per_doc <-
        rv$content_to_visualise$plotting_data %>%
        count(ID, Token, sort = T) %>%
        rename("Freq." = "n")
      # e.g.
      # ID                   Token Freq.
      # <chr>                <chr> <int>
      # 1 bleakhouse.txt      the   14980
      # 2 barnabyrudge.txt    the   12795
      
      # Generating total token count for each document, mean corpus token count
      # and overall corpus token count (mini summary stats)
      rv$content_to_visualise$corpus_summary_counts <-
        rv$content_to_visualise$token_counts_per_doc %>%
        group_by(ID) %>%
        summarise(`Token Count` = sum(`Freq.`)) %>%
        mutate(`Mean token count` =
                 floor(mean(`Token Count`)),
               `Std. dev. token count` =
                 floor(sd(`Token Count`)),
               `Corpus token count` = sum(`Token Count`))
      # e.g.
      # ID        `Token Count` `Mean token count` Std. dev. token coun…¹ `Corpus token..`
      # <chr>                 <int>             <dbl>             <dbl>           <int>
      # 1 ataleoftwoci…      137433           250670              110255          752010
      # 2 barnabyrudge…      256897           250670              110255          752010
      
      waiter_hide()
    }) # end observe event input$confirm_data_to_visualise
  
    
    # Creating datatable of content parameterised
    output$content_to_visualise_DT <- DT::renderDataTable({
      
      validate(need(rv$content_to_visualise_DT, "Submit text data in the
                    Text Selector Tab to unlock visualisation features."))
      
      DT::datatable(rv$content_to_visualise_DT,
                    options = list(
                      paging = TRUE, pageLength = 6,
                      scrollX = TRUE, scrollY = TRUE,
                      dom = 'frtip',
                      columnDefs = list(
                        list(targets = '_all', className = 'dt-left'),
                        list(targets = 1,
                             render = JS(
                               "function(data, type, row, meta) {",
                               "return type === 'display' && data.length > 100 ?",
                               "'<span title=\"' + data + '\">' +
                    data.substr(0, 100) + '...</span>' : data;","}")))
                    ),
                    selection = 'single',
                    rownames = FALSE
      )
    }) # end render datatable
    
    #######################################
    ###### Reporting server 
    ########################################
    # Reactive value list to pass to .qmd to render quick report
    mini_rv <- shiny::reactiveValues()

    ############
    # Rendering select input UI for comparison plot
    ###########
    output$comp_freq_single_ID <- renderUI({
      ns <- NS(id)
      
      if(!is.null(rv$content_to_visualise$num_files) && 
         rv$content_to_visualise$num_files > 1){
        
        valid_UI <- tagList(
          selectInput(session$ns("comp_freq_single_ID_input"),
                      label = "Choose a single text to compare to the corpus",
                      choices = rv$content_to_visualise$choices_ID,
                      selected = rv$content_to_visualise$choices_ID[1]),
          
          textInput(session$ns("comp_freq_plot_title"),
                    label = "Plot title:",
                    value = "Comparison of token frequencies (single vs. corpus)"),
          
          # textInput(session$ns("comp_freq_plot_caption"),
          #           label = "Customise plot caption",
          #           value = "Figure 2: Comparison of token frequencies  between a single selected text and the current corpus."),
          
          actionButton(session$ns("confirm_comp_freq_plot"),
                       label = "Plot",
                       class = "btn-success"),
        ) # end tagList of renderUI
        valid_UI
        
      } else {
        em("Upload at least two unique text documents to calculate and compare document versus corpus token proportions.", style = "color: gray;")
      }
    }) # end renderUI


    ########################
    ## Summary stats stuff #
    ########################
    # Summary stats text rendering
    output$stats1 <- renderText({

      validate(need(rv$content_to_visualise$num_files > 0,
                    'Submit valid text data to generate quick summaries.'))

      paste0("Corpus contains: ",
                     rv$content_to_visualise$num_files,
                     " document(s), with ", length(rv$stop_words_final$word),
                     " stop-word(s) omitted.", collapse = ",")
    })
    output$stats2 <- renderText({
      req(rv$content_to_visualise$num_files > 0)
      req(rv$content_to_visualise$corpus_summary_counts)

      paste("Total tokens in corpus: ",
            rv$content_to_visualise$total_tokens,
            ". Mean token count per document: ",
            rv$content_to_visualise$corpus_summary_counts[['Mean token count']][1],
            " (std. dev. ± ",
            rv$content_to_visualise$corpus_summary_counts[['Std. dev. token count']][1],
            ").",
            collapse = ",")
    })
    # Summary text for max and min document tokens
    max_min_token <- reactive({
      req(rv$content_to_visualise$corpus_summary_counts)
      rv$content_to_visualise$corpus_summary_counts %>%
        arrange(desc(`Token Count`)) %>%
        dplyr::select(ID,`Token Count`)
    })
    output$max_min_IDs <- renderText({
      req(max_min_token())

      highest_token_count <- slice_head(max_min_token())
      lowest_token_count <- slice_tail(max_min_token())

      paste("Highest token count of ",
            highest_token_count[1,2], " found in document",
            highest_token_count[1,1], ".",
            "Lowest token count of ",
            lowest_token_count[1,2], " found in document ",
            lowest_token_count[1,1], ".", collapse = ",")
     })


    ###########################
    #### Word cloud server ####
    ###########################
    wordCloudAttempt <- try(wordCloudServer("wordcloud", rv = rv))
    if("try-error" %in% class(wordCloudAttempt)){
      shinyalert(
        title = "The word cloud server broke!",
        text = "Word cloud could not be rendered.",
        size = "xs", 
        closeOnEsc = TRUE, closeOnClickOutside = TRUE,
        html = FALSE, type = "info",
        showConfirmButton = TRUE, showCancelButton = FALSE,
        confirmButtonText = "Dismiss",
        confirmButtonCol = "#4169E1",
        timer = 0, imageUrl = "", animation = TRUE
      )
      return()
    }
    

    ############################################
    ## Token frequency plot for entire corpus ##
    ############################################
    # render title
    observe({
      rv$title1 <- str_wrap(input$token_freq_plot_title, 70)
      # rv$cap1 <- str_wrap(input$token_freq_plot_caption, 100)
    })

    # Creating frequency plot, where users can customise min
    # frequency of a token to be displayed on the plot
    token_freq_plot <- reactive({
      req(rv$content_to_visualise$token_counts_corpus)
      req(is.numeric(input$min_freq))

      # Only rendering highest occurring n tokens 
      # (controlled with input$min_freq)
      rv$content_to_visualise$token_counts_corpus %>%
        head(input$min_freq) %>%
        ggplot(aes(n, Token)) +
        geom_col(fill = "royalblue") +
        labs(x = "Frequency", y = NULL) + # caption = rv$cap1
        ggtitle(rv$title1) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0, size = 12,
                                        vjust = 1),
              text = element_text(size = 12))
      # plot.caption = element_text(hjust=0, vjust = -1.2,
      #                             size = 10, face = "italic")

    })

    # output$word_freq <- renderPlot({
    #   validate(need(try(token_freq_plot()),
    #                 'Select a valid dataset with a numeric minimum frequency
    #               to render visualisation.'))
    #
    #   token_freq_plot()
    # })

    output$word_freq_plotly <- renderPlotly({
      validate(need(rv$content_to_visualise$token_counts_corpus,
                    'Select a valid dataset to render visualisations.'),
               need(is.numeric(input$min_freq),
                    'Mimimum frequency must be numeric.')
               )
      req(token_freq_plot())
      ggplotly(token_freq_plot())
    })

    
    #########################
    ## Comp frequency plot ##
    #########################
    # To plot the comparison of frequency of tokens between a single vs. corpus,
    # proportion is used for this, and ultimately content_prepared is
    # manipulated to calculate proportions for the single file, corpus and then these
    # are combined to create content_corr_freq
    
    # Creating comp_freq_single_props tibble with just single selected file's data by
    # filtering content_prepared ID column
    comp_freq_single_props <- reactive({
      
      req(rv$content_to_visualise$plotting_data[['Token']])
      req(input$comp_freq_single_ID_input)
      
      # Require that filtering by selected ID doesn't return nothing (so a valid
      # ID has been selected). Test written as similar as possible to what happens
      # a few lines down
      req(try(nrow(filter(rv$content_to_visualise$plotting_data,
                          ID == input$comp_freq_single_ID_input)) > 0))
      
      res <- rv$content_to_visualise$token_counts_per_doc %>%
        filter(ID == input$comp_freq_single_ID_input) %>%
        mutate(Proportion = `Freq.`/sum(`Freq.`)) %>% # proportion calc
        # rename("Source" = "ID")
        mutate(Source = input$comp_freq_single_ID_input) %>%
        select(-ID)
      
      print("comp freq single props created:")
      print(res)
      return(res)
      
    }) # end comp freq single 
    
    # Filtering content_prepared to be the rest of the corpus
    comp_freq_rest_props <- reactive({
      
      req(rv$content_to_visualise$plotting_data[['Token']])
      req(input$comp_freq_single_ID_input)
      
      # If there are more than one unique IDs in dataset
      if(rv$content_to_visualise$num_files > 1){
        
        res <- rv$content_to_visualise$plotting_data %>%
          filter(ID != input$comp_freq_single_ID_input) %>%
          count(Token, sort = T) %>%
          mutate(Proportion = n/sum(n)) %>% # proportion calc
          mutate(Source = "Corpus") %>%
          rename("Freq." = "n")
        
        return(res)
        
      } else {
        #### Validate to warn to have more than 1 ID?
        return(content_param_single())
      }
      
      # If there are no other tokens associated w other IDs, there must
      # only be 1 unique ID uploaded, so can just return comp_freq_single_props
      # again
      # if(nrow(filter(rv$content_to_visualise$plotting_data,
      #                ID != input$comp_freq_single_ID_input)) == 0){
      #   comp_freq_rest_props <- comp_freq_single_props()
      #   
      #   # else there is more than 1 unique doc to compare with
      # } else {
      # Calculating proportion each token makes up in the rest of the
      # combined corpus (excluding selected document)
      #   comp_freq_rest_props <-
      #     rv$content_to_visualise$plotting_data %>%
      #     filter(ID != input$comp_freq_single_ID_input) %>%
      #     count(Token, sort = T)  %>% # count total occurrences of each token
      #     mutate(Proportion = n/sum(n)) %>% # proportion calc
      #     mutate(Source = "Corpus") %>%
      #     rename("Freq." = "n")
      # }
      # 
      # return(comp_freq_rest_props)
    })
    
    # Creating final dataset of widened data for both single and rest to plot
    # Creates two proportions for each token, from single essay and
    # entire corpus
    content_comp_freq <- reactive({
      
      req(comp_freq_single_props())
      req(comp_freq_rest_props())
      
      req("Freq." %in% try(colnames(comp_freq_single_props())))
      req("Freq." %in% try(colnames(comp_freq_rest_props())))
      
      ##### Binding single and rest rows and replacing NAs with 0s,
      ##### then widening dataset for plotting
      content_comp_freq <-
        bind_rows(comp_freq_single_props(), comp_freq_rest_props()) %>%
        dplyr::select(-`Freq.`) %>% # have to deselect n or widening will fail
        pivot_wider(names_from = Source, values_from = Proportion) %>%
        mutate_all(~ifelse(is.na(.), 0, .)) # ~ to define anon func to check na
      # na.omit() # removing all rows containing NA - basically keeps only tokens
      # which are common to both the single and rest of corpus, means stuff like
      # names will be left out
      
      # Assigning col names
      colnames(content_comp_freq) <- c("Token", "Single", "Corpus")
      
      # Calculating difference in proportions of each token
      # between the single file and rest of corpus.
      # This delta proportion is used to colour points in plot.
      # If just have one text, thus the data excluding said text
      # consists of nothing, the difference in proportions b/t
      # itself and the rest of the corpus (itself) is 0.
      if(nrow(comp_freq_rest_props()) == 0){
        
        # if there is only one document the correlation test fails
        # So corpus is just the single document
        # content_comp_freq <- content_comp_freq %>%
        #   mutate(Diff = abs(Single - Single),
        #          Corpus = Single)
        content_comp_freq <- try(mutate(content_comp_freq,
                                        Diff = abs(Single - Single,
                                                   Corpus  = Single)))
      } else {
        # Getting absolute difference to plot
        content_comp_freq <- try(mutate(content_comp_freq,
                                        Diff = abs(Single - Corpus)))
      }
      
      # checking if mutate worked
      if("try-error" %in% class(content_comp_freq)){
        print("content_comp_freq try error detected")
        return(NULL)
      } else {
        print("content_comp_freq success")
        return(content_comp_freq)
      }
    })
    
    # Observe to assign reactives made for corr plot to rv list
    # If content is not tokenised, assign NULL to reactive
    observe({
      req(content_comp_freq())
      rv$content_to_visualise$content_comp_freq <- content_comp_freq()
      print("assigned content_comp_freq() to rv list:")
      print(rv$content_to_visualise$content_comp_freq)
    })
    
    
    observeEvent(input$confirm_comp_freq_plot, {
      
      ### Single vs. corpus frequency plot ####
      # Save title
      # observe({
        rv$title2 <- str_wrap(input$comp_freq_plot_title, 70)
        # rv$cap2 <- str_wrap(input$comp_freq_plot_caption, 100)
      # })
      
      req(rv$content_to_visualise$content_comp_freq)
      
      rv$comp_freq_plot <-
        ggplot(rv$content_to_visualise$content_comp_freq,
               aes(x = Corpus, y =  Single, color = Diff)) +
          geom_abline(color = "gray40", lty = 2) +
          # geom_point(alpha = 0.6, size = 1) +
          geom_text(aes(label = Token), alpha = 1,
                    check_overlap = T, cex = 3,
                    # nudge_y = 0.5, nugde_x = 0.5,
                    size = 11) +
          xgx_scale_x_log10(labels = percent_format(),
                            breaks = c(0.0, 0.001, 0.01, 0.03, 0.1, 0.3, 1),
                            limit = c(0.00001, 1.0)) + # c(0.0009, 1.0)) +
          xgx_scale_y_log10(labels = percent_format(),
                            breaks = c(0.0, 0.001, 0.01, 0.03, 0.1, 0.3, 1),
                            limit = c(0.00001, 1.0)) + # c(0.0009, 1.0)) +
          scale_color_gradient(limits = c(0.0, 0.075),
                               low = "#d0e0e3", high = "darkblue") +
          theme_bw() +
          theme(legend.position="right",
                text = element_text(size = 11)) +
          theme(plot.title = element_text(size = 12, hjust = 0,
                                          vjust = 1)) +
          # plot.caption = element_text(hjust=0, vjust = -1.2,
          #                             size = 12, face = "italic")
          labs(y = paste0(input$comp_freq_single_ID_input, " proportions \n (log)"),
               x = "Corpus proportions (log)",
               col = "Difference in \n proportions") + # caption = rv$cap2
          ggtitle(rv$title2)
    })
    
    # output$comp_freq <- renderPlot({
    #   validate(need(comp_freq_plot(),
    #                 'Select a valid dataset to render visualisations.'))
    #   comp_freq_plot()
    # })
    output$comp_freq_plotly <- renderPlotly({
      validate(need(rv$content_to_visualise$content_comp_freq,
                    "Select and submit more than one document to explore differences in token proportions between a single document and the corpus.")
      )
      validate(need(rv$comp_freq_plot,
                    "Select and submit more than one document to explore differences in token proportions between a single document and the corpus.")
      )
      
      # attempt conversion to ggploty
      plot_res <- try(ggplotly(rv$comp_freq_plot))
      validate(need(!inherits(plot_res, "try-error"),
                    "Select a document to compare using the siderbar. Ensure more than one document has been uploaded and a valid single document is selected to explore differences in token proportions.")
      )
      return(plot_res)
    }) # end render plotly
    
    
    ########################
    ## Correlation test ####
    ########################
    corr_test <- reactive({
      req(rv$content_to_visualise$plotting_data)
      req(rv$content_to_visualise$content_comp_freq)
      
      # Try cor test
      corr_test <- try(cor.test(rv$content_to_visualise$content_comp_freq$Single,
                                rv$content_to_visualise$content_comp_freq$Corpus))
      
      if("try-error" %in% class(corr_test)){
        print("corr_test failed w try error, returning NULL for corr_test")
        return(NULL)
      } else {
        return(corr_test)
      }
    })
    # Rendering the correlation test result to display
    output$corr_test_summary <- renderText({
      validate(need(rv$content_to_visualise$num_files > 1,
                    "Select a valid dataset with more than one document and enough finite observations to perform correlation test."))
      validate(need(try(corr_test()),
                    "Correlation test could not be rendered. Ensure comparison plot could be rendered and more than one unique document is selected."))
      
      paste(c("The estimate of correlation between the
                                   selected text (",
              input$comp_freq_single_ID_input, ")
                                    and the corpus proportions obtained
                                     from a product-moment correaltion test is ",
              format(round(corr_test()$estimate, 3)),
              " with a 95% CI of (",
              format(round(corr_test()$conf.int[1], 3)),
              ", ",
              format(round(corr_test()$conf.int[2], 3)),
              ")."))
    })

    ############################
    ### Zipf visualisations ####
    ############################
    # Need to use data without stop-words removed
    # And unnest on words only - possibility to implement with
    # bi-grams in future
    # So, taking rv$content and unnesting this
    # But, users could have 'words' that are paragraph markers etc.,
    # so will just put a note to clear stop-words and unnest on words

    # Counting up frequency of each token & token count in each text
    # Token count is analogous to a 'word count' of a text, but we
    # could be counting up bi-grams instead of words
    content_zipf <- reactive({
      req(rv$content_to_visualise$plotting_data$ID)
      req(rv$content_to_visualise$token_counts_per_doc)
      req(rv$content_to_visualise$corpus_summary_counts)

      # Joining token freqs within each doc with corpus token totals to have
      # each token and its frequency within a doc, and the overall summary counts
      left_join(rv$content_to_visualise$token_counts_per_doc,
                rv$content_to_visualise$corpus_summary_counts,
                by = c("ID" = "ID")) %>%
        group_by(ID) %>%
        mutate(Rank = row_number(),
               `Term freq.` = `Freq.`/`Token Count`) %>%
        ungroup()
    })

    # Assigning to reactive value list
    observe({
      req(content_zipf())
      rv$content_to_visualise$content_zipf <- content_zipf()

      rv$title3 <- input$zipfs_plot_title
    })

    # Zipf's law visualisation
    # Plotting rank on x and tf on y on log scales
    zipfs_plot <- reactive({
      req(rv$content_to_visualise$content_zipf)

      rv$content_to_visualise$content_zipf %>%
        ggplot(aes(Rank, `Term freq.`, color = ID)) +
        geom_line(alpha = 0.7, show.legend = F) +
        xgx_scale_x_log10() + # breaks = c(1, 10, 100, 1000, 10000)
        xgx_scale_y_log10() + #breaks = c(0.001, 0.01, 0.1, 1.0)
        labs(x = "Rank (log)", y = "Term frequency (log)",
             subtitle = "") + # caption = cap3
        ggtitle(str_wrap(rv$title3, 70)) +
        scale_color_manual(values =
                             rep(c("royalblue", "black", "lightseagreen"),
                                 nrow(rv$content_to_visualise$plotting_data))
        ) +
        theme_bw() +
        theme(plot.title = element_text(size = 12, hjust = 0,
                                        vjust = 1),
              legend.position = 'none',
              text = element_text(size = 11))

    })

    # output$zipfs_plot <- renderPlot({
    #   validate(need(try(zipfs_plot()),
    #                 'Select a valid dataset to render visualisations.'))
    #   zipfs_plot()
    # })

    output$zipfs_plotly <- renderPlotly({
      validate(need(try(zipfs_plot()),
                    'Select a valid dataset to render visualisations.')
      )
      try(ggplotly(zipfs_plot()))
    })

    ################################
    ##### Tf-idf visualisations ####
    ################################

    output$tf_idf_ID_selectors <- renderUI({
      ns <- NS(id)
      req(rv$content_to_visualise$choices_ID)

      res <- tagList(
        selectInput(session$ns("checkbox_content_ID"),
                    label = "Choose a document to investigate:",
                    choices = rv$content_to_visualise$choices_ID,
                    selected = rv$content_to_visualise$choices_ID[1]),
        selectInput(session$ns("checkbox_content_ID2"),
                    label = "(Optional) Choose a different document for comparison:",
                    choices = rv$content_to_visualise$choices_ID,
                    selected = rv$content_to_visualise$choices_ID[1]),
      ) # end tagList of renderUI

      return(res)
    }) # end renderUI


    # First creating tf-idf dataset
    content_tf_idf <- reactive({
      req(rv$content_to_visualise$content_zipf)

      res <- rv$content_to_visualise$content_zipf %>%
        bind_tf_idf(Token, ID, `Freq.`) %>%
        arrange(desc(tf_idf)) 

      return(res)
    })

    observe({
      req(content_tf_idf())
      rv$content_to_visualise$content_tf_idf <- content_tf_idf()
      # Get the IDs of the files to calc tf-idf of
      rv$content_to_visualise$tf_idf_IDs <- list(input$checkbox_content_ID,
                                                 input$checkbox_content_ID2)
    })

    # Setting rv$title4
    observe({
      rv$title4 <- str_wrap(input$tf_idf_plot_title, 70)
      # rv$cap4 <- str_wrap(input$tf_idf_plot_caption, 100)
    })

    tf_idf_plot <- reactive({
      req(rv$content_to_visualise$content_tf_idf)
      req(rv$content_to_visualise$tf_idf_IDs)
      
      # Filter out to check if no tf-idf scores above 0, no point in 
      # plotting as must have just 1 or same doc uploaded since 
      # idf = ln(1/1) = 0 which results in tf x idf = 0
      plot_data <- rv$content_to_visualise$content_tf_idf %>%
        # filtering for only tf-idf scores above 0
        filter(tf_idf > 0) 
      
      # If we do have rows with tf-idf scores above 0, return plot
      if(nrow(plot_data) > 0){
        
        plot_data <- plot_data %>%
          # filtering to just be IDs selected
          filter(ID %in% rv$content_to_visualise$tf_idf_IDs) %>%
          group_by(ID) %>%
          slice_max(tf_idf, n = 15) %>%
          ungroup()
        
        plot <- plot_data %>%
          ggplot(aes(tf_idf, fct_reorder(Token, tf_idf), fill = ID)) +
          geom_col(show.legend = F, width = 0.8)+
          labs(x = "tf-idf", y = NULL) + # caption = rv$cap4
          # facet_wrap(~ID, ncol = 2, scales = "free_y") +
          facet_grid(rows = vars(ID), scales = "free_y") +
          ggtitle(rv$title4) +
          theme_bw() +
          scale_fill_manual(values=c("royalblue", "darkblue")) +
          theme(plot.title = element_text(size = 12, hjust = 0,
                                          vjust = 3),
                legend.position = 'none',
                text = element_text(size = 12))
        # plot.caption = element_text(size = 12, face = "italic",
        #                             hjust = 0, vjust = -1.2)

        return(plot)
        
      } else {
        return(NULL)
      }
    })

    # Plotting tf-idf plot, facet wrap by ID too
    # output$tf_idf_plot <- renderPlot({
    #   validate(need(try(tf_idf_plot()),
    #                 'Select a valid dataset to render visualisations.'))
    #   tf_idf_plot()
    # })
    output$tf_idf_plotly <- renderPlotly({
      
      validate(need(rv$content_to_visualise$content_tf_idf,
                    "Select valid text data to render tf-idf plot.")
      )
      # check plot object isn't null (no valid rows)
      validate(need(!is.null(tf_idf_plot()),
                    "Tf-idf plot could not be rendered. Ensure at least two unique documents are imported to calculate non-zero tf-idf scores.")
      )
      
      # attempt conversion to ggploty
      plot_res <- try(ggplotly(tf_idf_plot()))
      
      validate(need(!inherits(plot_res, "try-error"),
                    "Tf-idf plot could not be rendered. Ensure at least two unique documents are imported to calculate non-zero tf-idf scores.")
      )
      
      plot_res %>% layout(height = 800)
    })

    ####  Interactive data table ####
    interact_DT <- reactive({
      req(rv$content_to_visualise$content_tf_idf)
      
      # Deselecting summary stat/repeat cols and formatting
      rv$content_to_visualise$content_tf_idf %>%
        dplyr::select(-tf, -`Std. dev. token count`,
                      -`Mean token count`, -`Corpus token count`) %>%
        mutate_at(vars(-1, -2), ~round(., 3)) # round everything but non-numerics
    })
    output$interact_DT <- DT::renderDataTable({
      
      validate(need(try(interact_DT()),
                    'Select a valid dataset to explore.'))
      DT::datatable(interact_DT(),
                    options = list(
                      # dom = "tfirp",
                      ordering = TRUE,
                      searching = TRUE,
                      paging = TRUE,
                      pageLength = 6
                    ),
                    selection = 'none',
                    class = 'row-border',
                    escape = T,
                    rownames = F,
                    filter = "bottom"
      )
    })

    ##########################
    #### Rendering images ####
    ##########################
    # Send a pre-rendered image, and don't delete the image after sending it
    output$zipfs_law <- renderImage({
      # When input$n is 3, filename is ./images/image3.jpeg
      filename <- './www/zipfs_law.png'
      # Return a list containing the filename and alt text
      list(src = filename,
           alt = paste("Zipf's law"),
           width = 160,
           height = 45
      )
    }, deleteFile = FALSE)

    output$idf_eqn <- renderImage({
      # When input$n is 3, filename is ./images/image3.jpeg
      filename <- normalizePath('./www/idf_eqn.png')
      # Return a list containing the filename and alt text
      list(src = filename,
           alt = paste("Inverse document frequency"),
           width = 250,
           height = 60)
    }, deleteFile = FALSE)


    #########################
    ## Quick report params ##
    #########################
    # Creating rvs list to pass to final report
    ##### Could probably put in if statements?
    observe({
      # Check if everything needed for the report is present
      req(rv$content_to_visualise$plotting_data)
      # Adding to rv list so can communicate to report
      mini_rv$content <- rv$content_to_visualise$plotting_data
      mini_rv$num_files <- rv$content_to_visualise$num_files
      mini_rv$stop_words_final<- rv$stop_words_final
      
      # saving plots to send to report
      req(rv$word_cloud)
      mini_rv$word_cloud <- rv$word_cloud
      
      req(token_freq_plot())
      mini_rv$token_freq_plot <- token_freq_plot() + ggtitle(rv$title1)
      rv$token_freq_plot <- token_freq_plot() + ggtitle(rv$title1)
      
      req(zipfs_plot())
      rv$zipfs_plot <- zipfs_plot() + ggtitle(rv$title3)
      mini_rv$zipfs_plot<- zipfs_plot() + ggtitle(rv$title3)
      
      mini_rv$comp_freq_plot<- rv$comp_freq_plot
      
      # Saving tf-idf stuff for report
      req(rv$content_to_visualise$content_tf_idf)
      mini_rv$content_tf_idf<- rv$content_to_visualise$content_tf_idf
      mini_rv$tf_idf_IDs<- rv$content_to_visualise$tf_idf_IDs
      req(try(tf_idf_plot()))
      rv$tf_idf_plot <- tf_idf_plot() + ggtitle(rv$title4)
      mini_rv$tf_idf_plot<- tf_idf_plot() + ggtitle(rv$title4)
    })


    #### Download plot buttons ####
    output$download_token_freq_plot <- downloadHandler(
      filename = function() {
        paste("Token Frequency Figure.png")
      },
      content = function(file){
        ggsave(file, token_freq_plot(),
               dpi = input$token_freq_plot_dpi,
               height = input$token_freq_plot_h,
               width = input$token_freq_plot_w,
               units = "cm"
        )
      }
    )

    output$download_comp_freq_plot <- downloadHandler(
      filename = function() {
        paste("Frequency Comparison Figure.png")
      },
      content = function(file){
        ggsave(file, comp_freq_plot(),
               dpi = input$comp_freq_plot_dpi,
               height = input$comp_freq_plot_h,
               width = input$comp_freq_plot_w,
               units = "cm")
      }
    )

    output$download_zipfs_plot <- downloadHandler(
      filename = function() {
        paste("Zipf's Law Plot.png")
      },
      content = function(file){
        ggsave(file, zipfs_plot(),
               dpi = input$zipfs_plot_dpi,
               height = input$zipfs_plot_h,
               width = input$zipfs_plot_w,
               units = "cm")
      }
    )

    # Something about the tf-idf plot means downloading it
    # same way as other plots doesn't work.
    # output$download_tf_idf_plot <- downloadHandler(
    #   filename = function() {
    #     paste("Tf-idf Figure.png")
    #   },
    #   content = function(file){
    #     ggsave(file, tf_idf_plot(),
    #            dpi = input$tf_idf_plot_dpi,
    #            height = input$td_idf_plot_h,
    #            width = input$tf_idf_plot_w,
    #            units = "cm")
    #   }
    # )

    #### Quick report generation ####
    observe({
      if(is.null(rv$content_to_visualise$content_tf_idf)) {
        hide(id = "download_quick_report")
      } else {
        show(id = "download_quick_report")
      }
    })

    # Generating quick report
    output$download_quick_report <- downloadHandler(
      filename = "Quick_report.docx",
      content = function(file) {

        # generating report with rmarkdown::render()
        rmarkdown::render("www/Quick_report.qmd",
                          output_file = file,
                          params = list(report_rv = mini_rv)
        )
      }
    )

  } # end reporting server
  )
}
