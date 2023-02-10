
######### Reporting module for text analysis app ###################

library(openxlsx)
library(ggplot2)
library(quarto)
library(rmarkdown)
library(wordcloud2)

# For the corr plot
library(scales)
library(xgxr)

# devtools::install_github("lchiffon/wordcloud2")

outputDir <- "R"

pleasetokeniseUI <- function(id){
  ns <- NS(id)
  tagList(
    
      h3("Tokenise your data to render visualisations.", 
         style = "color: crimson"),
      em("Select the 'Tokenisation' tab at the top of this page, 
         then click 'Tokenise'.")
      
  )
}

##### Reporting tab module ######

reportingUI <- function(id){
  ns <- NS(id)
  tagList(
    useShinyjs(),
    
    wellPanel(
      h1(paste(c("02| Visualise"), sep = ",")),
      em("Generate custom visualisations by interacting with settings.")
    ),
    
    # For testing
    # verbatimTextOutput(ns("test")),
    
    fluidRow(
      box(title = " ",
          status = "success", 
          collapsible = T, 
          solidHeader = T,
          width = 3, 
          
          h3("Quick summary"), 
          
          p(textOutput(ns("stats1"))),
          p(textOutput(ns("stats2"))), 
          p(textOutput(ns("max_min_IDs")))
        
      ), 
      
      #### Word cloud ####
      box(title = "Word cloud", 
          status = "primary",
          solidHeader = T,
          collapsible = T, 
          width = 6, 
          
          h4(textOutput(ns("tokenise_please4")), 
             style = "background-color: crimson; color: white;"),
          
          wordcloud2Output(ns("wordcloud"), width = "100%"),
          
          ),
      
      # Generating quick report
      box(title = " ", 
          status = "success", 
          solidHeader = T,
          collapsible = T, 
          width = 3, 
          
          h3("Quick report"),
          p("Generate a report using default figure settings below"),
          em("Note: generate your customised report in the reporting tab."),
          
          hr(),
          
          downloadButton(ns("download_quick_report"), 
                         label = "Generate report"),
          hr()
      )
    ),
    
    #### Generating token frequency plot of entire corpus ####
    fluidRow(
      
      # Token frequency customizing
      box(title = " ", 
          status = "primary",
          solidHeader = T, 
          collapsible = T,
          width = 4,
          
          h3("Token frequency"),
          p("This figure presents the most common tokens in your text data by calculating their frequency (number of times occuring)."),
          hr(),
          numericInput(ns("min_freq"), 
                    label = "Customise the minimum frequency of a token to be displayed in the figure", 
                    value = 30,
                    min = 1),
          
          textInput(ns("token_freq_plot_title"), 
                    label = "Customise plot caption", 
                    value = "Most common tokens by frequency"),
          
          textInput(ns("token_freq_plot_caption"), 
                    label = "Customise plot caption", 
                    value = "Figure 1: Frequency of most common tokens across the corpus."),

          hr(),
          # radioButtons(inputId = "token_freq_plot_type",
          #              label = "Choose a plot type:",
          #              choiceNames = list(
          #                "Something",
          #                "Something else"
          #              ),
          #              choiceValues = list(
          #                "hist", "scatter")
          # )
          
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

      ),
      
      # Word frequency plot box
      box(title = "Token frequency plot", 
          status = "primary", 
          solidHeader = T, 
          collapsible = T,
          width = 8,
          
          h4(textOutput(ns("tokenise_please")), 
             style = "background-color: crimson; color: white;"),
          plotOutput(ns("word_freq")),

      ), 
    ),
    
    # Plot of single vs. corpus frequency ####
    fluidRow(
      box(title = " ", 
          status = "primary",
          solidHeader = T, 
          collapsible = T,
          width = 4,
          
          h3("Token frequency comparison"),
          p("This figure plots the proportion of the total tokens that a particular token takes up in the single text compared to the corpus token proportions."),
          p("Tokens closer to the line have similar frequencies in both sets of texts."),
          hr(), 
          selectInput(ns("content_single_ID"), 
                       label = "Choose a single text to 
                      compare to the corpus", 
                       choices = 
                        list("Choices" = 
                               "N/A")
                      ),
          
          textInput(ns("comp_freq_plot_title"), 
                    label = "Customise plot title", 
                    value = "Comparison of token frequeny (single vs. corpus)"),
          
          textInput(ns("comp_freq_plot_caption"), 
                    label = "Customise plot caption", 
                    value = "Figure 2: Comparison of token frequencies  between a single selected text and the current corpus."),
          hr(),
          
          h4("Pearson's correlation test"),
          p("Pearson's product-moment correlation test allows us to quantify how similar texts are."),
          p(textOutput(ns("cor_value"))),
          
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
            })
          ),
          
          downloadButton(ns("download_comp_freq_plot"), 
                         label= "Save .png")
      ),
      
      box(title = "Token frequency comparison", 
          status = "primary", 
          solidHeader = T, 
          collapsible = T,
          width = 8,
          
          h4(textOutput(ns("tokenise_please2")), 
             style = "background-color: crimson; color: white;"),
          plotOutput(ns("comp_freq"))
          
      ), 
    ),
    
    # Zipfs law vis. ####
    fluidRow(
      box(title = " ", 
          status = "primary",
          solidHeader = T, 
          collapsible = T,
          width = 5,
          
          h3("Illustrating Zipf's Law"),
          p("Zipf's law states that the frequency of a word appearing is inversely proportional to its rank."),
          imageOutput(ns("zipfs_law"), height = '70px'),
          p("This inverse proportional relationship can be visualised by plotting these values on log scales"),
          tags$a(href="https://www.tidytextmining.com/tfidf.html#zipfs-law", 
                 "Learn more about Zipf's Law"),
          hr(),
          
          textInput(ns("zipfs_plot_title"), 
                    label = "Customise plot title", 
                    value = "Zipf's law (rank vs. term frequency)"),
          
          textInput(ns("zipfs_plot_caption"), 
                    label = "Customise plot caption", 
                    value = "Figure 3: A demonstration of Zipf's Law:  plotting term frequency against term rank on log-log scales."),
          
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
          
          h4(textOutput(ns("tokenise_please5")), 
             style = "background-color: crimson; color: white;"),
          
          plotOutput(ns("zipfs_plot"))
          
      ), 
    ),
    
    # Tf-idf vis. ####
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
          p("The method aims to decrease weighting of common terms and increase weighting of terms with low frequencies."),
          em("For the most accurate results, remove any stop-words from your data and tokenise by words.", style = "color: crimson;"),

          tags$a(href="https://www.tidytextmining.com/tfidf.html#tfidf", 
                 "Learn more about tf-idf"),
          
          hr(),
          # Checkboxes for tf-idf
          selectInput(ns("checkbox_content_ID"), 
                             label = "Choose texts to perform tf-idf on", 
                      choices = list("N/A" ="na")),
          selectInput(ns("checkbox_content_ID2"), 
                      label = "(Optional) Pick another text to compare to", 
                      choices = list("N/A" ="na")),
          
          hr(),
          
          textInput(ns("tf_idf_plot_title"), 
                    label = "Customise plot title", 
                    value = "Highest tf-idf tokens of selected documents"),
          
          textInput(ns("tf_idf_plot_caption"), 
                    label = "Customise plot caption", 
                    value = "Figure 4: Most important terms according to term frequency-inverse document frequency analysis."),

          hr(),

          h4("Download figure"),
          fluidRow(
            column(width = 4, {
              numericInput(ns("tf_idf_plot_dpi"), 
                           label = "DPI",
                           value = 72)
            }), 
            column(width = 4, {
              numericInput(ns("tf_idf_plot_h"),
                           label = "H (in)", 
                           value = 29.16)
            }), 
            column(width = 4, {
              numericInput(ns("tf_idf_plot_w"),
                           label = "W (in)", 
                           value = 29.16)
            })
          ),
          downloadButton(ns("download_tf_idf_plot"), 
                         label= "Save .png") 
      ),
      
      box(title = "Tf-idf", 
          status = "primary", 
          solidHeader = T, 
          collapsible = T,
          width = 8,
          
          h4(textOutput(ns("tokenise_please6")), 
             style = "background-color: crimson; color: white;"),
          
          plotOutput(ns("tf_idf_plot"))
          
      ), 
    ),
    
    
    
    ##### Interactive datatable #####
    fluidRow(
        
        box(title = " ", 
            status = "primary",
            solidHeader = T, 
            collapsible = T,
            width = 12,
            
            h2("Interactive Datatable"),
            p("Explore your tf-idf data by filtering, sorting, and searching with the buttons and boxes below."),
            em("Note: Actions performed on the datatable below will not affect your parameterised data. If you wish to fine-tune your data for analysis and reporting, visit the 'Search/Concordance' tab.", style = "color: gray;"),
            hr(),
            
            h4(textOutput(ns("tokenise_please3")), 
               style = "background-color: crimson; color: white;"),
            DT::dataTableOutput(ns("interact_DT")),
            hr()
        ),
    ), # end fluid row datatable
  )
}


reportingServer <- function(id, rv = rv, report_rv = report_rv){
  moduleServer(id, function(input, output, session){
    
   # Getting names of each file uploaded for the dropdown menu
   # choices
    choices_ID <- reactive({

      if(!is.null(rv$content_parameterised)){
        return(unique(rv$content_parameterised$ID))
      }

      return("N/A")
    })

    # To generate the list of IDs from
    # content_parameterised for the selectInput
    # drop down for single vs. corpus figure,
    # need to use observe & updateSelectInput
    observe({

      rv$choices_ID <- choices_ID()

      updateSelectInput(session, "content_single_ID",
                        choices = rv$choices_ID,
                        selected= rv$choices_ID[1])
    })


    # Summary stats stuff
    total_tokens <- reactive({
      req(rv$content_tf_idf)
      rv$content_tf_idf$`Corpus token count`[1]
    })
    output$total_tokens <- renderText(total_tokens())

    mean_token_count <- reactive({
      req(rv$content_tf_idf)
      rv$content_tf_idf$`Mean token count`[1]
    })

    sd_token_count <- reactive({
      req(rv$content_tf_idf)
      sd(rv$content_tf_idf$`Token Count`)
    })

    numStop <- reactive({
      if(is.null(rv$stop_words_final)){
        return(0)
      } else {
        return(nrow(rv$stop_words_final))
      }
    })

    output$stats1 <- renderText({
      req(rv$content_tf_idf)
      req(rv$numFiles)
      paste("Corpus contains:  ", rv$numFiles, " document(s). Text tokenised by", rv$token, " with ", numStop(), " stop-words omitted.", collapse = ",")
    })
    output$stats2 <- renderText({
      paste("Total tokens in corpus: ", total_tokens(), ".  Mean token count per document is ", mean_token_count(), " with a standard deviation of ", round(sd_token_count(), 4), collapse = ",")
    })

    # Summary text for max and min document tokens
    max_min_token <- reactive({
      req(rv$content_tf_idf)
      rv$content_tf_idf %>%
        arrange(desc(`Token Count`)) %>%
        dplyr::select(ID,`Token Count`)
    })

    highest_token_count <- reactive(slice_head(max_min_token()))
    lowest_token_count <- reactive(slice_tail(max_min_token()))

    output$max_min_IDs <- renderText({
      req(rv$content_tf_idf)
      req(rv$token)
      req(highest_token_count())
      req(lowest_token_count())
      paste("Document with the highest token count of ", highest_token_count()[1,2], " ", rv$token, " was ", highest_token_count()[1,1], ".", "Document with the lowest token count of ", lowest_token_count()[1,2], rv$token, " was ", lowest_token_count()[1,1], ".", collapse = ",")
                                     })


    output$content_parameterised <- DT::renderDataTable(
      rv$content_parameterised,
       options = list(
         dom = "tfrp",
         ordering = TRUE,
         paging = TRUE,
         pageLength = 5,
         searching = TRUE
       ),
      selection = 'none',
       class = 'row-border',
       escape = T,
       rownames = F,
      filter = "none"  ,
      width = 500
    )

    # Creating content_freq(), which has frequency of words across entire corpus
    content_freq <- reactive({

      req(rv$content_parameterised)
      req(rv$content_stop_rm)

      # If the column 'Token' exists (a proxy for data being tokenised)
      if(colnames(rv$content_parameterised)[2] == "Token"){

        rv$content_parameterised %>%
          # filter(!grepl("pppp", Token, fixed = TRUE)) %>%
          # mutate(Token = str_extract(Token, "[0-9A-Za-z'\"\"]+")) %>%
          # using str_extract to get just words & numbers since UTF-8
          # encoding has things like _a_ to indicate italics etc.
          # however results in tokenisation into words, need to include
          # spaces in extract too
          count(Token, sort = T) %>%
          mutate(Token = reorder(Token, n))
      }
    })


    observe({
      rv$content_freq <- content_freq()

      # Setting default as 7
      rv$min_freq <- {
        if(is.null(input$min_freq)){
          return(30)
        } else input$min_freq
      }

    })

    word_cloud <- reactive({
      if(!is.null(rv$content_freq)){
        wordcloud2(rv$content_freq,
                   size = 1.2,
                   color= rep_len( c("black","royalblue"),
                                   nrow(rv$content_freq ) ),
                   # minRotation = -pi/2, maxRotation = -pi/2,
                   minRotation = 0, maxRotation = 0,
                   fontFamily = "sans-serif",
                   fontWeight = "200"
        )
      } else {
        return(NULL)
      }
    })
    #### Word cloud ####
    output$wordcloud <- renderWordcloud2({
      word_cloud()
    })

    observe({
      rv$title1 <- str_wrap(input$token_freq_plot_title, 50)
      rv$cap1 <- str_wrap(input$token_freq_plot_caption, 100)
    })

    # Token frequency plot for entire corpus
    # Creating frequency plot, where users can customise min
    # frequency of a token to be displayed on the plot
    token_freq_plot <- reactive({
      req(rv$content_freq)

      title <- ifelse(is.null(rv$title1), rv$title1, "Most common tokens by frequency")
      caption <- ifelse(is.null(rv$cap1), rv$cap1,
                        "Figure 1: Frequency of most common tokens across the corpus.")

      rv$content_freq %>%
        filter(n >= rv$min_freq) %>%
        ggplot(aes(n,Token)) +
        geom_col(fill = "royalblue") +
        labs(x = "Frequency", y = NULL,
             caption = caption) +
        ggtitle(title) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0, size = 18,
                                        vjust = 1),
              text = element_text(size = 14),
              plot.caption = element_text(hjust=0, vjust = -1.2,
                                          size = 15, face = "italic"))

    })

    output$word_freq <- renderPlot({

      req(token_freq_plot())
      token_freq_plot()

    })


    # First get the ID of the single file to compare with
    observe({
      rv$content_single_ID <- input$content_single_ID
    })

    # To plot the comparison of frequency of tokens between a single vs. corpus,
    # proportion is used for this, and ultimately content_parameterised is
    # manipulated to calculate proportions for the single file, corpus and then these
    # are combined to create content_corr_freq

    # Creating content_freq_single tibble with just single selected file's data by
    # filtering content_parameterised ID column
    content_param_single <- reactive({

      req(rv$is_tokenised)
      req(rv$content_parameterised)
      # If a user uploads primary, tokenises, then goes back to upload
      # secondary, joins that, and tokenises it, the selected rv$content_single_ID
      # from the drop down menu for the frequency comparison data may not be 
      # present in rv$content_parameterised after the join. So need to check
      # that the selected ID is present in the data
      req(rv$content_single_ID)
      # Require that filtering by selected ID doesn't return nothing (so a valid
      # ID has been selected). Test written as similar as possible to what happens
      # a few lines down
      req(nrow(filter(rv$content_parameterised, ID == rv$content_single_ID)) > 0)

      validate(need(rv$is_tokenised, NULL))
      
      print("Printing content parameterised")
      print(rv$content_parameterised)
      
      dat <- rv$content_parameterised %>%
        filter(ID == rv$content_single_ID) %>%
        count(Token, sort=T) %>%
        mutate(Proportion = n/sum(n)) %>% # proportion calc
        mutate(Source = rv$content_single_ID)
      
      print("Content param single created:")
      print(dat)
      
      dat

    })

    # Filtering content_parameterised to be the rest of the corpus
    content_param_rest <- reactive({

      req(rv$content_single_ID)
      req(rv$content_parameterised$Token)

      validate(need(rv$is_tokenised, NULL))

      if(rv$is_tokenised && unique(rv$content_parameterised$ID) > 1){
        rv$content_parameterised %>%
          filter(ID != rv$content_single_ID) %>%
          count(Token, sort = T) %>%
          mutate(Proportion = n/sum(n)) %>% # proportion calc
          mutate(Source = "Corpus")
      } else if(!rv$is_tokenised &&
                unique(rv$content_parameterised$ID) == 1) {
        content_param_single()
      }

    })

    # Creating final dataset of widened data for both single and rest to plot
    # Creates two proportions for each token, from single essay and
    # entire corpus
    content_comp_freq <- reactive({

      req(rv$is_tokenised)
      req(content_param_single())
      req(content_param_rest())

      content_comp_freq <-
        bind_rows(content_param_single(), content_param_rest()) %>%
        dplyr::select(-n) %>% # have to deselect n or widening will fail
        pivot_wider(names_from = Source, values_from = Proportion)

      # Assigning col names
      colnames(content_comp_freq) <- c("Token", "Single", "Corpus")
      print(colnames(content_comp_freq))
      print(content_param_single())
      print(content_param_rest())
      print(content_comp_freq)

      # Calculating difference in proportions of each token
      # between the single file and rest of corpus.
      # This delta proportion is used to colour points in plot.
      # If just have one text, thus the data excluding said text
      # consists of nothing, the difference in proportions b/t
      # itself and the rest of the corpus (itself) is 0.
      if(nrow(content_param_rest()) == 0){

        # if there is only one document the correlation test fails
        # So corpus is just the single document
        content_comp_freq <- content_comp_freq %>%
          mutate(Diff = abs(Single - Single),
                 Corpus = Single)
        return(content_comp_freq)
      }

      req(content_comp_freq)
      content_comp_freq <- content_comp_freq %>%
        mutate(Diff = abs(Single - Corpus))
      return(content_comp_freq)
    })

    # Observe to assign reactives made for corr plot to rv list
    # If content is not tokenised, assign NULL to reactive
    observe({

      if(!rv$is_tokenised){
        rv$content_comp_freq <- NULL
      }

      req(content_comp_freq())
      rv$content_comp_freq <- content_comp_freq()

    })

    # Text output to let people know to tokenise their data first
    # if data is not tokenised let user know
    # I cannot find a way to make this easily reproducible.
    # Calling the same ns("tokenise_please") breaks the app's conditional panels.
    # Tried renderUI() in a conditional panel but the condition for the conditional
    # panel needs to check if rv$is_tokenised is true, which doesn't seem to be
    # possible. So for now, as much as it pains me, the same code is repeated with
    # differing output IDs.
    output$tokenise_please <- renderText({
      if(!rv$is_tokenised) "Tokenise your data to report on it."
    })
    output$tokenise_please2 <- renderText({
      if(!rv$is_tokenised) "Tokenise your data to report on it."
    })
    output$tokenise_please3 <- renderText({
      if(!rv$is_tokenised) "Tokenise your data to report on it."
    })
    output$tokenise_please4 <- renderText({
      if(is.null(rv$content_freq)) "Tokenise your data to report on it."
    })
    output$tokenise_please5 <- renderText({
      if(is.null(rv$content_freq)) "Tokenise your data to report on it."
    })
    output$tokenise_please6 <- renderText({
      if(!rv$is_tokenised) "Tokenise your data to report on it."
    })


    ### Single vs. corpus frequency plot ####

    observe({
      rv$title2 <- str_wrap(input$comp_freq_plot_title, 50)
      rv$cap2 <- str_wrap(input$comp_freq_plot_caption, 100)
    })

    comp_freq_plot <- reactive({
      req(rv$content_comp_freq)

      title2 <- ifelse(is.null(rv$title2), rv$title2,
                      "Comparison of token frequeny (single vs. corpus)")
      cap2 <- ifelse(is.null(rv$cap2), rv$cap2,
                        "Figure 2: Comparison of token frequencies between a single selected text and the current corpus.")

      ggplot(rv$content_comp_freq, aes(x = Corpus, y =  Single,
                                       color = Diff)) +
        geom_abline(color = "gray40", lty = 2) +
        geom_point(alpha = 0.6, size = 1) +
        geom_text(aes(label = Token), alpha = 1,
                  check_overlap = T, cex = 5,
                  nudge_y = 0.1,
                  size = 13) +
        xgx_scale_x_log10(labels = percent_format(),
                          breaks = c(0.001, 0.01, 0.03, 0.1, 0.3, 0.75),
                          limit = c(0.0009, 1.0)) +
        xgx_scale_y_log10(labels = percent_format(),
                          breaks = c(0.001, 0.01, 0.03, 0.1, 0.3, 0.75),
                          limit = c(0.0009, 1.0)) +
        scale_color_gradient(limits = c(0, 0.06),
                             low = "lightgray", high = "royalblue") +
        theme_bw() +
        theme(legend.position="right",
              text = element_text(size = 14)) +
        theme(plot.title = element_text(size = 18, hjust = 0,
                                        vjust = 1),
              plot.caption = element_text(hjust=0, vjust = -1.2,
                                          size = 15, face = "italic")) +
        labs(y = rv$content_single_ID,
             x = "Corpus proportions",
             col = "Prop. (%) difference",
             caption = cap2) +
        ggtitle(title2)
    })

    output$comp_freq <- renderPlot({
      req(rv$content_comp_freq)
      comp_freq_plot()
    })

    observe({

      req(rv$content_parameterised)
      req(rv$content_comp_freq$Single)
      req(rv$content_comp_freq$Corpus)

      rv$cor_test <- cor.test(rv$content_comp_freq$Single,
                              rv$content_comp_freq$Corpus)

    })

    output$cor_value <- renderText({

      if(!is.null(rv$cor_test)){
        text <- paste(c("The estimate of correlation obtained
                 from a product-moment
                 correaltion test is ",
                 format(round(rv$cor_test$estimate, 3)),
                 " with a 95% CI of (",
                 format(round(rv$cor_test$conf.int[1], 3)),
                 ", ",
                 format(round(rv$cor_test$conf.int[2], 3)),
                 ").")
                 )
        return(text)

      } else {
        return(NULL)
      }

    })


    #### Zipf visualisations ####
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

      req(rv$content_parameterised$Token)

      # Only perform is data is tokenised
      if(rv$is_tokenised){
        token_counts <- rv$content_parameterised %>%
          count(ID, Token, sort = T) %>%
          rename("Freq." = "n")

        token_count <- token_counts %>%
          group_by(ID) %>%
          summarise(`Token Count` = sum(`Freq.`)) %>%
          mutate(`Mean token count` =
                   floor(mean(`Token Count`)),
            `Corpus token count` = sum(`Token Count`))

        content_zipf <-
          left_join(token_counts, token_count, by = c("ID" = "ID")) %>%
          group_by(ID) %>%
          mutate(Rank = row_number(),
                 `Term freq.` = `Freq.`/`Token Count`) %>%
          ungroup()

        content_zipf
      }
    })

    # Assigning to reactive value list
    observe({
      rv$content_zipf <- content_zipf()
    })


    # Zipf's law visualisation
    # Plotting rank on x and tf on y on log scales

    observe({
      rv$title3 <- str_wrap(input$zipfs_plot_title, 50)
      rv$cap3 <- str_wrap(input$zipfs_plot_caption, 100)
    })


    zipfs_plot <- reactive({
      req(rv$content_zipf)

      title3 <- ifelse(is.null(rv$title3), rv$title3,
                       "Zipf's law (rank vs. term frequency)")
      cap3 <- ifelse(is.null(rv$cap3), rv$cap3,
                     "Figure 3: A demonstration of Zipf's Law: term frequency against rank on log-log scales.")

      rv$content_zipf %>%
        ggplot(aes(Rank, `Term freq.`, color = ID)) +
        geom_line(alpha = 0.7, show.legend = F) +
        xgx_scale_x_log10() + # breaks = c(1, 10, 100, 1000, 10000)
        xgx_scale_y_log10() + #breaks = c(0.001, 0.01, 0.1, 1.0)
        labs(x = "Rank (log)", y = "Term frequency (log)",
             caption = cap3) +
        ggtitle(title3) +
        scale_color_manual(values =
                             rep(c("royalblue", "black", "seagreen"),
                                 nrow(rv$content))
        ) +
        theme_bw() +
        theme(plot.title = element_text(size = 18, hjust = 0,
                                        vjust = 1),
              text = element_text(size = 14),
              plot.caption = element_text(hjust=0 , vjust = -1.2,
                                          size = 14, face = "italic"))
    })

    output$zipfs_plot <- renderPlot({

      req(zipfs_plot())

      zipfs_plot()

    })

    ##### Tf-idf visualisations ####

    # First creating tf-idf dataset
    content_tf_idf <- reactive({

      req(rv$content_zipf)

      rv$content_zipf %>%
        bind_tf_idf(Token, ID, `Freq.`) %>%
        arrange(desc(tf_idf))
    })


    observe({
      rv$content_tf_idf <- content_tf_idf()

      # Get the IDs of the files to calc tf-idf of
      rv$tf_idf_IDs <- list(input$checkbox_content_ID,
                                   input$checkbox_content_ID2)

    })

    # Updating the checkbox inputs to the unique IDs of data uploaded
    observe({

      updateSelectInput(session, "checkbox_content_ID",
                        choices = rv$choices_ID,
                        selected= rv$choices_ID[1])

      updateSelectInput(session, "checkbox_content_ID2",
                        choices = rv$choices_ID,
                        selected= rv$choices_ID[1])
    })

    observe({
      rv$title4 <- str_wrap(input$tf_idf_plot_title, 50)
      rv$cap4 <- str_wrap(input$tf_idf_plot_caption, 100)
    })

    tf_idf_plot <- reactive({

      req(rv$content_parameterised)
      req(rv$content_tf_idf)
      req(rv$tf_idf_IDs)

      title4 <- ifelse(is.null(rv$title4), rv$title4,
                       "Highest tf-idf tokens of selected documents")
      cap4 <- ifelse(is.null(rv$cap4), rv$cap4,
                     "Figure 4: Most important terms according to term frequency-inverse document frequency analysis.")

      rv$content_tf_idf %>%
        # filtering to just be IDs selected
        filter(ID %in% rv$tf_idf_IDs) %>%
        group_by(ID) %>%
        slice_max(tf_idf, n = 12) %>%
        ungroup() %>%
        ggplot(aes(tf_idf, fct_reorder(Token, tf_idf), fill = ID)) +
        geom_col(show.legend = F)+
        labs(x = "tf-idf", y = NULL,
             caption = cap4) +
        facet_wrap(~ID, ncol = 2, scales = "free") +
        ggtitle(title4) +
        theme_bw() +
        scale_fill_manual(values=c("royalblue", "seagreen")) +
        theme(plot.title = element_text(size = 18, hjust = 0,
                                        vjust = 1),
              text = element_text(size = 14),
              plot.caption = element_text(size = 14, face = "italic",
                                          hjust = 0, vjust = -1.2))
    })


    # Plotting tf-idf plot, facet wrap by ID too
    output$tf_idf_plot <- renderPlot({

      req(rv$tf_idf_IDs)
      req(rv$content_tf_idf)

      tf_idf_plot()
    })


    #### Rendering images ####

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


    # Creating parameters to send to Report.qmd
    observe({

      # Creating rv list to pass into mini report, is just everything

      mini_rv$content <- rv$content
      mini_rv$stop_words_final<- rv$stop_words_final
      mini_rv$token<- rv$token
      mini_rv$other_token<- rv$other_token
      mini_rv$word_cloud <- word_cloud()
      mini_rv$token_freq_plot <- token_freq_plot()
      mini_rv$comp_freq_plot<- comp_freq_plot()
      mini_rv$cor_test<- rv$cor_test
      mini_rv$zipfs_plot<- zipfs_plot()
      mini_rv$content_tf_idf<- rv$content_tf_idf
      mini_rv$tf_idf_IDs<- rv$tf_idf_IDs
      mini_rv$tf_idf_plot<- tf_idf_plot()

    }) # end observe


    # Creating rvs list to pass to final report, need to check if user
    # wants each one included first
    observe({

      # Adding to rv list so can communicate between models to isolate
      # reporting tab but still use tickboxes to include what
      rv$word_cloud <- word_cloud()
      rv$token_freq_plot <- token_freq_plot()
      rv$comp_freq_plot <- comp_freq_plot()
      rv$zipfs_plot <- zipfs_plot()
      rv$tf_idf_plot <- tf_idf_plot()

    })

    ####  Interactive data table ####

    output$interact_DT <- DT::renderDataTable(
      rv$content_tf_idf %>%
        dplyr::select(-tf),
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

    output$download_tf_idf_plot <- downloadHandler(
      filename = function() {
        paste("Tf-idf Figure.png")
      },
      content = function(file){
        ggsave(file, tf_idf_plot(),
               dpi = input$tf_idf_plot_dpi,
               height = input$td_idf_plot_h,
               width = input$tf_idf_plot_w,
               units = "cm")
      }
    )

    #### Quick report generation ####

    # Generating quick report
    output$download_quick_report <- downloadHandler(
      filename = "Report.docx",
      content = function(file) {

        # generating report with rmarkdown::render()
        rmarkdown::render("www/Report_word.qmd",
                          output_file = file,
                          params = list(report_rv = mini_rv)
        )
      }
    )
    
    } # end reporting server
  )
}

