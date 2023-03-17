
######## Statistical Analysis Tab Module ############

#### Stats Tab Module UI ####
statsUI <- function(id){
  ns <- NS(id)
  tagList(
    
    fluidPage(
      wellPanel(
        h1("0X | Statistical Analysis"),
        em("Perform exploratory data analyses, regression analyses and model checking through a customisable workflow."),
      ),
      
      sidebarLayout(
        sidebarPanel(width = 3, 
                     
                     h3("Data selection"),
                     p("Select a dataset to perform statistical analyses on:"),
                     selectInput(ns("content_stats_choose"), 
                                 label = NULL, 
                                 choices = list(
                                   "Raw submitted data" = "submitted", 
                                   "Parameterised data" = "parameterised", 
                                   "Tf-idf data" = "tfidf"
                                 )),
                     hr(),
                     
                     h3("Data filtering"),
                     p("Use the interactive datatable to filter attributes of your text data. Save your filtered data to use in further analyses."),
                     
                     fluidRow(
                       column(width = 6, 
                              actionButton(ns("submit_filters"), 
                                           label = "Submit filters", 
                                           class = "btn-primary")
                              ),
                       column(width = 6, 
                              actionButton(ns("undo_filters"), 
                                           label = "Undo filters", 
                                           class = "btn-danger")
                              )
                     ),
                     
                     hr(),
                     
                     h3("Summary statistics"),
                     p("Render summary statistics for your selected data using the checkbox option below."),
                     checkboxInput(ns("show_eda_summary_stats"), 
                                   label = "Show summary statistics")
        ),
        
        mainPanel(width = 9,
          
          wellPanel(
            h3("Selected Data"),
            
            uiOutput(ns("content_stats_display"))
            
          ),

        ),
      ),
      
      wellPanel(
        h2("Exploratory Data Analysis"),
        hr(),
        
        fluidRow(
          column(width = 8,
                 
                 wellPanel(
                   plotOutput(ns("eda_hist_plot"))
                 ),
                 
                 
          ), # end column
          column(width = 4, 
                 
                 h3("Histogram"),
                 
                 # Widgets to plot hist and customize
                 selectInput(ns("eda_hist_var"), 
                             label = "Select a variable:", 
                             choices = 
                               list("N/A" = 
                                      "N/A")
                 ),
                 
                 # Widgets to customise breaks in histogram
                 selectInput(ns("eda_hist_breaks"), 
                             label = "Choose a method to calculate the number of breaks:", 
                             choices = list(
                               "Sturges (default)" = "Sturges", 
                               "Scott" = "Scott", 
                               "Freedman-Diaconis" = "Freedman-Diaconis", 
                               "Custom" = "Custom"
                             )),
                 conditionalPanel(paste0("input['", ns("eda_hist_breaks"), 
                                         "'] == 'Custom' "), 
                                  numericInput(ns("eda_hist_breaks_custom"), 
                                               label = "Enter a custom number of breaks:", 
                                               value = 30)
                 ),
                 
                 checkboxInput(ns("include_density_curve"),
                               label = "Overlay fitted density curve", 
                               value = FALSE),
                 
                 hr(),
                 h4("Transformations"),
                 p("Apply a mathematical transformation to the selected variable. Saving a transformation will append a column to your data, not overwrite it."),
                 
                 selectInput(ns("eda_transformation"), 
                             label = NULL, 
                             choices = list(
                               "None" = "none", 
                               "Natural Logarithm (ln)" = "log", 
                               "Square Root" = "sqrt", 
                               "Arc-sine" = "arcsine",
                               "Reciprocal" = "recip"
                             ), 
                             selected = "none"),
                 
                 actionButton(ns("eda_save_transformed_var"),
                              label = "Save transformation",
                              class = "btn-success"),
                 
                 
                
            ), # end column
 
        ), # end fluid row
        
        hr(),
        
        fluidRow(
          column(width = 3, 
                 
                 h3("Normality tests"), 
                 p("Explore whether a selected variable approximates a theoretical normal distribution."),
                 hr(),
                 p("Select a variable:"),
                 
                 # Widgets to plot hist and customize
                 selectInput(ns("eda_normality_var"), 
                             label = NULL, 
                             choices = 
                               list("N/A" = 
                                      "N/A")
                 ),
                 
                 radioButtons(ns("normality_test_type"), 
                              label = "Type of statistical test:", 
                              choices = list("Shapiro-Wilk (default)" = "shapiro", 
                                             "Anderson-Darling (n > 500)" = "anderson"))
                 
                 ), # end normality testing column
          column(width = 5, 
                 plotOutput(ns("eda_qqplot"))
                 ),
          column(width = 4, 
                 h3("Statistical Test for Normality"),
                 p("Used to test whether the selected variable approximates a normal distribution. A significantly low p-value indicates the null hypothesis of normality can be rejected."),
                 hr(),
                 
                 # Render the different statistical tests for normality depending on user
                 # selection
                 conditionalPanel(
                   condition = paste0("input['", ns("normality_test_type"), 
                                      "'] == 'shapiro' "),
                   verbatimTextOutput(ns("eda_shapiro_wilk")),
                 ),
                 conditionalPanel(
                   condition = paste0("input['", ns("normality_test_type"), 
                                      "'] == 'anderson' "),
                   verbatimTextOutput(ns("eda_anderson_darling"))
                 ),
                 
                 ),
        ) # end fluid row

      ), # end EDA well panel
      
      fluidRow(
          box(title = NULL, 
              status = "success", 
              solidHeader = T, 
              collapsible = T,
              width = 12,
              
              plotlyOutput(ns("corr_plot")),
              
          ), # end box
          
          
      ), # end corr plot fluid row
      
      wellPanel(
        h2("Regression Analysis"), 
        p("Perform regression analysis to gain a deeper understanding of the relationships between your variables and to uncover patterns and insights in your data."),
        em("Disclaimer: Not all regression models make sense, and the validity of results from a regression model depends on several factors. Please consider carefully whether the models you fit are appropriate for your data. "),
        hr(), 
        
        fluidRow(
            column(
              width = 4,
              
              h3("Regression Type"),
              p("Select a type of regression to show a brief summary of the method and perform an analysis"),
              
              hr(),
              radioButtons(ns("regression_type"), 
                           label = NULL,
                           choices = list("Linear" = "linear", 
                                          "Logistic" = "logistic", 
                                          "Poisson" = "poisson", 
                                          "Mixed effects" = "mixed"
                           )
              ),
              
            ), # end column
            
            column(
              width = 8,
              box(
                status = "primary",
                width = 12,
                
                # Here render UI based on radio button selection
                conditionalPanel(
                  condition = paste0("input['", ns("regression_type"), 
                                     "'] == 'linear' "),
                  uiOutput(ns("info_linear"))
                ),
                conditionalPanel(
                  condition = paste0("input['", ns("regression_type"), 
                                     "'] == 'logistic' "),
                  uiOutput(ns("info_logistic"))
                ),
                conditionalPanel(
                  condition = paste0("input['", ns("regression_type"), 
                                     "'] == 'poisson' "),
                  uiOutput(ns("info_poisson"))
                ),
                conditionalPanel(
                  condition = paste0("input['", ns("regression_type"), 
                                     "'] == 'mixed' "),
                  uiOutput(ns("info_mixed"))
                ),
                # Add other regression types
                
              ), # end information box
            ), # end column
          
        ), # end regression type & info fluid row
        
        hr(), 
        
        fluidRow(
          column(width = 4, 
                 # Here render UI based on radio button selection
                 # Rendering same ui to choose vars for liner, log 
                 # and poisson, however different ui for mixed 
                 # regression to choose random & fixed effects
                 uiOutput(ns("perform_regression"))
                 ),
          
          column(width = 8, 
                   wellPanel(
                     tabsetPanel(
                       tabPanel("Tabularised summary", uiOutput(ns("lin_log_pois_res"))), 
                       tabPanel("Raw model summary", 
                                verbatimTextOutput(ns("reg_result_raw")))
                     ),
                   )
                 )
          
        ) # end fluid row
      ), # end regression analysis well panel 
      
      verbatimTextOutput(ns("test_stats")),
      
      wellPanel(
        
        h2("Model Checking"),
        p("Check your model meets required assumptions regarding residuals and variances."),
        
        wellPanel(
          plotOutput(ns("residual_plots"))
        ),
        
      ), # end model checking well panel
      
    
      wellPanel(
        
        h2("Further Statistical Testing"),
        p("Perform analyses of variances, t-tests, and/or Chi-squared tests based on previously specified formulas."),
        hr(),
        
         navlistPanel(
           tabPanel(
             
             "Analysis of Variances (ANOVA)", 
                    h3("Analysis of Variances (ANOVA)"),
                    p("The ANOVA test is used to determine if there is a statistically significant difference between the means of three or more independent groups."),
                    hr(),
             
               tabsetPanel(
                 tabPanel("Tabularised summary", tableOutput(ns("anova_table"))), 
                 tabPanel("Raw model summary", verbatimTextOutput(ns("anova_res_raw")))
               ),
           ), 
           tabPanel("T-test", 
                    h3("T-test"), 
                    p("A t-test is a statistical test used to determine if their is a significant difference in the means of two independent groups being compared."),
                    hr(),
                    tabsetPanel(
                      tabPanel("Tabularised summary", htmlOutput(ns("t_test_res_table"))), 
                      tabPanel("Raw model summary", verbatimTextOutput(ns("t_test_res_raw")))
                    ),
           ), 
           tabPanel("Chi-square test", 
                    h3("Chi-square test"), 
                    p("The Chi-square test is used to determine whether there is a significant association between two categorical variables."),
                    hr(),
                    uiOutput(ns("perform_chisq"))
                    
           ), 
           tabPanel("Non-parametric tests", 
                    
                    h3("Non-parametric Tests"),
                    p("Non-parametric tests do not require any assumptions to be made about data being analyzed, so can be used to analyze data that is not normally distributed or does not meet the assumptions of parametric tests."),
                    hr(),
                    
                    tabsetPanel(
                      tabPanel("Kruskal-Wallis", 
                               
                               h3("Kruskal-Wallis"), 
                               p("The Kruskal-Wallis test is used to test whether there are statistically significant differences between the medians of two or more independent groups. As this test is non-parametric, it does not assume normality of residuals.
The parametric equivalent of the Kruskal-Wallis is the one-way ANOVA."),
                               
                               verbatimTextOutput(ns("kw_res")),
                               
                               ), 
                      tabPanel("Welch one-way test", 
                               
                               h3("Welch one-way test"),
                               p("The Welch one-way test is used to test if there is a statistically significant difference in the means of two or more groups. This test is useful when the assumption of equal variances between groups is violated."),
                               p("The parametric equivalent of the Welch one-way test is the independent samples t-test."),
                               
                               verbatimTextOutput(ns("welch_res")),
                               
                               ),
                      
                      tabPanel("Mann-Whitney U Test", 
                               
                               h3("Mann-Whitney U Test"),
                               p("The Mann-Whitney U Test (also know as the Wilcoxon rank-sum test) is used to compare two independent samples which are not normally distributed and of small size (n < 30)"),
                               p("The parametric equivalent of this test is the two-sample independent t-test."),
                               
                               verbatimTextOutput(ns("mann_wil_res")),
                               
                      ),
                      
                      
                    ), # end tabset panel of non-para tests
                  
           ), 
         ),
      )
      
      
    ) # end fluidPage
  ) # end tagList
} # end module UI

  
### Stats Tab Module Server-side logic ####
statsServer <- function(id, rv = rv){
  moduleServer(
    id, 
    function(input, output, session){

      #### Creating contents_stats to be used  ####
      # Content stats is rendered to be whatever is selected in drop-down
      content_stats <- reactive({
        req(rv$content)
        
        if(input$content_stats_choose == "submitted"){
          rv$content %>%
            clean_names()
        } else if(input$content_stats_choose == "parameterised"){
          req(rv$content_parameterised)
          rv$content_parameterised %>%
            clean_names()
        } else if(input$content_stats_choose == "tfidf"){
          req(rv$content_parameterised)
          req(rv$content_tf_idf)
          req(rv$is_tokenised)
          rv$content_parameterised %>%
            full_join(rv$content_tf_idf, by = c('ID', 'Token')) %>%
            dplyr::select(-`Term freq.`, -`Mean token count`, -`Corpus token count`) %>%
            clean_names()
        }
      })
      
      # Ensuring rv$content_stats updated to content_stats() created above
      observe({
        req(content_stats())
        rv$content_stats <- content_stats()
      })
      
      # Table output for raw content stats
      output$content_stats_raw <- DT::renderDataTable(
        rv$content_stats,
        filter = 'top',
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
      
      # Glimpse from finalfit package used to show summary stats
      eda_summary_stats <- reactive({
        req(rv$content_stats)
        finalfit::ff_glimpse(rv$content_stats)
      })
      
      # Saving only the summary stats for continuous variables
      observe({
        req(eda_summary_stats())
        rv$content_stats_summary <- eda_summary_stats()$Continuous
      })
      
      # Table output for summary stats for content stats selected
      output$content_stats_summary <- renderDataTable({
        DT::datatable(rv$content_stats_summary,
                      rownames=FALSE, 
                      colnames = c("", "N", "Missing N", "Missing %", "Mean", "SD", "Min", "25% quartile", "Median", "75% quartile", "Max"),
                      options = list(dom = 't', 
                                     scrollX = TRUE, 
                                     paging=FALSE,
                                     fixedColumns = list(leftColumns = 1, 
                                                         rightColumns = 0),
                                     searching = FALSE
                      )
        )
      })
      
      # Generating ui for content stats display. If user selects checkbox to 
      # show summary stats, this will be rendered instead of raw data
      output$content_stats_display <- renderUI({
        validate(
          need(
            rv$content_stats,
            "Select and submit text data to continue"
          ),
          
          errorClass = "validation-red")
        
        ns <- NS(id)
        raw <- tagList(
          DT::dataTableOutput(ns("content_stats_raw"))
        )
        
        summary_stats <- tagList(
          DT::dataTableOutput(ns("content_stats_summary"))
        )
        
        # Rendering either raw or summary of content stats depending on input
        if(input$show_eda_summary_stats){
          summary_stats
        } else {
          raw
        }
        
      })
      
      #### Filtering data #####
      # When submit filters clicked, subset content_stats by filtered rows
      observeEvent(input$submit_filters, {
        
        req(rv$content_stats)
        
        # Creating temporary storage hold 
        temp <- rv$content_stats
        
        # Subsetting content_stats by filters
        rv$content_stats <- 
          temp[input[["content_stats_raw_rows_all"]], ]
        
      })
      
      # When undo filters button clicked, revert content_stats to original
      observeEvent(input$undo_filters, {
        
        req(content_stats()) # require original reactive value
        
        rv$content_stats <- content_stats() # revert to original 
        
      })
      
      
      #### Exploratory data analysis ####
      
      # Updates dropdown for histogram to column names as soon 
      # as content stats rendered
      # Rendering last variable in content_stats since this is most
      # likely what user wants to see, not ID (not numeric)
      # Also means that when transformation saved it is rendered immediately
      observeEvent(!is.null(rv$content_stats), {
        req(rv$content_stats)
        updateSelectInput(session, "eda_hist_var",
                          choices = colnames(rv$content_stats), 
                          selected = 
                            colnames(rv$content_stats)[ncol(rv$content_stats)])
        
        updateSelectInput(session, "eda_normality_var",
                          choices = colnames(rv$content_stats), 
                          selected = 
                            colnames(rv$content_stats)[ncol(rv$content_stats)])
      })

      # Plotting a histogram of selected variable

      # Token frequency plot for entire corpus
      # Creating frequency plot, where users can customise min
      # frequency of a token to be displayed on the plot
      eda_hist_plot_breaks <- reactive({
        switch(input$eda_hist_breaks,
               "Sturges" = "Sturges",
               "Scott" = "Scott",
               "Freedman-Diaconis" = "Freedman-Diaconis",
               "Custom" = input$eda_hist_breaks_custom)
      })
      
      # Reactive value for the variable to plot, with or without transformation
      # Passing to apply_transformation function defined in utils.R which 
      # applies the given transformation based on inputted variable and returns
      # transformed vector
      eda_hist_plot_var <- reactive({
        selected_var <- input$eda_hist_var
        transformation <- input$eda_transformation
        
        apply_transformation(var = rv$content_stats[[selected_var]],
                             transformation = transformation)
        
      })

      # Validate statements to ensure numeric value selected for histogram
      # Not neccessary to check if tokenised anymore
      eda_hist_plot <- reactive({
        req(rv$content_stats)

        validate(
          need(
            is_numeric(rv$content_stats[[input$eda_hist_var]]),
            "Select a numeric variable."
          ),
          errorClass = "validation-red")
        
        validate(
          need(
            eda_hist_plot_var(),
            "Transformation invalid."
          ),
          errorClass = "validation-red")

        # Creating histogram. If include density curve is selected, 
        # the probability / proportion density is 
        # plotted instead of frequency.
        hist(eda_hist_plot_var(),
             breaks = eda_hist_plot_breaks(),
             main = paste0("Histogram of ", input$eda_hist_var),
             xlab = input$eda_hist_var,
             col = "white",
             prob = input$include_density_curve)

      }) # end reactive val eda hist plot

      # If checkbox to include density curve is checked, the 
      # lines function is used to render on top of histogram
      output$eda_hist_plot <- renderPlot({
        req(eda_hist_plot())

        eda_hist_plot()
        
        if(input$include_density_curve == TRUE){
          
          validate(
            need(
              try(density(eda_hist_plot_var())),
              "Density curve cannot be plotted due to missing values."
            ),
            errorClass = "validation-red")
          
          lines(density(eda_hist_plot_var()),
                col = "royalblue")
        }
      })
      
      # When button clicked to save transformation...
      observeEvent(input$eda_save_transformed_var, {
        req(rv$content_stats)
        req(eda_hist_plot_var())
        
        if(input$eda_transformation != "none"){
          # Create copy of content_stats to avoid rv$content_stats calling itself
          temp <- cbind(rv$content_stats, 
                        "Transformed var" = eda_hist_plot_var())
          rv$content_stats <- temp
          
          # Renaming column name to transformation and variable used 
          colnames(rv$content_stats)[which(names(rv$content_stats) == "Transformed var")] <- paste0(input$eda_transformation, "_", input$eda_hist_var)
        }
        
      }) # end observe event save transformed var
      
      # Isolating and saving the selected variable to use in normality testing
      eda_normality_var <- reactive({
        req(rv$content_stats)
        var <- rv$content_stats[[input$eda_normality_var]]
        
        # ensuring variable is numeric
        validate(
          need(
            is_numeric(var),
            "Select a numeric variable."
          ),
          errorClass = "validation-red")
        
        var
        
      })
      
      # Rendering qqplot
      output$eda_qqplot <- renderPlot({
        req(eda_normality_var())
        qqnorm(eda_normality_var())
        qqline(eda_normality_var())
        
      })
      
      #### Normality statistical tests ####
      # Saving shapiro wilk test result
      shapiro_wilk_res <- reactive({
        req(eda_normality_var())
        req(length(eda_normality_var()) < 5000)
        
        shapiro.test(eda_normality_var())
      })
      
      # Saving Anderson-darling test result
      anderson_darling_res <- reactive({
        req(eda_normality_var())
        req(length(eda_normality_var()) > 7)
        
        nortest::ad.test(eda_normality_var())
      })
      
      # saving SW & AD results in reactive value list
      observe({
        rv$anderson_darling_res <- anderson_darling_res()
        rv$shapiro_wilk_res <- shapiro_wilk_res()
      })
      
      # Rendering table with shapiro-wilk result
      output$eda_shapiro_wilk <- renderPrint({
        
        # If SW length too high thus null, give user instructions 
        ifelse(length(eda_normality_var()) > 5000, 
               paste0("n > 5000, use Anderson-Darling test for normality."), 
               rv$shapiro_wilk_res)
      })
      
      # Rendering table with Anderson-darling result
      output$eda_anderson_darling <- renderPrint({
        rv$anderson_darling_res
      })
      
      #### Information on regression types ####
      output$info_linear <- renderUI({
        ns <- NS(id)
        tagList(
          h3("Linear Regression"),
          p("Linear regression is a statistical method used to predict a continuous outcome variable based on one or more predictor variables.")
        )
      })
      
      output$info_logistic <- renderUI({
        ns <- NS(id)
        tagList(
          h3("Logistic Regression"),
          em("Information regarding logistic regression...")
        )
      })
      
      output$info_poisson <- renderUI({
        ns <- NS(id)
        tagList(
          h3("Poisson Regression"),
          em("Information regarding poisson regression...")
        )
      })
      
      output$info_mixed <- renderUI({
        ns <- NS(id)
        tagList(
          h3("Mixed Effects Regression"),
          em("Information regarding mixed effects regression..."), 
          tags$a(href = "https://mspeekenbrink.github.io/sdam-r-companion/linear-mixed-effects-models.html")
        )
      })
      
      
      #### Perform regression ####
      # RenderUI to only render when linear, log or poisson regression option chosen
      output$perform_regression <- renderUI({
        req(rv$content_stats)
        
        ns <- NS(id)
        tagList(
          
          # Select input for dependent (response) variable
          varSelectInput(ns("dependent"), 
                         label = "Dependent (response) variable:", 
                         rv$content_stats, 
                         selected = NULL, 
                         multiple = FALSE
          ),
          
          # Select input for independent predictor variables
          varSelectInput(ns("independents"), 
                         label = "Independent (predictor) variable(s):", 
                         rv$content_stats, 
                         selected = NULL, 
                         multiple = TRUE
                         ),
          
          checkboxInput(ns("include_interaction_lin_log_pois"), 
                        label = "Include interaction (optional)"
                        ),
          
          # If include interaction box ticked, 
          conditionalPanel(
            condition = paste0("input['", ns("include_interaction_lin_log_pois"), 
                               "'] == true "),
            varSelectInput(ns("interactions"), 
                           label = "Interacting variables:", 
                           rv$content_stats, 
                           selected = NULL, 
                           multiple = TRUE
            ),
          ),
          
          conditionalPanel(
            condition = paste0("input['", ns("regression_type"), 
                               "'] == 'mixed' "),
            uiOutput(ns("perform_mixed"))
          ),

          
          p("Selected regression formula:"),
          verbatimTextOutput(ns("formula_reg")),
          
          # Action button to actually perform regression once clicked
          actionButton(ns("submit_regression"), 
                       label = "Submit", 
                       class = "btn-success")
  
        )
      }) # end renderUI 
      
      
      # Creating formulas for regression using formula() function defined in R/utils.R
      # returns formula to pass into perform_regression() function also in utils.R
      formula_reg <- reactive({
        req(rv$content_stats)
        req(input$dependent)
        req(input$independents)
        
        # Remove interactions if not ticked to include
        if(input$include_interaction_lin_log_pois == FALSE){
          interactions <- NULL
        } else {
          interactions <- input$interactions
        }
        
        # If checkbox to include interactions not checked, don't include
        if(input$regression_type == "mixed"){
          formula(input$regression_type, input$dependent, input$independents, 
                  interactions, input$mixed_random_slopes_1, 
                  input$mixed_random_intercept_1, input$mixed_random_slopes_2, 
                  input$mixed_random_intercept_2)
        } else {
          formula(input$regression_type, input$dependent, input$independents, 
                  interactions)
        }
      })

      
      # Render text to display selected formula
      output$formula_reg <- renderPrint({
        formula_reg()
      })
      
      # When submit regression clicked, validate that regression is 
      # valid with try in need statement.
      # If error produced, indicate invalid with rv$is_valid_regression
      # If no error, save result of perform_regression() in rv
      observeEvent(input$submit_regression, {
        req(rv$content_stats)
        req(formula_reg()) # req(rv$formula_reg) 
        rv$formula_reg <- formula_reg()
        
        # First checking that regression can happen w try statement, 
        reg_model <- try(perform_regression(rv$formula_reg, 
                                     input$regression_type,
                                     rv$content_stats))
        # If the try-catch produced error (class try-error), 
        # indicate with rv$is_valid_regression <- FALSE for global use 
        # If valid then indicate opposite 
        if("try-error" %in% class(reg_model)){
          rv$is_valid_regression <- FALSE
          rv$reg_error <- reg_model
        } else { # regression is valid so save summary too
          rv$is_valid_regression <- TRUE
          rv$reg_model <- reg_model
          rv$reg_summary_raw <- 
            summary(perform_regression(rv$formula_reg, 
                                       input$regression_type,
                                       rv$content_stats))
        }

      }) # end observe event 
      
      output$lin_log_pois_res <- renderUI({
        
        req(rv$content_stats)
        # req(!is.null(rv$reg_result_res))
        
        ns <- NS(id)
        
        # UI to be displayed if valid regression submitted
        validUI <- tagList(
          # Rendering results table as html output since using kable
          htmlOutput(ns("reg_model"))
        )
        
        # UI to be displayed if regression produces error (invalid)
        invalidUI <- tagList(
          p("Submitted regression resulted in the following error:"),
          verbatimTextOutput(ns("reg_error"))
        )
        
        validate(
          need(!is.null(rv$is_valid_regression), 
               "Submit a valid formula to begin.")
        )
        
        if(rv$is_valid_regression){
          return(validUI)
        } else if(!rv$is_valid_regression){
          return(invalidUI)
        } 
        
      })
      
      # Render text with regression error if present
      output$reg_error <- renderPrint({
        rv$reg_error
      })
      
      # Render datatable of regression results
      # Using tab_model to generate, then returning HTML
      output$reg_model <- renderText({
        
        table <- sjPlot::tab_model(rv$reg_model, 
                                   show.fstat = TRUE, 
                                   show.aic = TRUE, 
                                   show.se = TRUE, 
                                   # show.std = TRUE, 
                                   show.stat = TRUE,
                                   CSS = list(
                                     css.thead = 'font-size: 14px;', 
                                     css.summary = 'font-weight: bold;'
                                   ))
        HTML(table$knitr)
      })
      
      # Rendering raw model output
      output$reg_result_raw <- renderPrint({
        
        validate(
          need(!is.null(rv$is_valid_regression), 
               "Submit a valid formula to begin.")
          )
        
        if(rv$is_valid_regression){
          rv$reg_summary_raw
        } else {
          rv$reg_error
        }
        
      })

      
      #### Perform mixed regression ####
      # RenderUI to only render when linear, log or poisson regression option chosen
      output$perform_mixed <- renderUI({
        req(rv$content_stats)
        
        ns <- NS(id)
        tagList(
          
          fluidRow(
            column(width = 6, 
                   # Var selector for random effects
                   varSelectInput(ns("mixed_random_slopes_1"), 
                                  label = "Random slope(s)", 
                                  rv$content_stats, 
                                  selected = NULL, 
                                  multiple = TRUE
                   ),
                  ), 
            column(width = 6, 
                   # Var selector for random effects
                   varSelectInput(ns("mixed_random_slopes_2"), 
                                  label = "Secondary random slope(s) (optional)", 
                                  rv$content_stats, 
                                  selected = NULL, 
                                  multiple = TRUE)
                   ),
          ),
            
          fluidRow(
            column(width = 6, 
                   
                   # Var selector for grouping var
                   varSelectInput(ns("mixed_random_intercept_1"), 
                                  label = "Random intercept", 
                                  rv$content_stats, 
                                  selected = NULL, 
                                  multiple = FALSE)
                   ),
            
            column(width = 6, 
                   # varSelectize used to enable nothing to be selected
                   varSelectizeInput(ns("mixed_random_intercept_2"),
                                     label = 
                                       "Secondary random intercept 
                                     (optional)",
                                     rv$content_stats,
                                     selected = NULL,
                                     options = list(create = TRUE, 
                                                    maxItems = 1))
             )
             
            )
          )
      }) # end renderUI 
      
      
      
      # Creating correlation matrix for heatmap correlation plot
      output$corr_plot <- renderPlotly({
        
        validate(
          need(!is.null(rv$content_stats), 
               "Submit text files to continue."), 
          need(length(rv$content_stats) > 2, 
               "Not enough finite observations to produce heatmap."), 
        )
        
        # Filtering to only include numeric data
        content_stats_numeric <- rv$content_stats %>%
          dplyr::select(where(is.numeric))
        
        # Creating correlation matrix
        corr <- round(cor(content_stats_numeric), 3)

        # Reshaping data to has three cols (pair of vars & cor coef)
        cor_melted <- melt(corr)
        
        # Generating heat map with ggplot
        ggheatmap <- ggplot(cor_melted, aes(Var2, Var1, fill = value))+
          geom_tile()+
          geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
          scale_fill_gradient2(low = "royalblue", high = "darkred", mid = "white", 
                               midpoint = 0, limit = c(-1,1), space = "Lab", 
                               name="Pearson\nCorrelation") +
          theme_minimal() + # minimal theme
          theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                           size = 12, hjust = 1)
                ) +
          ggtitle("Correlation matrix heatmap")

        # Converting ggplot object into plotly object to be interactive
        ggplotly(ggheatmap)
      })
      
      
      # Generating residual plots for fitted model
      output$residual_plots <- renderPlot({
        
        validate(
          need(!is.null(rv$is_valid_regression),
               "Submit a valid formula to begin."),
          need(rv$is_valid_regression, 
               "Submit regression to begin.")
        )
        
        if(input$regression_type == "mixed"){
          plot(fitted(rv$reg_model), residuals(rv$reg_model), 
               xlab = "Fitted", ylab = "Residuals")
          abline(h = 0, lty = 2)
          lines(smooth.spline(fitted(rv$reg_model), residuals(rv$reg_model)))
        } else {
          autoplot(rv$reg_model) + 
            theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank())
        }

      })
      
      # Performing anova
      anova_res <- reactive({
        req(rv$formula_reg)
        req(rv$is_valid_regression)
        
        rstatix::anova_test(rv$reg_model)
      })
      
      # Saving anova result
      observe({
        req(anova_res())
        rv$anova_res <- anova_res()
      })
      
      # Rendering raw anova output
      output$anova_res_raw <- renderPrint({
        validate(
          need(rv$anova_res, "Submit a valid regression model to perform ANOVA.")
        )
        rv$anova_res
      })
      
      
      # Render datatable of ANOVA results
      # Using tab_model to generate, then returning HTML
      output$anova_table <- renderTable({
        get_anova_table(rv$anova_res)
      })
      
      # Performing t-test
      ttest_res <- reactive({
        req(rv$formula_reg)
        tt_formula <- try(as.formula(rv$formula_reg))
        
        validate(
          need(!("try-error" %in% class(tt_formula)), "Submit valid data and formula to perform t-test.")
        )
        
        # Put in try-catch
        return(try(rstatix::t_test(data = rv$content_stats, formula = tt_formula)))
      })
      
      # Saving t-test result
      observe({
        rv$ttest_res <- ttest_res()
      })
      
      # Rendering raw t-test result
      output$t_test_res_raw <- renderPrint({
        # Check that the t-test result is not an error
        validate(
          need(rv$formula_reg, "Submit valid data and formula to perform t-test."),
          need(!("try-error" %in% class(rv$ttest_res)), paste0("T-test yielded the following error: ", rv$ttest_res))
        )

        rv$ttest_res
      })
      
      # Rendering formatted t-test result, rendering HTML with kable
      output$t_test_res_table <- renderText({
        # Check that the t-test result is not an error
        validate(
          need(rv$formula_reg, "Submit valid data and formula to perform t-test."),
          need(!("try-error" %in% class(rv$ttest_res)), paste0("T-test yielded the following error: ", rv$ttest_res))
        )
        
        kable(rv$ttest_res) %>% 
          kable_styling(latex_options = 'striped')
        
      })
      
      #### Chi-square testing ####
      output$perform_chisq <- renderUI({
        ns <- NS(id)
        tagList(
          
          fluidRow(
            column(width = 4, 
                   varSelectInput(ns("chisq_var1"), 
                                  label = "Select variables for testing:", 
                                  rv$content_stats, 
                                  selected = NULL),
                   varSelectInput(ns("chisq_var2"), 
                                  label = NULL, 
                                  rv$content_stats, 
                                  selected = NULL),
                   actionButton(ns("submit_chisq"),
                                label = "Perform test",
                                class = "btn-success"),
            ),
            column(width = 8, 
                   verbatimTextOutput(ns("chisq_res"))
            ),
          ),
          
        )
      })
      
      # When perform chisq button clicked, perform with selected variables
      observeEvent(input$submit_chisq, {
        
        # Try handler to ensure conversion to factors does not produce a fatal error
        var1 <- try(as.factor(rv$content_stats[[input$chisq_var1]]))
        var2 <- try(as.factor(rv$content_stats[[input$chisq_var2]]))
        
        # Ensure that var1 & var2 are both categorical/can be converted to categorical
        
        validate(need(!("try-error" %in% class(var1)), "Select a categorical variable."), 
                 need(!("try-error" %in% class(var2)), "Select a categorical variable."))
        
        # Creating table of selected vars
        chisq_table <- table(var1, var2)
        
        # Saving reactive variable
        chisq_res <- reactive({
          req(chisq_table)
          res <- try(stats::chisq.test(chisq_table))
          
          if("try-error" %in% class(res)){
            paste0("Invalid categorical variables selected. Error returned: ", res)
          } else {
            res
          }
        })
        
        # Saving in reactive list to eventually export to report generation
        req(chisq_res())
        rv$chisq_res <- chisq_res()
        
      })
      
      output$chisq_res <- renderPrint({
        validate(need(rv$chisq_res, "Submit categorical variables to begin."))
        
        rv$chisq_res
      })
      
      #### Kruskal-Wallis test ####
      kw_res <- reactive({
        
        req(rv$formula_reg)
        
        formula <- try(as.formula(rv$formula_reg))
        
        res <- try(kruskal.test(formula, data = rv$content_stats))
        
        if("try-error" %in% class(res)){
          paste0("Invalid variables selected. Error returned: ", res)
        } else {
          res
        }
        
      })
      
      observe({
        req(kw_res())
        rv$kw_res <- kw_res()
      })
      
      output$kw_res <- renderPrint({
        validate(need(rv$kw_res, "Submit valid data and formula to perform Kruskal-Wallis test."))
        
        rv$kw_res
        
      })
      
      #### Welch one-way test ####
      welch_res <- reactive({
        
        req(rv$formula_reg)
        
        formula <- try(as.formula(rv$formula_reg))
        
        res <- try(oneway.test(formula, data = rv$content_stats, var.equal = FALSE))
        
        if("try-error" %in% class(res)){
          paste0("Invalid variables selected. Error returned: ", res)
        } else {
          res
        }
        
      })
      
      observe({
        req(welch_res())
        rv$welch_res <- welch_res()
      })
      
      output$welch_res <- renderPrint({
        validate(need(rv$welch_res, "Submit valid data and formula to perform 
                      Welch one-way test."))
        
        rv$welch_res
        
      })
      
      
      #### Mann-whitney/Wilcoxon ####
      mann_wil_res <- reactive({

        req(rv$formula_reg)

        formula <- try(as.formula(rv$formula_reg))

        res <- try(wilcox.test(formula, data = rv$content_stats))

        if("try-error" %in% class(res)){
          paste0("Invalid variables selected. Error returned: ", res)
        } else {
          res
        }

      })

      observe({
        req(mann_wil_res())
        rv$mann_wil_res <- mann_wil_res()
      })

      output$mann_wil_res <- renderPrint({
        validate(need(rv$mann_wil_res,
      "Submit valid data and formula to perform Mann-Whitney test."))

        rv$mann_wil_res

      })
      

    }) # end module server inner function
} # end module server









