
#############################################
# Report maker module for Text Analysis App #
#############################################

outputDir <- "R"

##### Reporting tab module ######

reportMakerUI <- function(id){
  ns <- NS(id)
  tagList(
    useShinyjs(),
    
    #### REPORT SECTION ####
    wellPanel(
      h1(paste(c("03| Report"), sep = ",")),
      em("Generate a report of your analysis, in a format of your choice. Currently supported formats include PDF (.pdf), Microsoft Word (.docx), and HTML (.html).")
    ),

    fluidRow(
      box(title = " ",
          status = "primary",
          solidHeader = T,
          collapsible = T,
          width = 4,
          
          h2("Build your report"),
          hr(),
          
          p("Select which figures to include in your report"),
          
          checkboxInput(ns("add_word_cloud"),
                        label = " Word-cloud "),
          
          checkboxInput(ns("add_token_freq_plot"),
                        label = " Token frequency plot "),
          
          checkboxInput(ns("add_comp_freq_plot"),
                        label = " Frequency comparison plot "),
          
          checkboxInput(ns("add_zipfs_plot"),
                        label = " Zipf's Law Plot "),
          
          checkboxInput(ns("add_tf_idf_plot"),
                        label = " Tf-idf plot "),
          
          hr(),
          
          h3("Statistical analysis"),
          checkboxInput(ns("add_corr_plot"),
                        label = " Correlation heat-map "),
          
          checkboxInput(ns("add_regression"),
                        label = " Regression analysis "),
          
          checkboxInput(ns("add_residual_plots"),
                        label = " Residual plots "),
          
          checkboxInput(ns("add_anova"),
                        label = " ANOVA results "),
          
          checkboxInput(ns("add_ttest"),
                        label = " T-test results "),
          
          
      ), # end box
      
      box(title = " ",
          status = "success", 
          solidHeader = T, 
          collapsible = T,
          width = 4, 
          
          h3("Report outline"),
          em("Based on your selections, your report will contain the following:"),
          hr(), 
          
          conditionalPanel(
            condition = paste0("input['", 
                               ns("add_word_cloud"), 
                               "'] == true "),
            p("Word cloud figure")
          ),
          
          conditionalPanel(
            condition = paste0("input['", 
                               ns("add_token_freq_plot"), 
                               "'] == true "),
            p("Token frequency plot")
          ),
          
          conditionalPanel(
            condition = paste0("input['", 
                               ns("add_comp_freq_plot"), 
                               "'] == true "),
            p("Frequency comparison plot")
          ),
          
          conditionalPanel(
            condition = paste0("input['", 
                               ns("add_zipfs_plot"), 
                               "'] == true "),
            p("Zipf's plot")
          ),
          
          conditionalPanel(
            condition = paste0("input['", 
                               ns("add_tf_idf_plot"), 
                               "'] == true "),
            p("Tf-idf figure")
          ),
          
          ########## 
          
          conditionalPanel(
            condition = paste0("input['", 
                               ns("add_corr_plot"), 
                               "'] == true "),
            p("Correlation heat-map")
          ),
          
          conditionalPanel(
            condition = paste0("input['", 
                               ns("add_regression"), 
                               "'] == true "),
            p("Regression analysis")
          ),
          conditionalPanel(
            condition = paste0("input['", 
                               ns("add_residual_plots"), 
                               "'] == true "),
            p("Residual plots")
          ),
          conditionalPanel(
            condition = paste0("input['", 
                               ns("add_anova"), 
                               "'] == true "),
            p("ANOVA results")
          ),
          conditionalPanel(
            condition = paste0("input['", 
                               ns("add_ttest"), 
                               "'] == true "),
            p("T-test results ")
          ),
          
      ), # end box
      
      box(title = " ",
          status = "primary", 
          solidHeader = T, 
          collapsible = T,
          width = 4, 
          
          h3("Formatting options"),
          hr(),
          selectInput(ns("report_format"), 
                      label = "Choose a format", 
                      choices = list("HTML file (.html)" ="html", 
                                     "PDF file (.pdf)" = "pdf", 
                                     "Word document (.docx)" = "word")
                      ),
        ), # end box
    ),
    
    fluidRow(
      column(6, 
             # Generate report button
             downloadButton(ns("download_report"), 
                            label = "Download text analysis report", 
                            class = "btn-success"),
             ),
      column(6, 
             # Generate report button
             downloadButton(ns("download_stats_report"), 
                            label = "Download statistics report", 
                            class = "btn-success"),
             )
      
    ),
    
    hr()
    
  )
}


reportMakerServer <- function(id, rv = rv, report_rv = report_rv){
  moduleServer(id, function(input, output, session){
    

    # Summary stats stuff 
    
    # total_tokens <- reactive({
    #   rv$content_tf_idf$`Corpus token count`[1]
    # })
    # 
    # mean_token_count <- reactive({
    #   rv$content_tf_idf$`Mean token count`[1]
    # })
    # 
    # sd_token_count <- reactive({
    #   sd(rv$content_tf_idf$`Token Count`)
    # })
    # 
    # numStop <- reactive({
    #   if(is.null(rv$stop_words_final)){
    #     return(0)
    #   } else {
    #     return(nrow(rv$stop_words_final))
    #   }
    # })
    
    
    # Creating rvs list to pass to final report, need to check if user 
    # wants each one included first
    observe({

      # Summary stats stuff - always included
      report_rv$content <- rv$content # for what files uploaded
      report_rv$stop_words_final <- rv$stop_words_final # for stop count
      report_rv$numFiles <- rv$numFiles # number of files count
      report_rv$token <- rv$token # token used to tokenise
      report_rv$other_token <- rv$other_token # ""
      
      report_rv$content_tf_idf <- rv$content_tf_idf
      
      stats_report_rv$corr_plot <- rv$corr_plot
      stats_report_rv$formula_reg <- rv$formula_reg
      stats_report_rv$reg_result_table <- rv$reg_result_table
      stats_report_rv$residual_plots <- rv$residual_plots
      stats_report_rv$anova_table <- rv$anova_table
      stats_report_rv$ttest_table <- rv$ttest_table
      
    })

    # Using onclick functions to determine whether
    # or not to include plots etc. in report, or 
    # leave blank. Cannot set as NULL or object
    # cannot be re-added to the report.
    shinyjs::onclick("add_word_cloud", {
        ifelse(input$add_word_cloud == TRUE, 
               report_rv$word_cloud <- rv$word_cloud, 
               report_rv$word_cloud <-  "")
    }) 
    shinyjs::onclick("add_token_freq_plot", {
      ifelse(input$add_token_freq_plot == TRUE, 
             report_rv$token_freq_plot <- rv$token_freq_plot, 
             report_rv$token_freq_plot <-  "")
    }) 
    shinyjs::onclick("add_comp_freq_plot", {
      ifelse(input$add_comp_freq_plot == TRUE, 
             report_rv$comp_freq_plot <- rv$comp_freq_plot,
             report_rv$comp_freq_plot <-  "")
    }) 
    shinyjs::onclick("add_zipfs_plot", {
      ifelse(input$add_zipfs_plot == TRUE, 
             report_rv$zipfs_plot <- rv$zipfs_plot, 
             report_rv$zipfs_plot <-  "")
    }) 
    shinyjs::onclick("add_tf_idf_plot", {
      ifelse(input$add_tf_idf_plot == TRUE, 
             {
               report_rv$tf_idf_IDs <- rv$tf_idf_IDs
               report_rv$tf_idf_plot <- rv$tf_idf_plot
             },
             {
               report_rv$tf_idf_IDs  <-  ""
               report_rv$content_tf_idf  <-  ""
               report_rv$tf_idf_plot  <-  ""
             })
    })
    
    shinyjs::onclick("add_corr_plot", {
      ifelse(input$add_corr_plot == TRUE, 
             stats_report_rv$corr_plot <- rv$corr_plot, 
             stats_report_rv$corr_plot <-  "")
    }) 
    shinyjs::onclick("add_regression", {
      ifelse(input$add_regression == TRUE, 
             {
               stats_report_rv$formula_reg <- rv$formula_reg
               stats_report_rv$reg_result_table <- rv$reg_result_table
             },
             {
               stats_report_rv$formula_reg <- ""
               stats_report_rv$reg_result_table <- ""
             })
    }) 
    shinyjs::onclick("add_residual_plots", {
      ifelse(input$add_residual_plots == TRUE, 
             {
               stats_report_rv$residual_plots <- rv$residual_plots
             },
             {
               stats_report_rv$residual_plots <- ""
             })
    }) 
    shinyjs::onclick("add_anova", {
      ifelse(input$add_anova == TRUE, 
             {
               stats_report_rv$anova_table <- rv$anova_table
             },
             {
               stats_report_rv$anova_table <- ""
             })
    }) 
    shinyjs::onclick("add_ttest", {
      ifelse(input$add_ttest == TRUE, 
             {
               stats_report_rv$ttest_table <- rv$ttest_table
             },
             {
               stats_report_rv$ttest_table <- ""
             })
    }) 
    
    
    # Formatting report. Have to return report source and file name
    # onclick functions won't work here so doing manual returns.
    report_file <- reactive({
      req(input$report_format)
      
      if(input$report_format == "html"){
        return("www/Report_html.qmd")
      } else if(input$report_format == "pdf"){
        return("www/Report_pdf.qmd")
      } else if(input$report_format == "word"){
        return("www/Report_word.qmd")
      } 
    })
    observe(rv$report_file <- report_file())
    
    report_name <- reactive({
      req(input$report_format)
      if(input$report_format == "html"){
        return("Report.html")
      } else if(input$report_format == "pdf"){
        return("Report.pdf")
      } else if(input$report_format == "word"){
        return("Report.docx")
      }
    })
    observe(rv$report_name <- report_name())
    
    #### Stats report ####
    stats_report_file <- reactive({
      req(input$report_format)
      
      if(input$report_format == "html"){
        return("www/Stats_report_html.qmd")
      } else if(input$report_format == "pdf"){
        return("www/Stats_report_pdf.qmd")
      } else if(input$report_format == "word"){
        return("www/Stats_report_word.qmd")
      } 
    })
    
    observe(rv$stats_report_file <- stats_report_file())
    
    stats_report_name <- reactive({
      req(input$report_format)
      if(input$report_format == "html"){
        return("Stats_Report.html")
      } else if(input$report_format == "pdf"){
        return("Stats_Report.pdf")
      } else if(input$report_format == "word"){
        return("Stats_Report.docx")
      }
    })
    observe(rv$stats_report_name <- stats_report_name())
    
    # Generating report
    # Download handler refuses passing in plain reactive values for filename, 
    # so made a mini function so they are treated normally to return the correct
    # filename as a proxy for the desired report format of the user.
    output$download_report <- downloadHandler(
      filename = function(){return(rv$report_name)},
      content = function(file) {
        
        # generating report with rmarkdown::render() 
        rmarkdown::render(rv$report_file , 
                          output_file = file,
                          params = list(report_rv = report_rv)
        )
      }
    )
    
    output$download_stats_report <- downloadHandler(
      filename = function(){return(rv$stats_report_name)},
      content = function(file) {
        
        # generating report with rmarkdown::render() 
        rmarkdown::render(rv$stats_report_file , 
                          output_file = file,
                          params = list(report_rv = report_rv, 
                                        stats_report_rv = stats_report_rv)
        )
      }
    )
    
    } # end reporting server
  )
}

