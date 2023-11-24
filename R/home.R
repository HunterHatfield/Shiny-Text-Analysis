######## Home page ###########

###############################

homeUI <- function(id){
  
  ns <- NS(id)
  
  tagList(

    fluidPage( 

      h1("Kia ora.", style = "font-size: 10vw;"),
      p("This interactive  app is designed for mining, exploring and visualising text data. Follow the steps below to get started, or take a look around using the sidebar to your left."),
      
      em("Developed at the University of Otago by Hunter Hatfield & Emelia Hogg with ShinyR (v1.7.0)."),
      
      fluidRow(style = "margin: 1vw;",
        
        fluidRow(
          column(12, {
            h1("1| Import", style = "color: royalblue;")
          }),
        ),
        fluidRow(
          column(12,{
            p("Choose from various methods to select and import text data.")
          }),
        ),
        fluidRow(
          column(12, {
            h1("2| Explore", style = "color: royalblue;")
          }),
        ),
        fluidRow(
          column(12,{
            p("Understand your text data and generate insights with interactive widgets and visualisations.")
          }),
        ),
        fluidRow(
          column(12, {
            h1("3| Report", style = "color: royalblue;")
          }),
        ),
        fluidRow(
          column(12,{
            p("Generate a report of your findings, in a format of your choice. Currently supported formats include PDF (.pdf), Microsoft Word (.docx), and HTML (.html).")
          }),
        ),
      
      ),
      
      hr(),
      
      fluidRow(
    
        column(6, {
          actionButton(ns("start"), 
                       label = "Start", 
                       class ="btn-success")
        }),
        
      )
    )
  )
}

###### SERVER ######
homeServer <- function(id, rv, parent){
  moduleServer(
    id, 
    function(input, output, session = parent){
      
      # When start is clicked, take user to text selector tab
      # In order to do this, the session passed to the update function
      # is the parent session of the app.R server. 
      # In app.R see that when homeServer() is called, the parent
      # session is passed in. Then in the function above session = 
      # parent. 
      observeEvent(input$start, {
        updateTabItems(parent, "sidebar", "textSelectorTab")
      }) # end observe submit
      
    }
  )
} # end upload server
