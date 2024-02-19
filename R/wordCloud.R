
# Issue with wordcloud2 package affecting htmlwidgets in ShinyR....
# Link to GitHub issue: https://github.com/rstudio/shinydashboard/issues/281
# wordcloud2 fix  solution - thank you to stevecondylios for this!

wordcloud2_fix <- function (data, size = 1, minSize = 0, gridSize = 0, fontFamily = "Segoe UI", 
                         fontWeight = "bold", color = "random-dark", backgroundColor = "white", 
                         minRotation = -pi/4, maxRotation = pi/4, shuffle = TRUE, 
                         rotateRatio = 0.4, shape = "circle", ellipticity = 0.65, 
                         widgetsize = NULL, figPath = NULL, hoverFunction = NULL) 
{
  if ("table" %in% class(data)) {
    dataOut = data.frame(name = names(data), freq = as.vector(data))
  }
  else {
    data = as.data.frame(data)
    dataOut = data[, 1:2]
    names(dataOut) = c("name", "freq")
  }
  if (!is.null(figPath)) {
    if (!file.exists(figPath)) {
      stop("cannot find fig in the figPath")
    }
    spPath = strsplit(figPath, "\\.")[[1]]
    len = length(spPath)
    figClass = spPath[len]
    if (!figClass %in% c("jpeg", "jpg", "png", "bmp", "gif")) {
      stop("file should be a jpeg, jpg, png, bmp or gif file!")
    }
    base64 = base64enc::base64encode(figPath)
    base64 = paste0("data:image/", figClass, ";base64,", 
                    base64)
  }
  else {
    base64 = NULL
  }
  weightFactor = size * 180/max(dataOut$freq)
  settings <- list(word = dataOut$name, freq = dataOut$freq, 
                   fontFamily = fontFamily, fontWeight = fontWeight, color = color, 
                   minSize = minSize, weightFactor = weightFactor, backgroundColor = backgroundColor, 
                   gridSize = gridSize, minRotation = minRotation, maxRotation = maxRotation, 
                   shuffle = shuffle, rotateRatio = rotateRatio, shape = shape, 
                   ellipticity = ellipticity, figBase64 = base64, hover = htmlwidgets::JS(hoverFunction))
  chart = htmlwidgets::createWidget("wordcloud2", settings, 
                                    width = widgetsize[1], height = widgetsize[2], sizingPolicy = htmlwidgets::sizingPolicy(viewer.padding = 0, 
                                                                                                                            browser.padding = 0, browser.fill = TRUE))
  chart
}



#####################
## Wordcloud module #
#####################
wordCloudUI <- function(id){
  ns <- NS(id)
  
  tagList(
    wordcloud2Output(ns("wordcloud"), width = "100%"),
  )

} # wordCloudUI end

wordCloudServer <- function(id, rv = NULL){
  moduleServer(id, function(input, output, session){

    # Using token counts for the overall corpus calculated to render
    # wordcloud with package <wordcloud2> and adjustments in function above
    
    word_cloud <- reactive({
      req(rv$content_to_visualise$token_counts_corpus)
      
      wordcloud2_fix(rv$content_to_visualise$token_counts_corpus,
                             size = 1.2,
                             color= rep_len(c("black","royalblue"),
                                            nrow(rv$content_to_visualise$token_counts_corpus)),
                             minRotation = 0, maxRotation = 0,
                             fontFamily = "sans-serif",
                             fontWeight = "200"
      )
    })
    
    output$wordcloud <- renderWordcloud2({
      validate(
        need(try(word_cloud()),
             'Select a valid dataset to render visualisations.'))
      word_cloud()
    })
    
    # Storing word cloud in rv list to eventually pass to report
    observe({
      req(word_cloud())
      rv$word_cloud <- word_cloud()
    })
    
      
    
  }) # end module server function
} # end server function
