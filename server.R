function(input, output, session) {
  
  library(tidyverse)
  library(ggplot2)
  
  fullset = read.csv('./Data/housing_fullset.csv')
  
  # Combine the selected variables into a new data frame
  selectedX <- reactive({
    fullset[, c(input$xcol)]
  })
  selectedY <- reactive({
    fullset[, c(input$ycol)]
  })
  selectedZ <- reactive({
    (fullset[,(input$zcol)])
  })
  selectedA <- reactive({
    (fullset[,(input$acol)])
  })
  selectedB <- reactive({
    (fullset[,(input$bcol)])
  })
  #selected <- reactive({fullset %>%})
  
  #clusters <- reactive({
  #  kmeans(selectedData(), input$clusters)
  #})

    
  output$plot1 <- renderPlot({
    
    df = data.frame(selectedX(), selectedY(), selectedZ())
    colnames(df)= c("x", "y", "z")
    
    df %>% ggplot(aes(x = df$x, y = df$y, color = as.factor(df$z))) +
             geom_point(position = "jitter") + 
              labs( x= input$xcol, y = input$ycol, color = input$zcol) +
      geom_smooth(method = "lm", se = FALSE)
    
    
  })
  
  output$plot2 <- renderPlot ({
    df1 = data.frame(selectedA(), selectedB())
    colnames(df)= c("A", "B")
    
    df1 %>% ggplot(aes(x = A, color = as.factor(B))) + geom_histogram(stat = "count")
  })
}