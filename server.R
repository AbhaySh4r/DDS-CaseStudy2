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
  
  #selected <- reactive({fullset %>%})
  
  #clusters <- reactive({
  #  kmeans(selectedData(), input$clusters)
  #})

    
  output$plot1 <- renderPlot({
    
    df = data.frame(selectedX(), selectedY(), selectedZ())
    colnames(df)= c("x", "y", "z")
    
    df %>% ggplot(aes(x = df$x, y = df$y, color = as.factor(df$z))) +
             geom_point(position = "jitter") + 
              labs( x= input$xcol, y = input$ycol, color = input$zcol) + geom_smooth(method = "lm", se = FALSE)
    
    
    #palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
    #          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    #par(mar = c(5.1, 4.1, 0, 1))
    #plot(selectedData(),
    #     col = clusters()$cluster,
    #     pch = 20, cex = 3)
    #points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
}