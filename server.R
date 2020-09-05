library(shiny)
library(vcd)
library(MASS)
library(RColorBrewer)
library(datasets)
library(corrgram)
library(visdat)
library(forecast)
library(tidytext)
library(tidyverse)
library(janeaustenr)
library(stringr)
library(reshape2)
library(pls)
library(dplyr)
library(shinyjs)
library(lubridate)

#setwd("/Charlotte/shepherding/423industry/Visualisation-20200219/Visualising/datasetexample ")
Ass1Data=read.csv("Ass1Data.csv",header = TRUE)
Ass1Data$Date=ymd(Ass1Data$Date)
Ass1Data$Priority=factor(Ass1Data$Priority,order=TRUE,levels = c("Low","Medium","High"))
Ass1Data$Price=factor(Ass1Data$Price,order=TRUE, levels = c("Cheap","Costly","Extravagant"))
Ass1Data$Speed=factor(Ass1Data$Speed,order=TRUE,levels = c("Slow","Medium","Fast"))
Ass1Data$Duration=factor(Ass1Data$Duration,order=TRUE,levels = c("Short","Long","Very Long"))
Ass1Data$Temp=factor(Ass1Data$Temp,order=TRUE,levels = c("Cold","Warm","Hot"))
lexicon <- "afinn"

categorical=Ass1Data[c(3,5,6,7,8,9,10,11,12,13,14)]
numerical = Ass1Data[c(1,15:44)]
display = Ass1Data[c(1,15:19)]
shinyServer(function(input, output, session) {
  output$myimage <- renderPlot({
    outfile <- tempfile(fileext = 'download.jpeg')
  })
  output$SummaryA1 <- renderPrint({
    glimpse(categorical)
  })
  output$SummaryA2 <- renderPrint({
    summary(as.data.frame(categorical))
  })
  output$categorical <- DT::renderDataTable({
    DT::datatable(data = categorical)
  })
  output$Mosaic <- renderPlot({
    formula <- as.formula(paste("~",paste(input$VariablesA, collapse = " + ")))
    mosaic(formula, data = Ass1Data,
           main = "relationship of 'Author', 'Speed' and 'Agreed'", shade = TRUE, legend = TRUE)
  })
  output$myplot=renderPlot({
    #if(is.null(input$choicesC) || is.null(input$choicesC)){return()}
    boxplot(Y~get(input$Categorical),data=Ass1Data)
  })
  

  output$Plot <- renderPlot({
    plot(Ass1Data)
  })
  output$numerical <- DT::renderDataTable({
    DT::datatable(data = numerical)
  })
  
  output$SummaryB1 <- renderPrint({
    glimpse(numerical)
  })
  output$SummaryB2 <- renderPrint({
    summary(numerical)
  })
  
  output$Boxplot <- renderPlot({
    data <- as.matrix(numerical)
    data <- scale(data, center = input$standardise, scale = input$standardise)
    boxplot(x = data, use.cols = TRUE, notch = FALSE, varwidth = FALSE,  
            horizontal = FALSE, outline = input$outliers, 
            col = brewer.pal(n = dim(Ass1Data)[2], name = "RdBu"),
            range = input$range)
  })
  
  output$Corrgram <- renderPlot({
    corrgram(Ass1Data[c(1,15:19)], order = "OLO", abs = input$abs,
             lower.panel = panel.shade,
             upper.panel = panel.pie, 
             text.panel = panel.txt,
             main = "Correlogram of Ass1Data")
  })

  #output$Missing <- renderPlot({
    #vis_miss(Ass1Data, sort_miss = FALSE)
  #})
  output$Missing <- renderPlot({
    vis_miss(Ass1Data, cluster = input$cluster) +
      labs(title = "Missingness of Ass1Data")
  })
  
  output$Pairs <- renderPlot({
    GGally::ggpairs(data = Ass1Data[c(1,15:19)], title = "Pairs of Ass1Data data")
  })
  output$SummaryC1 <- renderPrint({
    str(Ass1Data[c(1,4)])
  })
  
  output$SummaryC2 <- renderPrint({
    summary(Ass1Data[c(1,4)])
  })
  output$TimeSeries <- renderPlot({
    ggplot(data = Ass1Data, aes(x = Date, y = Y))+
      geom_line(color = "blue", size = 1,show.legend = FALSE)
  })
  
  output$AutoCorr <- renderPlot({
    acf(Ass1Data[1]~Ass1Data[4], plot = TRUE, lag.max = 100)
  })
  output$Continuity<-renderPlot({
    cols <- input$VariablesC # choose the numeric columns
    d <- Ass1Data[,cols]  # filter out non-numeric columns
    for (col in 1:ncol(d)) {
      d[,col] <- d[order(d[,col]),col] #sort each column in ascending order
    }
    d <- scale(x = d, center = TRUE, scale = TRUE)  # scale so they can be graphed with a shared Y axis
    mypalette <- rainbow(ncol(d))
    matplot(y = d, type = "l", xlab = "Observations", ylab = "Values", lty = 1, lwd = 1, col = mypalette, main = "Rising Order chart")
    legend(legend = colnames(d), x = "topleft", y = "top", lty = 1, lwd = 1, col = mypalette, ncol = round(ncol(d)^0.3))
    
  })
  
  output$SummaryD <- renderPrint({
    summary(numerical)
  })


  output$SummaryE <- renderPrint({
    glimpse(numerical)
  })

  output$Homogeneity <- renderPlot({
    cols <- input$VariablesD # choose the numeric columns
    numData <- scale(numerical[,cols], center = TRUE, scale = TRUE) # Normalise so they can share a common y axis
    matplot(numData, type = "l", col = alpha(rainbow(ncol(numData)), 0.4) ) #use transparency so you can see into the data
  })
  
})




#runApp(appDir = ".", display.mode = "showcase")
