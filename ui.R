library(shiny)
library(MASS)
library(datasets)
library(shinyjs)
Ass1Data=read.csv("Ass1Data.csv",header = TRUE)
choicesA <- colnames(as.data.frame(Ass1Data))
choicesA <- choicesA[c(3,7,11)]
attach(Ass1Data)
choicesB <- colnames(Ass1Data)
choicesB <- choicesB[c(1,15:44)]
choicesC <- colnames(Ass1Data)
choicesC <- choicesC[c(1,15:44)]
choicesD <- colnames(as.data.frame(Ass1Data))
choicesD <- choicesD[c(1,15:44)]
shinyUI(
  fluidPage(
    useShinyjs(),
    titlePanel("Charlotte Gao"),
    
    tabsetPanel(
      tabPanel("Home",h3("Welcome to my first Shiny app where l demonsrate on data exploration, l will be visualizing the data and exploring things like correlations, missingness etc."),
               #sidebarPanel("",icon=icon("table",lib = "font-awesome"))
               ),
                 #img(src="download.jpeg "),height=100,width=160)),
      tabPanel("Categorical",
               h3("Dataset - Ass1Data"),
               tabsetPanel(
                 tabPanel("Summary",
                          verbatimTextOutput(outputId="SummaryA1"),
                          verbatimTextOutput(outputId = "SummaryA2")
                 ),
                 tabPanel("Raw Data",
                          DT::dataTableOutput(outputId = "categorical"),
                 ),
                 tabPanel("Visualisation",
                          selectizeInput("VariablesA", label = "Show variables:", choices = choicesA, multiple = TRUE, selected = choicesA),
                          plotOutput("Mosaic")
                 ),
                 tabPanel("Visualisation for each categorical object by Y",
                          selectInput("Categorical", "Categorical objects 
                                      ", c("Author", "Priority", "Price","Speed","Duration","Temp","Location","Agreed","State","Class","Surface")),
                          plotOutput("myplot")
                 )
                 
               )
      ),
      tabPanel("Numeric",
               h3("Dataset - Ass1Data"),
               tabsetPanel(
                 tabPanel("Summary",
                          verbatimTextOutput("SummaryB1"),
                          verbatimTextOutput("SummaryB2")
                 ),
                 tabPanel("Raw Data",
                          DT::dataTableOutput(outputId = "numerical"),
                 ),
                 tabPanel("Visualisation", 
                          checkboxInput("standardise", label = "Show standardized", value = FALSE),
                          plotOutput("Boxplot"),
                          checkboxInput("outliers", label = "Show outliers", value = TRUE),
                          sliderInput("range", label = "IQR Multiplier", min = 0, max = 5, step = 0.1, value = 1.5),
                          plotOutput("Corrgram"),
                          checkboxInput(inputId = "abs", label = "Grouping uses absolute correlation", value = TRUE),
                          hr(),
                          plotOutput(outputId = "Missing"),
                          checkboxInput(inputId = "cluster", label = "Cluster missingness", value = TRUE),
                          hr(),
                          plotOutput(outputId = "Pairs")
                 )
               )
      ),
      tabPanel("Time Series",
               h3("Dataset - Ass1Data"),
               tabsetPanel(
                 tabPanel("Summary",
                          verbatimTextOutput("SummaryC1"),
                  
                                  verbatimTextOutput("SummaryC2")
                 ),
                 tabPanel("Visualisation",
                          #selectizeInput("VariablesB", label = "Show variables:", choices = choicesB, multiple = TRUE, selected = choicesB[1]),
                          plotOutput(outputId ="TimeSeries"),
                          #plotOutput("AutoCorr")
                 )
               )
      ),
      tabPanel("Continuity-Rising order chart",
               h3("Dataset - numerical data from Ass1Data"),
               tabsetPanel(
                 tabPanel("Summary",
                          verbatimTextOutput("SummaryD")
                    ),
                 tabPanel("Visualisation", 
                          selectInput("VariablesC", label = "Show variables:", choices = choicesD, multiple = TRUE, selected = choicesD[1:4]),
                          plotOutput(outputId ="Continuity")
                          #wordcloud2Output("Cloud")
                 )
               )
      ),
      tabPanel("Homogeneity",
               h3("Dataset - numerical data from Ass1Data"),
               tabsetPanel(
                 tabPanel("Summary",
                          verbatimTextOutput("SummaryE")
                 ),
                 tabPanel("Visualisation",
                          selectizeInput("VariablesD", label = "Show variables:", choices = choicesD, multiple = TRUE, selected = choicesD[1:4]),
                          
                          plotOutput("Homogeneity", height = "600px")
                 )
               )
      )
      
    )
  )
)
