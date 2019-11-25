#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Canadian Women's Rugby Sevens"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(

       # plots 
       selectInput("fatiguePlotOption", "Visualizations of 'Fatigue' in 'Plots' tab",
                   c("Overall Report of Fatigue Levels"="fatCounts",
                     "Average Fatigue by Month"="fatXmonth",
                     "Fatigue by Sleep Quality"="fatXsleepQ",
                     "Fatigue by Desire"="fatXdesire",
                     "Fatigue by Soreness"="fatXsoreness")),
      # player data
      selectInput("playerDataOption","Athlete by number in Player Data tab",
                  c("Number 5"="5",
                    "Number 16"="16",
                    "Number 3"="3",
                    "Number 15"="15",
                    "Number 11"="11",
                    "Number 13"="13",
                    "Number 2"="2",
                    "Number 14"="14",
                    "Number 7"="7",
                    "Number 4"="4",
                    "Number 10"="10",
                    "Number 1"="1",
                    "Number 6"="6",
                    "Number 17"="17",
                    "Number 8"="8",
                    "Number 12"="12",
                    "Number 9"="9")),
      
      selectInput("summarizeModel","Select prediction method you would like summarized in
                  'Modelling'",
                  c("Examination and Justification"="justExpl",
                    "Random Forest Classification"="forestExpl",
                    "Naive Bayes Classification"="bayesExpl",
                    "Ordinal Logistic Regression"="regrExpl")),
      
      sliderInput("nSims", "Select n for number of simulations on modelling methods in 
                  'Simulations'",
                  min = 1,
                  max = 10,
                  value = 3)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Summary", textOutput("summary"), tableOutput("welldftable") ), # data set and text
        tabPanel("Plots", textOutput("plotSummary"), plotOutput("fatXplot")), # plots of fatigue X ....
        tabPanel("Player Data", textOutput("playerSummary"), plotOutput("playerPlot") ), # select player & plot fatigue
        tabPanel("Modelling", textOutput("ovrSummary"),
                 textOutput("modelSummary"), 
                 img(src="plot50iterationA.png", height="400" , width="520")), 
        
        tabPanel("Simulations", plotOutput("modEvalPlot"))
  
      )
    )
  )
))
