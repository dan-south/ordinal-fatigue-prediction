#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  # summary tab
  output$summary <- renderText({"This app is for visualizing the process of model
    creation and selection for our task of creating a model to predict self-reported
    fatigue in the Canadian Women's Rugby Sevens. Below are the first 10 rows
    of our data set to give you an idea of what it looks like."})
  
  output$welldftable <- renderTable({
    headtable <- head(welldf, 10)
  })
  
  # plots tab
  output$plotSummary <- renderText({"In this section we show a bit of the process of
    examining our data. From the input box on the left feel free to select a few of 
    the options to see different visualizations of Fatigue and how it lines up with other
    variables."})
  
  output$fatXplot <- renderPlot({
    if(input$fatiguePlotOption == "fatCounts"){
      plot(x=fatiguedf$Fatigue, y=fatiguedf$count,
           main="Count of All Reported Fatigue Levels",
           xlab="Fatigue Level - Likert Scale",
           ylab="Number of Times Reported",
           ylim=c(0,2500) )
      with(fatiguedf, text(count~Fatigue, labels=count,pos=3))
    }else if(input$fatiguePlotOption == "fatXmonth"){
      plot(x=fatigueSummaryByMonth$month, y=fatigueSummaryByMonth$meanFatigue,
           main="Mean Fatigue by Month (Labels = Median for Month)",
           xlab="Months by Number",
           ylab="Mean Fatigue")
      with(fatigueSummaryByMonth, text(meanFatigue~month, labels=medFatigue,pos=4))
    }else if(input$fatiguePlotOption == "fatXsleepQ"){
      ggplot(welldf, aes(x=Sleep.Quality, y=Fatigue))+
        geom_jitter(alpha=0.3) +
        stat_ellipse(geom = "polygon", alpha=.7, size=1)
    }else if(input$fatiguePlotOption == "fatXdesire"){
      ggplot(welldf, aes(x=Desire, y=Fatigue))+
        geom_jitter(alpha=0.3) +
        stat_ellipse(geom = "polygon", alpha=.7, size=1)
    }else if(input$fatiguePlotOption == "fatXsoreness"){
      ggplot(welldf, aes(x=Soreness, y=Fatigue))+
        geom_jitter(alpha=0.3) +
        stat_ellipse(geom = "polygon", alpha=.7, size=1)
    }
  })
  
  # player data tab
  output$playerSummary <- renderText({"Since generalizing your fatigue on a scale of 1-7 
                        (7 being'Fresher Than Usual', 1 being 'Exhausted') is a very 
                      subjective way of examining fatigue in athletes, one curiousity
                      we had was to look at fatigue by player and also over time to see
                      how each individual athlete's self-reported fatigue changes."})
  
  output$playerPlot <- renderPlot({
    if(input$playerDataOption=="5"){
      df <- player5
    }else if(input$playerDataOption=="16"){
      df <- player16
    }else if(input$playerDataOption=="3"){
      df <- player3
    }else if(input$playerDataOption=="15"){
      df <- player15
    }else if(input$playerDataOption=="11"){
      df <- player11
    }else if(input$playerDataOption=="13"){
      df <- player13
    }else if(input$playerDataOption=="2"){
      df <- player2
    }else if(input$playerDataOption=="14"){
      df <- player14
    }else if(input$playerDataOption=="7"){
      df <- player7
    }else if(input$playerDataOption=="4"){
      df <- player4
    }else if(input$playerDataOption=="10"){
      df <- player10
    }else if(input$playerDataOption=="1"){
      df <- player1
    }else if(input$playerDataOption=="6"){
      df <- player6
    }else if(input$playerDataOption=="17"){
      df <- player17
    }else if(input$playerDataOption=="8"){
      df <- player8
    }else if(input$playerDataOption=="12"){
      df <- player12
    }else if(input$playerDataOption=="9"){
      df <- player9
    }
    plot(x=as.Date(df$date), y=as.numeric(df$Fatigue),
         main=paste0("Player ",input$playerDataOption," Fatigue Summary over Time"),
         xlab="Date",ylab="Fatigue")
  })
  
  # models and sims tab
  output$ovrSummary <- renderText({"We are now brought to the most important secion
    of this analysis - models and justifications. Predicting the Ordinal Factor 
    response variable we had 3 primary methods. Two considered 'classification' methods
    and one 'regression' method. Our classification methods go by the names of Random
    Forest and Naive Bayes.  Regression method was Multinomial Logistic Regression also
    called Ordinal Multinomial Regression. Classification methods in short use the other
    variables to classify the response variable while regression uses those variables in 
    an equation to predict the response. Classification is very popular for discrete, non-
    continuous data."})
  
  output$modelSummary <- renderText({
    if(input$summarizeModel=="justExpl"){
      justificationExplained
    }else if(input$summarizeModel=="forestExpl"){
      forestExplained
    }else if(input$summarizeModel=="bayesExpl"){
      bayesExplained
    }else if(input$summarizeModel=="regrExpl"){
      regressionExplained
    }
  })
  
  output$modEvalPlot <- renderPlot({
    smmrystats <- simModelTestN(welldf, input$nSims)
    boxplot.matrix(smmrystats, 
                   main=paste0("Modelling Methods and Their Errors (",input$nSims," Simulations)"))
  
  })
  
})
