# App: Hypothesis Testing
# Author: Javier Zapata
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(plotly)
library(Stat2Data)
source('functions.R')

# to input Rmd into shiny
library(knitr)
rmdfiles <- c("DataDescription.Rmd","Theory.Rmd","DataChallenge.Rmd",
              'PoorSolution.Rmd', 'WhatIsWrong.Rmd','GoodSolution.Rmd','References.Rmd')
sapply(rmdfiles, knit, quiet = T)

# For the hypothesis test
dataHT<-get_dataHT()
#print(dataHT$df)

mySteps = c("Step 1: Designing the Test", "Step 2: Using the Test")
#choiceHypothesis = c('\\(H_0\\): \\(\\mu_1 - \\mu_2 \\geq 0 \\) vs \\(H_A\\): \\(\\mu_1 - \\mu_2 < 0 \\)'='option1')
#hypothesisChoices = c('\\(\\mu_1 - \\mu_2 \\geq 0 \\)  vs  \\(\\mu_1 - \\mu_2 < 0 \\)','\\(\\mu_1 - \\mu_2 = 0 \\)  vs  \\(\\mu_1 - \\mu_2 \\neq 0 \\)','\\(\\mu_1 - \\mu_2 \\leq 0 \\)  vs  \\(\\mu_1 - \\mu_2 > 0 \\)')
H0Choices = c('\\(\\mu_1 - \\mu_2 \\leq 0 \\)','\\(\\mu_1 - \\mu_2 = 0 \\)','\\(\\mu_1 - \\mu_2 \\geq 0 \\)')
HAChoices = c('\\(\\mu_1 - \\mu_2 > 0 \\)','\\(\\mu_1 - \\mu_2 \\neq 0 \\)','\\(\\mu_1 - \\mu_2 < 0 \\)')
hypothesisChoices=sapply(1:3, function(i){paste0('\\(H_0\\): ',H0Choices[i],' vs \\(H_A\\): ', HAChoices[i]) })


data(Pines)
id<-which(Pines$Spacing==10 & !is.na(Pines$Hgt96))
id<- sample(id, size=length(id),replace = F)
x<-Pines$Hgt96[id]

x.length<- length(x)
x.mean<-sapply(1:length(x), function(i) mean(x[1:i]) )
x.sd<-sapply(1:length(x), function(i) sd(x[1:i]) )

df<-data.frame(id=1:length(x.mean),sample.mean=x.mean,sample.sd=x.sd)

myData<-list(df=df,mu=mean(x),sigma=sd(x), n=x.length)

body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  tags$script(HTML('
      $(document).ready(function() {
                   $("header").find("nav").append(\'<span class="myClass"> Hypothesis Testing </span>\');
                   })
  ')),  
  
  #1st row: Choice of dataset + picture
  fluidRow(
    box(
      #title = "Title 2", 
      width = 6, solidHeader = TRUE,
      imageOutput("picture",width='auto',height='100%')#height='auto',
    ),
    box(title = "Data Description", width = 6, solidHeader = TRUE, status = "primary",
      withMathJax(includeMarkdown("DataDescription.md"))
    )
  ),
  fluidRow(
    box(width=12, title= "Data Challenge" ,status = "warning", solidHeader = TRUE,
        withMathJax(includeMarkdown("DataChallenge.md"))       
    )
  ),
  fluidRow(
    box(width=6, title= "Poor Solution" ,status = "danger", solidHeader = TRUE,
        #"You could go to the forest to measure the height of all the pines, and then compare the average height of pines with and without fertilizer.
        #If pines with fertilizer exhibit a greater sample mean you conclude that the fertilizer promotes plant growth."),
        withMathJax(includeMarkdown("PoorSolution.md"))
        ),
    box(width=6, title= "What is Wrong Here?" ,status = "danger", solidHeader = TRUE,
        #"By concluding that the fertilizer promotes plant growth you are deciding in favor of such hypothesis. 
        #However such decision is made solely based on the data at hand. 
        #What if this is not correct because it does not hold for other pine forests with the same characteristics? 
        #You would like to have an 'error guarantee' for taking such decision."
        withMathJax(includeMarkdown("WhatIsWrong.md"))
        )
    ),
  fluidRow(
    box(width=12, title= "Good Solution Using Statistics" ,status = "success", solidHeader = TRUE,
        #"Using the same data as in the previous solution you may attempt an Hypothesis Test. 
        #With this approach you will have an error guarantee on your decision by means of a confidence level.")
        withMathJax(includeMarkdown("GoodSolution.md"))
        )
    ),
  
  fluidRow(
    tabBox(width=5,
           #title = "Hypothesis Test",
           # The id lets us use input$tabset1 on the server to find the current tab
           id = "tabset1", height = "250px", side="left", selected="1.Test Design",
           
           
           tabPanel("1.Test Design", 
                    uiOutput("hypothesisChoiceTitle"),
                    radioButtons(inputId = "hypothesisChoice", 
                                 #label= "1.a) Choice of \\(H_0\\) and \\(H_A\\)", 
                                 label='',#label= "Choose the null and alternative hypothesis", 
                                 choiceNames = list(
                                   div(hypothesisChoices[1]),
                                   div(hypothesisChoices[2]),
                                   div(hypothesisChoices[3])
                                 ),
                                 choiceValues = list("1", "2", "3")
                    ),
                    uiOutput("alphaTitle"),
                    sliderInput(inputId = "alpha", 
                                label='',#label="1.b) Choose a significance level \\(\\alpha\\)", 
                                min=0.01, max=0.1, value=0.05,step = 0.01,
                                animate=animationOptions(interval=250, loop=F))
                    #verbatimTextOutput("txt"),
                    #htmlOutput("text2")
                    
           ), # end tabPanel
           
           tabPanel("2. Using the Test",
                    #helpText('You are testing: '),
                    uiOutput('testChosen1'),uiOutput('testChosen2'),
                    
                    uiOutput("testStatisticTitle"),
                    sliderInput(inputId = "testStatistic", label='', #label='2.a) Test Statistic', 
                                min=-3.60, max=3.60, value=0.05,step = 0.01),
                    #min=0.01, max=0.1, value=0.05,step = 0.01,
                    #animate=animationOptions(interval=200, loop=F))
                    checkboxInput("checkboxTestStatistic", 
                                  label = '...or choose a value with the slider',#'Customize test statistic', 
                                  value = F),
                    uiOutput("pValueTitle"),uiOutput("pValue"),
                    #uiOutput("testDecisionTitle"),
                    uiOutput("testDecision")
                    
           ) # end tabPanel
           
    ), # end tabBox
    
    
    box(width=7,status = "warning",#background = "light-blue",
        #plotlyOutput("plot1")
        conditionalPanel(condition = "input.tabset1 == '1.Test Design'",plotlyOutput("plot1")),
        conditionalPanel(condition = "input.tabset1 == '2. Using the Test'",plotlyOutput("plot2"))
      )
    ), # end fluidRow 
  
  fluidRow(
    box(width=12, title= "References" ,status = "info", solidHeader = TRUE,

        withMathJax(includeMarkdown("References.md"))
    ))
  
  
  )


  
# dashboard header
title_logo<-tags$a(href='http://www.pstat.ucsb.edu/',
                   tags$img(src='UC_Santa_Barbara_Wordmark_White_RGB.png',
                            height='50%'))
# User Interface
ui <- dashboardPagePlus(
  
  title="Hypothesis Testing", 
  
  #tags$head(includeCSS("style.css")),
  sidebar_fullCollapse=T,
  
  #preloader
  #enable_preloader = TRUE,loading_duration = 1,
  
  dashboardHeader(title = title_logo,titleWidth = 400,disable=F),
  dashboardSidebar(width=0),
  dashboardBody(
    useShinyjs(),body
  )#,
  #body
)

  server<- function(input, output,session) {
    
    #Stop app when closing browser
    session$onSessionEnded(stopApp)
    sigma=1
    z<-seq(-3.6,3.6,by=0.01)
    
    output$testChosen1 <- renderUI({
      #withMathJax(helpText('Dynamic output 1:  $$\\alpha^2$$'))
      #withMathJax(helpText(div(hypothesisChoices[as.numeric(input$hypothesisChoice)])))
      i=as.numeric(input$hypothesisChoice)
      #withMathJax(helpText(div(paste0('$\\quad $\\(H_0\\): ',H0Choices[i],' vs ','\\(H_A\\): ',HAChoices[i]))))
      withMathJax(div(paste0('$','\\textbf{Testing: }','\\quad $\\(H_0\\): ',H0Choices[i],' vs ','\\(H_A\\): ',HAChoices[i])))
    })
    
    output$testChosen2 <- renderUI({
      #withMathJax(helpText(div(paste0('with Significance Level \\(\\alpha = \\) ', round(input$alpha,2)))))
      withMathJax(div(paste0('with $','\\textbf{with Significance Level } \\alpha = ', round(input$alpha,2), '$')))
    })
    
    #input$testStatistic <- round(dataHT$t,2)
    updateSliderInput(session, "testStatistic", value = round(dataHT$t,2))
    
    # The currently selected tab from the first box
    output$tabset1Selected <- renderText({input$tabset1})

    #output$text2 <- renderUI({HTML("my awesome text message in HTML!!!")})
    #output$txt <- renderText({"some text"})
    
    #output$pValue<-renderText({0.4})
    #output$pValue<-renderUI({0.4})
    #output$testDecision<-withMathJax(helpText("Reject \\(H_0\\)"))
    
    #output$testDecision<-renderUI({withMathJax(helpText("Test Decision:     Reject \\(H_0\\)")) })
    
    
    output$hypothesisChoiceTitle<-renderUI({
      withMathJax("$\\textbf{Choose the null and alternative hypothesis: }$")
    })
    
    output$alphaTitle<-renderUI({
      withMathJax("$\\textbf{ and choose a Significance Level } \\alpha \\textbf{: }$")
    })
    
    output$testStatisticTitle<-renderUI({
      withMathJax("$\\textbf{Compute the Test Statistic...}$")
    })
    output$pValueTitle<-renderUI({
      #withMathJax("$\\textbf{Compute P-Value:}$")
    })
    output$pValue<-renderUI({
      #withMathJax("$\\textbf{Compute P-Value:} \\quad = 0.04$")
      pValue = round(1-pnorm(input$testStatistic),3)
      withMathJax(paste0("$\\textbf{Compute P-Value:} \\quad = ",pValue,"$"))
      })
    #output$testDecisionTitle<-renderUI({
    #  withMathJax("$\\textbf{Test Decision: }$")
    #})
    output$testDecision <- renderUI({
      ##withMathJax(paste0("<p>","Use this formula: $$\\hat{A}_{\\small{\\textrm{M€}}} =",7,"$$","</p>"))
      #withMathJax("$\\qquad \\text{Reject } H_0$")
      #withMathJax("$\\textbf{Test Decision: } \\quad \\text{P-Value} < \\alpha \\quad \\text{ then } \\quad \\text{reject } H_0$")
      pValue = round(1-pnorm(input$testStatistic),3)
      alpha = input$alpha
      if(pValue<alpha)
        withMathJax("$\\textbf{Test Decision: } \\quad \\text{P-Value} < \\alpha \\text{ then } \\text{we reject } H_0$")
      else
        withMathJax("$\\textbf{Test Decision: } \\quad \\text{P-Value} \\geq \\alpha  \\text{ then } \\text{we fail to reject } H_0$")
    })
    
    
    # changing description text
    output$dataDescription <- renderText({ 
      pineHeightsDescription
    })
    
    # loading picture
    output$picture <- renderImage({
      return(list(
        src = paste0("images/",'Pine Heights',".jpg"),
        contentType = "image/jpg",alt = "Picture not found",width = '100%'
      ))},deleteFile = F)
    
    observe({
      #if(input$tabs =="1.Test Design") print(1) else print(2)
      shinyjs::toggleState("hypothesisChoice", input$stepChoice == 'step1')
      shinyjs::toggleState("alpha", input$stepChoice == 'step1')
      shinyjs::toggleState("testStatistic", input$checkboxTestStatistic == TRUE)
      #shinyjs::toggleState("TestStatistic", input$checkboxTestStatistic == T)
      #shinyjs::toggleState("hypothesisChoice", input$radios == "enabled")
    })
    
    # Plot1
    output$plot1 <- renderPlotly({
      x_H0<-z*sigma + 0
      xMin=min(x_H0);xMax=max(x_H0)
      x_alpha= qnorm(1-input$alpha/2,mean=0,sd=sigma)
      xLeft<-x_H0[x_H0<=x_alpha]
      #xRight<-x_H0[x_H0>=x_alpha]
      xRight<-x_H0[length(xLeft):length(x_H0)]
      dnormLeft<-dnorm(xLeft,mean=0,sd=sigma)
      dnormRight<-dnorm(xRight,mean=0,sd=sigma)
      
      p <- plot_ly(alpha = 0.6) %>% 
        # Non-rejection Region
        add_ribbons(name = "Non-rejection Region",x = xLeft, 
                    ymin = rep(-0.008,length(xLeft)),
                    ymax = rep(0.001,length(xLeft)),
                    line = list(color = 'rgba(7, 181, 91, 0.05)'), # original: 'rgba(7, 164, 181, 0.05)'
                    fillcolor = 'rgba(7, 181, 91, 0.9)',
                    hoverinfo="none") %>%
        # Rejection Region
        add_ribbons(name = "Rejection Region",x = xRight, 
                    ymin = rep(-0.008,length(xRight)),
                    ymax = rep(0.001,length(xRight)),
                    line = list(color = 'rgba(181, 67, 7, 0.05)'),
                    fillcolor = 'rgba(181, 67, 7, 0.9)',
                    hoverinfo="none") %>%
        # Left
        add_trace(name = "Statistic Density",x = xLeft, y = dnormLeft, 
                  type = "scatter", 
                  mode = "lines", 
                  line = list(color = 'rgba(34, 145, 232, 0.45)', dash = 'dot'), # original: 'rgba(34, 145, 232, 0.15)'
                  fillcolor = 'rgba(34, 145, 232, 0)', # original: 'rgba(34, 145, 232, 0.15)'
                  fill = "tozeroy" ) %>%
        # Right
        add_trace(name = "Significance Level",x = xRight, y = dnormRight, 
                  type = "scatter", 
                  mode = "lines", 
                  line = list(color = 'rgba(229, 175, 32, 0.55)'),
                  fillcolor = 'rgba(229, 175, 32, 0.75)', 
                  fill = "tozeroy" ) %>%
        # Vertical line at mu_0
        #add_segments(name ='', mode='lines',
        #             line = list(color = 'blue',dash = "dash"),
        #             yaxis = "y",
        #             x = isolate(input$mu_0),xend=isolate(input$mu_0),
        #             y = 0.001,yend=max(c(dnormLeft,dnormRight))) %>%
        layout(xaxis=list(title='Test Statistic Value',range = c(xMin, xMax),showgrid = F),
               yaxis=list(showgrid = F),
               legend = list(#yanchor = 'middle',
                             y=0.95,
                             font=list(size=9)),
               showlegend = T)%>%
        config(displayModeBar = F) 
      
      p
      
    })# end plot1
    
    # Plot 2
    output$plot2 <- renderPlotly({
      x_H0<-z*sigma + 0
      xMin=min(x_H0);xMax=max(x_H0)
      x_alpha= qnorm(1-input$alpha/2,mean=0,sd=sigma)
      xLeft<-x_H0[x_H0<=x_alpha]
      #xRight<-x_H0[x_H0>=x_alpha]
      xRight<-x_H0[length(xLeft):length(x_H0)]
      dnormLeft<-dnorm(xLeft,mean=0,sd=sigma)
      dnormRight<-dnorm(xRight,mean=0,sd=sigma)
      tTest <- input$testStatistic
      color.tTest <- if(tTest<=max(xLeft)) 'rgba(7, 181, 91, 0.4)' else 'rgba(181, 67, 7, 0.8)'
      color.pValue<- if(tTest<=max(xLeft)) 'rgba(7, 181, 91, 0.4)' else 'rgba(181, 67, 7, 0.8)'
      color.text.tTest <- if(tTest<=max(xLeft)) 'rgba(7, 181, 91, 1.0)' else 'rgba(181, 67, 7, 1.0)'
      text.tTest <- if(tTest<=max(xLeft)) 'Fail to reject the Null Hypothesis' else 'Reject the Null Hypothesis'
      x.tTest<-seq(round(tTest,digits=2),3.6,by=0.01)
      dnorm.tTest<-dnorm(x.tTest,mean=0,sd=sigma)
      #x.tTest<-x_H0[length(xLeft):length(x_H0)]
      
      annotation.tTest <- list(
        #xref = 'paper',
        x = tTest, y = max(c(dnormLeft,dnormRight))+0.05,
        xanchor = 'left', yanchor = 'bottom',
        #text = text.tTest,#paste('Internet'),
        text = paste0("Test Statistic = ",round(tTest,2)),#paste('Internet'),
        font = list(family = 'Arial', size = 13, color = color.text.tTest),
        showarrow = FALSE
      )
      
      p <- plot_ly(alpha = 0.6) %>% 
        # Non-rejection Region
        add_ribbons(name = "Non-rejection Region",x = xLeft, 
                    ymin = rep(-0.008,length(xLeft)),
                    ymax = rep(0.001,length(xLeft)),
                    line = list(color = 'rgba(7, 181, 91, 0.05)'), # original: 'rgba(7, 164, 181, 0.05)'
                    fillcolor = 'rgba(7, 181, 91, 0.9)',
                    hoverinfo="none") %>%
        # Rejection Region
        add_ribbons(name = "Rejection Region",x = xRight, 
                    ymin = rep(-0.008,length(xRight)),
                    ymax = rep(0.001,length(xRight)),
                    line = list(color = 'rgba(181, 67, 7, 0.05)'),
                    fillcolor = 'rgba(181, 67, 7, 0.9)',
                    hoverinfo="none") %>%
        # Left
        add_trace(name = "Statistic Density",x = xLeft, y = dnormLeft, 
                  type = "scatter", 
                  mode = "lines", 
                  line = list(color = 'rgba(34, 145, 232, 0.45)', dash = 'dot'), # original: 'rgba(34, 145, 232, 0.15)'
                  fillcolor = 'rgba(34, 145, 232, 0)', # original: 'rgba(34, 145, 232, 0.15)'
                  fill = "tozeroy", hoverinfo="none"
                  ) %>%
        # Right
        add_trace(name = "Significance Level",x = xRight, y = dnormRight, 
                  type = "scatter", 
                  mode = "lines", 
                  line = list(color = 'rgba(229, 175, 32, 0.55)'),
                  fillcolor = 'rgba(229, 175, 32, 0.75)', 
                  fill = "tozeroy",hoverinfo="none") %>%
        # Vertical line for Test Statistic
        add_segments(name ='Test Statistic', mode='lines',
                     #line = list(color = 'rgba(130, 138, 150, 1.0)', dash = "dash"), #original: color='blue'
                     line = list(color = color.tTest, dash = "dash"), #original: color='blue'
                     yaxis = "y",
                     x = tTest,xend=tTest,
                     y = 0.001,yend=max(c(dnormLeft,dnormRight)),hoverinfo="none") %>%
        # P-Value
        add_trace(name = "P-Value",x = x.tTest, y = dnorm.tTest, 
                  type = "scatter", 
                  mode = "lines", 
                  line = list(color = color.pValue), #'rgba(130, 138, 150, 0.55)'
                  fillcolor = color.pValue,#'rgba(130, 138, 150, 0.3)', 
                  fill = "tozeroy",hoverinfo="none") %>%
        layout(xaxis=list(title='Test Statistic Value',range = c(xMin, xMax),showgrid = F),
               yaxis=list(showgrid = F),
               showlegend = T, 
               legend = list(#yanchor = 'middle',
                             y=0.95,
                             font=list(size=9))
               )%>%
        layout(annotations = annotation.tTest) %>%
        config(displayModeBar = F) 
      
      p
      
    })# end plot2
    
  }
  

shinyApp(ui = ui, server=server)