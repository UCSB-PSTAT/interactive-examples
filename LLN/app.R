# App: Law of Large Numbers
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
              'PoorSolution.Rmd', 'WhatIsWrong.Rmd','GoodSolution.Rmd',
              'References.Rmd', 'FigureDescription.Rmd')
sapply(rmdfiles, knit, quiet = T)



data(Pines)
pineHeightsDescription= 'This dataset contains information data from an experiment conducted by the Department of Biology at Kenyon College at a site near the campus in Gambier, Ohio. In April 1990, student and faculty volunteers planted 1000 white pine (Pinus strobes) seedlings at the Brown Family Environmental Center. These seedlings were planted in two grids, distinguished by 10- and 15-foot spacings between the seedlings. Several variables were measured and recorded for each seedling over time (in 1990, 1996, and 1997).'
x.length<- length(which(Pines$Spacing==10 & !is.na(Pines$Hgt96)))

id<-which(Pines$Spacing==10 & !is.na(Pines$Hgt96))

# Case one sample dataset created
#id<- sample(id, size=length(id),replace = F)
#x<-Pines$Hgt96[id]
#dfGrid<-get_dfGrid(x)
#x.mean<-sapply(1:length(x), function(i) mean(x[1:i]) )
#x.sd<-sapply(1:length(x), function(i) sd(x[1:i]) )
#df<-data.frame(id=1:length(x.mean),sample.mean=x.mean,sample.sd=x.sd)
#myData<-list(df=df,mu=mean(x),sigma=sd(x), n=x.length)

# Multpile random sets
nSamples = 20
lengthSim = 300 # maximum number of observations visualized in a sample path
myDataList<-lapply(1:nSamples, function(j){
  #set.seed(j)
  id<- sample(id, size=length(id),replace = F)
  x<-Pines$Hgt96[id]
  dfGrid<-get_dfGrid(x)
  x.mean<-sapply(1:length(x), function(i) mean(x[1:i]) )
  x.sd<-sapply(1:length(x), function(i) sd(x[1:i]) )
  df<-data.frame(id=1:length(x.mean),sample.mean=x.mean,sample.sd=x.sd)
  myData<-list(df=df,mu=mean(x),sigma=sd(x), n=x.length)
  return(list(df=df,mu=mean(x),sigma=sd(x), n=x.length))
})
sampleIndex=1

body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  tags$script(HTML('
      $(document).ready(function() {
                   $("header").find("nav").append(\'<span class="myClass"> The Law Of Large Numbers </span>\');
                   })
                   ')),
  
  
  #1st row: Choice of dataset + picture
  fluidRow(
    box(
      #title = "Title 2", 
      width = 6, solidHeader = TRUE,
      imageOutput("picture",width='auto',height='100%')#height='auto',
    ),
    box(
      title = "Description", width = 6, solidHeader = TRUE, status = "primary",
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
        withMathJax(includeMarkdown("PoorSolution.md"))
        ),
    
    box(width=6, title= "What is Wrong Here?" ,status = "danger", solidHeader = TRUE,

        withMathJax(includeMarkdown("WhatIsWrong.md"))
    )
  ),  
  fluidRow(
    box(width=12, title= "Good Solution Using Statistics" ,status = "success", solidHeader = TRUE,
        withMathJax(includeMarkdown("GoodSolution.md"))
        )
  ),
  
  fluidRow(
    column(width = 3,style='font-size:10px;',
           # slider input box
           box(width='100%',status = "warning",background = "yellow",
               sliderInput("numberObs", ticks=F,
                           label = "Number of measurements",
                           min = 10, 
                           max = lengthSim,#x.length, 
                           post=' trees', 
                           value = 10, step =5,
                           animate=animationOptions(interval=250, loop=F)),
               tags$hr(),
               actionButton(inputId="resampleButton", label="Resample")
               
           ), #end box
           # box
           box(width = NULL, status = "warning", solidHeader=T,#title='Using the Plots',
               withMathJax(includeMarkdown("FigureDescription.md"))

           )
           ), #end column
    

    box(width=9,status = "warning",#background = "light-blue",
        plotlyOutput("allPlots"))
    
    #  )
    #)
    ), #end fluidrow
  
  
 
  
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

  title="The Law of Large Numbers", 
  
  #tags$head(includeCSS("style.css")),
  sidebar_fullCollapse=T,
  
  #preloader
  enable_preloader = TRUE,loading_duration = 1,
  
  dashboardHeader(title = title_logo,titleWidth = 400,disable=F),
  dashboardSidebar(width=0),
  dashboardBody(
    useShinyjs(),body
  )#,
  #body
)


# server.R
server <- function(input, output,session) {
  #Stop app when closing browser
  session$onSessionEnded(stopApp)
  
  observeEvent(input$resampleButton, {
    updateSliderInput(session, "numberObs", value=10 )
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

  output$allPlots <- renderPlotly({
    i<-input$numberObs
    myData<-myDataList[[as.numeric(input$resampleButton)%%nSamples + 1]]

    # Sample Mean
    ### Text
    annotation.SampleMeanText <- list(x = 60, y = 350,xanchor = 'left', yanchor = 'bottom',
      text = paste0('Sample Mean'),
      font = list(size = 11, color = '#1f77b4'),showarrow = FALSE)
    
    ### value
    annotation.SampleMeanValue <- list(x = 65, y = 325,xanchor = 'left', yanchor = 'bottom',
      text = paste0(round(myData$df$sample.mean[isolate(input$numberObs)],2)),
      font = list(size = 18, color = '#1f77b4'),showarrow = FALSE
    )
    
    # Population Mean
    ### Text
    annotation.PopMeanText <- list(x = 120, y = 350,xanchor = 'left', yanchor = 'bottom',
      text = paste0('Population Mean'),
      font = list(size = 11, color = '#1f77b4'),showarrow = FALSE
    )
    
    ### value
    annotation.PopMeanValue <- list(x = 125, y = 325,xanchor = 'left', yanchor = 'bottom',
      text = paste0(round(myData$mu,2)),
      font = list(size = 18, color = '#1f77b4'),showarrow = FALSE
    )
    
    # p1: convergence of sample mean
    p1 <- plot_ly(myData$df[1:isolate(input$numberObs),], x = ~id, 
                  y = ~sample.mean, 
                  line = list(color = '#1f77b4'),
                  name='Sample Mean',
                  type = 'scatter', mode = 'lines') %>%
      add_segments(name ='Population Mean',mode='lines', 
                   #line = list(color = '#febc15'),
                   line = list(color = '#1f77b4',dash = 'dash'),
                   x = 1, xend=lengthSim,#xend = isolate(input$numberObs), 
                   y = myData$mu, yend = myData$mu) %>%
      add_annotations(x = 210, y = 350,xanchor = 'left', yanchor = 'bottom',
                      text = paste0(round(myData$df$sample.mean[isolate(input$numberObs)],2)),
                      font = list(size = 18, color = '#1f77b4'),showarrow = FALSE
                      ) %>%
      layout(
        orientation = 'h',
        xaxis = list(title = "",range=c(1,lengthSim)), 
        yaxis = list(title = 'Sample Mean <br> of Pine Heights (cm)', 
                     titlefont= list(size=12),
                     range=c(200,350)
                     ),
                     #range=c(0.8*mean(df$sample.mean),1.1*max(df$sample.mean))),
                     #range=c(0,max(df$sample.mean))),
        legend = list(showlegend = F)) %>%
      config(displayModeBar = F)

    
    
    # Sample SD
    ### Text
    annotation.SampleSDText <- list(x = 200, y = 350, xanchor = 'left', yanchor = 'bottom',
                                      text = paste0('Sample SD'),
                                      font = list(size = 11, color = '#febc15'),showarrow = FALSE)
    
    ### value
    annotation.SampleSDValue <- list(x = 205, y = 325,xanchor = 'left', yanchor = 'bottom',
                                       text = paste0(round(myData$df$sample.sd[isolate(input$numberObs)],2)),
                                       font = list(size = 18, color = '#febc15'),showarrow = FALSE
    )

    # Population SD
    ### Text
    annotation.PopSDText <- list(x = 260, y = 350, xanchor = 'left', yanchor = 'bottom',
                                    text = paste0('Population SD'),
                                    font = list(size = 11, color = '#febc15'),showarrow = FALSE)
    
    ### value
    annotation.PopSDValue <- list(x = 265, y = 325,xanchor = 'left', yanchor = 'bottom',
                                     text = paste0(round(myData$sigma,2)),
                                     font = list(size = 18, color = '#febc15'),showarrow = FALSE
    )    
        
    
    # p2: convergence of sample standard deviation
    p2 <- plot_ly(myData$df[1:isolate(input$numberObs),], x = ~id, 
                  y = ~sample.sd, 
                  name='Sample Std Dev',
                  line = list(color = '#febc15'),
                  type = 'scatter', mode = 'lines') %>%
      add_segments(name ='Population Std Dev', mode='lines',
                   #line = list(color = '#febc15'),
                   line = list(color = '#febc15',dash = 'dash'),
                   x = 1, xend=lengthSim, #xend = isolate(input$numberObs), 
                   y = myData$sigma, yend = myData$sigma) %>%
      layout(
             orientation = 'h',
             xaxis = list(title = "Number of Pines Sampled", range=c(1,lengthSim)), 
             yaxis = list(title = 'Standard Deviation <br> of Pine Heights (cm)', 
                          titlefont= list(size=12),
                          #range=c(0,2*max(df$sample.sd))
                          range=c(0,120)
                          #range=c(0.8*mean(df$sample.sd),1.1*max(df$sample.sd))
                          ),
             legend = list(showlegend = F)) %>%
      config(displayModeBar = F)
    
    
    
    plotly::subplot(p1,p2,margin=0.10,
                    nrows=2, shareY = F, titleY=T, shareX=F,titleX=T,
                    heights=c(0.5,0.5))%>%
      layout(annotations = annotation.SampleMeanText) %>%
      layout(annotations = annotation.SampleMeanValue) %>%
      layout(annotations = annotation.PopMeanText) %>%
      layout(annotations = annotation.PopMeanValue) %>%
      layout(annotations = annotation.SampleSDText) %>%
      layout(annotations = annotation.SampleSDValue) %>%
      layout(annotations = annotation.PopSDText) %>%
      layout(annotations = annotation.PopSDValue) %>%
      layout(showlegend=T#,legend=list(orientation = 'h')
             )
    
    }) #end renderPlotly

  
}



shinyApp(ui = ui, server=server)