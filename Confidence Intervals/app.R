# App: Confidence Intervals
# Author: Javier Zapata
# editing: setwd('/Users/jzapata/Dropbox/F 18/Shiny Apps/Apps/7_ConfidenceIntervals')
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
              'References.Rmd','FigureDescription.Rmd')
sapply(rmdfiles, knit, quiet = T)


sampleSizeChoices<-c(20,50)
dfList<-lapply(sampleSizeChoices,function(n){
  get_dataCI(n)
})

id <- which(Pines$Spacing==10 & !is.na(Pines$Hgt96))
pines.height <- Pines$Hgt96[id]
mu<-mean(pines.height)
# visualization parameters
regionColors <-c('green','red')
names(regionColors) <-c('green','red')
regionColors2<-c('rgba(7, 181, 91, 0.3)','rgba(181, 67, 7, 0.3)')
names(regionColors2) <-c('green','red')
regionColorsLabels <-c('CI contains the population mean','CI does not contains the population mean')
names(regionColorsLabels) <-c('green','red')

pineHeightsDescription= 'This dataset contains information data from an experiment conducted by the Department of Biology at Kenyon College at a site near the campus in Gambier, Ohio. In April 1990, student and faculty volunteers planted 1000 white pine (Pinus strobes) seedlings at the Brown Family Environmental Center. These seedlings were planted in two grids, distinguished by 10- and 15-foot spacings between the seedlings. Several variables were measured and recorded for each seedling over time (in 1990, 1996, and 1997).'



body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  tags$script(HTML('
      $(document).ready(function() {
                   $("header").find("nav").append(\'<span class="myClass"> Confidence Intervals </span>\');
                   })
                   ')), 
  
  #1st row: Choice of dataset + picture
  fluidRow(
    box(
      width = 6, solidHeader = TRUE,
      imageOutput("picture",width='auto',height='100%')#height='auto',
    ),
    box(title = "Description", width = 6, solidHeader = TRUE, status = "primary",
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
        
    box(width=6, title= "What Is Wrong Here?" ,status = "danger", solidHeader = TRUE,
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
           box(width='100%',status = "warning",background = "yellow",
               # Choices of Sample Size
               radioButtons(inputId = "sampleSize", label='Sample Size', inline = T,
                            choiceNames = list(
                              div(sampleSizeChoices[1]),
                              div(sampleSizeChoices[2])
                            ),
                            choiceValues = list("1", "2")
                            #choices = c(sampleSizes[1] ='1', sampleSizes[2] ='2')
               ),
               
              # Confidence Level     
              sliderInput(inputId = "confidenceLevel", label='Confidence Level',
                          min=0.50, max=0.99, value=0.50,step = 0.01,
                          animate=animationOptions(interval=250, loop=F))  
               
           ), 
           #end box
           # box
           box(width = NULL, status = "warning", solidHeader=T,#title='Using the Plots',
               withMathJax(includeMarkdown("FigureDescription.md"))
               #"We started your sample by choosing ten trees randomly. 
               #Try adding more observations using the panel above and look at the plots carefully. 
               #Can you distinguish any pattern as you increase the sample size?"
           )#,#end box

 
           ), #end column
    
    #column(width = 9,style='font-size:10px;',
    #    fluidRow(
    #box(width=3, background = "yellow",plotlyOutput("scatterplotAndBoxPlot")),
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

  title="Confidence Intervals", 
  
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
  
  observe({
    #print(as.numeric(input$sampleSize))
    #df<-dfList[[as.numeric(input$sampleSize)]]
    #df$margin.error<-df$margin.error*qt(0.95,df=as.numeric(df$sample.size)-1)
  })
  
  #Stop app when closing browser
  session$onSessionEnded(stopApp)
  
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

  # Plots
  output$allPlots <- renderPlotly({
    

    df<-dfList[[as.numeric(input$sampleSize)]]
    df$margin.error<-df$margin.error*qt(input$confidenceLevel,df=as.numeric(df$sample.size[1])-1)
    
    df$Color<-ifelse(mu>=df$xbar-df$margin.error & mu<= df$xbar+df$margin.error, regionColors[1],regionColors[2])
    
    greenIntervals = sum(df$Color==regionColors[1])
    #print(sum(df$Color==regionColors[1]))
    annotation.CI <- list(
      x = 0, y = 350,
      xanchor = 'left', yanchor = 'bottom',
      text = paste0(greenIntervals,'/',length(df$Color),' confidence intervals contain the true value'),
      font = list(size = 16, color = 'rgb(7, 181, 91)'),
      showarrow = FALSE
    )
  
    
    p <- plot_ly()
    for (dfColor in unique(df$Color)){
      df_filtered <- filter(df, Color == dfColor)
      p <- plotly::add_trace(p, #orientation = 'h',
                             mode='markers',type = 'scatter',
                             #y = df_filtered$id, x = df_filtered$xbar,
                             x = df_filtered$id, y = df_filtered$xbar,
                             error_y = list(type = "data", thickness=5,
                                            array = df_filtered$margin.error, 
                                            color = regionColors2[dfColor]#[[1]]
                                            #color = 'rgba(56, 244, 232,0.1)'
                             ), 
                             #text = ~paste0(df_filtered$Avg," (+/- ", df_filtered$StdDev, ")"),
                             marker = list(size = 9, 
                                           color=regionColors[dfColor]#[[1]]
                                           #line = list(color = 'red')
                             ),
                             hoverinfo = "text", 
                             name = regionColorsLabels[dfColor])
      
    }
    p %>% add_segments(name ='population mean',mode='lines', 
                       #line = list(color = '#febc15'),
                       line = list(color = 'rgb(252, 152, 2)',
                                   dash = 'dot', 
                                   width=5),
                       x = 0, xend = max(df$id)+1, 
                       y = mu, yend = mu) %>%
      layout(annotations = annotation.CI) %>%
      layout(
        xaxis = list(title='Sample Number'),
        yaxis = list(title = 'Pine Height (in cms)',range=c(200,350)),
        legend = list(orientation="h", 
                      y=0.10,
                      x=0.05,
                      font=list(size=9))
      ) %>%
      config(displayModeBar = F)
    
    
  })
  
}



shinyApp(ui = ui, server=server)