# App: Real Life Gaussian Data
# Author: Javier Zapata

source("functions.R",chdir=TRUE)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(plotly)

# to input Rmd into shiny
library(knitr)
rmdfiles <- c("References.Rmd", "Figure Description.Rmd","DataDescriptionSat.Rmd",
              "DataDescriptionNewborn.Rmd","DataDescriptionPines.Rmd")
sapply(rmdfiles, knit, quiet = T)

# parameters
bins.number<-25

# Importing data
data<-getData2()
dictionary<-c(1:length(data))
titles<-sapply(data, function(x) x$title)
names(dictionary)<-titles


body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),

  tags$script(HTML('
      $(document).ready(function() {
                   $("header").find("nav").append(\'<span class="myClass"> Normal Distribution in Every Day Life </span>\');
                   })
    ')),

  fluidRow(
    box(
      title = "Datasets", width = 4, solidHeader = TRUE, status = "warning",
      selectInput("var", 
                  label = "Choose a dataset to display",
                  choices = titles,
                  selected = data[[sample(length(data),1)]]$title
      )
    ),
    box(
      #title = "Title 2", 
      width = 4, solidHeader = TRUE,
      imageOutput("picture",width='auto',height='100%')#height='auto',
    ),
    box(
      title = "Description", width = 4, solidHeader = TRUE, status = "primary",
      #textOutput("dataDescription"),
      uiOutput("dataDescription")
    )
  ),
  
  fluidRow(
    column(width = 3,style='font-size:10px;',
      # slider input box
      box(width='100%',status = "warning",background = "yellow",
          sliderInput("percentObs", ticks=F,
                      label = "Percentage of data to display",
                      min = 10, max = 100, post='%', 
                      value = 20, step =5,
                      animate=animationOptions(interval=500, loop=F))
          
      ),
      # Comment box
      box(width = NULL, status = "warning",#background = "yellow",
          withMathJax(includeMarkdown("Figure Description.md"))
      )
    ),

      box(width=9,status = "primary",#background = "light-blue",
          plotlyOutput("boxplotAndhistogramDensity"))
    
  ), #end Visualization row
  
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
  title="Normal Distribution in Every Day Life", 
  #tags$head(HTML("<title>Baller Lab</title>",type = "header",class='main-header')),

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
  
  # changing description text
  output$dataDescription <- renderText({ 
    data[[dictionary[input$var]]]$description
  })
  
  output$dataDescription <- renderUI({
    #withMathJax(div(paste0('with $','\\textbf{with Significance Level } \\alpha = ', round(input$alpha,2), '$')))
    if (input$var=="Newborn Weights"){
      withMathJax(includeMarkdown("DataDescriptionNewborn.md"))
    }
    else if(input$var=="SAT Scores"){
      withMathJax(includeMarkdown("DataDescriptionSat.md"))
    }
    else if(input$var=="Pine Heights"){
      withMathJax(includeMarkdown("DataDescriptionPines.md"))
    }
  })
  
  # loading picture
  output$picture <- renderImage({
    return(list(
      src = paste0("images/",data[[dictionary[input$var]]]$title,".jpg"),
      contentType = "image/jpg",alt = "Picture not found",width = '100%'
      #height = 300
    ))},deleteFile = F)
  

  
  output$boxplotAndhistogramDensity <- renderPlotly({
    
    # data & variables
    data.list<-data[[dictionary[input$var]]]
    x<-data.list$x
    units<-data.list$units
    muHat<-data.list$muHat
    sigmaHat<-data.list$sigmaHat
    percentObs<-input$percentObs/100
    #percentObs<-1
    
    
    nObs<-round(percentObs*length(x))
    
    
    #x.obs<-sample(x,size=nObs,replace = F)
    x.obs<-x[1:nObs]
    #x.fit<-data.list$breaks
    x.fit<-muHat+(3*sigmaHat)*seq(-1 ,1,length=bins.number)
    #fit <- dnorm(x.fit,mean=muHat,sd=sigmaHat)
    fit <- dnorm(x.fit,mean=mean(x.obs),sd=sd(x.obs))
    
    ay_p1 <- list(
      title = 'Values',
      zeroline = TRUE,
      showline = TRUE,
      showticklabels = FALSE,
      showgrid =FALSE,
      autorange=TRUE,
      overlaying='y2'
    )
    
    ay_p2 <- list(
      title = 'Counts',
      zeroline = TRUE,
      showline = F,
      showticklabels = FALSE,
      showgrid =FALSE,
      autorange=TRUE,
      overlaying='y4'
    )
    
    p1<-plot_ly(y = x.obs, type = "box", boxpoints = "all", 
                jitter = 0.3,pointpos = -1.8,name='data') %>%
      layout(yaxis2=ay_p1,legend = list(showlegend = F),
             xaxis=list(showticklabels = FALSE))
    
    p2<-plot_ly(x = x.obs, type = "histogram", name = "Histogram",
                marker=list(color = '#247ab5'),
            autobinx=F,
            xbins = list(start= muHat-3*sigmaHat,
                         end= muHat+3*sigmaHat,
                         size=6*sigmaHat/bins.number)
    ) %>% 
      add_trace(x = x.fit, y = fit, 
                type = "scatter", 
                mode = "lines", 
                line = list(color = '#febc11'),
                marker=list(color =  '#febc11'),
                fillcolor = 'rgba(254, 188, 21, 0.5)',
                fill = "tozeroy", 
                yaxis = 'y4',#"y2", 
                #yaxis = list(title="y2",showline=F, showticklabels = F,showgrid = F, zeroline=F),
                name = "Density") %>% 
      layout(yaxis4 = ay_p2,
               #list(overlaying = "y2",  #w/ subplot:'y2', wo: 'y'
              #             side = "right",showline=F,showticklabels = F,showgrid = F),
             yaxis=list(title="Counts", titlefont= list(size=12)),
             xaxis=list(title="Values", titlefont= list(size=12)),
             legend = list(showlegend = F,orientation='h')) %>%
      config(displayModeBar = F)
    
    
    
    subplot(p1,p2,widths=c(0.3,0.7)) %>% 
      layout(showlegend=T,shareY=F,titleY=T) %>%
      layout(yaxis2 = list(title = "Values"), 
             yaxis4 = list(title = "Counts"),
             xaxis2 = list(title = data[[dictionary[input$var]]]$xaxis))
    

  }) #end renderPlotly
  
}



shinyApp(ui = ui, server=server)