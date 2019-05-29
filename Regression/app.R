# App: Least Squares Regression
# Author: Javier Zapata
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(plotly)
library(Stat2Data)

# to input Rmd into shiny
library(knitr)
rmdfiles <- c("DataDescription.Rmd","Theory.Rmd","DataChallenge.Rmd",
              'PoorSolution.Rmd', 'WhatIsWrong.Rmd','GoodSolution.Rmd',
              'References.Rmd','FigureDescription.Rmd')
sapply(rmdfiles, knit, quiet = T)


mySteps = c("Step 1: Designing the Test", "Step 2: Using the Test")

data(Pines)
id<-which(!is.na(Pines$Hgt97) &  !is.na(Pines$Diam97))
nSims = 20; sampleIndex=1; sampleSize = 30
slopeMin = 1.0; slopeMax = 3.0; slopeValue=1.7; slopeStep = 0.05

ls.List<-lapply(1:nSims, function(j){
  id.sample<-sample(id,sampleSize, replace=F)
  df<-data.frame(x=Pines$Hgt97[id.sample]/100.0,y=Pines$Diam97[id.sample])
  fit<- lm(y ~ x, data=df)
  beta <-round(fit$coefficients,3)
  df$yhat <- fitted(fit)
  df$position<- ifelse(df$yhat- df$y >=0,'below','above')
  df$residuals<-df$y - (beta[1] + beta[2]*df$x)
  TotalSS = sum(df$residuals^2)
  slope.data <-lapply(seq(from=slopeMin, to= slopeMax, by = slopeStep), function(s){
    new.residuals = df$y - (beta[1] + s*df$x)
    TotalSS = sum(new.residuals^2)  
    return(list(new.residuals=new.residuals,
                TotalSS=TotalSS))
  })
  
  return(list(TotalSS=TotalSS,df=df,beta=beta,fit=fit,slope.data=slope.data))
})
#slope.data<-ls.List[[1]]$slope.data
#new.residuals<- slope.data[[j]]$new.residuals
#TotalSS<- slope.data[[j]]$TotalSS

#id.sample<-sample(id,30, replace=F)
#df<-data.frame(x=Pines$Hgt97[id.sample]/100.0,y=Pines$Diam97[id.sample])
#fit<- lm(y ~ x, data=df)
#beta <-round(fit$coefficients,3)
#df$yhat <- fitted(fit)
#df$position<- ifelse(df$yhat- df$y >=0,'below','above')
#df$residuals<-df$y - (beta[1] + beta[2]*df$x)
#TotalSS = sum(df$residuals^2)  



#slope.data <-lapply(seq(from=slopeMin, to= slopeMax, by = slopeStep), function(s){
#  new.residuals = ls.List[sampleIndex]$df$y - (ls.List[sampleIndex]$beta[1] + s*ls.List[sampleIndex]$df$x)
#  TotalSS = sum(new.residuals^2)  
#  return(list(new.residuals=new.residuals,
#              TotalSS=TotalSS))
#})



# summary(Pines$Hgt97[id])
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 87.0   303.0   357.0   347.3   405.8   558.0 

# summary(Pines$Diam97[id])
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.400   4.700   6.200   5.943   7.400  10.700 

body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  tags$script(HTML('
                   $(document).ready(function() {
                   $("header").find("nav").append(\'<span class="myClass"> Least Squares Regression </span>\');
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
    column(width = 3,style='font-size:12px;',
           # slider input box
           box(width='100%',status = "warning",background = "yellow",
               uiOutput("beta"),
               uiOutput("beta1Interpretation"),
               #uiOutput("ChangingSlope1"),
               sliderInput(inputId = "customSlope", label='Choose a slope value on the right figure', #label='2.a) Test Statistic', 
                           min=slopeMin, max=slopeMax, value=slopeValue,step = slopeStep,
                           animate=animationOptions(interval=150, loop=T)),
               tags$hr(),
               actionButton(inputId="resampleButton", label="Resample")
               
           ), #end box
           # box
           box(width = NULL, status = "warning", solidHeader=T,#title='Using the Plots',
               withMathJax(includeMarkdown("FigureDescription.md"))
           )#,#end box

    ), #end column
    
    box(width=4,status = "warning",
        plotlyOutput("plot1")
    ),
    
    box(width=4,status = "warning",
        plotlyOutput("plot2")
    )#,

    
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
  
  title="Least Squares Regression", 
  
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

server<- function(input, output,session) {
  
  #Stop app when closing browser
  session$onSessionEnded(stopApp)
  sigma=1
  z<-seq(-3.6,3.6,by=0.01)
  
  observeEvent(input$resampleButton, {
    updateSliderInput(session, "input$customSlope", value=slopeValue )
    
  })

  # The currently selected tab from the first box
  output$tabset1Selected <- renderText({input$tabset1})
  

  
  output$beta<-renderUI({
    beta<-ls.List[[as.numeric(input$resampleButton)%%nSims + 1]]$beta
    withMathJax(div(paste0("$\\textbf{Intercept} \\quad \\hat{\\beta}_0 = ",beta[1],"\\\\ ",
                       "\\textbf{Slope} \\quad \\hat{\\beta}_1 = ",beta[2],"$")))
  })
  
  output$beta1Interpretation<-renderUI({
    beta<-ls.List[[as.numeric(input$resampleButton)%%nSims + 1]]$beta
    withMathJax(div(paste0("$\\textbf{Interpretation:} \\text{ An increase in 1(m) in the height} \\\\  \\text{of a pine increases the diameter in }", 
                           beta[2], " \\text{ cms} \\\\$")))
  })

  # loading picture
  output$picture <- renderImage({
    return(list(
      src = paste0("images/",'Pine Heights',".jpg"),
      contentType = "image/jpg",alt = "Picture not found",width = '100%'
    ))},deleteFile = F)
  
  
  # Plot1: Estimated OLS 
  output$plot1 <- renderPlotly({
    
    df<-ls.List[[as.numeric(input$resampleButton)%%nSims + 1]]$df
    beta<-ls.List[[as.numeric(input$resampleButton)%%nSims + 1]]$beta
    fit<-ls.List[[as.numeric(input$resampleButton)%%nSims + 1]]$fit
    TotalSS<-ls.List[[as.numeric(input$resampleButton)%%nSims + 1]]$TotalSS
    
    # Squares for each point
    shapes_list=lapply(1:nrow(df), function(i){
      delta= 0.5*abs(df[i,]$yhat-df[i,]$y)
      #if(df[i,]$position == 'above'){
      list(type = 'square',
           xref = 'x', yref = 'y', 
           x0 = df[i,]$x + ifelse(df[i,]$position == 'above',-2,0)*delta, 
           x1 = df[i,]$x + ifelse(df[i,]$position == 'below',2,0)*delta,
           y0 = df[i,]$y + ifelse(df[i,]$position == 'above',-2,0)*delta, 
           y1 = df[i,]$y+ ifelse(df[i,]$position == 'below',2,0)*delta,
           #line = list(color = 'rgb(244, 98, 66)',dash='dash'),
           line = list(color = 'rgb(255, 255, 255)'),
           fillcolor = 'rgb(244, 98, 66)', opacity = 0.2)
    })
    
    # Total Sum of Squares
    #annotation.TotalSS <- list(
    #  x = 5, y = 1,
    #  xanchor = 'left', yanchor = 'bottom',
    #  text = paste0('Sum of Squares =  ',round(TotalSS,3)),
    #  font = list(size = 10, color = 'rgb(244, 98, 66)'),
    #  showarrow = FALSE
    #)
    
    # Intercept  (title)
    annotation.Intercept.text <- list(x = 7.7, y = 6,xanchor = 'left', yanchor = 'bottom',
                                      text = paste0('Intercept'),
                                      font = list(size = 11, color = 'rgb(244, 98, 66)'),showarrow = FALSE
    )
    # Intercept  (value)
    annotation.Intercept.value <- list(x = 7.7, y = 5,xanchor = 'left', yanchor = 'bottom',
                                       text = paste0(as.numeric(beta[1])),
                                       font = list(size = 18, color = 'rgb(244, 98, 66)'),showarrow = FALSE
    )
    # Slope  (title)
    annotation.Slope.text <- list(x = 7.7, y = 4,xanchor = 'left', yanchor = 'bottom',
                                  text = paste0('Slope'),
                                  font = list(size = 11, color = 'rgb(244, 98, 66)'),showarrow = FALSE
    )
    # Slope  (value)
    annotation.Slope.value <- list(x = 7.7, y = 3,xanchor = 'left', yanchor = 'bottom',
                                   text = paste0(as.numeric(beta[2])),
                                   font = list(size = 18, color = 'rgb(244, 98, 66)'),showarrow = FALSE
    )
    
    # Total Sum of Squares (title)
    annotation.TotalSS.text <- list(x = 7.7, y = 2,xanchor = 'left', yanchor = 'bottom',
      text = paste0('Sum of Squares'),font = list(size = 11, color = 'rgb(244, 98, 66)'),
      showarrow = FALSE
    )
    
    # Total Sum of Squares (value)
    annotation.TotalSS.value <- list(x = 7.7, y = 1,xanchor = 'left', yanchor = 'bottom',
      text = paste0(round(TotalSS,3)),font = list(size = 18, color = 'rgb(244, 98, 66)'),
      showarrow = FALSE
    )

    df.new<-data.frame(x=seq(from=0.8, to=6, by=0.05))
    df.new$y<-predict(fit, df.new)
    
    p <- plot_ly(df, x = ~x) %>% 
      add_markers(name='data', y = ~y,
                  marker=list(size=7)
      ) %>%
      layout(title = 'Least Squares Estimation',
             yaxis = list(range = c(0.4, 11), title= "Diameter (in cm)"), #Diam97
             xaxis = list(range = c(0.4, 11), title= "Height (in m)"), #Hgt96 for Diam96
             shapes = shapes_list
      ) %>%
      #add_lines(name='regression line', x=~x, y~fitted(fit)) %>%
      add_lines(name='regression line', x=df.new$x, y~df.new$y) %>%
      layout(annotations = annotation.Intercept.text) %>%
      layout(annotations = annotation.Intercept.value) %>%
      layout(annotations = annotation.Slope.text) %>%
      layout(annotations = annotation.Slope.value) %>%
      layout(annotations = annotation.TotalSS.text) %>%
      layout(annotations = annotation.TotalSS.value) %>%
      layout(showlegend = F,
             autosize = F, width = 350, height = 350) %>% config(displayModeBar = F)
    p
    
  })# end plot1
  
  # Plot 2: Varying Slope
  output$plot2 <- renderPlotly({
    
    j=(input$customSlope - slopeMin)/slopeStep + 1
    #slope.data<-ls.List[[as.numeric(input$resampleButton)%%nSims + 1]]$slope.data
    df<-ls.List[[as.numeric(input$resampleButton)%%nSims + 1]]$df
    beta<-ls.List[[as.numeric(input$resampleButton)%%nSims + 1]]$beta
    slope.data<- ls.List[[as.numeric(input$resampleButton)%%nSims + 1]]$slope.data
    new.residuals<- slope.data[[j]]$new.residuals
    TotalSS<- slope.data[[j]]$TotalSS
    
    shapes_list=lapply(1:nrow(df), function(i){
      #delta= 0.5*abs(df[i,]$yhat-df[i,]$y)
      delta = 0.5*abs(new.residuals[i])
      list(type = 'square',
           xref = 'x', yref = 'y', 
           x0 = df[i,]$x + ifelse(new.residuals[i]>0,-2,0)*delta, 
           x1 = df[i,]$x + ifelse(new.residuals[i]<0,2,0)*delta,
           y0 = df[i,]$y + ifelse(new.residuals[i]>0,-2,0)*delta, 
           y1 = df[i,]$y+ ifelse(new.residuals[i]<0,2,0)*delta,
           #line = list(color = 'rgb(244, 98, 66)',dash='dash'),
           line = list(color = 'rgb(255, 255, 255)'),
           fillcolor = 'rgb(244, 98, 66)', opacity = 0.2)
      
    })
    
    #annotation.TotalSS <- list(x = 5, y = 1,xanchor = 'left', yanchor = 'bottom',text = paste0('Sum of Squares =  ',round(TotalSS,3)),
    #  font = list(size = 10, color = 'rgb(244, 98, 66)'),
    #  showarrow = FALSE
    #)
    
    # Intercept  (title)
    annotation.Intercept.text <- list(x = 7.7, y = 6,xanchor = 'left', yanchor = 'bottom',
                                      text = paste0('Intercept (fixed)'),
                                      font = list(size = 11, color = 'rgb(244, 98, 66)'),showarrow = FALSE
    )
    # Intercept  (value)
    annotation.Intercept.value <- list(x = 7.7, y = 5,xanchor = 'left', yanchor = 'bottom',
                                       text = paste0(as.numeric(beta[1])),
                                       font = list(size = 18, color = 'rgb(244, 98, 66)'),showarrow = FALSE
    )
    # Slope  (title)
    annotation.Slope.text <- list(x = 7.7, y = 4,xanchor = 'left', yanchor = 'bottom',
                                  text = paste0('Slope'),
                                  font = list(size = 11, color = 'rgb(244, 98, 66)'),showarrow = FALSE
    )
    # Slope  (value)
    annotation.Slope.value <- list(x = 7.7, y = 3,xanchor = 'left', yanchor = 'bottom',
                                   text = paste0(as.numeric(input$customSlope)),
                                   font = list(size = 18, color = 'rgb(244, 98, 66)'),showarrow = FALSE
    )
    # Total Sum of Squares (title)
    annotation.TotalSS.text <- list(x = 7.7, y = 2,
                                    xanchor = 'left', yanchor = 'bottom',text = paste0('Sum of Squares'),
      font = list(size = 11, color = 'rgb(244, 98, 66)'),showarrow = FALSE
    )
    
    # Total Sum of Squares (value)
    annotation.TotalSS.value <- list(x = 7.7, y = 1,
                                     xanchor = 'left', yanchor = 'bottom',text = paste0(round(TotalSS,3)),
      font = list(size = 18, color = 'rgb(244, 98, 66)'),showarrow = FALSE
    )

    
    
    
    
        
    df.line<-data.frame(x=seq(from=0.8, to=6, by=0.05))
    df.line$y<-beta[1] + input$customSlope*df.line$x
    
    p <- plot_ly(df, x = ~x) %>% 
      add_markers(name='data', y = ~y,
                  marker=list(size=7)
      ) %>%
      layout(title = 'Fitting a Line With Other Slope',
             yaxis = list(range = c(0.4, 11), title= "Diameter (in cm)"), #Diam97
             xaxis = list(range = c(0.4, 11), title= "Height (in m)"), #Hgt96 for Diam96
             shapes = shapes_list
      ) %>%
      #add_lines(name='regression line', x=~x, y~fitted(fit)) %>%
      add_lines(name='regression line', x=df.line$x, y~df.line$y) %>%
      layout(annotations = annotation.Intercept.text) %>%
      layout(annotations = annotation.Intercept.value) %>%
      layout(annotations = annotation.Slope.text) %>%
      layout(annotations = annotation.Slope.value) %>%
      layout(annotations = annotation.TotalSS.text) %>%
      layout(annotations = annotation.TotalSS.value) %>%
      layout(showlegend = F,
             autosize = F, width = 350, height = 350) %>% config(displayModeBar = F)
    p
    
    
  })# end plot2
  
}


shinyApp(ui = ui, server=server)