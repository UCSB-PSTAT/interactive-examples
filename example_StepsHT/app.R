library(reshape2)
library(shiny)
#library(ggplot2)
library(plotly) # greek symbols: https://help.plot.ly/adding-HTML-and-links-to-charts/
library(gridExtra)
library(patchwork) #https://github.com/thomasp85/patchwork
library(scales) # for pretty break in ggplot
library(shinyjs)
library(shinythemes) 
library(shinycssloaders)
library(grid)

myData = c("Step 1", "Step 2", "Step 3")

ui <- fluidPage(
  sidebarPanel(
  withMathJax(),
  checkboxGroupInput("choosemedia", "Hypothesis Test for the Population Mean", 
                     choices  = myData,
                     selected = c()),
  textOutput("myText"),

  conditionalPanel(condition = "input.choosemedia.includes('Step 1') || input.choosemedia.includes('Step 2') || input.choosemedia.includes('Step 3')",
                   numericInput(inputId="mu_0", 
                                label="Step 1: Enter a value for \\(\\mu_0\\)", 
                                value=0,step=1)
                  ),

  conditionalPanel(condition = "input.choosemedia.includes('Step 2') || input.choosemedia.includes('Step 3')",
                   sliderInput("alpha", "Step 2: Choose a significance level \\(\\alpha\\)", 
                               min=0.01, max=0.1, value=0.05,step = 0.01,
                               animate=animationOptions(interval=200, loop=F))
                  ),
  
  conditionalPanel(condition = "input.choosemedia.includes('Step 3')", 
                   numericInput(inputId="xbar", 
                                label="Step 3: Enter a value for the sample mean \\(\\bar{x}\\)", 
                                value=0,step=1)
                  )

  ), #end SideBarPanel
  
  
  fluidRow(column(7,
                  conditionalPanel(
                    condition = "input.choosemedia.includes('Step 1') && !input.choosemedia.includes('Step 2') && !input.choosemedia.includes('Step 3')",
                    #condition = "input.choosemedia.match(/\d/g).join('') == '1'",
                    plotlyOutput("plot1")
                  ),
                  conditionalPanel(
                    condition = "input.choosemedia.includes('Step 2') && !input.choosemedia.includes('Step 3')",
                    plotlyOutput("plot2")
                  ),
                  conditionalPanel(
                    condition = "input.choosemedia.includes('Step 3')",
                    plotlyOutput("plot3")
                  )
           ) # end column   
  )# end fluidrow
  
)# end fluidPage

# Define server logic 
server <- function(input, output,session) {
  
  #Stop app when closing browser
  session$onSessionEnded(stopApp)
  sigma=1
  z<-seq(-3.6,3.6,by=0.01)
  
  # mu_0
  observeEvent(input$mu_0, {
    if (!'Step 3' %in% isolate(input$choosemedia)) { 
      #input$xbar<-input$mu_0
      updateNumericInput(session, "xbar", value = input$mu_0)
    }
  }) #end of mu_0
  
  # Plot1
  output$plot1 <- renderPlotly({
    x_H0<-z*sigma + input$mu_0
    dnorm_H0<- dnorm(x_H0,mean=isolate(input$mu_0),sd=sigma)
    xMin=min(x_H0);xMax=max(x_H0)
    plot_ly(alpha=0.6) %>%
        add_trace(x = x_H0, y = dnorm_H0, 
                  type = "scatter", 
                  mode = "lines", 
                  line = list(color = 'rgba(34, 145, 232, 0.15)'),
                  fillcolor = 'rgba(34, 145, 232, 0.15)',
                  fill = "tozeroy", 
                  #yaxis = "y2", 
                  yaxis = list(title="y",range=c(min(dnorm_H0),max(dnorm_H0))),
                  name = "Density") %>%   
      # Vertical line at mu_0
      add_segments(name ='', mode='lines',
                   line = list(color = 'blue',dash = "dash"),
                   yaxis = "y",
                   x = isolate(input$mu_0),xend=isolate(input$mu_0),
                   y = 0.001,yend=max(dnorm_H0)) %>%
        layout(xaxis=list(title='',range = c(xMin, xMax)),
               showlegend = F)%>%
      config(displayModeBar = F)

  })# end plot1
  
  # Plot2
  output$plot2 <- renderPlotly({
    x_H0<-z*sigma + input$mu_0
    xMin=min(x_H0);xMax=max(x_H0)
    x_alpha= qnorm(1-input$alpha/2,mean=isolate(input$mu_0),sd=sigma)
    xLeft<-x_H0[x_H0<=x_alpha]
    #xRight<-x_H0[x_H0>=x_alpha]
    xRight<-x_H0[length(xLeft):length(x_H0)]
    dnormLeft<-dnorm(xLeft,mean=isolate(input$mu_0),sd=sigma)
    dnormRight<-dnorm(xRight,mean=isolate(input$mu_0),sd=sigma)
    p <- plot_ly(alpha = 0.6) %>% 
      # Left
      add_trace(x = xLeft, y = dnormLeft, 
                type = "scatter", 
                mode = "lines", 
                line = list(color = 'rgba(34, 145, 232, 0.15)'),
                fillcolor = 'rgba(34, 145, 232, 0.15)',
                fill = "tozeroy", 
                name = "Density") %>%
      # Right
      add_trace(x = xRight, y = dnormRight, 
                type = "scatter", 
                mode = "lines", 
                line = list(color = 'rgba(229, 175, 32, 0.55)'),
                fillcolor = 'rgba(229, 175, 32, 0.55)',
                fill = "tozeroy", 
                name = "Density") %>%
      # Vertical line at mu_0
      add_segments(name ='', mode='lines',
                   line = list(color = 'blue',dash = "dash"),
                   yaxis = "y",
                   x = isolate(input$mu_0),xend=isolate(input$mu_0),
                   y = 0.001,yend=max(c(dnormLeft,dnormRight))) %>%
      layout(xaxis=list(title='',range = c(xMin, xMax)),
             showlegend = F)%>%
      config(displayModeBar = F) 
    
    p
    
  })# end plot2
  
  # Plot 3
  output$plot3 <- renderPlotly({
    x_H0<-z*sigma + input$mu_0
    xMin=min(x_H0);xMax=max(x_H0)
    x_alpha= qnorm(1-input$alpha/2,mean=isolate(input$mu_0),sd=sigma)
    xLeft<-x_H0[x_H0<=x_alpha]
    xRight<-x_H0[x_H0>x_alpha]
    dnormLeft<-dnorm(xLeft,mean=isolate(input$mu_0),sd=sigma)
    dnormRight<-dnorm(xRight,mean=isolate(input$mu_0),sd=sigma)
    colorBar =  if (input$xbar<=x_alpha) 'rgba(34, 145, 232, 1)' else 'rgba(229, 175, 32,1)'
    p <- plot_ly(alpha = 0.6) %>% 
      # Left
      add_trace(x = xLeft, y = dnormLeft, 
                type = "scatter", 
                mode = "lines", 
                line = list(color = 'rgba(34, 145, 232, 0.15)'),
                fillcolor = 'rgba(34, 145, 232, 0.15)',
                fill = "tozeroy", 
                name = "Density") %>%
      # Right
      add_trace(x = xRight, y = dnormRight, 
                type = "scatter", 
                mode = "lines", 
                line = list(color = 'rgba(229, 175, 32, 0.55)'),
                fillcolor = 'rgba(229, 175, 32, 0.55)',
                fill = "tozeroy", 
                name = "Density") %>%
      # Vertical line at mu_0
      add_segments(name ='', mode='lines',
                   line = list(color = 'blue',dash = "dash"),
                   yaxis = "y",
                   x = isolate(input$mu_0),xend=isolate(input$mu_0),
                   y = 0.001,yend=max(c(dnormLeft,dnormRight))) %>%
      # Vertical line at xbar
      add_segments(name ='Statistic', mode='lines',
                   line = list(color = colorBar,dash = "dash"),
                   yaxis = "y",
                   x = isolate(input$xbar),xend=isolate(input$xbar),
                   y = 0.001,yend=max(c(dnormLeft,dnormRight))) %>%
      layout(xaxis=list(title='',range = c(xMin, xMax)),
             showlegend = F)%>%
      config(displayModeBar = F) 
    
    p
    
  })# end plot3
}
# Run the application 
shinyApp(ui = ui, server = server)