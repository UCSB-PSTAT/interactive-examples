# My examplePValue: Run all simulations at the start and then just update the plots
# This uses plotly

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
#library(shinyWidgets): useful to customize sliderTextInput valu
# free CSS themes: https://bootswatch.com/

# For greek letters in the UI use:
# http://www.javascripter.net/faq/greekletters.htm

# Global variables
maxReps<-2000
sdValues<-seq(1:10)

# storing mydata
runMyData <- function(mu=0, n=10, rep=10, sd=1, maxReps=2000){
  
  repMeans<-sapply(1:(maxReps),function(i) mean(rnorm(n,mean=mu,sd=sd)))
  df<-data.frame(repMeans)
  df$meanAvg <- sapply(1:(maxReps),function(i) mean(df$repMeans[1:i]))
  df$sdAvg <- sapply(1:(maxReps),function(i) sd(df$repMeans[1:i]))
  df$replications<-1:(maxReps)
  yvaluesMean <- c(abs(df$meanAvg), mu - abs(df$meanAvg-mu))
  pretty_MeanAvg <- pretty(yvaluesMean)[abs(pretty(yvaluesMean)- mu )> 0.01]
  yvaluesSDAvg <- c(abs(df$sdAvg), sd/sqrt(n) - abs(df$sdAvg- sd/sqrt(n)))
  
  pretty_SDAvg <- pretty(yvaluesSDAvg)[abs(pretty(yvaluesSDAvg)- sd/sqrt(n) )> 0.01]
  
  list(df=df,
       mu=mu,
       sd=sd,
       n=n,
       rep=rep,
       pretty_MeanAvg=pretty_MeanAvg,
       pretty_SDAvg=pretty_SDAvg)
}

# Function for subplots
plotList <- function(nplots) {
  lapply(seq_len(nplots), function(x) plot_ly())
}


# For loading screen
appCSS <- "
#loading-content {
position: absolute;
background: #000000;
opacity: 0.9;
z-index: 100;
left: 0;
right: 0;
height: 100%;
text-align: center;
color: #FFFFFF;
}
"

title <- tags$a(tags$img(src="UCSB_Tab_Navy_RGB.png", height = '50', width = '50'),
                'Diamonds Explorer', target="_blank")

# User Interface
ui <- fluidPage(
  tags$script(HTML("src='mathjax/MathJax.js?config=TeX-AMS-MML_SVG';")),
  #tags$script(HTML("src='plotly.js' charset='utf-8';")),
  #theme = "sketchy.css",
  #tags$style(type="text/css",
  #           "label {font-size: 12px;}",
  #           ".recalculating {opacity: 1.0;}"),
  titlePanel(title=div(img(src="UC-Santa-Barbara-seal-2 Color RGB.png",height = 30), 
                       img(src="UC_Santa_Barbara_Wordmark_Navy_RGB.png",height = 30))),
  #title="Central Limit Theorem",
  title=title,
  tags$h2("Hypothesis Testing"),
  p("This Shiny app shows the steps needed for an Hypothesis Test"
  ),
  p('For normal observations \\(X_i \\sim No(\\mu, \\sigma)\\) with \\(i=1,...,n\\)',
    'the sample mean has distribution',
    '$\\bar{X} \\sim No\\bigg(\\mu, \\frac{\\sigma}{\\sqrt{n}}\\bigg)$'
  ),
  hr(),
  
  #titlePanel("Playing with the Central Limit Theorem"),
  #titlePanel(title=div(img(src="UC_Santa_Barbara_Wordmark_Navy_RGB.png",height = 30), "My Title")),
  
  
  #tags$head(HTML("<title>Baller Lab</title>")), #Without company logo
  #navbarPage(title = div(img(src="UC-Santa-Barbara-seal-2 Color RGB.png", style="margin-top: -14px;", height = 50)))),
  
  useShinyjs(),
  inlineCSS(appCSS),
  #tags$head(tags$style(HTML(mycss))),
  # Loading message
  #div(id = "loading-content", h2("Loading...")),
  
  #hidden(
  #  div(id = "app-content",
  
  
  sidebarPanel(
    
    h4("Parameters"),
    withMathJax(),
    numericInput(inputId="sigma", 
                 label="Select a value for the known standard deviation hypothesis mean \\(\\sigma\\)", 
                 value=0,step=1),
    helpText('Step 1:'),
    numericInput(inputId="mu_0", 
                 label="Select a value for the null hypothesis mean \\(\\mu_0\\)", 
                value=0,step=1),
    helpText('Step 2:'),
    numericInput(inputId="alpha", 
                 label="Select a value for the significance level \\(\\alpha\\)", 
                 value=0.05,step=0.01,min=0.01,max=0.10),
    sliderInput("rep", "# of replications of \\(\\bar{X}\\) ", 
                min = 10, max = maxReps, value = 5, step= 10),
    #h5('Add more replications with the buttons below!'),
    actionButton("buttonRep10", "+10 replications")#,
    #actionButton("buttonRep20", "+20 replications")
  ), #end sideBarPanel
  # Show a plot of the generated distribution
  fluidRow(
    #column(7,
    #       sliderInput("rep", "# of replications of \\(\\bar{X}\\) ", 
    #                   min = 10, max = maxReps, value = 5, step= 10)
    #),
    column(7,
           #withSpinner(plotOutput("plotAllPlots"),type=4,color="#1d3f72")
           plotlyOutput("plotAllPlots")
           
    )
  )# fluidRow
  #) #end div
  #) #end hidden  
)# end fluidPage ui



## Server

server <- function(input, output, session) {
  #Stop app when closing browser
  session$onSessionEnded(stopApp)
  x<-rnorm(n=maxReps,mean=0,sd=1)
  xright<-x[x > 1]
  xleft<-x[x < -1]
  xcenter<-x[x>=-1 & x<=1]
  xLCR<-lapply(1:length(x), function(i) as.numeric(table(cut(x[1:i],c(-3,-1,1,3),right=F))))
  xfit<-seq(-3 ,3,length=100)
  fit <- dnorm(xfit,mean=0,sd=1)
  
  # buttonRep10
  observeEvent(input$buttonRep10, {
    reps=10
    #print('repMeans in buttonRep');print(repMeans)
    updateSliderInput(session, "rep", value=isolate(input$rep) + reps)
    
  }) #end of buttonRep10
  
  
  # Plots
  output$plotAllPlots <- renderPlotly({
    i<-input$rep
    xcounts<-table(cut(x[1:i],seq(from=-3,to=3,by=0.1),right=F))
    barsHeight<-max(xcounts)
    fit2 <- dnorm(xfit,mean=input$muA,sd=1)
    p <- plot_ly(alpha = 0.9) %>%
      # Central part of the histogram
      add_histogram(x = xcenter[1:xLCR[[i]][2]],
                    name ='Center Hist',
                    yaxis = "y",
                    marker=list(color='rgba(0, 54, 96, 0.45)'),
                    autobinx=F,
                    xbins = list(start= -1,
                                 end= 1,
                                 size=0.1)
      ) %>%
      # Left part of the histogram
      add_histogram(x = xleft[1:xLCR[[i]][1]],
                    name ='Left Hist',
                    #color='red',
                    #marker=list(color='red', opacity=0.9),
                    marker=list(color='rgba(229, 175, 32, 0.55)'),
                    yaxis = "y",
                    autobinx=F,
                    xbins = list(start= -3,
                                 end= -1,
                                 size=0.1)
      ) %>%
      # Right part of the histogram
      add_histogram(x = xright[1:xLCR[[i]][3]],
                    name ='Right Hist',
                    #color='#89C5DA',
                    #marker=list(color='red', opacity=0.9),
                    marker=list(color='rgba(229, 175, 32, 0.55)'),
                    yaxis = "y",
                    autobinx=F,
                    xbins = list(start= 1,
                                 end= 3,
                                 size=0.1)
      ) %>%
      
      # Density for Null hypothesis: mu=0
      add_trace(x = xfit, y = fit, 
                type = "scatter", 
                mode = "lines", 
                line = list(color = 'rgba(34, 145, 232, 0.15)'),
                fillcolor = 'rgba(34, 145, 232, 0.15)',
                fill = "tozeroy", 
                #yaxis = "y2", 
                yaxis = list(title="y2",range=c(min(fit),max(fit))),
                name = "Density")  %>%
      #Alternative Hypothesis: mu !=0
      add_trace(x = xfit, y = fit2, 
                type = "scatter", 
                mode = "lines", 
                #line = list(color = 'rgba(254, 188, 21, 0.2)'),
                #fillcolor = 'rgba(254, 188, 21, 0.3)',
                line = list(color = 'rgba(255, 195, 33, 0.4)'),
                fillcolor = 'rgba(255, 195, 33, 0.1)',
                fill = "tozeroy", 
                #yaxis = "y2", 
                yaxis = list(title="y2",range=c(min(fit),max(fit))),
                name = "Density")  %>%
      layout(yaxis2 = list(overlaying = "y", 
                           side = "right",
                           showline=F,
                           showticklabels = F,
                           showgrid = F
      ),
      yaxis = list(title="Counts", 
                   titlefont= list(size=12)
      ),
      xaxis=list(title='X'),
      showlegend = F
      ) %>%
      config(displayModeBar = F)
    
    
  })
  
}
shinyApp(ui = ui, server = server)