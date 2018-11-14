# My example2: Run all simulations at the start and then just update the plots
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
# free CSS themes: https://bootswatch.com/

# For greek letters in the UI use:
# http://www.javascripter.net/faq/greekletters.htm

# Global variables
maxReps<-1000

# storing mydata
runMyData <- function(mu=0, n=10, rep=10, sd=1, maxReps=1000){
  #mu <- input$mu
  #n <- input$n
  #rep <- input$rep
  #sd <- input$sd
  
  
  # Initial parameters
  #if (length(params)==1){
  #  params$mu <- mu
  #  params$sd <- sd
  #  params$n <- n
  #}
  
  repMeans<-sapply(1:(maxReps),function(i) mean(rnorm(n),mean=mu,sd=sd))
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

#function to compute number of bins in plotly histogram
#compute_bins <- function(x, n) {
#  list(
#    start = min(x),
#    end = max(x),
#    size = (max(x) - min(x)) / n
#  )
#}

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
  tags$h2("Central Limit Theorem"),
  p("This Shiny app shows how the Central Limit Theorem in action."
    #tags$a(href="https://systematicinvestor.wordpress.com/2013/04/06/retirement-simulating-wealth-with-random-returns-inflation-and-withdrawals-shiny-web-application/", "retirement app"),
    #"from",
    #tags$a(href="http://systematicinvestor.wordpress.com/", "Systematic Investor"),
    #"to demonstrate the use of Shiny's new grid options."),
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
          
          h4("Central Limit Theorem"),
          withMathJax(),
          helpText('For normal observations \\(X_i \\sim No(\\mu, \\sigma)\\) with \\(i=1,...,n\\)'),
          helpText('the sample mean has distribution'),
          helpText('$$\\bar{X} \\sim No\\bigg(\\mu, \\frac{\\sigma}{\\sqrt{n}}\\bigg)$$'),
          #sliderInput("mu", "Normal mean \u03BC:", 
          # All your styles will go here
          #tags$style(HTML(".js-irs-0 .irs-single {color:black; background:none;}, 
          #                .js-irs-0 .irs-bar-edge, 
          #                .js-irs-0 .irs-bar {background: none; border-top: none; border-bottom: none;}"
          #                )),
          #tags$style(type = "text/css", "
          #  .irs-bar {background: none; border-top: none; border-bottom: none;}
          #           .irs-bar-edge {background: none; border: none; border-radius: 0px; width: 20px;}
          #           .irs-grid-pol {display: none;}
          #           "),
          #sliderInput("mu", "Mean \\(\\mu\\)", 
          #            min=-10, max=10, value=1,step=0.01),
          #sliderInput("sd", "Normal std. deviation \u03C3:", 
          sliderInput("sd", "Select a value for the std. deviation \\(\\sigma\\)", 
                      min=1, max=10, value=1,step=0.5),
          #sliderInput("n", "Sample size n:", 
          #sliderInput("n", "Sample size \\(n\\)", 
          #            min = 5, max = 100, value = 5, step= 5),
          helpText('Add more replications to see the convergence of \\(\\bar{X}\\)!!'),
          sliderInput("rep", "# of replications of \\(\\bar{X}\\) ", 
          #sliderInput("rep", "Add more replications to see the convergence of \\(\\bar{X}\\) ", 
                      min = 10, max = maxReps, value = 5, step= 10),
          #h5('Add more replications with the buttons below!'),
          actionButton("buttonRep10", "+10 replications")#,
          #actionButton("buttonRep20", "+20 replications")
        ), #end sideBarPanel
        # Show a plot of the generated distribution
        fluidRow(
          #column(7,plotOutput("plotAllPlots"))
          
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
  # Hide the loading message when the rest of the server function has executed
  #Sys.sleep(1)
  #hide(id = "loading-content", anim = TRUE, animType = "fade")
  #show("app-content")
  
  myData<-runMyData(maxReps = maxReps)
  #Stop app when closing browser
  session$onSessionEnded(stopApp)
  # Hide the loading message when the rest of the server function has executed
  #Sys.sleep(1)
  #hide(id = "loading-content", anim = TRUE, animType = "fade")
  #show("app-content")
  
  #if(F){
  # buttonRep10
  observeEvent(input$buttonRep10, {
    reps=10
    #print('repMeans in buttonRep');print(repMeans)
    updateSliderInput(session, "rep", value=isolate(input$rep) + reps)
    
  }) #end of buttonRep10

  
  # Plots
  output$plotAllPlots <- renderPlotly({
    
    input$rep
    
    x <- myData$df[1:input$rep,]$repMeans
    #xfit<-seq(min(myData$df$repMeans),max(myData$df$repMeans),length=100)
    xfit<-myData$mu+(3*myData$sd/sqrt(myData$n))*seq(-1 ,1,length=100)
    fit <- dnorm(xfit,mean=myData$mu,sd=myData$sd/sqrt(myData$n))
    
    p1<-plot_ly(x = x, type = "histogram", name = "Histogram",
                autobinx=F,
                xbins = list(start= myData$mu-3*myData$sd/sqrt(myData$n),
                             end= myData$mu+3*myData$sd/sqrt(myData$n),
                             size=6*myData$sd/sqrt(myData$n)/100)
                #nbinsx=floor(80/sqrt(myData$n))
                ) %>% 
                #xbins=list(start=min(myData$df$repMeans), 
                #           end=min(myData$df$repMeans),
                #           size=0.1)) %>% 
      add_trace(x = xfit, y = fit, 
                #nbinsx=floor(80/sqrt(myData$n)),
                type = "scatter", 
                mode = "lines", 
                line = list(color = '#febc15'),
                fillcolor = 'rgba(254, 188, 21, 0.5)',
                fill = "tozeroy", 
                yaxis = "y2", 
                #yaxis = list(title="y2",showline=F, showticklabels = F,showgrid = F, zeroline=F),
                name = "Density") %>% 
      layout(yaxis2 = list(overlaying = "y2", side = "right",showline=F,showticklabels = F,showgrid = F),
             yaxis=list(title="Counts", titlefont= list(size=12)),
             legend = list(showlegend = F))


    # p2: 
    p2 <- plot_ly(myData$df[1:isolate(input$rep),], x = ~replications, 
                  y = ~meanAvg, 
                  line = list(color = '#1f77b4'),
                  name='Average <br> of Sample Mean',
                  #marker = list(size = 8,
                  #              color = '#1f77b4'),
                  type = 'scatter', mode = 'lines') %>%
      add_segments(name ='&mu;',mode='lines', 
                   #line = list(color = 'rgb(205, 12, 24)'),
                   line = list(color = '#febc15'),
                   #marker = list(size = 0, color= '#ff7f0e'),
                   x = 1, xend = isolate(input$rep), 
                   y = myData$mu, yend = myData$mu) %>%
      layout(
          xaxis = list(title = ""), 
             yaxis = list(title = 'Average of <br> Sample Mean', titlefont= list(size=12)),
             legend = list(showlegend = F)) 
             #yaxis = list(title = "$$\\bar{X}$$", titlefont= list(size=12))) %>% 
      #config(mathjax = "cdn")
           
    # p3: 
    p3 <- plot_ly(myData$df[1:isolate(input$rep),], x = ~replications, 
                  y = ~sdAvg, 
                  name='St. Dev. of Sample Mean',
                  line = list(color = '#1f77b4'),
                  type = 'scatter', mode = 'lines') %>%
      add_segments(name ='&sigma;', mode='lines',
                   line = list(color = '#febc15'),
                   x = 1, xend = isolate(input$rep), 
                   y = myData$sd/sqrt(myData$n), yend = myData$sd/sqrt(myData$n)) %>%
      layout(xaxis = list(title = "# of replications"), 
             yaxis = list(title = 'Standard Deviation <br> of Sample Mean', titlefont= list(size=12)),
             legend = list(showlegend = F)) 
      
    
    if(F){
    p1 <- ggplot(myData$df[1:isolate(input$rep),], aes(repMeans)) + 
      geom_histogram(aes(y = stat(density)),
                     color="darkblue", 
                     fill="lightblue",
                     binwidth = 0.05*myData$sd) +
      xlim(c(myData$mu-3*myData$sd,myData$mu+3*myData$sd)) + 
      xlab(expression(bar(X))) + 
      geom_vline(aes(xintercept=myData$mu),color="red", linetype="dashed", size=1) + 
      stat_function(
        fun = dnorm, args = list(mean = myData$mu, sd = myData$sd/sqrt(myData$n)), 
        lwd = 2,col = 'red', alpha=0.3)
    
    # p2: Evolution of Avg of replications
    p2<-ggplot(myData$df[1:isolate(input$rep),], aes(x=replications,y=meanAvg)) + 
      geom_line(color='darkblue')+ ylab(expression(paste('Average of ',bar(x))))+
      geom_hline(aes(yintercept=myData$mu),
                 color="red", linetype="dashed", size=1) + 
      xlab('# of replications') +
      scale_x_continuous(breaks= pretty_breaks()) +
      scale_y_continuous(breaks = c(myData$pretty_MeanAvg, myData$mu), 
                         labels = c(myData$pretty_MeanAvg, expression(mu))
      )

    
    # p3: Evolution of SD of replications
    
    p3<-ggplot(myData$df[1:isolate(input$rep),], aes(x=replications,y=sdAvg)) + 
      geom_line(color='darkblue')+ ylab(expression(paste('Std. Deviation of ',bar(x))))+
      xlab('# of replications') + scale_x_continuous(breaks= pretty_breaks()) +
      geom_hline(aes(yintercept=myData$sd/sqrt(myData$n)),
                 color="red", linetype="dashed", size=1) + 
      scale_y_continuous(breaks = c(myData$pretty_SDAvg , 0, myData$sd/sqrt(myData$n)), 
                         labels = c(myData$pretty_SDAvg , 0, expression(sigma/sqrt(n)))
      )
    
    # Combining plots for UI
    #p3
    #p2
    #p1
    #p1 + {
    #  p2 + p3 + plot_layout(ncol = 2)
    #} + plot_layout(ncol = 1)
    pushViewport(viewport(layout = grid.layout(2, 2)))
    vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
    print(p1, vp = vplayout(1, 1:2))
    print(p2, vp = vplayout(2, 1))
    print(p3, vp = vplayout(2, 2))
    
    #p1 + p2
    } #end if(F)
    
    #p1
    #p2
    
    
    
    subplot(p1,plotly_empty(),p2,plotly_empty(),p3, 
            nrows=5, shareY = F, titleY=T, shareX=F,titleX=T,
            heights=c(0.29,0.065,0.29,0.065,0.29))%>%
      layout(showlegend=FALSE,showlegend2=FALSE)
    #subplot(p2,plotly_empty(),p3, ncol=2, shareY = F, titleY=T, shareX=F,titleX=T)
    #p23 <- subplot(p2,plotly_empty(),p3, ncols=3, shareY = F, titleY=T, shareX=F,titleX=T)
    #subplot(style(p1, showlegend = FALSE),
    #        style(p23, showlegend = FALSE),
    #        nrows=2,
    #        shareX=F,titleX=T,
    #        titleY=T, shareY=F)
    
    #subplot(p1, p23, nrows = 2) %>%
    #  layout(title = "Walmart Store Openings by Year",
    #         xaxis = list(domain=list(x=c(0,0.5),y=c(0,0.5))),
    #         scene = list(domain=list(x=c(0.5,1),y=c(0,0.5))),
    #         xaxis2 = list(domain=list(x=c(0.5,1),y=c(0.5,1))),
    #         showlegend=FALSE,showlegend2=FALSE)
    
    #subplot(p1, p2, p3, nrows=3,
    #        shareX=F,titleX=T,
    #        titleY=T, shareY=F)
                
    
  })
  
#} #end if(F)
}
shinyApp(ui = ui, server = server)