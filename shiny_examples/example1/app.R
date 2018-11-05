# My example
library(reshape2)
library(shiny)
library(ggplot2)
library(gridExtra)
library(patchwork) #https://github.com/thomasp85/patchwork
library(scales) # for pretty break in ggplot
library(shinyjs)
library(shinythemes) 
# free CSS themes: https://bootswatch.com/

# For greek letters in the UI use:
# http://www.javascripter.net/faq/greekletters.htm

# Global variables
maxReps<-500
repMeans<-c()
params<-list(rep=10)
lastPoints<-c()

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



# User Interface
ui <- fluidPage(
  title="Central Limit Theorem",
  
  #titlePanel("Playing with the Central Limit Theorem"),
  #titlePanel(title=div(img(src="UC_Santa_Barbara_Wordmark_Navy_RGB.png",height = 30), "My Title")),
  titlePanel(title=div(img(src="UC-Santa-Barbara-seal-2 Color RGB.png",height = 30), 
                       img(src="UC_Santa_Barbara_Wordmark_Navy_RGB.png",height = 30))),
  #theme = shinytheme("sketchy"),
  theme = "sketchy.css",
  tags$head(HTML("<title>Baller Lab</title>")), #Without company logo
  #navbarPage(title = div(img(src="UC-Santa-Barbara-seal-2 Color RGB.png", style="margin-top: -14px;", height = 50)))),
  
  useShinyjs(),
  inlineCSS(appCSS),
  #tags$head(tags$style(HTML(mycss))),
  # Loading message
  div(id = "loading-content", h2("Loading...")),
  
  hidden(
    div(id = "app-content",
  
  
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
    sliderInput("mu", "Mean \\(\\mu\\)", 
                min=-10, max=10, value=1,step=0.01),
    #sliderInput("sd", "Normal std. deviation \u03C3:", 
    sliderInput("sd", "Std. deviation \\(\\sigma\\)", 
                min=1, max=10, value=1,step=0.5),
    #sliderInput("n", "Sample size n:", 
    sliderInput("n", "Sample size \\(n\\)", 
                min = 5, max = 100, value = 5, step= 5),
    sliderInput("rep", "# of replications of \\(\\bar{X}\\) ", 
                min = 10, max = maxReps, value = 5, step= 10),
    #actionButton("buttonN", "+5n"),
    h5('Add more replications with the buttons below!'),
    actionButton("buttonRep10", "+10 replications"),
    actionButton("buttonRep20", "+20 replications")
  ),
  # Show a plot of the generated distribution
  fluidRow(
    #column(7,plotOutput("plotAllPlots"))
    
    column(10,
           title=div(
             img(src="UC_Santa_Barbara_Wordmark_Navy_RGB.png",
                 height='30',
                 align = "right"),
             img(src="UC-Santa-Barbara-seal-2 Color RGB.png",
                 height='40',
                 align = "right")
           ),
              withSpinner(plotOutput("plotAllPlots"),type=4,color="#1d3f72")
           #)
    )
  )# fluidRow
) #end div
) #end hidden  
)# end ui



#Server
server <- function(input, output, session) {
  #Stop app when closing browser
  session$onSessionEnded(stopApp)
  # Hide the loading message when the rest of the server function has executed
  Sys.sleep(1)
  hide(id = "loading-content", anim = TRUE, animType = "fade")
  show("app-content")
  
  observeEvent(input$buttonRep10, {
    reps=10
    params$rep<<-params$rep + reps
    lastPoints<<-sapply(1:reps,
                       function(i) mean(rnorm(input$n,mean=input$mu,sd=input$sd)))
    repMeans<<-c(repMeans,lastPoints) 
    #print('repMeans in buttonRep');print(repMeans)
    updateSliderInput(session, "rep", value=input$rep + reps)

  })

  observeEvent(input$buttonRep20, {
    reps=20
    params$rep<<-params$rep + reps
    lastPoints<<-sapply(1:reps,
                       function(i) mean(rnorm(input$n,mean=input$mu,sd=input$sd)))
    repMeans<<-c(repMeans,lastPoints) 
    #repMeans<<-c(repMeans,
    #             sapply(1:reps,
    #                    function(i) mean(rnorm(input$n,mean=input$mu,sd=input$sd)))
    #) 
    #print('repMeans in buttonRep');print(repMeans)
    updateSliderInput(session, "rep", value=input$rep + reps)
    
  })
  
  
  # storing data from interactive menu
  mydata <- reactive({
    mu <- input$mu
    n <- input$n
    rep <- input$rep
    sd <- input$sd
    
    
    # Initial parameters
    if (length(params)==1){
      params$mu <<- mu
      params$sd <<- sd
      params$n <<- n
    }
    
    
    # Initial replications
    if(length(repMeans)==0){
      lastPoints<<-sapply(1:rep,function(i) mean(rnorm(n,mean=mu,sd=sd)))
      repMeans<<-c(repMeans,lastPoints)
      #repMeans<<-c(repMeans,sapply(1:rep,function(i) mean(rnorm(n,mean=mu,sd=sd))))
      params$rep <<- 10
    }
    
    # Changes in replication slider
    if(length(repMeans)<rep){
      lastPoints<<-sapply(1:(length(repMeans)-rep),
                          function(i) mean(rnorm(input$n,mean=input$mu,sd=input$sd)))
      repMeans<<-c(repMeans,lastPoints)
      #repMeans<<-c(repMeans,
      #             sapply(1:(length(repMeans)-rep),
      #                    function(i) mean(rnorm(input$n,mean=input$mu,sd=input$sd)))
      #        )
      params$rep <<- params$rep + length(repMeans)-rep
      }
    
    # Changes in standard deviation, mu (Reset)
    if(params$mu != mu || params$sd != sd || params$n != n){
      
      # update parameters
      print("change in mu"); print(params$mu != mu);
      print("change in sd"); print(params$sd != sd);
      print("change in n"); print(params$n != n);
      
      params$mu <<- mu
      params$sd <<- sd
      params$n <<- n
      params$rep <<- 10
      updateSliderInput(session, "rep", value=10)
      
      #repMeans=c()
      lastPoints<<-sapply(1:10,function(i) mean(rnorm(n,mean=mu,sd=sd)))
      repMeans<<-lastPoints
      #repMeans<<-sapply(1:10,function(i) mean(rnorm(n,mean=mu,sd=sd)))
      print('repMeans in update params')
      print(repMeans)
      
      }
    
    dfReplications<-data.frame(repMeans)
    # Average of replications 
    step= round(0.05 *rep)
    #subRep = seq(from=step,to=rep,by=step)
    subRep = 1:rep
    meanAvg <- sapply(1:rep,function(i) mean(repMeans[1:i]))
    dfMeanAvg <- data.frame(y=meanAvg[subRep],x=subRep)
    colnames(dfMeanAvg) <- c("meanAvg","replications")
    # SD of replications 
    #sdAvg <- sapply(1:rep,function(i) sd(repMeans[1:i]))
    subRep = 2:rep
    sdAvg <- sapply(2:rep,function(i) sd(repMeans[1:i]))
    dfSDAvg <- data.frame(y=sdAvg,x=subRep)
    colnames(dfSDAvg) <- c("sdAvg","replications")
    
    list(dfMeanAvg=dfMeanAvg,
         dfSDAvg=dfSDAvg,
         dfReplications=dfReplications,
         mu=input$mu,
         sd=input$sd,
         n=input$n
         )
  })
  
  # Plots
  output$plotAllPlots <- renderPlot({
    
    # p1: Histogram of sample means
    myData<-mydata()
    dfReplications <- myData$dfReplications
    #print('repMeans in plot')
    #print(repMeans)
    #z<-rev(tail(repMeans,10))
    #z <- data.frame(variable=levels(dfReplications$repMeans), 
    #                mean=c(-2,-1,0,0.5,1,2))
    
    p1 <- ggplot(dfReplications, aes(repMeans)) + 
      geom_histogram(aes(y = stat(density)),
                     color="darkblue", 
                     fill="lightblue",
                     #binwidth = 0.05) +
                     binwidth = 0.05*myData$sd) +
      xlim(c(myData$mu-3*myData$sd,myData$mu+3*myData$sd)) + 
      xlab(expression(bar(X))) + 
      geom_vline(aes(xintercept=input$mu),color="red", linetype="dashed", size=1) + 
      stat_function(
        fun = dnorm, args = list(mean = input$mu, sd = input$sd/sqrt(input$n)), 
        lwd = 2,col = 'red', alpha=0.3
        )

      xlim(c(myData$mu-3*myData$sd,myData$mu+3*myData$sd))
    
    
    # p2: Evolution of Avg of replications
    dfMeanAvg <- myData$dfMeanAvg
    yvaluesMean <- c(abs(dfMeanAvg$meanAvg), myData$mu - abs(dfMeanAvg$meanAvg-myData$mu))
    #print(yvalues)
    
    pretty_MeanAvg <- pretty(yvaluesMean)[abs(pretty(yvaluesMean)- myData$mu )> 0.01]
    #print(sort(c(pretty_MeanAvg, myData$mu)))
    p2<-ggplot(dfMeanAvg, aes(x=replications,y=meanAvg)) + 
      geom_line(color='darkblue')+ ylab(expression(paste('Average of ',bar(x))))+
      geom_hline(aes(yintercept=input$mu),
                 color="red", linetype="dashed", size=1) + 
      xlab('# of replications') +
      scale_x_continuous(breaks= pretty_breaks()) +
      scale_y_continuous(breaks = c(pretty_MeanAvg, myData$mu), 
                         labels = c(pretty_MeanAvg, expression(mu))
                         )+
      geom_rect(mapping=aes(xmin=length(replications)-10, xmax=length(replications), 
                            ymin=min(c(pretty_MeanAvg, myData$mu)), 
                            ymax=max(c(pretty_MeanAvg, myData$mu))),
                fill="khaki1", alpha=0.02)
    
    # p3: Evolution of SD of replications
    dfSDAvg <- myData$dfSDAvg
    #print('dfSDAvg')
    #print(dfSDAvg)
    yvaluesSDAvg <- c(abs(dfSDAvg$sdAvg), myData$sd/sqrt(myData$n) - abs(dfSDAvg$sdAvg- myData$sd/sqrt(myData$n)))
    #print(yvaluesSDAvg)
    pretty_SDAvg <- pretty(yvaluesSDAvg)[abs(pretty(yvaluesSDAvg)- myData$sd/sqrt(myData$n) )> 0.01]
    #print(sort(c(pretty_SDAvg, myData$sd/sqrt(myData$n))))
    

    p3<-ggplot(dfSDAvg, aes(x=replications,y=sdAvg)) + 
      geom_line(color='darkblue')+ ylab(expression(paste('Std. Deviation of ',bar(x))))+
      xlab('# of replications') + scale_x_continuous(breaks= pretty_breaks()) +
      geom_hline(aes(yintercept=input$sd/sqrt(input$n)),
                 color="red", linetype="dashed", size=1) + 
      scale_y_continuous(breaks = c(pretty_SDAvg , 0, myData$sd/sqrt(myData$n)), 
                          labels = c(pretty_SDAvg , 0, expression(sigma/sqrt(n)))
                         )+
      geom_rect(mapping=aes(xmin=length(replications)-9, xmax=length(replications)+1, 
                            ymin=min(c(pretty_SDAvg , 0, myData$sd/sqrt(myData$n))), 
                            ymax=max(c(pretty_SDAvg , 0, myData$sd/sqrt(myData$n)))),
                fill="khaki1", alpha=0.02)
    
    # Combining plots for UI
    p1 + {
      p2 + p3 + plot_layout(ncol = 2)
    } + plot_layout(ncol = 1)
    
  })
  

  
  
  
}# end server

shinyApp(ui = ui, server = server)