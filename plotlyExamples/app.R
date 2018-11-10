library(shiny)
library(plotly)

ui <- fluidPage(
  plotlyOutput("plot"),
  verbatimTextOutput("event")
)

server <- function(input, output) {
  
  # renderPlotly() also understands ggplot2 objects!
  output$plot <- renderPlotly({
    
    x <- seq(0,10, length.out = 1000)
    
    # create data
    aval <- list()
    for(step in 1:100){
      aval[[step]] <-list(visible = FALSE,
                          name = paste0('v = ', step),
                          x=x,
                          y=sin(step*x))
    }
    aval[3][[1]]$visible = TRUE
    
    # create steps and plot all traces
    steps <- list()
    p <- plot_ly()
    for (i in 1:100) {
      p <- add_lines(p,x=aval[i][[1]]$x,  y=aval[i][[1]]$y, visible = aval[i][[1]]$visible, 
                     name = aval[i][[1]]$name, type = 'scatter', mode = 'lines', hoverinfo = 'name', 
                     line=list(color='00CED1'), showlegend = FALSE)
      
      step <- list(args = list('visible', rep(FALSE, length(aval))),
                   method = 'restyle')
      step$args[[2]][i] = TRUE  
      steps[[i]] = step 
    }  
    
    # add slider control to plot
    p <- p %>%
      layout(sliders = list(list(active = 3,
                                 currentvalue = list(prefix = "Frequency: "),
                                 steps = steps)))
    p
  
    })
  
  output$event <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover on a point!" else d
  })
}

shinyApp(ui, server)