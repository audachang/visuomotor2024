require(shiny)

source("setup.R")
source("proc.R")
# Define UI for app that draws a histogram ----

ui <- fluidPage(
  
  # App title ----
  titlePanel("Plotting visuomotor adaptation group outcomes..."),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    selectInput("participants", 
                label = "Choose participants for group plot",
                choices = sids,
                selected = NULL, 
                multiple = T),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      textOutput("selected_participants"),
      plotOutput(outputId = "distPlot")
      
    )
  )
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  #processing data

  
  dall.summ <- reactive({
    proc(input$participants)
  })
  
  plotInput <- reactive({
    a <- 3
    b <- 2
    N <- length(input$participants)
    
    f_gperr <- ggplot(dall.summ(), 
                      aes(x = errblock, 
                          y = error, 
                          group = condition, 
                          color = condition))+
      geom_point(size = 2) + 
      geom_errorbar(aes(ymin=lower, ymax = upper),
                    width = 0.5,alpha=0.3) +
      #  stat_smooth(method="lm", 
      #              formula= y ~ log(x), se=F,
      #              linetype = 2, 
      #              color = 'black')+
      stat_smooth(method = "nls",
                  formula = y ~ p1 + p2*log(x),
                  start = list(p1 = a, p2 = b),
                  se = FALSE,
                  control = list(maxiter = 100),
                  color = 'black')+
      xlab('Mini-block (4-trials)')+
      ylab('Angular Error (deg)')+
      theme(legend.position="bottom")+
      scale_x_continuous(breaks=seq(1,73, 12))+
      geom_hline(yintercept=0, 
                 color = '#0099ee',
                 alpha=0.3,size=3)+
      ylim(-30, 30) +
      ggtitle(sprintf('Group Average (N = %d)', N))
    
  })
  
  
  output$selected_participants <- renderText({ 
    paste(input$participants, sep=",")
  })
  
  output$distPlot <- renderPlot({
    
    print(plotInput())
    #ggsave(paste('figures/errors/', input$participant, '_error.png', sep=''),
    #       width = 1920, height= 1080, units = 'px', dpi =300 )
    
  })
  
  
 
  

}

shinyApp(ui = ui, server = server)

