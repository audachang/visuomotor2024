require(shiny)

source("setup.R")
source("proc.R")
# Define UI for app that draws a histogram ----

ui <- fluidPage(
  
  # App title ----
  titlePanel("Plotting visuomotor adaptation outcomes..."),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    selectInput("participant", 
                label = "Choose a participant to display",
                choices = sids,
                selected = NULL),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      textOutput("selected_participant"),
      plotOutput(outputId = "distPlot")
      
    )
  )
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  #processing data

  
  d_cross_bk <- reactive({
    proc(droot1, droot2, input$participant)
  })
  
  plotInput <- reactive({
    xlabstr_err <- 'Mini blocks (4 trials)'
    p <- ggplot(d_cross_bk(), 
                aes(x = errblock, y = angle, 
                    color = condition,
                    group = condition)) +
      geom_point(size = 3) +
      stat_smooth(method="lm", 
                  formula= y ~ x, se=FALSE,
                  linetype = 1)+
      stat_smooth(method="lm", 
                  formula= y ~ log(x), se=FALSE,
                  linetype = 2, color='black')+
      ggtitle(input$participant) +
      ylab("angular error (deg)") +
      xlab(xlabstr_err) +
      theme(legend.position="bottom")+
      scale_x_continuous(breaks=seq(1,73, 12))
      
      
  })
  
  
  output$selected_participant <- renderText({ 
    paste("You have selected this: ", input$participant)
  })
  
  output$distPlot <- renderPlot({
    
    print(plotInput())
    ggsave(paste('figures/errors/', input$participant, '_error.png', sep=''),
           width = 1920, height= 1080, units = 'px', dpi =300 )
    
  })
  
  
  # output$downloadPlot <- downloadHandler(
  #   filename = function() { 
  #     sprintf('figures/errors/%s_error.png',
  #             input$participant) },
  #   content = function(file) {
  #     ggsave(file, plot = plotInput(), device = "png")
  #   }
  #)
  

}

shinyApp(ui = ui, server = server)

