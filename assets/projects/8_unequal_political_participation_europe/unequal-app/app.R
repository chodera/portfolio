library(tidyverse)

load("data/unequal_plots.RData")

# User interface ----
ui <- fluidPage(

  
  tags$head(
    tags$style(HTML("
                    @import url('//fonts.googleapis.com/css?family=Oswald|Merriweather');
                    
                    p {
                    font-family: 'Merriweather', serif;
                    }
                    
                    "))
    ),
  
  tags$style("#Id004, #var {font-family: 'Merriweather', serif;}"), 
  
  tags$style(type='text/css', 
             ".selectize-input {font-family:'Merriweather', serif;} 
              text {font-family: 'Merriweather', serif}
             .selectize-dropdown {font-family:'Merriweather', serif;"),
  
  tags$head(tags$style(
    HTML('
         #sidebar {
         background-color: white;
         border-color: white;
         }'),
    
    tags$head(
     tags$style(type="text/css", "text {font-family: 'Merriweather', serif}")
    )
    
  )),
  
  mainPanel(
      fluidRow(
        
        column(12, align="left",
               selectInput("var", 
                           label = p("Choose the form of participation:"),
                           choices = c("Voting", 
                                       "Attending demonstrations",
                                       "Working for a party",
                                       "Boycotting products"),
                           selected = "Voting")
        )
        

      ),
      
      fluidRow(
        
        column(12, align="center",
           plotOutput("plot", width = "100%")
        )
      )
  ))
  

# Server logic ----
server <- function(input, output) {
  output$plot <- renderPlot({
    plot <- switch(input$var, 
                   "Voting" = vote_bar, 
                   "Attending demonstrations" = demo_bar,
                   "Working for a party" = work_bar,
                   "Boycotting products" = boycott_bar
    )

    plot

  })
}

# Run app ----
shinyApp(ui, server)