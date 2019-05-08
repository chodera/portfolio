library(tidyverse)

#setwd("~/Documents/GitHub/chodera.github.io/assets/projects/6. Cinemas in Strasbourg/cinemas-app")

load("data/cinemas_plots.RData")

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
         }')
  )),
  
  mainPanel(
      fluidRow(
        
        column(12, align="left",
               selectInput("var", 
                           label = p("Choose the indicator to be visualized:"),
                           choices = c("Number of seats", 
                                       "Number of screens",
                                       "Number of movies shown",
                                       "Number of entries",
                                       "Revenue",
                                       "Frequentation index",
                                       "Occupancy rate"),
                           selected = "Number of seats")
        )
        

      ),
      
      fluidRow(
        
        column(12, align="center",
           plotOutput("plot", width = "120%")
        )
      )
  ))
  

# Server logic ----
server <- function(input, output) {
  output$plot <- renderPlot({
    plot <- switch(input$var, 
                   "Number of seats" = seats,  
                   "Number of screens" = screens,
                   "Number of movies shown" = movies,
                   "Number of entries" = entries,
                   "Revenue" = revenue,
                   "Frequentation index" = freq,
                   "Occupancy rate" = occ
    )

    plot

  })
}

# Run app ----
shinyApp(ui, server)