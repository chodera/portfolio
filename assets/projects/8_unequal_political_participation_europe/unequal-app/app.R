library(tidyverse)
library(leaflet)
library(sf)

load("data/unequal_plots.RData")
ess_full <- read_rds("data/ess_plus_nuts.rds")

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
             leafletOutput("map", width = "100%"), 
             
             absolutePanel(top = 8, right = 20,
                           selectInput(inputId = "map_selector", 
                                       label = "",
                                       choices = c("Voting", 
                                                   "Attending demonstrations",
                                                   "Working for a party",
                                                   "Boycotting products"),
                                       selected = "Voting") # end vector
                                       ) # end absolutePanel
                           ) # end column
     ), # end fluidRow
    
    fluidRow(
      column(12, align="left",
             br(),
             p("How do patterns of inequality in participation vary across Europe? Do persons with higher 
               socioeconomic status participate more than persons with lower socioeconomic status? A first 
               step to answer this question is to take a look at the relationship between socioeconomic 
               status and political participation across European countries. The diagrams below represent 
               participation rates for each mode of engagement of those with lowest (ESeC = 1) and highest 
               socioeconomic status (ESeC = 9) by country."),
             br()
      )
    ),
    
    
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
server <- function(input, output, session) {
  
  # Reactive element: color palette
  make_pal <- reactive({
    switch(input$map_selector, 
                   "Voting" = colorBin("RdYlGn", domain = ess_full$vote, bins = c(seq(50, 100, by = 10))), 
                   "Attending demonstrations" = colorBin("RdYlGn", domain = ess_full$demo, bins = c(seq(0, 30, by = 5))),
                   "Working for a party" = colorBin("RdYlGn", domain = ess_full$workparty, bins = c(seq(0, 15, by = 3))),
                   "Boycotting products" = colorBin("RdYlGn", domain = ess_full$boycott, bins = c(seq(0, 60, by = 10))))
    })
  
  variable <- reactive({
    switch(input$map_selector, 
           "Voting" = ess_full$vote, 
           "Attending demonstrations" = ess_full$demo,
           "Working for a party" = ess_full$workparty,
           "Boycotting products" = ess_full$boycott)
  })
  
  # Reactive element: legend
  make_legend <- reactive({
      labels <- sprintf(
        "<strong>NUTS code: %s</strong><br/>%g&#37;  demonstrating<br/>",
        ess_full$id_r, ess_full$demo
      ) %>% lapply(htmltools::HTML)
  })
  
  # Plot interactive map
  output$map <- renderLeaflet({
    leaflet(ess_full) %>%
      setView(10, 51, 3) %>%
      addTiles()
  })
  
  # Observe changes
  observe({
    # Colors
    pal <- make_pal()

    # Labels
    labels <- make_legend

    # Plot changes
    leafletProxy("map", data = ess_full) %>%
      clearShapes() %>% 
      clearControls() %>% 
      addPolygons(
        fillColor = ~pal(variable()),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = make_legend(),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>% 
      addLegend(pal = pal, values = ~variable(), opacity = 0.7, title = NULL,
                position = "bottomright")
  })
  
  # Plot bar plots
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