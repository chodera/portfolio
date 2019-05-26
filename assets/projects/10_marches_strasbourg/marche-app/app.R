# Load packages
library(tidyverse)
library(shinyWidgets)
library(leaflet)
library(leaflet.extras)

# Load data set
df <- read_rds("data/marche_stras.rds")

# Define icons
marketIcons <- awesomeIconList(
  
  basket = makeAwesomeIcon(
    icon = 'shopping-basket',
    iconColor = '#ffffff',
    library = 'fa',
    markerColor = "green"
  ),
  
  book = makeAwesomeIcon(
    icon = 'book',
    iconColor = '#ffffff',
    library = 'fa',
    markerColor = "green"
  ),
  
  flower = makeAwesomeIcon(
    icon = 'ios-rose',
    iconColor = 'white',
    library = 'ion',
    markerColor = "green"
  ),
  
  brocante = makeAwesomeIcon(
    icon = 'shopping-bag',
    iconColor = '#ffffff',
    library = 'fa',
    markerColor = "green"
  )
)



# User interface ----
ui <- bootstrapPage(

  
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
             .selectize-dropdown {font-family:'Merriweather', serif;}
             label {font-family:'Merriweather', serif; font-weight: bold}"), # Check box label text
  
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
  
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  
  leafletOutput("map", width = "100%", height = "100%"),
  
  absolutePanel(top = 10, right = 10,
                selectInput(inputId = "jours", 
                            label = "",
                            choices = list(
                "Tous les jours",
                "Jour de la semaine" = c("Lundi",
                                         "Mardi",
                                         "Mercredi",
                                         "Jeudi",
                                         "Vendredi",
                                         "Samedi") # end vector
                ) # end list
                ),
                prettyCheckbox("food", "Uniquement alimentation", FALSE, icon = icon("check")) # end selectInput
  ) # end absolutePanel
) # end bootstrapPage


# Server logic ----
server <- function(input, output, session) {

  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    day_filter <- switch(input$jours,
                         "Tous les jours" = str_detect(df$day, ".*"),
                         "Lundi" = str_detect(df$day, "Lundi"),
                         "Mardi" = str_detect(df$day, "Mardi"),
                         "Mercredi" = str_detect(df$day, "Mercredi"),
                         "Jeudi" = str_detect(df$day, "Jeudi"),
                         "Vendredi" = str_detect(df$day, "Vendredi"),
                         "Samedi" = str_detect(df$day, "Samedi")
                         )
    if (input$food) {
      df %>%
        filter(day_filter) %>% 
        filter(food == TRUE)
    } else {
      df %>%
        filter(day_filter)
    }

  })
  
  # Plot interactive map
  output$map <- renderLeaflet({
    leaflet(df) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat)) %>% 
      addFullscreenControl()
    })
 
  # Filter by day and food
  observe({
    proxy <- leafletProxy("map", data = filteredData()) %>% 
    clearMarkers() %>% 
    addAwesomeMarkers(
          lng = ~lon,
          lat = ~lat,
          popup = ~name,
          icon = ~marketIcons[type])
  })

}

# Run app ----
shinyApp(ui, server)