library(tidyverse)
library(readxl)
library(maptools)
library(classInt)	
library(RColorBrewer)	
library(sf)
library(shinyWidgets)

alsace <- readRDS("data/alsace.rds")
# Drop mssing values to avoid warning messages when running the app
alsace <- alsace %>% 
  drop_na(densite, chomage, natalite, mortalite, logements_vacants)

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
  
  sidebarLayout(
    sidebarPanel(id="sidebar",
                 helpText(p('Créez des cartes d\'Alsace avec les informations de la «Base comparateur de territoires» (2018) de l\'INSEE.')),
                 
                 br(),
                 
                 selectInput("var", 
                             label = p("Chosissez un indicateur à visualiser:"),
                             choices = c("Densité", 
                                         "Taux de chômage",
                                         "Taux de natalité",
                                         "Taux de mortalité",
                                         "Logements vacants"),
                             selected = "Densité"),
                 
                 br(),
                 
                 radioButtons(
                   inputId = "Id004",
                   label = p("Interval:"), 
                   choices = c("Quantile", "Pretty")

                 )
                 
                 
    ),
    
    mainPanel(plotOutput("plot", width = "100%", height = "730px"))
  )
    )

# Server logic ----
server <- function(input, output) {
  output$plot <- renderPlot({
    data <- switch(input$var, 
                   "Densité" = select(alsace, densite, geometry),
                   "Taux de chômage" = select(alsace, chomage, geometry),
                   "Taux de natalité" = select(alsace, natalite, geometry),
                   "Taux de mortalité" = select(alsace, mortalite, geometry),
                   "Logements vacants" = select(alsace, logements_vacants, geometry)
    )
    col <- switch(input$var, 
                  "Densité" = alsace$densite,
                  "Taux de chômage" = alsace$chomage,
                  "Taux de natalité" = alsace$natalite,
                  "Taux de mortalité" = alsace$mortalite ,
                  "Logements vacants" = alsace$logements_vacants
    )    
    legend_title <- switch(input$var, 
                           "Densité" = "Habitants par km²",
                           "Taux de chômage" = "% Chômeurs",
                           "Taux de natalité" = "Pour 1 000 habitants",
                           "Taux de mortalité" = "Pour 1 000 habitants",
                           "Logements vacants" = "Pour 1 000 habitants"
    )
    interval <- switch(input$Id004, 
                           "Quantile" = "quantile",
                           "Pretty" = "pretty"
    )
    
    ## Define range and colors 
    distr	<- classIntervals(col, 9, style=interval)$brks
    couleurs<- brewer.pal(9,"YlOrRd")
    colMap <-couleurs[(findInterval(col, distr,	all.inside=TRUE))]	
    par(family = "Merriweather")
    plot(data, col=colMap, main = "")	
    
    legend("left", 
           title = legend_title,
           legend = leglabs(round(distr,2),	
                            over=">",	
                            under="<"),	
           fill = couleurs, bty = "n")
    
  })
}

# Run app ----
shinyApp(ui, server)