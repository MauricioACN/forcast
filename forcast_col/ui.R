library(shiny)
source("config.R")

# Define UI for application that draws a histogram
shinyUI(#shiny UI
  fluidPage(
    leafletjs,
    titlePanel("COVID 19  Case Development"),

    sidebarPanel(width = 2,

                 radioButtons(inputId = "mapType",
                              label = "Select Map Type",
                              choices = c("Markers", "Choropleth"),
                              selected = "Choropleth",
                              inline = TRUE),
                 uiOutput("dateUI")

    ),

    mainPanel(width = 10,

              leafletOutput("map", width = "35%", height = "500px"),
              textOutput("texto")


    )
  )

)
