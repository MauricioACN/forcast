#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  leafletjs,
  titlePanel("COVID 19  Case Development"),

  sidebarPanel(width = 2,

               radioButtons(inputId = "mapType",
                            label = "Select Map Type",
                            choices = c("Markers", "Choropleth"),
                            selected = "Markers",
                            inline = TRUE),

               radioButtons(inputId = "frequency",
                            label = "Select Data Frequency",
                            choices = c("days", "weeks"),
                            selected = "weeks",
                            inline = TRUE
               ),

               uiOutput("dateUI")

  ),

  mainPanel(width = 10,

            leafletOutput("map", width = "70%", height = "750px")

  )
)
)
