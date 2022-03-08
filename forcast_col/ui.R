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

               uiOutput("dateUI")

  ),

  mainPanel(width = 10,

            leafletOutput("map", width = "100%", height = "500px")

  )
)
)
