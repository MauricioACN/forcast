library(shiny)
source("config.R")
library(dashboardthemes)
library(shinydashboard)

# Define UI for application that draws a histogram
ui <- dashboardPage(skin = 'blue',
    # leafletjs,
    dashboardHeader(title = "Análisis de crédito"),
    dashboardSidebar(disable = F,
                     sidebarMenu(id = 'menu',
                                 menuItem('Sobre la herramienta', tabName = 'panel_general', icon=shiny::icon('file')),
                                 menuItem('Análisis Departamento', tabName = 'cargue_dataset', icon=shiny::icon('file-upload')),
                                 menuItem('Análisis Municipio', tabName = 'calculo_ratios', icon=shiny::icon('cog')),
                                 menuItem('Proyecciones', tabName = 'calculo_fw', icon=shiny::icon('cogs'))
                     )),
    dashboardBody(
      fluidRow(
        column(width = 12,
               valueBoxOutput("value1",width = 2),
               valueBoxOutput("value2",width = 2),
               valueBoxOutput("value3",width = 2),
               valueBoxOutput("value4",width = 2),
               valueBoxOutput("value5",width = 2),
               valueBoxOutput("value6",width = 2))),
       fluidRow(
         box(title = "Configuración",uiOutput("dateUI")),
         box(title = "Comportamiento por Departamento",
             leafletOutput("map")
             )
    )
  )
)

# width = 2,
# uiOutput("varUI"),
#
