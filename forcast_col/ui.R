library(shiny)
source("config.R")
library(dashboardthemes)
library(shinydashboard)
library(plotly)

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
      tabItems(#panel general
        tabItem(tabName = 'panel_general'),
        tabItem(tabName = 'cargue_dataset',
      fluidRow(
        column(width = 12,
               valueBoxOutput("value1",width = 2),
               valueBoxOutput("value2",width = 2),
               valueBoxOutput("value3",width = 2),
               valueBoxOutput("value4",width = 2),
               valueBoxOutput("value5",width = 2),
               valueBoxOutput("value6",width = 2))),
       fluidRow(column(width = 6,
         box(title = "Configuración",uiOutput("dateUI"),width = 12,status = "primary",solidHeader = TRUE),
         box(title = "Participación por Sector General",plotlyOutput("plot_sector",height = 550),width = 12,status = "primary",solidHeader = TRUE)),
         column(width = 6,box(title = "Comportamiento por Departamento",leafletOutput("map", height = 740),width = 12,status = "primary",solidHeader = TRUE))
    )
    ),
    tabItem(tabName = 'calculo_ratios'),
    tabItem(tabName = 'calculo_fw')
  )
)
)

