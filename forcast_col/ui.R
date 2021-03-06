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
        tabItem(tabName = 'panel_general',
                tabBox(id='tab_files', title = 'Sobre la herramienta', width = '600px',height = '100%',
                       tabPanel('Sobre los Datos', value = 'pg_datos'),
                       tabPanel('Sobre los Gráficos', value = 'pg_graficos'),
                       tabPanel('Sobre los Modelos', value = 'pg_modelos'))),
        tabItem(tabName = 'cargue_dataset',
                tabBox(id='cargue_dataset_1', title = 'Análisis Departamento', width = '600px',height = '100%',
                       tabPanel('Análisis General', value = 'cd_general',
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
         box(title = "Participación por Sector en Subsidios - Top 5",plotlyOutput("plot_sector",height = 550),width = 12,status = "primary",solidHeader = TRUE)),
         column(width = 6,box(title = "Comportamiento por Departamento",leafletOutput("map", height = 740),width = 12,status = "primary",solidHeader = TRUE))
         )
      ),
      tabPanel('Análisis Detallado', value = 'cd_detalle',
               fluidRow(column(width = 4,
                               box(title = "Configuración",uiOutput("deptoUI"),width = 12,status = "primary",solidHeader = TRUE)
                               ),
                        column(width = 8,
                               box(title = "Participación Sector por Monto de Subsidios",plotlyOutput("part_graf_depto"),width = 12,status = "primary",solidHeader = TRUE),
                               box(title = "Serie Histórica Monto de Subsidios",plotlyOutput("hist_graf_depto"),width = 12,status = "primary",solidHeader = TRUE)
                               )
                        )
               )
      )
      ),
    tabItem(tabName = 'calculo_ratios',
            tabBox(id='ct_1', title = 'Análisis Municipio', width = '600px',height = '100%',
                   tabPanel(title = "Análisis General",value = 'cr_general',
                            fluidRow(
                              column(width = 12,
                                     valueBoxOutput("value7",width = 2),
                                     valueBoxOutput("value8",width = 2),
                                     valueBoxOutput("value9",width = 2),
                                     valueBoxOutput("value10",width = 2),
                                     valueBoxOutput("value11",width = 2),
                                     valueBoxOutput("value12",width = 2))),
                            fluidRow(column(width = 6,
                                            box(title = "Configuración",uiOutput("mpioUI"),width = 12,status = "primary",solidHeader = TRUE),
                                            box(title = "Participación por Sector en Subsidios - Top 5",plotlyOutput("plot_sector_mp",height = 550),width = 12,status = "primary",solidHeader = TRUE)
                                            ),
                                     column(width = 6,box(title = "Comportamiento por Departamento",leafletOutput("map_mp", height = 740),width = 12,status = "primary",solidHeader = TRUE))
                                     )
                            ),
                   tabPanel(title = "Análisis Detallado",value = "cr_detalle",
                            fluidRow(column(width = 4,
                                            box(title = "Configuración",uiOutput("standarUI"),width = 12,status = "primary",solidHeader = TRUE)
                            ),
                            column(width = 8,
                                   box(title = "Participación Sector por Monto de Subsidios",plotlyOutput("part_graf_mpio"),width = 12,status = "primary",solidHeader = TRUE),
                                   box(title = "Serie Histórica Monto de Subsidios",plotlyOutput("hist_graf_mpio"),width = 12,status = "primary",solidHeader = TRUE)
                                   )
                            )
                            )
                   )
            ),
    tabItem(tabName = 'calculo_fw',
            tabBox(id='cfw', title = 'Proyecciones', width = '600px',height = '100%',
                   tabPanel(title = "Análisis General",value = 'cr_general',
                            fluidRow(column(width = 3,
                                            box(title = "Configuración",uiOutput("proyeUI"),width = 12,status = "primary",solidHeader = TRUE)
                                            ),
                            column(width = 9,
                              tabBox(id = "sds",title = "Proyecciones por Departamento",width = 12,
                                     tabPanel(title = "Crédito",plotOutput("proye_depto")),
                                     tabPanel(title = "Subsidio",plotOutput("proye_depto_subsi"))),
                              box(title = "Métrica de Desempeño de Modelo(s)",
                                             dataTableOutput("mape"),
                                             width = 12,
                                             status = "primary",
                                             solidHeader = TRUE)
                            )
                            )
                            )
                   )
            )
  )
)
)

