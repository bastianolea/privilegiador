library(shiny)
library(shinyWidgets)
library(htmltools)
library(thematic)
library(bslib)

ui <- fluidPage(
  
  # header ----
  fluidRow(
    column(12,
           h1("Privilegiador"),
           em("Bastián Olea Herrera"),
           
           markdown(""),
           hr()
    )
  ),
  
  fluidRow(
    column(12,
           
           # selectores ----
           fluidRow(
             column(12,
                    
                    selectInput("genero", 
                                label = "Género",
                                choices = c("Femenino", "Masculino")
                    ),
                    
                    sliderInput("edad",
                                label = "Edad",
                                min = 18,
                                max = 99,
                                step = 1,
                                value = 30)
             )
           ),
           
           # educación ----
           fluidRow(
             column(12,
                    selectInput("educacion", 
                                label = "Nivel educacional",
                                choices = c()
                    )
             )
           ),
           
           # ingresos ----
           fluidRow(
             column(12,
                    sliderInput("ingreso",
                                label = "Ingresos mensuales aprox",
                                min = 200000,
                                value = 500000,
                                max = 6000000,
                                step = 100000
                    )
             )
           ),
           
           # vivienda ----
           fluidRow(
             column(12,
                    radioGroupButtons("vivienda", 
                                      label = "Vivienda",
                                      choices = c("Arriendo", "Propia")
                    )
             )
           )
    )
  ),
  
  # firma ----
  fluidRow(
    column(12, style = css(padding = "28px"),
           hr(),
           
           markdown("Desarrollado por [Bastián Olea Herrera.](https://bastian.olea.biz)"),
           
           markdown("Puedes explorar mis otras [aplicaciones interactivas sobre datos sociales en mi portafolio.](https://bastianolea.github.io/shiny_apps/)"),
           
           markdown("Código de fuente de esta app y del procesamiento de los datos [disponible en GitHub.](https://github.com/bastianolea/casen_comparador_ingresos)"),
           
           div(style = "height: 40px")
           
    )
  )
)

server <- function(input, output) {
  
}

shinyApp(ui = ui, server = server)
