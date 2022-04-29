library(shiny)
library(tidyverse)

# Leer datos
df <- read_csv("alcohol_union.csv")
# Eliminar datos faltantes
df = drop_na(df)


# Interfaz de usuario
ui <- fluidPage(
  
  # Titulo de la aplicacion
  titlePanel("Consumo de Alcohol - Rusia"),
  
  # Barra con una barra deslizadora para ingresar el número de intervalos
  sidebarLayout(
    sidebarPanel(
      sliderInput("nanio", "Anio de consumo:", value = 1998, min = 1998, max = 2016)
      ),
    
    # Grafico de la distribucion general
    mainPanel( plotOutput("plot_congreso") )
  )

)

# Logica del servidor
server <- function(input, output) {
  
  output$plot_congreso <- renderPlot({
    
    ggplot(
      filter(df, anio == input$nanio),
      aes(x = consumo, color = marca, fill=marca))+
      geom_density(alpha = 0.5)+
      xlim(-1.5, 1.5)+
      xlab("Consumo - Valor nominal")+
      ylab("Densidad")+
      scale_fill_manual(values = c("gray", "green", "purple", "orange", "blue"))+
      scale_color_manual(values = c("gray", "green", "purple", "orange", "blue"))
  })
  
}


shinyApp(ui, server)
