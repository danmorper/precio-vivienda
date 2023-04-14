library(readxl)
library(tidyr)
library(dplyr)
library(zoo)

source("funciones.R")

house_price_index <- read_xlsx("data/house_price_index.xlsx", sheet = 3, range = "A9:AJ47")

house_price_index <- eliminar_columnas_con_NA(house_price_index)
house_price_index

house_price_index <- eliminar_filas_con_NA(house_price_index)

names(house_price_index) <- c("Codigos", "Paises", 2005:2021)

house_price_index <- replace(house_price_index, house_price_index == ":", NA)
write.csv(house_price_index,"mydatasets/house_price_index.csv", row.names = FALSE)

# Ordenar
index_ordenado <- house_price_index %>%
  gather(as.character(2005:2021), key = "year", value = "index")
index_ordenado$index <- as.numeric(index_ordenado$index)

write.csv(index_ordenado, "mydatasets/index_ordenado.csv", row.names = FALSE)


# crear un dataframe con las mismas dimensiones que df, donde se indique con 0 los valores no faltantes y con 1 los valores faltantes
df_indicator <- ifelse(is.na(house_price_index), 1, 0)

# cargar la librería plotly
library(plotly)


add_dropdown <- function(plot, x_values, y_values, names) {
  
  # agregar un botón desplegable para seleccionar el país
  plot %>% subplot(
    plotly::list(
      x = x_values[[1]],
      y = y_values[[1]],
      name = names[[1]],
      type = "scatter",
      mode = "lines"
    ),
    plotly::list(
      x = x_values,
      y = y_values,
      name = names,
      type = "scatter",
      mode = "lines",
      visible = FALSE
    ),
    nrows = length(y_values),
    margin = 0.05,
    heights = rep(1, length(y_values))
  ) %>% layout(
    updatemenus = list(
      list(
        active = 0,
        buttons = lapply(1:length(y_values), function(i) {
          list(
            method = "update",
            args = list(
              list(visible = rep(FALSE, length(y_values))),
              list(visible = TRUE, y = list(y_values[[i]])),
              list(title = names[[i]])
            ),
            label = names[[i]]
          )
        })
      )
    )
  )
  
}


# crear una función para generar el plot interactivo
interactive_line_plot <- function(df) {
  
  # definir una lista de países
  paises <- select(house_price_index, "Codigos")
  
  # definir una lista de años
  años <- colnames(df)[3:ncol(house_price_index)]
  
  # crear un trace para cada país
  traces <- lapply(paises, function(pais) {
    
    # obtener el índice del país
    i <- which(paises == pais)
    
    # crear un trace para el país
    trace <- list(
      x = años,
      y = df[i,],
      name = pais,
      type = "scatter",
      mode = "lines"
    )
    
    return(trace)
  })
  
  # crear el layout
  layout <- list(
    xaxis = list(title = "Año"),
    yaxis = list(title = "Precio de las casas"),
    updatemenus = list(
      list(
        buttons = lapply(paises, function(pais) {
          list(
            method = "update",
            args = list(list(visible = rep(FALSE, length(paises))),
                        list(visible = TRUE, y = df[which(paises == pais),], 
                             title = paste("Evolución de precios de las casas en ", pais)),
                        list(title = paste("Evolución de precios de las casas en ", pais))),
            label = pais
          )
        }),
        x = años[1],
        y = max(df),
        showactive = TRUE,
        active = which(paises == paises[1]),
        type = "dropdown",
        direction = "down"
      )
    )
  )
  
  # crear el plot interactivo
  plot_ly(
    data = traces,
    layout = layout
  )
}


house_price_index <- na.locf(house_price_index)


# llamar a la función para generar el plot interactivo

library(plotly)

# Carga de datos
data <- read.csv("datos.csv")


# Creación del gráfico interactivo
plot <- plot_ly(index_ordenado, x = ~year, y = ~index, color = ~Codigos, type = 'scatter', mode = 'lines') %>%
  layout(title = "Índice de precios de la vivienda por país y año",
         xaxis = list(title = "Año"),
         yaxis = list(title = "Índice de precios de la vivienda"))

# Visualización del gráfico
plot

interactive_line_plot(house_price_index)

row.names(house_price_index) <- house_price_index$Codigos
write.csv(house_price_index, "house_price_index.csv")





library(shiny)
library(ggplot2)

# Cargar los datos
datos <- read.csv("index_ordenado.csv")

# Crear una lista con los nombres de los países
Codigos <- unique(datos$Codigos)

# Definir la interfaz de usuario
ui <- fluidPage(
  # Crear un menú desplegable para elegir el país
  selectInput("pais", "Seleccionar país", choices = Codigos),
  # Crear un rango deslizante para elegir los años
  sliderInput("anios", "Seleccionar rango de años",
              min = 2005, max = 2021, value = c(2005, 2021)),
  # Crear el gráfico
  plotOutput("grafico")
)

# Definir el servidor
server <- function(input, output) {
  # Crear un subconjunto de los datos según la selección del usua srio
  datos_filtrados <- reactive({
    datos %>% 
      filter(Codigos == input$Codigos & 
               between(as.numeric(as.character(Año)), input$year[1], input$year[2]))
  })
  
  # Crear el gráfico a partir de los datos filtrados
  output$grafico <- renderPlot({
    ggplot(datos_filtrados(), aes(x = Año, y = Valor)) +
      geom_line() +
      ggtitle(paste("Índice de precio de la vivienda en", input$pais))
  })
}

# Ejecutar la aplicación
shinyApp(ui, server)

