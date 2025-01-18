
##PREGUNTA 1.1

# Cargar librerías
library(httr)
library(XML)
library(dplyr)



# Descargar la página
url <- "https://www.mediawiki.org/wiki/MediaWiki"
response <- GET(url)

# Convertir HTML a XML
content_xml <- htmlParse(content(response, as = "text"))


##PREGUNTA 1.2

# Extraer el título

# Extraer el título de la página
obtener_titulo <- function(xml_contenido) {
  titulo <- xpathSApply(xml_contenido, "//title", xmlValue)
  return(titulo)
}

# Ejemplo de uso
titulo <- obtener_titulo(content_xml)
cat("Título de la página:", titulo, "\n")


##PREGUNTA 1.3

# Extraer enlaces y textos
links <- xpathSApply(content_xml, "//a", xmlGetAttr, "href")
texts <- xpathSApply(content_xml, "//a", xmlValue)

# Limpiar valores nulos
links[is.null(links)] <- NA
texts[is.null(texts)] <- NA
links <- unlist(links)
texts <- unlist(texts)

##PREGUNTA 1.4

# Función para generar tabla de frecuencias y mantener el dataframe original
procesar_enlaces <- function(links, texts) {
  # Crear el dataframe con enlaces y textos
  data <- data.frame(Link = links, Text = texts, stringsAsFactors = FALSE)
  
  # Generar la tabla de frecuencias
  data_summary <- as.data.frame(table(data$Link), stringsAsFactors = FALSE)
  
  # Renombrar columnas para mayor claridad
  colnames(data_summary) <- c("Link", "Frequency")
  
  # Ordenar la tabla de frecuencias por frecuencia descendente
  data_summary <- data_summary[order(-data_summary$Frequency), ]
  
  # Devolver tanto la tabla de frecuencias como el dataframe original
  return(list(data = data, data_summary = data_summary))
}

# Llamar a la función y guardar los resultados
resultados <- procesar_enlaces(links, texts)

# Separar los resultados
data <- resultados$data
data_summary <- resultados$data_summary

# Mostrar las primeras filas de la tabla de frecuencias
head(data_summary)


##PREGUNTA 1.5

# Base URL del dominio
base_url <- "https://www.mediawiki.org"

# Procesar cada enlace del data.frame original 'data'
data$status_code <- sapply(data$Link, function(link) {
  # Manejo de enlaces (relativos, absolutos, subdominios e internos)
  if (is.na(link)) {
    full_url <- NA
  } else if (grepl("^http", link)) {
    full_url <- link  # URLs absolutas
  } else if (grepl("^//", link)) {
    full_url <- paste0("https:", link)  # Subdominios
  } else if (grepl("^#", link)) {
    full_url <- paste0(base_url, link)  # URLs internas con tags
  } else {
    full_url <- paste0(base_url, link)  # URLs relativas
  }
  
  # Intentar obtener el código de estado usando HEAD
  tryCatch({
    if (!is.na(full_url)) {
      response <- HEAD(full_url)
      status_code <- response$status_code
    } else {
      status_code <- NA
    }
  }, error = function(e) {
    status_code <- NA  # Asignar NA en caso de error
  })
  
  # Pausa entre solicitudes para evitar ser "baneados"
  Sys.sleep(1)
  
  # Retornar el código de estado
  return(status_code)
})

# Resumir los datos en el data.frame final 'final_data'


final_data <- data %>%
  group_by(Link) %>%
  summarize(
    Text = first(Text),
    Frequency = n(),
    Status_Code = first(status_code)
  )

# Mostrar el resumen final
print(final_data)

##PREGUNTA 2.1

# Categorizar URLs como absolutas o relativas
data$Type <- ifelse(grepl("^http", data$Link), "Absoluta", "Relativa")

# Crear histograma
library(ggplot2)

histogram_plot <- ggplot(data, aes(x = Type, fill = Type)) +
  geom_bar() +
  ggtitle("Frecuencia de URLs Absolutas vs Relativas") +
  xlab("Tipo de URL") +
  ylab("Frecuencia") +
  theme_minimal()

print(histogram_plot)

##PREGUNTA 2.2
# Función para clasificar enlaces y generar el gráfico de barras
clasificar_y_graficar_enlaces <- function(data, dominio_interno = "mediawiki.org") {
  library(ggplot2)
  
  # Clasificar enlaces como internos o externos
  data$Domain <- ifelse(
    grepl(dominio_interno, data$Link, fixed = TRUE), 
    "Interno", 
    "Externo"
  )
  
  # Crear el gráfico de barras
  bar_plot <- ggplot(data, aes(x = Domain, fill = Domain)) +
    geom_bar() +
    ggtitle("Enlaces Internos vs Externos") +
    xlab("Tipo de Enlace") +
    ylab("Cantidad") +
    scale_fill_manual(values = c("Interno" = "#1f78b4", "Externo" = "#33a02c")) + # Colores personalizados
    theme_minimal() +
    theme(
      legend.position = "none", # Ocultar leyenda (opcional)
      plot.title = element_text(hjust = 0.5, face = "bold") # Centrar título
    )
  
  # Imprimir el gráfico
  print(bar_plot)
}

# Llamada a la función
clasificar_y_graficar_enlaces(data)


##PREGUNTA 2.3

# Resumir códigos de estado
status_summary <- table(data$status_code)
status_percent <- prop.table(status_summary) * 100

# Darle color al pastel
library(RColorBrewer)
myPalette <- brewer.pal(5, "Set2") 

# Crear gráfico de pastel
pie_chart <- pie(
  status_percent,
  labels = paste0(names(status_percent), " (", round(status_percent, 1), "%)"),
  main = "Distribución de Códigos de Estado",border="white", col=myPalette
)
