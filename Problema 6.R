# ==========================================================================
# Análisis de Conductividad Eléctrica en Agua de Riego
# ==========================================================================
# Autor: Dr. José Juan Torres Martínez
# Docente de Ciencias Químicas
# Universidad Autónoma de San Luis Potosí (UASLP)
# México
#
# Material académico para estudiantes
#
# Descripción: Evaluar aptitud agrícola según normativa FAO
# ==========================================================================


# Datos de conductividad (µS/cm)  
conductividad <- c(850, 1200, 650, 1500)  

# 1. Estadísticas descriptivas  
media <- mean(conductividad)  
desviacion <- sd(conductividad)  
rango <- range(conductividad)  

# 2. Clasificación FAO  
clasificar_ce <- function(ce) {  
  ifelse(ce < 700, "Excelente",  
         ifelse(ce < 1500, "Aceptable",  
                ifelse(ce < 3000, "Marginal", "No apta")))  
}  

clasificacion <- sapply(conductividad, clasificar_ce)  

# 3. Resultados  
cat("----------------------------------------\n")  
cat("INFORME TÉCNICO - CALIDAD DE AGUA DE RIEGO\n")  
cat("----------------------------------------\n")  
cat("Fecha de análisis:", format(Sys.Date(), "%d/%m/%Y"), "\n\n")  
cat("Estadísticas descriptivas:\n")  
cat("- Media:", round(media, 1), "µS/cm\n")  
cat("- Desviación estándar:", round(desviacion, 1), "µS/cm\n")  
cat("- Rango:", paste(rango, collapse = " a "), "µS/cm\n\n")  
cat("Clasificación FAO por muestra:\n")  
for (i in 1:length(conductividad)) {  
  cat("Muestra", i, "(", conductividad[i], "µS/cm):", clasificacion[i], "\n")  
}  
cat("----------------------------------------\n")  

# 4. Gráfico  
library(ggplot2)  
grafico_conductividad <- ggplot(data.frame(Muestra = factor(1:4), CE = conductividad),  
       aes(x = Muestra, y = CE, fill = clasificacion)) +  
  geom_bar(stat = "identity") +  
  geom_hline(yintercept = c(700, 1500, 3000),  
             linetype = "dashed", color = c("green", "orange", "red")) +  
  scale_fill_manual(values = c("Excelente" = "darkgreen",  
                               "Aceptable" = "goldenrod",  
                               "Marginal" = "orange",  
                               "No apta" = "firebrick")) +  
  labs(title = "Clasificación de Agua de Riego por Conductividad",  
       y = "Conductividad Eléctrica (µS/cm)",  
       fill = "Calidad") +  
  theme_minimal()  

print(grafico_conductividad)
ggsave("problema_06_conductividad.png", plot = grafico_conductividad, width = 8, height = 5, dpi = 300)

# --------------------------------------------------------------------------
# Interpretación: Clasificación de Agua de Riego por Conductividad
# --------------------------------------------------------------------------
# - Eje X: las cuatro muestras de agua (Muestra 1 a Muestra 4). Es una categoría, no una cantidad medida, por eso no tiene unidades.
# - Eje Y: la conductividad eléctrica en µS/cm (microsiemens por centímetro). La conductividad mide qué tan bien conduce el agua la corriente eléctrica, y sube cuando hay más sales disueltas. Las líneas discontinuas en 700, 1500 y 3000 µS/cm marcan los límites de clasificación de la FAO, y el color de cada barra indica su categoría.
# - Lo que se aprecia: la Muestra 1 (850 µS/cm) y la Muestra 2 (1200 µS/cm) caen en la categoría Aceptable, la Muestra 3 (650 µS/cm) es Excelente y la Muestra 4 (1500 µS/cm) queda justo sobre el límite de 1500 y se clasifica como Marginal.
# - Qué significa: tres de las cuatro muestras sirven para riego, pero la Muestra 4 está en el límite y su uso sería marginal: a mayor conductividad, mayor salinidad y mayor riesgo para los cultivos y el suelo.

# --------------------------------------------------------------------------
# Qué aprendimos
# --------------------------------------------------------------------------
# - La conductividad eléctrica (µS/cm) es un indicador rápido de la salinidad del agua de riego.
# - La FAO clasifica el agua en Excelente (menos de 700), Aceptable (menos de 1500), Marginal (menos de 3000) y No apta (3000 µS/cm o más).
# - Un valor justo en un límite cambia de categoría, por eso conviene mirar el número y no solo el color de la barra.
