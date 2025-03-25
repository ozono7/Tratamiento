# -----------------------------------------------------------
# Autor: José Juan Torres Martínez  
# Doctorado en Ciencias en Bioprocesos  
# Laboratorio de Biotecnología Molecular de Células Vegetales y Bioinformática  
# -----------------------------------------------------------

# Tema: Análisis de Conductividad Eléctrica en Agua de Riego  
# Objetivo: Evaluar aptitud agrícola según normativa FAO  

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
ggplot(data.frame(Muestra = factor(1:4), CE = conductividad),  
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
