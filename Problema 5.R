# -----------------------------------------------------------
# Autor: José Juan Torres Martínez  
# Laboratorio de Tratamiento de Aguas  
# ----------------------------------------------------------- 

# Tema: Análisis de Turbidez en Agua Potable  
# Objetivos: Estadísticas básicas y cumplimiento de normativas  

# Datos de turbidez (NTU)  
turbidez <- c(0.85, 0.92, 0.88)  

# Cálculos estadísticos  
promedio <- mean(turbidez)  
desviacion <- sd(turbidez)  
coef_variacion <- (desviacion / promedio) * 100  # %  

# Cumplimiento de normativas  
cumple_oms <- promedio < 1.0  
cumple_cv <- coef_variacion < 5  

# Resultados  
cat("----------------------------------------\n")  
cat("Resultados del Análisis de Turbidez:\n")  
cat("----------------------------------------\n")  
cat("Turbidez promedio:", round(promedio, 2), "NTU\n")  
cat("Desviación estándar:", round(desviacion, 3), "NTU\n")  
cat("Coeficiente de variación (CV):", round(coef_variacion, 1), "%\n")  
cat("----------------------------------------\n")  
cat("¿Cumple con el límite de la OMS (<1.0 NTU)?:", ifelse(cumple_oms, "SÍ", "NO"), "\n")  
cat("¿Precisión aceptable (CV < 5%)?:", ifelse(cumple_cv, "SÍ", "NO"), "\n")  
cat("----------------------------------------\n")  

# Gráfico de barras (opcional)  
barplot(turbidez, names.arg = c("Réplica 1", "Réplica 2", "Réplica 3"),  
        col = "lightblue", main = "Mediciones de Turbidez",  
        ylab = "Turbidez (NTU)", ylim = c(0, 1.2))  
abline(h = 1.0, col = "red", lty = 2, lwd = 2)  
text(x = 1:3, y = turbidez + 0.05, labels = turbidez)  

