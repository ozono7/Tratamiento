# ==========================================================================
# Análisis de Turbidez en Agua Potable
# ==========================================================================
# Autor: Dr. José Juan Torres Martínez
# Docente de Ciencias Químicas
# Universidad Autónoma de San Luis Potosí (UASLP)
# México
#
# Material académico para estudiantes
#
# Descripción: Estadísticas básicas y cumplimiento de normativas
# ==========================================================================


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

# Gráfico de barras
library(ggplot2)
grafico_turbidez <- ggplot(data.frame(Réplica = factor(c("Réplica 1", "Réplica 2", "Réplica 3"),
                                                      levels = c("Réplica 1", "Réplica 2", "Réplica 3")),
                                     Turbidez = turbidez),
                           aes(x = Réplica, y = Turbidez)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  geom_hline(yintercept = 1.0, linetype = "dashed", color = "red", linewidth = 1) +
  geom_text(aes(label = Turbidez), vjust = -0.5) +
  labs(title = "Mediciones de Turbidez", y = "Turbidez (NTU)", x = "") +
  ylim(0, 1.2) +
  theme_minimal()

print(grafico_turbidez)
ggsave("problema_05_turbidez.png", plot = grafico_turbidez, width = 8, height = 5, dpi = 300)

# --------------------------------------------------------------------------
# Interpretación: Mediciones de Turbidez
# --------------------------------------------------------------------------
# - Eje X: las tres réplicas de la muestra (Réplica 1, Réplica 2 y Réplica 3). Es una categoría, no una cantidad medida, por eso no tiene unidades.
# - Eje Y: la turbidez en NTU (unidades nefelométricas de turbidez). La línea roja discontinua marca el límite de la OMS de 1.0 NTU y los números escritos sobre cada barra son el valor medido en esa réplica.
# - Lo que se aprecia: las tres barras están muy juntas, entre 0.85 y 0.92 NTU, y las tres quedan por debajo de la línea roja de 1.0 NTU.
# - Qué significa: el agua cumple el límite de la OMS (menos de 1.0 NTU) y las tres mediciones son muy parecidas entre sí, lo que indica buena precisión: el coeficiente de variación que calcula el script queda por debajo del 5 % aceptado.

# --------------------------------------------------------------------------
# Qué aprendimos
# --------------------------------------------------------------------------
# - La turbidez se mide en NTU y el límite de la OMS para agua potable es 1.0 NTU.
# - Repetir la medición (réplicas) permite comprobar la precisión: si los valores son parecidos, la medición es confiable.
# - Un gráfico de barras con una línea de referencia deja ver de un vistazo si se cumple o no la norma.
