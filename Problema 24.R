# ==========================================================================
# Cálculo de DBO5 en efluente usando modelo Schulze
# ==========================================================================
# Autor: Dr. José Juan Torres Martínez
# Docente de Ciencias Químicas
# Universidad Autónoma de San Luis Potosí (UASLP)
# México
#
# Material académico para estudiantes
#
# Descripción: Determinar la eficiencia de un filtro percolador de baja carga
# ==========================================================================


# DATOS DE ENTRADA
# -----------------------------------------------------------
Diametro <- 30            # Diámetro del filtro (m)
Profundidad <- 2.0        # Profundidad del lecho (m)
Q <- 2000                 # Caudal (m³/día)
S0 <- 150                 # DBO5 influente (mg/L)
K <- 2.3                  # Constante de velocidad [(m/d)^0.67/m]
n <- 0.67                 # Coeficiente de la ecuación

# -----------------------------------------------------------
# CÁLCULOS PRINCIPALES
# -----------------------------------------------------------

# 1. Cálculo del área superficial
Area <- (pi/4) * Diametro^2  # m²

# 2. Cálculo de la carga hidráulica (Lh)
Lh <- Q / Area  # m³/(m²·día)

# 3. Aplicación del modelo Schulze (Ecuación 7.8)
Se <- S0 * exp(-K * Profundidad / (Lh^n))  # mg/L

# -----------------------------------------------------------
# PRESENTACIÓN DE RESULTADOS
# -----------------------------------------------------------
cat("\n----------------------------------------\n")
cat(" FILTRO PERCOLADOR (SCHULZE)\n")
cat("----------------------------------------\n")

cat("\n1. PARÁMETROS GEOMÉTRICOS:\n")
cat(" - Diámetro del filtro:", Diametro, "m\n")
cat(" - Profundidad del lecho:", Profundidad, "m\n")
cat(" - Área superficial:", round(Area, 0), "m²\n")

cat("\n2. PARÁMETROS HIDRÁULICOS:\n")
cat(" - Caudal (Q):", Q, "m³/día\n")
cat(" - Carga hidráulica (Lh):", round(Lh, 2), "m³/(m²·día)\n")

cat("\n3. RESULTADOS DE TRATAMIENTO:\n")
cat(" - DBO5 influente (S0):", S0, "mg/L\n")
cat(" - DBO5 efluente (Se):", round(Se, 1), "mg/L\n")
cat(" - Eficiencia de remoción:", round((1 - Se/S0)*100, 1), "%\n")

# -----------------------------------------------------------
# ANÁLISIS DE SENSIBILIDAD
# -----------------------------------------------------------
library(ggplot2)

# Variar la profundidad del lecho
prof_range <- seq(1, 3, by = 0.1)
Se_range <- S0 * exp(-K * prof_range / (Lh^n))

df <- data.frame(Profundidad = prof_range, DBO5_efluente = Se_range)

g <- ggplot(df, aes(x = Profundidad, y = DBO5_efluente)) +
  geom_line(color = "blue", linewidth = 1.5) +
  geom_vline(xintercept = Profundidad, linetype = "dashed", color = "red") +
  annotate("text", x = Profundidad + 0.2, y = 50, 
           label = paste("Profundidad =", Profundidad, "m"), color = "red") +
  labs(title = "Efecto de la Profundidad del Lecho en la DBO5 Efluente",
       x = "Profundidad del Lecho (m)",
       y = "DBO5 Efluente (mg/L)") +
  theme_minimal()

print(g)

# --------------------------------------------------------------------------
# Interpretación: Efecto de la Profundidad del Lecho en la DBO5 Efluente
# --------------------------------------------------------------------------
# - Eje X: la profundidad del lecho filtrante en m (metros), de 1 a 3 m. La línea roja discontinua marca la profundidad de diseño, 2.0 m.
# - Eje Y: la DBO5 del efluente (Se) en mg/L (miligramos por litro). Es la materia orgánica que queda en el agua después de pasar por el filtro.
# - Lo que se aprecia: la curva baja de forma pronunciada al aumentar la profundidad y luego se aplana: con la profundidad de diseño de 2.0 m la DBO5 efluente es de unos 15.2 mg/L, frente a 150 mg/L de entrada.
# - Qué significa: un lecho más profundo limpia más el agua, pero con rendimientos decrecientes: pasar de 2 a 3 m de profundidad baja poco la DBO5 efluente, así que la profundidad de diseño equilibra eficiencia y coste de construcción.

# --------------------------------------------------------------------------
# Qué aprendimos
# --------------------------------------------------------------------------
# - En el modelo de Schulze, la DBO5 efluente baja de forma exponencial al aumentar la profundidad del lecho.
# - La carga hidráulica (Lh) se expresa en m³/(m²·día) y resume cuánto caudal recibe cada metro cuadrado de filtro.
# - Con la profundidad de diseño (2.0 m) el filtro logra una remoción cercana al 90 %, de 150 a unos 15 mg/L.
