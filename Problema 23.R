# ==========================================================================
# Cálculo de eficiencia en filtro percolador usando ecuaciones NRC
# ==========================================================================
# Autor: Dr. José Juan Torres Martínez
# Docente de Ciencias Químicas
# Universidad Autónoma de San Luis Potosí (UASLP)
# México
#
# Material académico para estudiantes
#
# Descripción: Determinar la DBO5 del efluente para un filtro de baja carga
# ==========================================================================


# DATOS DE ENTRADA
# -----------------------------------------------------------
Q <- 2000                # Caudal (m³/día)
V <- 1500                # Volumen de relleno (m³)
S0 <- 150                # DBO5 influente (mg/L)
F <- 2.5                 # Factor de recirculación

# Convertir unidades
S0_kg_m3 <- S0 / 1000    # Convertir mg/L a kg/m³

# -----------------------------------------------------------
# CÁLCULOS SEGÚN MODELO NRC
# -----------------------------------------------------------

# 1. Cálculo de la carga orgánica volumétrica (Lv) - Ecuación 7.2
Lv <- (Q * S0_kg_m3) / V  # kg DBO5/(m³·día)

# 2. Cálculo de la eficiencia (E) - Ecuación 7.3
E <- 1 / (1 + 0.4432 * sqrt(Lv / F))

# 3. Cálculo de la DBO5 en el efluente
Se <- (1 - E) * S0        # mg/L

# -----------------------------------------------------------
# PRESENTACIÓN DE RESULTADOS
# -----------------------------------------------------------
cat("\n----------------------------------------\n")
cat("FILTRO PERCOLADOR (NRC)\n")
cat("----------------------------------------\n")

cat("\n1. PARÁMETROS DE ENTRADA:\n")
cat(" - Caudal (Q):", Q, "m³/día\n")
cat(" - Volumen relleno (V):", V, "m³\n")
cat(" - DBO5 influente (S0):", S0, "mg/L\n")
cat(" - Factor recirculación (F):", F, "\n")

cat("\n2. CÁLCULOS INTERMEDIOS:\n")
cat(" - Carga orgánica volumétrica (Lv):", round(Lv, 3), "kg DBO5/(m³·día)\n")

cat("\n3. RESULTADOS FINALES:\n")
cat(" - Eficiencia del filtro (E):", round(E*100, 1), "%\n")
cat(" - DBO5 efluente (Se):", round(Se, 1), "mg/L\n")

cat("\n----------------------------------------\n")
cat(" VERIFICACIÓN CON SOLUCIÓN PROPORCIONADA\n")
cat("----------------------------------------\n")
cat(" - Carga orgánica calculada:", round(Lv, 2), "kg/m³·d (debe ser 0.20)\n")
cat(" - Eficiencia calculada:", round(E*100, 1), "% (debe ser 88.9%)\n")
cat(" - DBO5 efluente calculada:", round(Se, 1), "mg/L (debe ser 16.6 mg/L)\n")

# -----------------------------------------------------------
# GRÁFICO DE SENSIBILIDAD
# -----------------------------------------------------------
library(ggplot2)

# Crear datos para gráfico de sensibilidad
F_range <- seq(1, 4, by = 0.1)
E_range <- 1 / (1 + 0.4432 * sqrt(Lv / F_range))

df <- data.frame(Recirculacion = F_range, Eficiencia = E_range*100)

g <- ggplot(df, aes(x = Recirculacion, y = Eficiencia)) +
  geom_line(color = "blue", linewidth = 1.5) +
  geom_vline(xintercept = F, linetype = "dashed", color = "red") +
  annotate("text", x = F + 0.3, y = 80, 
           label = paste("F =", F), color = "red") +
  labs(title = "Efecto de la Recirculación en la Eficiencia",
       x = "Factor de Recirculación (F)",
       y = "Eficiencia (%)") +
  theme_minimal()

print(g)

# --------------------------------------------------------------------------
# Interpretación: Efecto de la Recirculación en la Eficiencia
# --------------------------------------------------------------------------
# - Eje X: el factor de recirculación (F), de 1 a 4. F es adimensional: es un cociente entre el caudal recirculado y el caudal de entrada, por eso no tiene unidades. La línea roja discontinua marca el valor de diseño, F = 2.5.
# - Eje Y: la eficiencia del filtro en % (porcentaje de la DBO5 de entrada que el filtro logra remover).
# - Lo que se aprecia: la eficiencia sube al aumentar la recirculación, rápido al principio y cada vez más despacio: con F = 2.5 la eficiencia es del 88.9 % y al seguir subiendo F la curva se aplana.
# - Qué significa: recircular más agua mejora el tratamiento, pero cada unidad extra de recirculación aporta cada vez menos: más allá de F = 3 la ganancia es pequeña y ya no compensa el coste de bombear más agua.

# --------------------------------------------------------------------------
# Qué aprendimos
# --------------------------------------------------------------------------
# - El factor de recirculación F es adimensional porque es un cociente entre dos caudales.
# - La carga orgánica volumétrica (Lv) se expresa en kg DBO5/(m³·día) y es el dato que alimenta el modelo NRC.
# - La eficiencia de un filtro percolador crece con la recirculación, pero con rendimientos decrecientes: la curva se aplana.
