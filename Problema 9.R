# ==========================================================================
# Problema 9: Cálculo de caudales en un canal Parshall de 9 pulgadas
# ==========================================================================
# Autor: Dr. José Juan Torres Martínez
# Docente de Ciencias Químicas
# Universidad Autónoma de San Luis Potosí (UASLP)
# México
#
# Material académico para estudiantes
# Fecha: 2023-10-30
#
# Descripción: Cálculo de caudales en un canal Parshall de 9 pulgadas.
# ==========================================================================


K <- 0.535  # Coeficiente K para un canal Parshall de 9 pulgadas
n <- 1.530  # Exponente n para un canal Parshall de 9 pulgadas

# Alturas de agua medidas en metros
H_max <- 0.48  # Altura a máximo caudal en m
H_medio <- 0.35  # Altura a caudal medio en m
H_min <- 0.25  # Altura a mínimo caudal en m

# ---------------------- Calcular los caudales ----------------------
# Fórmula: Q = K * H^n
Q_max <- K * (H_max)^n  # Caudal máximo en m³/s
Q_medio <- K * (H_medio)^n  # Caudal medio en m³/s
Q_min <- K * (H_min)^n  # Caudal mínimo en m³/s

# Convertir los caudales a L/s
Q_max_Ls <- Q_max * 1000  # Caudal máximo en L/s
Q_medio_Ls <- Q_medio * 1000  # Caudal medio en L/s
Q_min_Ls <- Q_min * 1000  # Caudal mínimo en L/s

# ---------------------- Mostrar resultados ----------------------
cat("\n---- RESULTADOS ----\n")
cat("Caudal máximo: ", Q_max, "m³/s (", Q_max_Ls, "L/s)\n")
cat("Caudal medio: ", Q_medio, "m³/s (", Q_medio_Ls, "L/s)\n")
cat("Caudal mínimo: ", Q_min, "m³/s (", Q_min_Ls, "L/s)\n")


# --------------------------------------------------------------------------
# Qué aprendimos
# --------------------------------------------------------------------------
# - El caudal en un canal Parshall se calcula con Q = K * H^n, con K y n propios de la garganta (9 pulgadas).
# - Al medir la altura de agua en tres condiciones se obtienen el caudal máximo, el medio y el mínimo.
# - Pasar de m3/s a L/s es multiplicar por 1000.
