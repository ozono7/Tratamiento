# ==========================================================================
# Problema 7: Dimensiones de las rejas en la etapa de desbaste
# ==========================================================================
# Autor: Dr. José Juan Torres Martínez
# Docente de Ciencias Químicas
# Universidad Autónoma de San Luis Potosí (UASLP)
# México
#
# Material académico para estudiantes
# Fecha: 2023-10-30
#
# Descripción: Dimensiones de las rejas en la etapa de desbaste.
# Nota: el título lleva el número del archivo (7). En el problemario original este ejercicio era el número 4; aquí se cita por su número de archivo para que título y archivo coincidan.
# ==========================================================================


Q_max_total <- 1.130  # Caudal máximo total en m³/h
num_lineas <- 2        # Número de líneas de tratamiento
v <- 1.0               # Velocidad de paso del agua entre las rejas en m/s
a <- 7                 # Ancho de los barrotes en mm
s <- 20                # Separación entre barrotes en mm
C_rej <- 0.1           # Coeficiente de seguridad en m

# ---------------------- Calcular el caudal máximo por línea ----------------------
Q_max_linea <- Q_max_total / num_lineas  # Caudal máximo por línea en m³/h
Q_max_linea_m3s <- Q_max_linea / 3600    # Convertir a m³/s

# ---------------------- Calcular el nivel de agua (D) ----------------------
D <- 0.15 + 0.74 * sqrt(Q_max_linea_m3s)  # Nivel de agua en m

# ---------------------- Calcular el ancho del canal (W) ----------------------
W <- (Q_max_linea_m3s / (v * D)) * ((a + s) / s) + C_rej  # Ancho del canal en m

# ---------------------- Mostrar resultados ----------------------
cat("\n---- RESULTADOS ----\n")
cat("Caudal máximo por línea: ", Q_max_linea_m3s, "m³/s\n")
cat("Nivel de agua (D): ", D, "m\n")
cat("Ancho del canal (W): ", W, "m\n")


# --------------------------------------------------------------------------
# Qué aprendimos
# --------------------------------------------------------------------------
# - El caudal de diseño se reparte entre las líneas de tratamiento: cada línea lleva la mitad del caudal total.
# - El nivel de agua en las rejas depende del caudal por línea y el ancho del canal del caudal, la velocidad y la separación entre barrotes.
# - El coeficiente de seguridad se suma al ancho calculado del canal.
