# Problema 4: Dimensiones de las rejas en la etapa de desbaste
# Autor: José Juan Torres Martínez
# Doctorado en Ciencias en Bioprocesos
# Laboratorio de Biotecnología Molecular de Células Vegetales y Bioinformática
# Fecha: 2023-10-30

# ---------------------- Definir constantes y variables ----------------------
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

