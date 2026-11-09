# ==========================================================================
# Problema 5: Dimensiones de un desarenador horizontal
# ==========================================================================
# Autor: Dr. José Juan Torres Martínez
# Docente de Ciencias Químicas
# Universidad Autónoma de San Luis Potosí (UASLP)
# México
#
# Material académico para estudiantes
# Fecha: 2023-10-30
#
# Descripción: Dimensiones de un desarenador horizontal.
# ==========================================================================


Q_m_d <- 12000        # Caudal medio en m³/d
Q_m_s <- Q_m_d / (3600 * 24)  # Caudal medio en m³/s
anchura_canal <- 1    # Anchura del canal en m
v_m <- 0.3            # Velocidad óptima aconsejada en m/s
v_sed <- 0.02         # Velocidad de sedimentación en m/s

# ---------------------- Calcular la altura del canal ----------------------
altura <- Q_m_s / (anchura_canal * v_m)  # Altura del canal en m

# ---------------------- Calcular la longitud del canal ----------------------
longitud <- (v_m * altura) / v_sed  # Longitud del canal en m

# ---------------------- Mostrar resultados ----------------------
cat("\n---- RESULTADOS ----\n")
cat("Caudal medio: ", Q_m_s, "m³/s\n")
cat("Altura del canal: ", altura, "m\n")
cat("Longitud del canal: ", longitud, "m\n")

