# ==========================================================================
# Problema 8: Dimensiones de un desarenador horizontal
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
# Nota: el título lleva el número del archivo (8). En el problemario original este ejercicio era el número 5; aquí se cita por su número de archivo para que título y archivo coincidan.
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


# --------------------------------------------------------------------------
# Qué aprendimos
# --------------------------------------------------------------------------
# - La altura del canal sale del caudal, la anchura y la velocidad: A = Q / v.
# - La longitud del desarenador depende de la velocidad horizontal y de la velocidad de sedimentación de la arena.
# - Una velocidad de 0.3 m/s mantiene la arena en suspensión hasta que sedimenta sin arrastrarla.
