# ==========================================================================
# Problema 10: Diseño de un desarenador de sección rectangular
# ==========================================================================
# Autor: Dr. José Juan Torres Martínez
# Docente de Ciencias Químicas
# Universidad Autónoma de San Luis Potosí (UASLP)
# México
#
# Material académico para estudiantes
# Fecha: 2023-10-30
#
# Descripción: Diseño de un desarenador de sección rectangular.
# Nota: el título lleva el número del archivo (10). En el problemario original este ejercicio era el número 7; aquí se cita por su número de archivo para que título y archivo coincidan.
# ==========================================================================


Q_m <- 450  # Caudal medio en m³/h
Q_m_s <- Q_m / 3600  # Caudal medio en m³/s
v_canal <- 0.3  # Velocidad del agua a lo largo del canal en m/s
v_sed <- 0.022  # Velocidad de sedimentación de las arenas en m/s
generacion_arenas <- 0.08  # Generación de arenas en m³/1000 m³ de agua residual
relacion_profundidad_anchura <- 1.5  # Relación profundidad/anchura del canal

# ---------------------- Calcular la anchura del desarenador ----------------------
# Área de la sección transversal: A = Q_m_s / v_canal
A <- Q_m_s / v_canal  # Área en m²

# Anchura del desarenador: anchura = sqrt(A / relacion_profundidad_anchura)
anchura <- sqrt(A / relacion_profundidad_anchura)  # Anchura en m

# ---------------------- Calcular la altura del desarenador ----------------------
altura <- relacion_profundidad_anchura * anchura  # Altura en m

# ---------------------- Calcular la longitud del desarenador ----------------------
longitud <- (v_canal * altura) / v_sed  # Longitud en m

# ---------------------- Calcular el volumen de arenas recogidas diariamente ----------------------
volumen_arenas_diario <- (generacion_arenas / 1000) * Q_m * 24  # Volumen en m³/día

# ---------------------- Mostrar resultados ----------------------
cat("\n---- RESULTADOS ----\n")
cat("a) Anchura del desarenador: ", anchura, "m\n")
cat("b) Altura del desarenador: ", altura, "m\n")
cat("c) Longitud del desarenador: ", longitud, "m\n")
cat("d) Volumen de arenas recogidas diariamente: ", volumen_arenas_diario, "m³/día\n")

# --------------------------------------------------------------------------
# Qué aprendimos
# --------------------------------------------------------------------------
# - El área del desarenador sale del caudal y la velocidad: A = Q / v, y con la relación profundidad/anchura se sacan las dos dimensiones.
# - La longitud depende de la velocidad del agua y de la velocidad de sedimentación de las arenas.
# - El volumen de arenas recogido al día es la generación por m3 de agua por el caudal diario.
