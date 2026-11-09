# ==========================================================================
# Problema 2: Determinación de la dureza total, magnésica y cálcica del agua
# ==========================================================================
# Autor: Dr. José Juan Torres Martínez
# Docente de Ciencias Químicas
# Universidad Autónoma de San Luis Potosí (UASLP)
# México
#
# Material académico para estudiantes
# Fecha: 2023-10-30
#
# Descripción: Determinación de la dureza total, magnésica y cálcica del agua.
# ==========================================================================


volumen_muestra <- 10.0        # Volumen de la muestra en mL
concentracion_AEDT <- 0.0125   # Concentración de AEDT en M
volumen_AEDT_total <- 20.0     # Volumen de AEDT para la dureza total en mL
volumen_AEDT_magnesio <- 8.0   # Volumen de AEDT para la dureza magnésica en mL
peso_molecular_CaCO3 <- 100.09 # Peso molecular del CaCO₃ en g/mol

# ---------------------- Cálculo de los moles de AEDT ----------------------
moles_AEDT_total <- concentracion_AEDT * volumen_AEDT_total / 1000  # moles de AEDT
moles_AEDT_magnesio <- concentracion_AEDT * volumen_AEDT_magnesio / 1000  # moles de AEDT

# ---------------------- Cálculo de los moles de Ca²⁺ y Mg²⁺ ----------------------
moles_Ca <- max(moles_AEDT_total - moles_AEDT_magnesio, 0)  # moles de Ca²⁺ (evitar valores negativos)
moles_Mg <- moles_AEDT_magnesio                             # moles de Mg²⁺

# ---------------------- Cálculo de la dureza total, magnésica y cálcica en mg CaCO₃/L ----------------------
# Cada mol de AEDT gastado equivale a un mol de CaCO3: la dureza (mg CaCO3/L)
# es moles de AEDT por peso molecular del CaCO3, entre el volumen de muestra
# en litros, en mg. La dureza total es la suma de la cálcica y la magnésica.
dureza_total <- moles_AEDT_total * peso_molecular_CaCO3 / (volumen_muestra / 1000) * 1000  # mg CaCO3/L
dureza_magnesica <- moles_AEDT_magnesio * peso_molecular_CaCO3 / (volumen_muestra / 1000) * 1000  # mg CaCO3/L
dureza_calcica <- moles_Ca * peso_molecular_CaCO3 / (volumen_muestra / 1000) * 1000  # mg CaCO3/L

# ---------------------- Mostrar resultados ----------------------
cat("\n---- RESULTADOS ----\n")
cat("Dureza total del agua: ", dureza_total, "mg CaCO₃/L\n")
cat("Dureza magnésica del agua: ", dureza_magnesica, "mg CaCO₃/L\n")
cat("Dureza cálcica del agua: ", dureza_calcica, "mg CaCO₃/L\n")


# --------------------------------------------------------------------------
# Qué aprendimos
# --------------------------------------------------------------------------
# - La dureza se mide por complexometría con AEDT: cada mol de AEDT gastado equivale a un mol de Ca2+ o Mg2+.
# - La dureza se expresa como mg CaCO3/L: moles por peso molecular del CaCO3, entre el volumen de muestra en litros.
# - La dureza total es la suma de la cálcica y la magnésica.
