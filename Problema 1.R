# Problema 1: Determinación del porcentaje de nitrógeno en un agua residual
# Autor: José Juan Torres Martínez
# Doctorado en Ciencias en Bioprocesos
# Laboratorio de Biotecnología Molecular de Células Vegetales y Bioinformática
# Fecha: 2023-10-30

# ---------------------- Definir constantes y variables ----------------------
volumen_muestra <- 50          # Volumen inicial de la muestra en mL
volumen_dilucion <- 1000       # Volumen de dilución en mL
volumen_alicuota <- 100        # Volumen de la alícuota en mL
concentracion_H2SO4 <- 0.0900  # Concentración de H₂SO₄ en M
volumen_H2SO4 <- 10.0          # Volumen de H₂SO₄ en mL
concentracion_NaOH <- 0.0900   # Concentración de NaOH en M
volumen_NaOH <- 9.7            # Volumen de NaOH gastado en mL
peso_molecular_N <- 14         # Peso molecular del nitrógeno en g/mol

# ---------------------- Cálculo de los moles de H₂SO₄ iniciales ----------------------
moles_H2SO4_iniciales <- concentracion_H2SO4 * volumen_H2SO4 / 1000  # moles de H₂SO₄

# ---------------------- Cálculo de los moles de NaOH gastados ----------------------
moles_NaOH <- concentracion_NaOH * volumen_NaOH / 1000  # moles de NaOH

# ---------------------- Cálculo de los moles de H₂SO₄ que reaccionaron con NH₃ ----------------------
moles_H2SO4_reaccionados <- max(moles_H2SO4_iniciales - (moles_NaOH / 2), 0)  # moles de H₂SO₄

# ---------------------- Cálculo de los moles de NH₃ ----------------------
moles_NH3 <- moles_H2SO4_reaccionados * 2  # moles de NH₃

# ---------------------- Cálculo de los moles y la masa de N ----------------------
moles_N <- moles_NH3  # moles de N
masa_N <- moles_N * peso_molecular_N  # masa de N en g

# ---------------------- Cálculo del porcentaje de nitrógeno ----------------------
porcentaje_N <- (masa_N / (volumen_muestra / 1000)) * 100  # %

# ---------------------- Mostrar resultado ----------------------
cat("\n---- RESULTADOS ----\n")
cat("Porcentaje de nitrógeno en el agua residual: ", round(porcentaje_N, 2), "%\n")

