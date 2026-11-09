# ==========================================================================
# Problema 1: Determinación del porcentaje de nitrógeno en un agua residual
# ==========================================================================
# Autor: Dr. José Juan Torres Martínez
# Docente de Ciencias Químicas
# Universidad Autónoma de San Luis Potosí (UASLP)
# México
#
# Material académico para estudiantes
# Fecha: 2023-10-30
#
# Descripción: Determinación del porcentaje de nitrógeno en un agua residual.
# ==========================================================================


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
# Método Kjeldahl (APHA, Standard Methods for the Examination of Water and
# Wastewater, 4500-Norg B, macro-Kjeldahl): el nitrógeno orgánico y amoniacal se
# convierte en amonio, se destila como NH3 sobre un volumen conocido de ácido
# estándar y el exceso de ácido se valora con base estándar. La masa de N es
# (meq de ácido - meq de base) x 14.007 mg/meq (el script usa 14, redondeado);
# con molaridades, los moles de NaOH se dividen entre 2 porque 2 NaOH neutralizan
# 1 H2SO4. El resultado se refiere a la muestra original con el factor de
# dilución/alícuota (volumen_dilucion / volumen_alicuota).
# La masa de N calculada está en la alícuota: la alícuota es solo una parte de
# la dilución, así que la masa de N de la muestra completa es masa_N por el
# factor de dilución (volumen_dilucion / volumen_alicuota). El porcentaje
# peso/volumen es masa (g) entre volumen de muestra (mL), por 100.
masa_N_muestra <- masa_N * (volumen_dilucion / volumen_alicuota)  # g de N en la muestra
porcentaje_N <- (masa_N_muestra / volumen_muestra) * 100  # % p/v

# ---------------------- Mostrar resultado ----------------------
cat("\n---- RESULTADOS ----\n")
cat("Porcentaje de nitrógeno en el agua residual: ", round(porcentaje_N, 2), "%\n")


# --------------------------------------------------------------------------
# Qué aprendimos
# --------------------------------------------------------------------------
# - El nitrógeno de la muestra se determina por retrovaloración (Kjeldahl): el amoniaco destilado neutraliza parte del ácido y el resto se valora con base.
# - La alícuota es solo una parte de la dilución: hay que multiplicar por el factor de dilución para referir el resultado a la muestra original.
# - El porcentaje peso/volumen es masa en gramos entre volumen en mililitros, por 100.
