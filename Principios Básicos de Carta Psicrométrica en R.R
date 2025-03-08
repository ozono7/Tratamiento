# Principios Básicos de Carta Psicrométrica en R

# -----------------------------------------------------------
# Autor: José Juan Torres Martínez
# Doctorado en Ciencias en Bioprocesos
# Laboratorio de Biotecnología Molecular de Células Vegetales y Bioinformática
# -----------------------------------------------------------

# 1. Cálculo de Humedad Relativa
# Fórmula: HR = (Presión de vapor real / Presión de vapor de saturación) * 100
calcular_humedad_relativa <- function(presion_vapor, presion_saturacion) {
  return((presion_vapor / presion_saturacion) * 100)
}

# Ejercicio 1: Calcular la humedad relativa
presion_vapor <- 2.3  # kPa (ejemplo)
presion_saturacion <- 3.2  # kPa (ejemplo)
humedad_relativa <- calcular_humedad_relativa(presion_vapor, presion_saturacion)
print(paste("Humedad Relativa:", humedad_relativa, "%"))


# 2. Cálculo del Punto de Rocío
# Aproximación usando la fórmula de Magnus-Tetens:
# Td = (b * alfa) / (a - alfa), donde alfa = ln(HR/100) + (a * T) / (b + T)
# a y b son constantes para el agua: a = 17.27, b = 237.7
calcular_punto_rocio <- function(temperatura, humedad_relativa) {
  a <- 17.27
  b <- 237.7
  alfa <- log(humedad_relativa / 100) + (a * temperatura) / (b + temperatura)
  punto_rocio <- (b * alfa) / (a - alfa)
  return(punto_rocio)
}

# Ejercicio 2: Calcular el punto de rocío
temperatura <- 25  # °C
punto_rocio <- calcular_punto_rocio(temperatura, humedad_relativa)
print(paste("Punto de Rocío:", punto_rocio, "°C"))


# 3. Cálculo de Volumen Específico del Aire Húmedo
# Fórmula: v = (R * T) / P
# Donde:
#   v = volumen específico (m^3/kg)
#   R = constante de gases (0.287 kJ/kg K para aire seco)
#   T = temperatura absoluta (K)
#   P = presión absoluta (kPa)
calcular_volumen_especifico <- function(temperatura, presion) {
  R <- 0.287  # kJ/kg K
  temperatura_K <- temperatura + 273.15  # Convertir a Kelvin
  return((R * temperatura_K) / presion)
}

# Ejercicio 3: Calcular el volumen específico del aire húmedo
presion_aire <- 101.3  # kPa
volumen_especifico <- calcular_volumen_especifico(temperatura, presion_aire)
print(paste("Volumen Específico del Aire Húmedo:", volumen_especifico, "m^3/kg"))


# 4. Intercambio de Calor con Agua
# Fórmula: Q = m * Cp * (Tf - Ti)
# Donde:
#   Q = calor transferido (kJ)
#   m = masa de agua (kg)
#   Cp = calor específico del agua (4.18 kJ/kg °C)
#   Tf = temperatura final (°C)
#   Ti = temperatura inicial (°C)
calcular_intercambio_calor <- function(masa, temp_inicial, temp_final) {
  Cp <- 4.18  # kJ/kg °C
  return(masa * Cp * (temp_final - temp_inicial))
}

# Ejercicio 4: Calcular el intercambio de calor con agua
masa_agua <- 2  # kg
temp_inicial <- 20  # °C
temp_final <- 80  # °C
calor_transferido <- calcular_intercambio_calor(masa_agua, temp_inicial, temp_final)
print(paste("Calor Transferido:", calor_transferido, "kJ"))

