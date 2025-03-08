# Ejercicios Básicos de Hidráulica en R

# -----------------------------------------------------------
# Autor: José Juan Torres Martínez
# Doctorado en Ciencias en Bioprocesos
# Laboratorio de Biotecnología Molecular de Células Vegetales y Bioinformática
# -----------------------------------------------------------

# 1. Cálculo del Número de Reynolds
# Fórmula: Re = (densidad * velocidad * diametro) / viscosidad
# El Número de Reynolds indica el régimen de flujo:
#   - Re < 2000: Flujo Laminar
#   - 2000 <= Re <= 4000: Flujo Transicional
#   - Re > 4000: Flujo Turbulento

numero_reynolds <- function(densidad, velocidad, diametro, viscosidad) {
  return((densidad * velocidad * diametro) / viscosidad)
}

# Ejercicio 1: Calcular el Número de Reynolds y determinar el tipo de flujo
# Datos de ejemplo:
densidad <- 998  # kg/m^3 (agua a temperatura ambiente)
velocidad <- 1.5  # m/s
diametro <- 0.1  # m
viscosidad <- 0.001  # Pa.s

re <- numero_reynolds(densidad, velocidad, diametro, viscosidad)
print(paste("Número de Reynolds:", re))

# Determinar el tipo de flujo
if (re < 2000) {
  print("Tipo de Flujo: Laminar")
} else if (re >= 2000 & re <= 4000) {
  print("Tipo de Flujo: Transicional")
} else {
  print("Tipo de Flujo: Turbulento")
}


# 2. Cálculo de la viscosidad dinámica
# Fórmula: viscosidad = (fuerza de corte / gradiente de velocidad)
calcular_viscosidad <- function(fuerza_corte, gradiente_velocidad) {
  return(fuerza_corte / gradiente_velocidad)
}

# Ejercicio 2: Calcular la viscosidad dinámica
fuerza_corte <- 0.05  # Pa
gradiente_velocidad <- 10  # s^-1
viscosidad_dinamica <- calcular_viscosidad(fuerza_corte, gradiente_velocidad)
print(paste("Viscosidad Dinámica:", viscosidad_dinamica, "Pa.s"))


# 3. Conversión de unidades de densidad
# De kg/m^3 a g/cm^3
convertir_densidad <- function(densidad_kg_m3) {
  return(densidad_kg_m3 / 1000)
}

# Ejercicio 3: Convertir densidad de agua de kg/m^3 a g/cm^3
densidad_agua <- 998  # kg/m^3
print(paste("Densidad del agua:", convertir_densidad(densidad_agua), "g/cm^3"))


# 4. Cálculo de caudal volumétrico
# Fórmula: Q = A * v
# Donde:
#   Q = Caudal (m^3/s)
#   A = Área de la sección transversal (m^2)
#   v = Velocidad del flujo (m/s)
calcular_caudal <- function(diametro, velocidad) {
  area <- pi * (diametro / 2)^2
  return(area * velocidad)
}

# Ejercicio 4: Calcular el caudal volumétrico
velocidad_flujo <- 2  # m/s
diametro_tubo <- 0.1  # m
caudal <- calcular_caudal(diametro_tubo, velocidad_flujo)
print(paste("Caudal Volumétrico:", caudal, "m^3/s"))

