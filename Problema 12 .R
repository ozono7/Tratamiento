# -----------------------------------------------------------
# Autor: José Juan Torres Martínez  
# Doctorado en Ciencias en Bioprocesos  
# Laboratorio de Biotecnología Molecular de Células Vegetales y Bioinformática  
# -----------------------------------------------------------

# Tema: Diseño de Sedimentador Circular Primario  
# Objetivo: Calcular TRH, WOR y CS  

# Datos de entrada  
D <- 20                          # Diámetro (m)  
P <- 2.5                         # Profundidad (m)  
Q <- 10000                       # Caudal (m³/día)  

# 1. Cálculo del área superficial (A) y volumen (V)  
radio <- D / 2  
A <- pi * radio^2                # Área (m²)  
V <- A * P                       # Volumen (m³)  

# 2. Tiempo de retención hidráulica (TRH) en horas  
TRH_dias <- V / Q                # TRH en días  
TRH_horas <- TRH_dias * 24       # TRH en horas  

# 3. Caudal unitario sobre vertedero (WOR)  
perimetro <- 2 * pi * radio      # Perímetro del vertedero (m)  
WOR <- (Q / 24) / perimetro      # WOR en m³/m·h  

# 4. Carga superficial (CS)  
CS <- (Q / 24) / A               # CS en m³/m²·h  

# Resultados  
cat("----------------------------------------\n")  
cat("DISEÑO DE SEDIMENTADOR CIRCULAR\n")  
cat("----------------------------------------\n")  
cat("Diámetro (D):", D, "m\n")  
cat("Profundidad (P):", P, "m\n")  
cat("Caudal (Q):", Q, "m³/día\n\n")  
cat("a) Tiempo de retención (TRH):", round(TRH_horas, 2), "horas\n")  
cat("b) Caudal unitario sobre vertedero (WOR):", round(WOR, 4), "m³/m·h\n")  
cat("c) Carga superficial (CS):", round(CS, 2), "m³/m²·h\n")  
cat("----------------------------------------\n")  

# Verificación de estándares  
if (TRH_horas >= 1.5 && TRH_horas <= 2.5) {  
  cat("✅ TRH dentro del rango típico (1.5-2.5 horas)\n")  
} else {  
  cat("⚠️ TRH fuera de rango. Ajustar dimensiones.\n")  
}  

if (WOR <= 10) {  
  cat("✅ WOR cumple con estándares (≤10 m³/m·h)\n")  
} else {  
  cat("⚠️ WOR excede el límite recomendado.\n")  
}  

if (CS <= 1.5) {  
  cat("✅ CS aceptable (≤1.5 m³/m²·h)\n")  
} else {  
  cat("⚠️ CS demasiado alta. Riesgo de arrastre de sólidos.\n")  
}  

