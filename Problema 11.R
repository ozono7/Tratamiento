# -----------------------------------------------------------
# Autor: José Juan Torres Martínez  
# Doctorado en Ciencias en Bioprocesos  
# Laboratorio de Biotecnología Molecular de Células Vegetales y Bioinformática  
# -----------------------------------------------------------

# Tema: Diseño de Decantador Primario Rectangular  
# Objetivo: Calcular dimensiones y tiempo de retención  

# Datos de entrada  
Q <- 35000                     # Caudal medio (m³/d)  
n <- 2                         # Número de tanques  
V <- 40                        # Carga hidráulica (m³/m²·d)  
A <- 6.2                       # Anchura por tanque (m)  
P <- 4                         # Profundidad (m)  

# 1. Cálculo del área superficial total (S)  
S <- Q / V                     # Área total (m²)  

# 2. Longitud por tanque (L)  
L <- S / (A * n)               # Longitud por tanque (m)  

# 3. Volumen por tanque y tiempo de retención (TRH)  
Vol_tanque <- A * L * P        # Volumen por tanque (m³)  
TRH <- (Vol_tanque * n) / Q    # Tiempo de retención en días  
TRH_horas <- TRH * 24          # Conversión a horas  

# Resultados  
cat("----------------------------------------\n")  
cat("DISEÑO DE DECANTADOR PRIMARIO RECTANGULAR\n")  
cat("----------------------------------------\n")  
cat("Caudal medio (Q):", Q, "m³/d\n")  
cat("Número de tanques (n):", n, "\n")  
cat("Carga hidráulica (V):", V, "m³/m²·d\n")  
cat("Anchura por tanque (A):", A, "m\n")  
cat("Profundidad (P):", P, "m\n\n")  
cat("Área superficial total (S):", round(S, 1), "m²\n")  
cat("Longitud por tanque (L):", round(L, 1), "m\n")  
cat("Volumen por tanque:", round(Vol_tanque, 1), "m³\n")  
cat("Tiempo de retención hidráulica (TRH):", round(TRH_horas, 1), "horas\n")  
cat("----------------------------------------\n")  

# Verificación de estándares  
if (TRH_horas >= 1.5 && TRH_horas <= 2.5) {  
  cat("✅ TRH dentro del rango recomendado (1.5-2.5 horas)\n")  
} else {  
  cat("⚠️ TRH fuera de rango. Ajustar dimensiones.\n")  
}  
