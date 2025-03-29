# -----------------------------------------------------------
# Autor: José Juan Torres Martínez  
# Doctorado en Ciencias en Bioprocesos  
# Laboratorio de Biotecnología Molecular de Células Vegetales y Bioinformática  
# -----------------------------------------------------------

# Tema: Cálculo de HRT y Edad del Lodo en Lodos Activados  
# Objetivos: Determinar HRT y θ₀  

# Datos de entrada  
Q <- 4000                    # Caudal (m³/d)  
V <- 6000                    # Volumen del reactor (m³)  
S0 <- 300                    # DBO5 influente (mg/L)  
S <- 20                      # DBO5 efluente (mg/L)  
um <- 3.0                    # Tasa máxima de crecimiento (d⁻¹)  
Ks <- 60                     # Constante de saturación (mg/L)  
kd <- 0.06                   # Coeficiente de decaimiento (d⁻¹)  

# 1. Cálculo del Tiempo de Retención Hidráulico (HRT)  
HRT <- V / Q                 # HRT en días  

# 2. Cálculo de la Edad del Lodo (θ₀)  
# Tasa de crecimiento específico (μ)  
mu <- um * (S / (Ks + S))    # 0.75 d⁻¹  

# Edad del lodo  
theta_c <- 1 / (mu - kd)     # 1.45 días  

# Resultados  
cat("----------------------------------------\n")  
cat("ANÁLISIS DE LODOS ACTIVADOS\n")  
cat("----------------------------------------\n")  
cat("Caudal (Q):", Q, "m³/d\n")  
cat("Volumen del reactor (V):", V, "m³\n")  
cat("DBO5 influente (S0):", S0, "mg/L\n")  
cat("DBO5 efluente (S):", S, "mg/L\n\n")  
cat("a) Tiempo de Retención Hidráulico (HRT):", round(HRT, 2), "días\n")  
cat("b) Edad del Lodo (θ₀):", round(theta_c, 2), "días\n")  
cat("----------------------------------------\n")  

# Interpretación  
if (theta_c >= 1 && theta_c <= 5) {  
  cat("✅ La edad del lodo está en el rango típico (1-5 días) para sistemas convencionales.\n")  
} else if (theta_c < 1) {  
  cat("⚠️ Edad del lodo demasiado baja. Riesgo de lavado de biomasa.\n")  
} else {  
  cat("⚠️ Edad del lodo elevada. Puede indicar sobrecarga de sólidos.\n")  
}  

if (HRT == theta_c) {  
  cat("Nota: HRT = θ₀ indica que no hay recirculación de sólidos.\n")  
}  
