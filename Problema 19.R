# -----------------------------------------------------------
# Autor: José Juan Torres Martínez  
# Doctorado en Ciencias en Bioprocesos  
# Laboratorio de Biotecnología Molecular de Células Vegetales y Bioinformática  
# -----------------------------------------------------------

# Tema: Cálculo de la relación F/M y tasa de utilización U  
# Objetivos: Determinar parámetros clave de operación en lodos activados  

# Datos de entrada
S0 <- 300       # DBO5 influente (mg/L)
S <- 20         # DBO5 efluente (mg/L)
theta <- 6/24   # Tiempo de retención hidráulico (6 horas convertidas a días)
X <- 2500       # Concentración de microorganismos (mg SSV/L)

# -----------------------------------------------------------
# 1. Cálculo de la relación alimento/microorganismos (F/M)
# -----------------------------------------------------------
F_M <- S0 / (theta * X)

# -----------------------------------------------------------
# 2. Cálculo de la tasa de utilización (U)
# -----------------------------------------------------------
U <- (S0 - S) / (theta * X)

# -----------------------------------------------------------
# Resultados y análisis
# -----------------------------------------------------------
cat("----------------------------------------\n")  
cat("RELACIÓN F/M Y TASA DE UTILIZACIÓN\n")  
cat("----------------------------------------\n")  
cat("DATOS DE ENTRADA:\n")
cat("DBO5 influente (S0):", S0, "mg/L\n")  
cat("DBO5 efluente (S):", S, "mg/L\n")  
cat("Tiempo retención hidráulico (θ):", theta, "días (6 horas)\n")
cat("Concentración microorganismos (X):", X, "mg SSV/L\n\n")  

cat("RESULTADOS:\n")
cat("a) Relación F/M:", round(F_M, 2), "mg DBO/(mg SSV·d)\n")
cat("b) Tasa de utilización U:", round(U, 2), "mg DBO/(mg SSV·d)\n")
cat("----------------------------------------\n\n")  

# Evaluación de rangos típicos
cat("INTERPRETACIÓN TÉCNICA:\n")
if (F_M >= 0.2 && F_M <= 0.6) {
  cat("✅ La relación F/M de", round(F_M, 2), "está en el rango óptimo\n   (0.2-0.6 d⁻¹) para sistemas convencionales\n")
} else {
  cat("⚠️ Relación F/M fuera del rango típico, revisar condiciones de operación\n")
}

if (U >= 0.3 && U <= 0.7) {
  cat("✅ La tasa de utilización U de", round(U, 2), "indica buena actividad biológica\n")
} else {
  cat("⚠️ Tasa de utilización atípica, evaluar posible inhibición o baja eficiencia\n")
}

# Comparación F/M vs U
cat("\nEFICIENCIA DEL SISTEMA:\n")
cat("- La diferencia entre F/M (entrada) y U (consumo) es de", 
    round(F_M - U, 3), "mg DBO/(mg SSV·d)\n")
cat("- Esto representa una eficiencia de remoción del", 
    round((U/F_M)*100, 1), "%\n")

