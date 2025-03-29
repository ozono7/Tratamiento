# -----------------------------------------------------------
# Autor: José Juan Torres Martínez  
# Concentración de microorganismos
# -----------------------------------------------------------

# Datos comunes para ambos casos
S0 <- 300       # DBO5 influente (mg/L)
S <- 20         # DBO5 efluente (mg/L)
Y <- 0.6        # Coeficiente de rendimiento (mg SSV/mg DBO5)
kd <- 0.06      # Coeficiente de decaimiento (d⁻¹)

# -----------------------------------------------------------
# a) Reactor de mezcla perfecta sin recirculación (θ = θc = 5 días)
# -----------------------------------------------------------
theta_a <- 5    # Tiempo de retención hidráulico = edad del lodo (días)

# Cálculo de la concentración de microorganismos (Ecuación 5.31)
X_a <- (Y * (S0 - S)) / (1 + kd * theta_a)

# -----------------------------------------------------------
# b) Reactor de mezcla perfecta con recirculación (θ = 6h, θc = 5 días)
# -----------------------------------------------------------
theta_b <- 6/24  # Tiempo de retención hidráulico (6 horas convertidas a días)
theta_c_b <- 5   # Edad del lodo (días)

# Cálculo de la concentración de microorganismos
X_b <- (Y * (S0 - S) * theta_c_b) / (theta_b * (1 + kd * theta_c_b))

# -----------------------------------------------------------
# Resultados
# -----------------------------------------------------------
cat("----------------------------------------\n")  
cat("CONCENTRACIÓN DE MICROORGANISMOS\n")  
cat("----------------------------------------\n")  
cat("DATOS COMUNES:\n")
cat("DBO5 influente (S0):", S0, "mg/L\n")  
cat("DBO5 efluente (S):", S, "mg/L\n")  
cat("Coef. rendimiento (Y):", Y, "mg SSV/mg DBO5\n")  
cat("Coef. decaimiento (kd):", kd, "d⁻¹\n\n")  

cat("a) REACTOR SIN RECIRCULACIÓN:\n")
cat(" - Tiempo retención (θ):", theta_a, "días\n")
cat(" - Edad del lodo (θc):", theta_a, "días\n")
cat(" - Concentración microorganismos (X):", round(X_a, 1), "mg SSV/L\n\n")

cat("b) REACTOR CON RECIRCULACIÓN:\n")
cat(" - Tiempo retención (θ):", theta_b, "días (6 horas)\n")
cat(" - Edad del lodo (θc):", theta_c_b, "días\n")
cat(" - Concentración microorganismos (X):", round(X_b, 1), "mg SSV/L\n")
cat("----------------------------------------\n")  

# Interpretación de resultados
cat("\nINTERPRETACIÓN:\n")
cat("- El sistema con recirculación mantiene una concentración de biomasa", 
    round(X_b/X_a, 1), "veces mayor que el sistema sin recirculación.\n")
cat("- Esto demuestra la eficiencia de los sistemas con recirculación para mantener\n")
cat("  alta concentración de microorganismos a pesar de un HRT más corto.\n")

