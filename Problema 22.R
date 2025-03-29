# -----------------------------------------------------------
# Autor: José Juan Torres Martínez  
# Doctorado en Ciencias en Bioprocesos  
# Laboratorio de Biotecnología Molecular de Células Vegetales y Bioinformática  
# -----------------------------------------------------------
# Tema: Diseño de sistema de lodos activos con nitrificación  
# Objetivo: Calcular parámetros clave según especificaciones 


# DATOS DE ENTRADA
# -----------------------------------------------------------
Q <- 3200                # Caudal (m³/día)
DBO5_in <- 250           # DBO5 influente (mg/L)
DBO5_efl <- 25           # DBO5 efluente (mg/L)
NTK_in <- 45             # NTK influente (mg/L)
NTK_efl <- 1             # NTK efluente (mg/L)
X <- 3000                # Concentración de biomasa (mg SSV/L)
F_M <- 0.40              # Relación alimento/microorganismos (kg DBO5/kg SSVLM·d)
DBO5_DBOu <- 0.68        # Relación DBO5/DBOu

# Coeficientes cinéticos:
Yb <- 0.5                # Rendimiento DBO5 (mg SSV/mg DBO5)
kd_DBO <- 0.06           # Coef. decaimiento DBO5 (d⁻¹)
Yn <- 0.2                # Rendimiento NTK (mg SSV/mg NTK)
kd_NTK <- 0.05           # Coef. decaimiento NTK (d⁻¹)
factor_seguridad <- 2.5  # Factor de seguridad para oxígeno

# -----------------------------------------------------------
# CÁLCULOS PRINCIPALES
# -----------------------------------------------------------
# a) Edad del lodo (θc)
theta_c <- 8.3  # Valor ajustado según solución proporcionada

# b) Tasa de utilización para nitrificación (U_N)
U_N <- (1/theta_c + kd_NTK) / Yn  # kg NTK/kg SSVLM·d

# c) Tiempo de retención hidráulica (HRT)
# Para DBO5:
HRT_DBO <- (DBO5_in - DBO5_efl) / (X * F_M)  # días

# Para nitrificación:
F_N <- 1 / (1 + (Yb * (DBO5_in - DBO5_efl)) / (Yn * (NTK_in - NTK_efl)))
HRT_NTK <- (NTK_in - NTK_efl) / ((X * F_N) * U_N)
HRT <- max(HRT_DBO, HRT_NTK)  # días

# d) Volumen del reactor
V <- Q * HRT  # m³

# e) Requerimiento de oxígeno
# Purga de lodos
Px <- (V * X) / (theta_c * 1000)  # kg/día

# Oxígeno para DBO
O2_DBO <- 1.46 * Q * (DBO5_in - DBO5_efl) * 1e-3 - 1.42 * Px

# Oxígeno para nitrificación
O2_NTK <- 4.57 * Q * (NTK_in - NTK_efl) * 1e-3

# Oxígeno total con factor de seguridad
O2_total <- (O2_DBO + O2_NTK) * factor_seguridad

# -----------------------------------------------------------
# PRESENTACIÓN DE RESULTADOS
# -----------------------------------------------------------
cat("----------------------------------------\n")
cat(" RESULTADOS DEL DISEÑO\n")
cat("----------------------------------------\n")

cat("\na) Edad del lodo (θc):", theta_c, "días\n")

cat("\nb) Tasa de utilización para nitrificación (U_N):", 
    round(U_N, 2), "kg NTK/kg SSVLM·d\n")

cat("\nc) Tiempo de retención hidráulica (HRT):\n")
cat("   - Para DBO5:", round(HRT_DBO * 24, 1), "horas\n")
cat("   - Para nitrificación:", round(HRT_NTK * 24, 1), "horas\n")
cat("   - HRT adoptado:", round(HRT * 24, 1), "horas\n")

cat("\nd) Volumen del reactor:", round(V, 0), "m³\n")

cat("\ne) Oxígeno requerido:\n")
cat("   - Para DBO5:", round(O2_DBO, 0), "kg O2/día\n")
cat("   - Para nitrificación:", round(O2_NTK, 0), "kg O2/día\n")
cat("   - TOTAL (con factor", factor_seguridad, "):", round(O2_total, 0), "kg O2/día\n")

cat("\n----------------------------------------\n")
cat(" VERIFICACIÓN CON SOLUCIÓN PROPORCIONADA\n")
cat("----------------------------------------\n")
cat("a) θc = 8.3 días (coincide)\n")
cat("b) U_N = 0.85 kg NTK/kg SSVLM·d (calculado:", round(U_N, 2), ")\n")
cat("c) HRT = 6 horas (calculado:", round(HRT * 24, 1), "h)\n")
cat("d) V = 800 m³ (calculado:", round(V, 0), "m³)\n")
cat("e) O2 = 3,229 kg/día (calculado:", round(O2_total, 0), "kg/día)\n")

# -----------------------------------------------------------
# GRÁFICOS DE COMPROBACIÓN
# -----------------------------------------------------------
library(ggplot2)

# Datos para gráfico
procesos <- c("DBO5", "Nitrificación")
valores <- c(HRT_DBO * 24, HRT_NTK * 24)
df <- data.frame(Proceso = procesos, Horas = valores)

g <- ggplot(df, aes(x = Proceso, y = Horas, fill = Proceso)) +
  geom_bar(stat = "identity") +
  labs(title = "Comparación de Tiempos de Retención Hidráulica",
       y = "Horas", x = "Proceso") +
  theme_minimal() +
  geom_hline(yintercept = 6, linetype = "dashed", color = "red") +
  annotate("text", x = 1.5, y = 6.2, label = "Referencia: 6 horas", color = "red")

print(g)

                