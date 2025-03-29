# -----------------------------------------------------------
# Autor: José Juan Torres Martínez  
# Doctorado en Ciencias en Bioprocesos  
# Laboratorio de Biotecnología Molecular de Células Vegetales y Bioinformática  
# -----------------------------------------------------------

# Tema: Cálculo de reactor y requerimientos de oxígeno para lodos activados  
# 
# -----------------------------------------------------------
# DATOS DE ENTRADA
# -----------------------------------------------------------
Q <- 3500                # Caudal (m³/día)
theta_c <- 9             # Tiempo retención celular (días)
X <- 2200                # Concentración microorganismos (mg SSV/L)

# Calidades del agua:
DBO5_in <- 250           # DBO5 influente (mg/L)
DBO5_efl <- 25           # DBO5 efluente (90% remoción)
NTK_in <- 9             # NTK influente (mg/L) #error
NTK_efl <- 0.39          # NTK efluente (99% remoción)

# Coeficientes:
Yb <- 0.5                # Coef. rendimiento DBO5
Yn <- 0.2                # Coef. rendimiento NTK
kd <- 0.05               # Coef. decaimiento (para nitrificación)

# -----------------------------------------------------------
# CÁLCULOS 
# -----------------------------------------------------------

# 1. Cálculo de parámetros U 
U_DBO <- 0.34            # Dato proporcionado en la imagen (kg DBO/kg SSVLM-d)
U_NTK <- (1/theta_c + kd)/Yn  # Ecuación 
U_NTK <- (1/9 + 0.05)/0.2     # = 0.81 kg NTK/kg SSVLM-d

# 2. Cálculo de HRT (Tiempo Retención Hidráulica)
# Para DBO:
HRT_DBO <- (DBO5_in - DBO5_efl)/(X * U_DBO)  # = 0.301 días = 7.2 h

# Para Nitrificación:
# Cálculo de F_N (factor de corrección)
F_N <- 1/(1 + (Yb*(DBO5_in - DBO5_efl))/(Yn*(NTK_in - NTK_efl)))  # = 0.064

HRT_NTK <- (NTK_in - NTK_efl)/((X * F_N) * U_NTK)  # = 0.34 días = 8.2 h

# 3. Volumen del reactor (según etapa limitante)
HRT <- max(HRT_DBO, HRT_NTK)  # Nitrificación es limitante
V_reactor <- Q * HRT           # = 1190 m³

# 4. Cálculo de purga de lodos (Px)
Px <- (V_reactor * X)/(theta_c * 1000)  # = 290.89 kg/día

# 5. Requerimiento de oxígeno (OUR)
OUR <- (1.46 * Q * (DBO5_in - DBO5_efl) * 1e-3 - 1.42 * Px) + 
  (4.57 * Q * (NTK_in - NTK_efl) * 1e-3)  # = 1362 kg O2/día

# -----------------------------------------------------------
# PRESENTACIÓN DE RESULTADOS
# -----------------------------------------------------------
cat("\n----------------------------------------\n")
cat(" RESULTADOS\n")
cat("----------------------------------------\n")

cat("\n1. PARÁMETROS DE DISEÑO:\n")
cat(" - Relación F/M para DBO5:", U_DBO, "kg DBO/kg SSVLM-d\n")
cat(" - Tasa utilización NTK:", round(U_NTK, 2), "kg NTK/kg SSVLM-d\n")
cat(" - Factor de corrección F_N:", round(F_N, 3), "\n")

cat("\n2. TIEMPOS DE RETENCIÓN:\n")
cat(" - HRT para DBO5:", round(HRT_DBO, 3), "días (", round(HRT_DBO*24,1), "h)\n")
cat(" - HRT para nitrificación:", round(HRT_NTK, 3), "días (", round(HRT_NTK*24,1), "h)\n")
cat(" - HRT TOTAL (nitrificación limita):", round(HRT, 3), "días\n")

cat("\n3. DIMENSIONAMIENTO:\n")
cat(" - Volumen reactor:", round(V_reactor, 0), "m³\n")
cat(" - Purga de lodos (Px):", round(Px, 0), "kg/día\n")
cat(" - Oxígeno requerido (OUR):", round(OUR, 0), "kg O2/día\n")

cat("\n----------------------------------------\n")
cat(" NOTAS TÉCNICAS:\n")
cat("----------------------------------------\n")
cat("1. La nitrificación es la etapa limitante (8.2 h vs 7.2 h para DBO5)\n")
cat("2. El oxígeno para nitrificación representa el", 
    round(4.57*Q*(NTK_in-NTK_efl)*1e-3/OUR*100, 1), "% del total\n")

# -----------------------------------------------------------
# GRÁFICO COMPARATIVO
# -----------------------------------------------------------
require(ggplot2)
datos_grafico <- data.frame(
  Proceso = c("DBO5", "Nitrificación"),
  HRT_horas = c(HRT_DBO*24, HRT_NTK*24),
  Oxigeno_kg = c(1.46*Q*(DBO5_in-DBO5_efl)*1e-3 - 1.42*Px, 
                 4.57*Q*(NTK_in-NTK_efl)*1e-3)
)

g1 <- ggplot(datos_grafico, aes(x = Proceso, y = HRT_horas, fill = Proceso)) +
  geom_bar(stat = "identity") +
  labs(title = "Comparación de Tiempos de Retención",
       y = "HRT (horas)", x = "") +
  theme_minimal()

g2 <- ggplot(datos_grafico, aes(x = Proceso, y = Oxigeno_kg, fill = Proceso)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribución de Requerimientos de Oxígeno",
       y = "kg O2/día", x = "") +
  theme_minimal()

# Visualizar gráficos
print(g1)
print(g2)

