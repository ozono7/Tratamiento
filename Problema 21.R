# ==========================================================================
# Cálculo de reactor y requerimientos de oxígeno para lodos activados
# ==========================================================================
# Autor: Dr. José Juan Torres Martínez
# Docente de Ciencias Químicas
# Universidad Autónoma de San Luis Potosí (UASLP)
# México
#
# Material académico para estudiantes
#
# Descripción: Cálculo de reactor y requerimientos de oxígeno para lodos activados
# ==========================================================================


# DATOS DE ENTRADA
# -----------------------------------------------------------
Q <- 3500                # Caudal (m³/día)
theta_c <- 9             # Tiempo retención celular (días)
X <- 2200                # Concentración microorganismos (mg SSV/L)

# Calidades del agua:
DBO5_in <- 250           # DBO5 influente (mg/L)
DBO5_efl <- 25           # DBO5 efluente (90% remoción)
NTK_in <- 39            # NTK influente (mg/L)
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
U_NTK <- (1/theta_c + kd)/Yn  # = 0.81 kg NTK/kg SSVLM-d

# 2. Cálculo de HRT (Tiempo Retención Hidráulica)
# Para DBO:
HRT_DBO <- (DBO5_in - DBO5_efl)/(X * U_DBO)  # = 0.301 días = 7.2 h

# Para Nitrificación:
# Cálculo de F_N (factor de corrección)
F_N <- 1/(1 + (Yb*(DBO5_in - DBO5_efl))/(Yn*(NTK_in - NTK_efl)))  # = 0.064

HRT_NTK <- (NTK_in - NTK_efl)/((X * F_N) * U_NTK)  # = 0.34 días = 8.1 h

# 3. Volumen del reactor (según etapa limitante)
HRT <- max(HRT_DBO, HRT_NTK)  # Nitrificación es limitante
V_reactor <- Q * HRT           # = 1187 m³

# 4. Cálculo de purga de lodos (Px)
Px <- (V_reactor * X)/(theta_c * 1000)  # = 290 kg/día

# 5. Requerimiento de oxígeno (OUR)
OUR <- (1.46 * Q * (DBO5_in - DBO5_efl) * 1e-3 - 1.42 * Px) + 
  (4.57 * Q * (NTK_in - NTK_efl) * 1e-3)  # = 1355 kg O2/día

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
cat("1. La nitrificación es la etapa limitante (", round(HRT_NTK*24, 1), " h vs ", round(HRT_DBO*24, 1), " h para DBO5)\n")
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
ggsave("problema_21_hrt.png", plot = g1, width = 8, height = 5, dpi = 300)

# --------------------------------------------------------------------------
# Interpretación: Comparación de Tiempos de Retención
# --------------------------------------------------------------------------
# - Eje X: el proceso (DBO5 o Nitrificación). Es una categoría, no una cantidad medida, por eso no tiene unidades.
# - Eje Y: el tiempo de retención hidráulica (HRT) en horas. El HRT es el tiempo promedio que el agua permanece dentro del reactor.
# - Lo que se aprecia: la barra de nitrificación (unas 8.1 horas según el cálculo del código) es un poco más alta que la de DBO5 (7.2 horas).
# - Qué significa: la nitrificación necesita más tiempo que la remoción de DBO5, por eso es la etapa limitante y es la que fija el tamaño del reactor: el volumen se calcula con el HRT mayor.
print(g2)
ggsave("problema_21_oxigeno.png", plot = g2, width = 8, height = 5, dpi = 300)

# --------------------------------------------------------------------------
# Interpretación: Distribución de Requerimientos de Oxígeno
# --------------------------------------------------------------------------
# - Eje X: el proceso (DBO5 o Nitrificación). Es una categoría, no una cantidad medida, por eso no tiene unidades.
# - Eje Y: el oxígeno requerido en kg O2/día (kilogramos de oxígeno por día). Es el oxígeno que hay que suministrar con la aireación para oxidar la materia orgánica (DBO5) y para nitrificar el nitrógeno (NTK).
# - Lo que se aprecia: la barra de DBO5 (unos 738 kg O2/día según el cálculo del código) es un poco más alta que la de nitrificación (unos 618 kg O2/día).
# - Qué significa: con los datos corregidos, el oxígeno se reparte casi a partes iguales: unos 738 kg O2/día para oxidar la DBO5 y unos 618 kg O2/día para nitrificar, alrededor del 46 % del total. La nitrificación ya no es un consumo menor: con un NTK de entrada de 39 mg/L hay mucho nitrógeno que oxidar.

# --------------------------------------------------------------------------
# Qué aprendimos
# --------------------------------------------------------------------------
# - El HRT (tiempo de retención hidráulica, en horas o días) dice cuánto tiempo permanece el agua en el reactor; la etapa que pide más tiempo es la limitante y fija el volumen.
# - El requerimiento de oxígeno (OUR, en kg O2/día) se calcula por separado para la DBO5 y para la nitrificación, y luego se suma.
# - El dato de entrada NTK_in es 39 mg/L: con un efluente de 0.39 mg/L la remoción de NTK es del 99.0 %, el factor F_N es 0.064, el HRT de nitrificación es de 8.1 horas, el volumen del reactor es de 1187 m³ y el oxígeno total es de 1355 kg O2/día, con alrededor del 46 % para nitrificación.
