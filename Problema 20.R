# ==========================================================================
# PROBLEMA: OPTIMIZACIÓN DE PLANTA DE LODOS ACTIVADOS
# ==========================================================================
# Autor: Dr. José Juan Torres Martínez
# Docente de Ciencias Químicas
# Universidad Autónoma de San Luis Potosí (UASLP)
# México
#
# Material académico para estudiantes
#
# Descripción: OPTIMIZACIÓN DE PLANTA DE LODOS ACTIVADOS
# ==========================================================================


library(ggplot2)  # Para gráficos profesionales

# -----------------------------------------------------------
# DATOS DE ENTRADA (Caso real reportado por operadores)
# -----------------------------------------------------------
datos <- list(
  Q = 4000,           # Caudal (m³/día)
  V = 6000,           # Volumen reactor (m³)
  S0 = 300,           # DBO5 influente (mg/L)
  S_efluente = 20,    # DBO5 efluente deseada (mg/L)
  S_actual = 35,      # DBO5 efluente actual (mg/L, variable)
  X = 3000,           # Biomasa en reactor (mg SSV/L)
  Xr = 8000,          # Biomasa en recirculación (mg SSV/L)
  R = 0.5,            # Razón de recirculación (Qr/Q)
  um = 3.0,           # Tasa máxima crecimiento (d⁻¹)
  Ks = 60,            # Constante de saturación (mg/L)
  Y = 0.6,            # Coeficiente de rendimiento (mg SSV/mg DBO5)
  kd = 0.06,          # Coeficiente de decaimiento (d⁻¹)
  coste_aireacion = 0.15  # €/m³ de aire
)

# 1. Cálculo de parámetros clave
calcular_parametros <- function(datos) {
  theta <- datos$V / datos$Q  # HRT (días)
  mu <- datos$um * (datos$S_actual / (datos$Ks + datos$S_actual))  # Tasa crecimiento
  theta_c <- 1 / (mu - datos$kd)  # Edad del lodo
  
  F_M <- datos$S0 / (theta * datos$X)  # Relación alimento/microorganismos
  U <- (datos$S0 - datos$S_actual) / (theta * datos$X)  # Tasa utilización
  eficiencia <- (datos$S0 - datos$S_actual) / datos$S0 * 100  # % remoción DBO5
  
  return(list(
    theta = theta,
    theta_c = theta_c,
    F_M = F_M,
    U = U,
    eficiencia = eficiencia
  ))
}

# 2. Simulación de escenarios
simular_escenarios <- function(datos, theta_c_range = seq(3, 10, 0.5)) {
  resultados <- data.frame(
    theta_c = theta_c_range,
    X_sim = numeric(length(theta_c_range)),
    F_M_sim = numeric(length(theta_c_range)),
    U_sim = numeric(length(theta_c_range)),
    coste_aireacion = numeric(length(theta_c_range))
  )
  
  for (i in seq_along(theta_c_range)) {
    X_calc <- (datos$Y * (datos$S0 - datos$S_efluente) * theta_c_range[i]) /
      (datos$V/datos$Q * (1 + datos$kd * theta_c_range[i]))
    
    resultados$X_sim[i] <- X_calc
    resultados$F_M_sim[i] <- datos$S0 / (datos$V/datos$Q * X_calc)
    resultados$U_sim[i] <- (datos$S0 - datos$S_efluente) / (datos$V/datos$Q * X_calc)
    resultados$coste_aireacion[i] <- X_calc * 0.0002 * datos$coste_aireacion  # Modelo simplificado
  }
  
  return(resultados)
}

# Ejecutar cálculos
parametros <- calcular_parametros(datos)
escenarios <- simular_escenarios(datos)

# Gráfico 1: Efecto de θc en la biomasa y costos
g1 <- ggplot(escenarios, aes(x = theta_c)) +
  geom_line(aes(y = X_sim, color = "Biomasa (X)"), linewidth = 1.5) +
  geom_line(aes(y = coste_aireacion * 10000, color = "Coste aireación (x10⁴)"), linewidth = 1.5) +
  labs(title = "Optimización: Edad del lodo vs Biomasa y Costos",
       x = "Edad del lodo θc (días)",
       y = "Concentración (mg/L) / Coste relativo",
       color = "Variable") +
  scale_color_manual(values = c("Biomasa (X)" = "blue", "Coste aireación (x10⁴)" = "red")) +
  theme_minimal()

# Gráfico 2: Relación F/M y eficiencia
g2 <- ggplot(escenarios, aes(x = theta_c)) +
  geom_line(aes(y = F_M_sim, color = "F/M"), linewidth = 1.5) +
  geom_line(aes(y = U_sim, color = "U"), linewidth = 1.5) +
  geom_hline(yintercept = 0.3, linetype = "dashed", color = "gray50") +
  annotate("text", x = 8, y = 0.32, label = "Límite operativo", vjust = -0.5) +
  labs(title = "Control de Carga Orgánica",
       x = "Edad del lodo θc (días)",
       y = "Tasa (d⁻¹)",
       color = "Parámetro") +
  scale_color_manual(values = c("F/M" = "darkgreen", "U" = "orange")) +
  theme_minimal()

# DIAGNÓSTICO AUTOMATIZADO
generar_recomendaciones <- function(parametros, datos) {
  cat("\nDIAGNÓSTICO DE LA PLANTA:\n")
  cat("----------------------------------------\n")
  cat(sprintf("EFICIENCIA ACTUAL: %.1f%% (Meta >90%%)\n", parametros$eficiencia))
  cat(sprintf("RELACIÓN F/M: %.2f d⁻¹ (Óptimo 0.2-0.6)\n", parametros$F_M))
  cat(sprintf("EDAD DEL LODO: %.1f días\n", parametros$theta_c))
  
  if (parametros$eficiencia < 90) {
    cat("\n⚠️ PROBLEMA: Eficiencia insuficiente\n")
    if (parametros$F_M > 0.6) {
      cat(" - Causa probable: Sobrecarga orgánica (F/M alto)\n")
      optimal_theta_c <- escenarios$theta_c[which.min(abs(escenarios$F_M_sim - 0.4))]
      cat(" - SOLUCIÓN: Aumentar θc a", round(optimal_theta_c, 1), "días\n")
    } else if (parametros$theta_c < 5) {
      cat(" - Causa probable: Edad del lodo insuficiente\n")
      cat(" - SOLUCIÓN: Reducir purgas para aumentar θc a 5-7 días\n")
    }
  } else {
    cat("\n✅ SISTEMA DENTRO DE PARÁMETROS ÓPTIMOS\n")
  }
  
  # Recomendación de ahorro energético
  optimal_row <- escenarios[which.min(escenarios$coste_aireacion), ]
  cat(sprintf("\n💡 OPORTUNIDAD: Reducir costes un %.0f%% ajustando θc a %.1f días\n",
              (1 - min(escenarios$coste_aireacion)/max(escenarios$coste_aireacion)) * 100,
              optimal_row$theta_c))
}

# EJECUCIÓN Y REPORTE
cat("========================================\n")
cat(" INFORME TÉCNICO: OPTIMIZACIÓN DE LODOS ACTIVADOS\n")
cat("========================================\n")

print(g1)
ggsave("problema_20_biomasa_costes.png", plot = g1, width = 8, height = 5, dpi = 300)

# --------------------------------------------------------------------------
# Interpretación: Optimización: Edad del lodo vs Biomasa y Costos
# --------------------------------------------------------------------------
# - Eje X: la edad del lodo (θc, theta_c) en días, de 3 a 10 días. Es el tiempo promedio que los microorganismos permanecen en el sistema.
# - Eje Y: dos cosas a la vez en un mismo eje, por eso hay que leerlo con cuidado: la biomasa X en mg/L (línea azul, miligramos de sólidos suspendidos volátiles por litro) y el coste de aireación multiplicado por 10 000 (línea roja). Ese factor 10 000 solo sirve para que ambas curvas quepan en el mismo gráfico: los números que lee la línea roja en este eje NO son euros ni mg/L, son un valor relativo escalado para comparar. El coste del modelo es un cálculo simplificado del script y su unidad final no está especificada en el archivo.
# - Lo que se aprecia: al aumentar la edad del lodo, tanto la biomasa como el coste suben: la biomasa crece rápido al principio y más despacio después, y el coste sigue esa misma tendencia porque depende de la biomasa.
# - Qué significa: envejecer el lodo acumula más biomasa en el reactor, pero cuesta más aireación; el punto de trabajo debe equilibrar un tratamiento suficiente con un coste razonable. La lectura del coste en este gráfico es solo comparativa, por el factor de escala de 10 000.
print(g2)
ggsave("problema_20_carga_organica.png", plot = g2, width = 8, height = 5, dpi = 300)

# --------------------------------------------------------------------------
# Interpretación: Control de Carga Orgánica
# --------------------------------------------------------------------------
# - Eje X: la edad del lodo (θc, theta_c) en días, de 3 a 10 días.
# - Eje Y: una tasa en d⁻¹ (por día, es decir, 1/día). Las dos curvas comparten la misma unidad: F/M es la relación alimento/microorganismos (kg de DBO5 por kg de biomasa y por día) y U es la tasa de utilización del sustrato (kg de DBO5 removida por kg de biomasa y por día). La línea gris discontinua marca 0.3 d⁻¹ como valor de referencia operativo.
# - Lo que se aprecia: al aumentar la edad del lodo, tanto F/M como U bajan: con el lodo más viejo hay menos alimento disponible por cada microorganismo. La curva de F/M se mueve dentro de la zona cercana a la referencia de 0.3 d⁻¹.
# - Qué significa: con el lodo más viejo el sistema trabaja con menos carga por microorganismo, lo que suele dar un efluente más limpio; el valor de referencia ayuda a ver si F/M está dentro del rango óptimo de 0.2 a 0.6 d⁻¹ que usa el diagnóstico del script.
generar_recomendaciones(parametros, datos)


# --------------------------------------------------------------------------
# Qué aprendimos
# --------------------------------------------------------------------------
# - La edad del lodo (θc, en días) controla a la vez la biomasa del reactor y el coste de aireación: más edad, más biomasa y más coste.
# - La relación F/M y la tasa de utilización U se expresan en d⁻¹ (por día) y su rango óptimo orienta la operación de la planta.
# - La eficiencia de remoción de DBO5 se juzga contra la meta del 90 %: por debajo de ese valor hay que actuar sobre la edad del lodo o sobre las purgas.
# - Un gráfico con dos variables en un mismo eje exige avisar cuando una de ellas está escalada, como el coste multiplicado por 10 000 en el primer gráfico.

cat("\n----------------------------------------\n")
cat("Resumen de parámetros actuales:\n")
cat(sprintf("- HRT (θ): %.2f días\n", parametros$theta))
cat(sprintf("- Edad del lodo (θc): %.1f días\n", parametros$theta_c))
cat(sprintf("- F/M: %.2f d⁻¹\n", parametros$F_M))
cat(sprintf("- U: %.2f d⁻¹\n", parametros$U))
