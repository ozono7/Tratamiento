# ==========================================================================
# Cálculo de etapas en contactor biológico rotativo
# ==========================================================================
# Autor: Dr. José Juan Torres Martínez
# Docente de Ciencias Químicas
# Universidad Autónoma de San Luis Potosí (UASLP)
# México
#
# Material académico para estudiantes
#
# Descripción: Determinar número de etapas para reducir DBO5 de 60 a 20 mg/L
# ==========================================================================


# DATOS DE ENTRADA
# -----------------------------------------------------------
S0 <- 60                  # DBO5 inicial (mg/L)
Se_deseado <- 20          # DBO5 objetivo (mg/L)
Q <- 20000                # Caudal (m³/d)
A_etapa <- 20000          # Área de discos por etapa (m²)
k <- 0.00974              # Constante del modelo Optiken

# -----------------------------------------------------------
# FUNCIÓN DEL MODELO OPTIKEN
# -----------------------------------------------------------
calcular_DBO_salida <- function(S_entrada, A_Q) {
  (-1 + sqrt(1 + 4 * k * A_Q * S_entrada)) / (2 * k * A_Q)
}

# -----------------------------------------------------------
# SIMULACIÓN POR ETAPAS
# -----------------------------------------------------------
A_Q <- A_etapa/Q         # Relación área/caudal (d/m)
resultados <- data.frame(Etapa = integer(),
                         DBO_entrada = numeric(),
                         DBO_salida = numeric())

S_actual <- S0
etapa <- 1

while(S_actual > Se_deseado) {
  S_salida <- calcular_DBO_salida(S_actual, A_Q)
  
  resultados <- rbind(resultados, 
                      data.frame(Etapa = etapa,
                                 DBO_entrada = S_actual,
                                 DBO_salida = S_salida))
  
  S_actual <- S_salida
  etapa <- etapa + 1
  
  # Prevención de bucle infinito
  if(etapa > 20) break
}

# -----------------------------------------------------------
# PRESENTACIÓN DE RESULTADOS
# -----------------------------------------------------------
cat("\n----------------------------------------\n")
cat(" CONTACTOR BIOLÓGICO ROTATIVO\n")
cat("----------------------------------------\n")

cat("\nPARÁMETROS DE DISEÑO:\n")
cat(" - DBO5 inicial (S0):", S0, "mg/L\n")
cat(" - DBO5 objetivo:", Se_deseado, "mg/L\n")
cat(" - Caudal (Q):", Q, "m³/d\n")
cat(" - Área por etapa:", A_etapa, "m²\n")
cat(" - Relación A/Q:", round(A_Q, 2), "d/m\n")

cat("\nRESULTADOS POR ETAPA:\n")
print(resultados, row.names = FALSE)

cat("\nCONCLUSIÓN:\n")
cat("Se requieren", nrow(resultados), "etapas para alcanzar\n") 
cat("una DBO5 de", round(tail(resultados$DBO_salida, 1), 1), "mg/L\n")

# -----------------------------------------------------------
# GRÁFICO DE EVOLUCIÓN POR ETAPAS
# -----------------------------------------------------------
library(ggplot2)

g <- ggplot(resultados, aes(x = Etapa)) +
  geom_line(aes(y = DBO_entrada, color = "Entrada"), linewidth = 1.5) +
  geom_line(aes(y = DBO_salida, color = "Salida"), linewidth = 1.5) +
  geom_hline(yintercept = Se_deseado, linetype = "dashed", color = "red") +
  annotate("text", x = 1, y = Se_deseado + 3, 
           label = paste("Objetivo:", Se_deseado, "mg/L"), color = "red") +
  labs(title = "Reducción de DBO5 en Contactor Biológico Rotativo",
       x = "Número de Etapa",
       y = "DBO5 (mg/L)",
       color = "") +
  scale_color_manual(values = c("Entrada" = "blue", "Salida" = "green")) +
  theme_minimal() +
  scale_x_continuous(breaks = 1:nrow(resultados))

print(g)

# --------------------------------------------------------------------------
# Interpretación: Reducción de DBO5 en Contactor Biológico Rotativo
# --------------------------------------------------------------------------
# - Eje X: el número de etapa (1, 2, 3, ...). Es un conteo, por eso es adimensional y no tiene unidades.
# - Eje Y: la DBO5 en mg/L (miligramos por litro). Hay dos líneas: la azul es la DBO5 que entra a cada etapa y la verde es la DBO5 que sale de esa etapa. La línea roja discontinua marca el objetivo, 20 mg/L.
# - Lo que se aprecia: las dos líneas bajan etapa tras etapa y la caída más grande ocurre en la primera (de 60 a unos 42.5 mg/L); después cada etapa quita cada vez menos. La línea verde cruza el objetivo de 20 mg/L en la quinta etapa, con una salida de unos 18.2 mg/L.
# - Qué significa: se necesitan 5 etapas para pasar de 60 a menos de 20 mg/L, y las primeras etapas hacen la mayor parte del trabajo: añadir etapas más allá de la quinta aportaría muy poco.

# --------------------------------------------------------------------------
# Qué aprendimos
# --------------------------------------------------------------------------
# - Un contactor biológico rotativo trabaja por etapas en serie: la salida de una etapa es la entrada de la siguiente.
# - La relación área/caudal (A/Q) se expresa en d/m (días por metro) y es el parámetro que alimenta el modelo de Optiken.
# - Cada etapa remueve menos que la anterior: la caída más grande ocurre en la primera etapa, cuando la DBO5 es más alta.
