# -----------------------------------------------------------
# Autor: José Juan Torres Martínez  
# Doctorado en Ciencias en Bioprocesos  
# Laboratorio de Biotecnología Molecular de Células Vegetales y Bioinformática  
# -----------------------------------------------------------

# Tema: Cálculo de DBO5 en efluente usando modelo Schulze  
# Objetivo: Determinar la eficiencia de un filtro percolador de baja carga  

# -----------------------------------------------------------
# DATOS DE ENTRADA
# -----------------------------------------------------------
Diametro <- 30            # Diámetro del filtro (m)
Profundidad <- 2.0        # Profundidad del lecho (m)
Q <- 2000                 # Caudal (m³/día)
S0 <- 150                 # DBO5 influente (mg/L)
K <- 2.3                  # Constante de velocidad [(m/d)^0.67/m]
n <- 0.67                 # Coeficiente de la ecuación

# -----------------------------------------------------------
# CÁLCULOS PRINCIPALES
# -----------------------------------------------------------

# 1. Cálculo del área superficial
Area <- (pi/4) * Diametro^2  # m²

# 2. Cálculo de la carga hidráulica (Lh)
Lh <- Q / Area  # m³/(m²·día)

# 3. Aplicación del modelo Schulze (Ecuación 7.8)
Se <- S0 * exp(-K * Profundidad / (Lh^n))  # mg/L

# -----------------------------------------------------------
# PRESENTACIÓN DE RESULTADOS
# -----------------------------------------------------------
cat("\n----------------------------------------\n")
cat(" FILTRO PERCOLADOR (SCHULZE)\n")
cat("----------------------------------------\n")

cat("\n1. PARÁMETROS GEOMÉTRICOS:\n")
cat(" - Diámetro del filtro:", Diametro, "m\n")
cat(" - Profundidad del lecho:", Profundidad, "m\n")
cat(" - Área superficial:", round(Area, 0), "m²\n")

cat("\n2. PARÁMETROS HIDRÁULICOS:\n")
cat(" - Caudal (Q):", Q, "m³/día\n")
cat(" - Carga hidráulica (Lh):", round(Lh, 2), "m³/(m²·día)\n")

cat("\n3. RESULTADOS DE TRATAMIENTO:\n")
cat(" - DBO5 influente (S0):", S0, "mg/L\n")
cat(" - DBO5 efluente (Se):", round(Se, 1), "mg/L\n")
cat(" - Eficiencia de remoción:", round((1 - Se/S0)*100, 1), "%\n")

# -----------------------------------------------------------
# ANÁLISIS DE SENSIBILIDAD
# -----------------------------------------------------------
library(ggplot2)

# Variar la profundidad del lecho
prof_range <- seq(1, 3, by = 0.1)
Se_range <- S0 * exp(-K * prof_range / (Lh^n))

df <- data.frame(Profundidad = prof_range, DBO5_efluente = Se_range)

g <- ggplot(df, aes(x = Profundidad, y = DBO5_efluente)) +
  geom_line(color = "blue", linewidth = 1.5) +
  geom_vline(xintercept = Profundidad, linetype = "dashed", color = "red") +
  annotate("text", x = Profundidad + 0.2, y = 50, 
           label = paste("Profundidad =", Profundidad, "m"), color = "red") +
  labs(title = "Efecto de la Profundidad del Lecho en la DBO5 Efluente",
       x = "Profundidad del Lecho (m)",
       y = "DBO5 Efluente (mg/L)") +
  theme_minimal()

print(g)

