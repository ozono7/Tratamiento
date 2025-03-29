# -----------------------------------------------------------
# Autor: José Juan Torres Martínez  
# Doctorado en Ciencias en Bioprocesos  
# Laboratorio de Biotecnología Molecular de Células Vegetales y Bioinformática  
# -----------------------------------------------------------

# Tema: Cálculo de Velocidad Ascensional en Decantador Circular  
# Objetivo: Determinar Vₐ para evaluar eficiencia  

# Datos de entrada  
D <- 25                          # Diámetro (m)  
Q <- 1400                        # Caudal (m³/h)  

# 1. Cálculo del área superficial (A)  
radio <- D / 2  
A <- pi * radio^2                # Área (m²)  

# 2. Velocidad ascensional (Vₐ)  
Va <- Q / A                      # Velocidad (m/h)  
Va_mm_s <- Va * 1000 / 3600      # Conversión a mm/s  

# Resultados  
cat("----------------------------------------\n")  
cat("CÁLCULO DE VELOCIDAD ASCENSIONAL\n")  
cat("----------------------------------------\n")  
cat("Diámetro del decantador (D):", D, "m\n")  
cat("Caudal de entrada (Q):", Q, "m³/h\n")  
cat("Área superficial (A):", round(A, 2), "m²\n\n")  
cat("Velocidad ascensional (Vₐ):\n")  
cat("- En m/h:", round(Va, 4), "m/h\n")  
cat("- En mm/s:", round(Va_mm_s, 2), "mm/s\n")  
cat("----------------------------------------\n")  

# Verificación de estándares  
if (Va_mm_s <= 0.7) {  
  cat("✅ Vₐ dentro del rango típico para decantadores primarios (≤0.7 mm/s)\n")  
} else {  
  cat("⚠️ Vₐ excede el límite recomendado. Riesgo de arrastre de sólidos.\n")  
}  



# Gráfico de relación caudal-velocidad  
library(ggplot2)  
caudal_range <- seq(1000, 2000, by = 100)  
va_range <- caudal_range / A  

ggplot(data.frame(Caudal = caudal_range, Velocidad = va_range),  
       aes(x = Caudal, y = Velocidad * 1000 / 3600)) +  
  geom_line(color = "blue", linewidth = 1) +  
  geom_hline(yintercept = 0.7, linetype = "dashed", color = "red") +  
  labs(title = "Relación Caudal-Velocidad Ascensional",  
       x = "Caudal (m³/h)",  
       y = "Velocidad Ascensional (mm/s)") +  
  theme_minimal()  

