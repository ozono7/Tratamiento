# -----------------------------------------------------------
# Autor: José Juan Torres Martínez  
# Laboratorio de Tratamiento de Aguas  
# -----------------------------------------------------------

# Tema: Análisis de Sólidos Totales en Aguas Residuales  
# Objetivo: Calcular ST, SST y SDT  

# Datos del problema  
vol_muestra <- 250                # Volumen de muestra en mL  
peso_crisol_vacio <- 25.5000      # Peso del crisol vacío en g  
peso_crisol_ST <- 25.5235         # Peso del crisol + residuo ST en g  
peso_filtro <- 1.2000             # Peso del filtro en g  
peso_filtro_SST <- 1.2050         # Peso del filtro + residuo SST en g  

# Cálculos  
ST <- (peso_crisol_ST - peso_crisol_vacio) * (1000 / vol_muestra)  # en mg/L  
SST <- (peso_filtro_SST - peso_filtro) * (1000 / vol_muestra)      # en mg/L  
SDT <- ST - SST                                                    # en mg/L  

# Resultados  
cat("----------------------------------------\n")  
cat("Resultados del Análisis:\n")  
cat("----------------------------------------\n")  
cat("Sólidos Totales (ST):", round(ST, 2), "mg/L\n")  
cat("Sólidos Suspendidos Totales (SST):", round(SST, 2), "mg/L\n")  
cat("Sólidos Disueltos Totales (SDT):", round(SDT, 2), "mg/L\n")  
cat("----------------------------------------\n")  

# Interpretación  
if (ST > 500) {  
  cat("¡Alerta! Alta carga de sólidos. Considerar tratamiento adicional.\n")  
} else {  
  cat("Carga de sólidos dentro de límites normales.\n")  
}  

