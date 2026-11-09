# ==========================================================================
# Análisis de Sólidos Totales en Aguas Residuales
# ==========================================================================
# Autor: Dr. José Juan Torres Martínez
# Docente de Ciencias Químicas
# Universidad Autónoma de San Luis Potosí (UASLP)
# México
#
# Material académico para estudiantes
#
# Descripción: Calcular ST, SST y SDT
# ==========================================================================


# Datos del problema  
vol_muestra <- 250                # Volumen de muestra en mL  
peso_crisol_vacio <- 25.5000      # Peso del crisol vacío en g  
peso_crisol_ST <- 25.5235         # Peso del crisol + residuo ST en g  
peso_filtro <- 1.2000             # Peso del filtro en g  
peso_filtro_SST <- 1.2050         # Peso del filtro + residuo SST en g  

# Cálculos  
# La masa del residuo está en g y el volumen en mL: mg/L = masa (g) * 1000 (mg/g)
# entre volumen (mL) / 1000 (mL/L), es decir, masa (g) * 1e6 / volumen (mL).
ST <- (peso_crisol_ST - peso_crisol_vacio) * 1e6 / vol_muestra  # en mg/L
SST <- (peso_filtro_SST - peso_filtro) * 1e6 / vol_muestra      # en mg/L  
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


# --------------------------------------------------------------------------
# Qué aprendimos
# --------------------------------------------------------------------------
# - Los sólidos totales se determinan por gravimetría: pesar el residuo seco de un volumen conocido de muestra.
# - Los SST quedan retenidos en el filtro y los SDT pasan: ST = SST + SDT, así que SDT se obtiene por diferencia.
# - Las unidades importan: la masa en gramos y el volumen en mililitros dan mg/L con el factor 1e6.
