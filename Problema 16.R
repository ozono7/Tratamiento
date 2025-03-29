# -----------------------------------------------------------
# Autor: José Juan Torres Martínez  
# Doctorado en Ciencias en Bioprocesos  
# Laboratorio de Biotecnología Molecular de Células Vegetales y Bioinformática  
# -----------------------------------------------------------

# Tema: Dosificación y Coste de PAC en Tratamiento de Agua  
# Objetivos: Caudal de PAC y coste de tratamiento  

# Datos de entrada  
densidad <- 1.23                # g/cm³  
riqueza <- 0.30                 # 30%  
dosis_optima <- 12              # mg/L (PAC puro)  
caudal_agua <- 9000             # m³/día  
coste_producto <- 150           # €/m³  

# 1. Calcular concentración de PAC activo en producto comercial (g/L)  
masa_PAC_comercial <- densidad * 1000  # 1230 g/L  
masa_PAC_activo <- masa_PAC_comercial * riqueza  # 369 g/L  

# 2. Masa total de PAC activo requerida por día (convertir mg a g)  
masa_PAC_dia <- (dosis_optima / 1000) * caudal_agua  # 108,000 g/día  

# 3. Volumen de producto comercial necesario (L/día)  
vol_PAC_comercial <- masa_PAC_dia / masa_PAC_activo  # 292.68 L/día  

# 4. Coste diario y por m³  
coste_diario <- (vol_PAC_comercial / 1000) * coste_producto  # 43.90 €/día  
coste_m3 <- coste_diario / caudal_agua  # 0.0049 €/m³  

# Resultados  
cat("----------------------------------------\n")  
cat("CÁLCULO DE DOSIFICACIÓN Y COSTE DE PAC\n")  
cat("----------------------------------------\n")  
cat("Densidad del PAC comercial:", densidad, "g/cm³\n")  
cat("Riqueza en PAC activo:", riqueza * 100, "%\n")  
cat("Dosis óptima (jar-test):", dosis_optima, "mg/L\n")  
cat("Caudal de agua a tratar:", caudal_agua, "m³/día\n\n")  
cat("a) Caudal de PAC comercial requerido:", round(vol_PAC_comercial, 2), "L/día\n")  
cat("b) Coste del tratamiento:\n")  
cat("   - Coste diario:", round(coste_diario, 2), "€/día\n")  
cat("   - Coste por m³:", round(coste_m3, 4), "€/m³\n")  
cat("----------------------------------------\n")  

# Conversión a unidades prácticas  
cat("\nEquivalencias prácticas:\n")  
cat("- Caudal de PAC:", round(vol_PAC_comercial / 24, 2), "L/hora\n")  
cat("- Coste anual:", round(coste_diario * 365, 2), "€/año\n")  

