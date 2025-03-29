# -----------------------------------------------------------
# Autor: José Juan Torres Martínez  
# Doctorado en Ciencias en Bioprocesos  
# Laboratorio de Biotecnología Molecular de Células Vegetales y Bioinformática  
# -----------------------------------------------------------

# Tema: Cálculo de Dosis de Coagulante en Tratamiento de Agua  
# Objetivo: Determinar dosis práctica en g/m³  

# Datos de entrada  
densidad <- 1.335              # g/cm³  
riqueza <- 0.48                # 48%  
dosis_volumetrica <- 0.29      # L/min  
caudal <- 850                  # m³/h  

# 1. Convertir densidad a g/L  
densidad_g_L <- densidad * 1000  # 1335 g/L  

# 2. Calcular masa de producto activo por litro de coagulante  
masa_activa_g_L <- densidad_g_L * riqueza  # 640.8 g/L  

# 3. Convertir dosis volumétrica a L/h  
dosis_L_h <- dosis_volumetrica * 60  # 17.4 L/h  

# 4. Calcular masa total de producto activo por hora  
masa_activa_g_h <- masa_activa_g_L * dosis_L_h  # 11149.92 g/h  

# 5. Calcular dosis práctica en g/m³  
dosis_g_m3 <- masa_activa_g_h / caudal  # 13.12 g/m³  

# Resultados  
cat("----------------------------------------\n")  
cat("CÁLCULO DE DOSIS DE COAGULANTE\n")  
cat("----------------------------------------\n")  
cat("Densidad del sulfato de aluminio:", densidad, "g/cm³\n")  
cat("Riqueza en producto activo:", riqueza * 100, "%\n")  
cat("Dosificación volumétrica:", dosis_volumetrica, "L/min\n")  
cat("Caudal de agua a tratar:", caudal, "m³/h\n\n")  
cat("Cálculos intermedios:\n")  
cat("1. Densidad en g/L:", densidad_g_L, "g/L\n")  
cat("2. Masa de producto activo por litro:", round(masa_activa_g_L, 1), "g/L\n")  
cat("3. Dosis volumétrica en L/h:", dosis_L_h, "L/h\n")  
cat("4. Masa de producto activo por hora:", round(masa_activa_g_h, 2), "g/h\n\n")  
cat("RESULTADO FINAL:\n")  
cat("Dosis práctica a dosificar:", round(dosis_g_m3, 2), "g/m³\n")  
cat("----------------------------------------\n")  

# Verificación de rangos típicos  
if (dosis_g_m3 >= 5 && dosis_g_m3 <= 20) {  
  cat("✅ La dosis está dentro del rango típico para coagulación (5-20 g/m³)\n")  
} else if (dosis_g_m3 < 5) {  
  cat("⚠️ Dosis demasiado baja. Puede ser insuficiente para la coagulación.\n")  
} else {  
  cat("⚠️ Dosis demasiado alta. Riesgo de sobredosificación y aumento de lodos.\n")  
}  
