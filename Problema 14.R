# -----------------------------------------------------------
# Autor: José Juan Torres Martínez  
# Doctorado en Ciencias en Bioprocesos  
# Laboratorio de Biotecnología Molecular de Células Vegetales y Bioinformática  
# -----------------------------------------------------------

# Tema: Preparación de Disolución de Policloruro de Aluminio  
# Objetivo: Calcular volumen de producto comercial necesario  

# Datos de entrada  
densidad <- 1.15                # g/cm³  
riqueza <- 0.25                 # 25%  
vol_disolución <- 0.5           # 500 mL = 0.5 L  
concentración_deseada <- 15     # g/L (de PAC activo)  

# 1. Calcular concentración de PAC activo en producto comercial (g/L)  
masa_PAC_comercial <- densidad * 1000  # g/L (1 L = 1000 cm³)  
masa_PAC_activo <- masa_PAC_comercial * riqueza  # 287.5 g/L  

# 2. Masa de PAC activo necesaria para 500 mL  
masa_PAC_necesaria <- concentración_deseada * vol_disolución  # 7.5 g  

# 3. Volumen de producto comercial a añadir (en mL)  
vol_producto_comercial <- (masa_PAC_necesaria / masa_PAC_activo) * 1000  # 26.09 mL  

# Resultados  
cat("----------------------------------------\n")  
cat("PREPARACIÓN DE DISOLUCIÓN DE PAC\n")  
cat("----------------------------------------\n")  
cat("Densidad del producto comercial:", densidad, "g/cm³\n")  
cat("Riqueza en PAC activo:", riqueza * 100, "%\n")  
cat("Volumen de disolución a preparar:", vol_disolución * 1000, "mL\n")  
cat("Concentración deseada de PAC activo:", concentración_deseada, "g/L\n\n")  
cat("Cálculos:\n")  
cat("1. Masa de PAC activo en producto comercial:", round(masa_PAC_activo, 1), "g/L\n")  
cat("2. Masa de PAC activo necesaria:", masa_PAC_necesaria, "g\n")  
cat("3. Volumen de producto comercial a añadir:", round(vol_producto_comercial, 1), "mL\n")  
cat("----------------------------------------\n")  

# Instrucciones de preparación  
cat("\nPASOS PARA PREPARAR LA DISOLUCIÓN:\n")  
cat("1. Medir", round(vol_producto_comercial, 1), "mL del producto comercial.\n")  
cat("2. Verter en un matraz aforado de 500 mL.\n")  
cat("3. Añadir agua destilada hasta la marca de 500 mL.\n")  
cat("4. Agitar hasta homogeneizar.\n")  

