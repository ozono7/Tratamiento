# Problema 3: Cálculo del coste total del tratamiento adicional de agua
# Autor: José Juan Torres Martínez
# Doctorado en Ciencias en Bioprocesos
# Laboratorio de Biotecnología Molecular de Células Vegetales y Bioinformática
# Fecha: 2023-10-30

# ---------------------- Definir constantes y variables ----------------------
concentracion_glucosa <- 2.6e-3  # mol/L de glucosa
relacion_O2_glucosa <- 6         # moles de O₂ por mol de glucosa
peso_molecular_O2 <- 32          # g/mol de O₂
DBO5_permitida <- 25             # mg/L de DBO₅ permitida
caudal_vertido <- 15             # m³/h de caudal
duracion_vertido <- 2            # h de duración del vertido
coste_tratamiento <- 0.1         # $/g O₂ eliminado

# ---------------------- Cálculo de la DBO₅ inicial generada ----------------------
DBO5_inicial <- concentracion_glucosa * relacion_O2_glucosa * peso_molecular_O2 * 1000  # mg/L

# ---------------------- Cálculo de la DBO₅ a eliminar ----------------------
DBO5_eliminar <- max(DBO5_inicial - DBO5_permitida, 0)  # mg/L (se usa max() para evitar valores negativos)

# ---------------------- Cálculo de la cantidad total de O₂ a eliminar ----------------------
volumen_vertido <- caudal_vertido * duracion_vertido * 1000  # L
DBO5_total_eliminar <- DBO5_eliminar * volumen_vertido  # mg
DBO5_total_eliminar_g <- DBO5_total_eliminar / 1000  # g

# ---------------------- Cálculo del coste total del tratamiento ----------------------
coste_total <- DBO5_total_eliminar_g * coste_tratamiento  # €

# ---------------------- Mostrar resultados ----------------------
cat("\n---- RESULTADOS ----\n")
cat("DBO₅ inicial generada por el vertido: ", round(DBO5_inicial, 2), "mg/L\n")
cat("DBO₅ que se debe eliminar: ", round(DBO5_eliminar, 2), "mg/L\n")
cat("Cantidad total de O₂ que se debe eliminar: ", round(DBO5_total_eliminar_g, 2), "g\n")
cat("Coste total del tratamiento: ", round(coste_total, 2), "$\n")

                  
                  