# ============================================
# DISEÑO DE PTAR PARA INDUSTRIA LÁCTEA
# ============================================
# Sistema especializado para tratamiento de:
# - Alta carga orgánica (DBO 1,500 mg/L)
# - Grasas y aceites (300 mg/L)
# - Sólidos suspendidos (800 mg/L)
# ============================================

# -----------------------------------
# 1. INSTALACIÓN DE PAQUETES
# -----------------------------------
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("DiagrammeR")) install.packages("DiagrammeR")
if (!require("gt")) install.packages("gt")

library(tidyverse)
library(ggplot2)
library(DiagrammeR)
library(gt)

# -----------------------------------
# 2. DATOS DE ENTRADA INDUSTRIALES
# -----------------------------------
datos_industriales <- list(
  proceso = "Procesamiento de lácteos",
  capacidad = "500,000 L leche/día",
  caudal = list(
    promedio = 2.89,    # L/s
    maximo = 5,         # L/s
    diario = 250        # m³/día
  ),
  calidad_agua = list(
    DBO = 1500,         # mg/L
    DQO = 2500,         # mg/L
    SST = 800,          # mg/L
    Grasas = 300,       # mg/L
    N_total = 100,      # mg/L
    P_total = 40,       # mg/L
    pH = 5.0            # Unidad de pH
  ),
  normativas = list(
    DBO = 50,           # mg/L
    SST = 30,           # mg/L
    Grasas = 10,        # mg/L
    N_total = 15,       # mg/L
    P_total = 2         # mg/L
  )
)

# -----------------------------------
# 3. FUNCIONES DE DISEÑO INDUSTRIAL
# -----------------------------------

# Función para trampa de grasas (diseño industrial)
calcular_trampa_grasas <- function(Q, concentracion_grasas) {
  # Parámetros de diseño para industria láctea:
  tiempo_ret <- 45 * 60  # 45 minutos a segundos
  velocidad_asc <- 0.03  # m/min (velocidad ascenso grasas)
  profundidad <- 2.0     # m
  
  # Volumen requerido (V = Q*t)
  volumen <- Q * tiempo_ret / 1000  # m³
  
  # Área superficial (A = Q/Vasc)
  area <- (Q / 1000) / (velocidad_asc / 60)  # m²
  
  # Dimensiones para rectangular (relación 3:1)
  ancho <- sqrt(area / 3)
  largo <- 3 * ancho
  
  # Eficiencia esperada (>90% para Q promedio)
  eficiencia <- 0.95 - (0.0005 * concentracion_grasas)
  
  return(list(
    tipo = "Trampa de grasas rectangular",
    volumen = round(volumen, 2),
    largo = round(largo, 2),
    ancho = round(ancho, 2),
    profundidad = profundidad,
    tiempo_ret = tiempo_ret / 60,  # min
    eficiencia = round(eficiencia * 100, 1)  # %
  ))
}

# Función para sistema DAF (Flotación por Aire Disuelto)
calcular_daf <- function(Q, SST, Grasas) {
  # Parámetros de diseño:
  carga_superficial <- 5.0  # m³/m²/h (típico para lácteos)
  presion <- 600            # kPa
  razon_aire_solidos <- 0.05 # kg aire/kg sólidos
  
  # Convertir caudal
  Q_m3h <- Q * 3.6  # m³/h
  
  # Área requerida
  area <- Q_m3h / carga_superficial
  
  # Volumen de contacto
  volumen <- area * 2.5  # Asumiendo 2.5 m de profundidad
  
  # Requerimiento de aire
  solidos_kgd <- Q * 86.4 * (SST + Grasas) / 1000
  aire_kgd <- solidos_kgd * razon_aire_solidos
  
  return(list(
    tipo = "DAF industrial",
    area = round(area, 2),
    volumen = round(volumen, 2),
    presion = presion,
    aire_requerido = round(aire_kgd, 2),
    eficiencia = list(
      SST = 85,  # %
      Grasas = 95 # %
    )
  ))
}

# Función para reactor SBR (Sequential Batch Reactor)
calcular_sbr <- function(Q, DBO, N_total, ciclos_dia = 6) {
  # Parámetros de diseño para aguas lácteas:
  carga_volumetrica <- 1.2  # kgDBO/m³/d
  MLSS <- 4000              # mg/L (mayor que municipal)
  tiempo_ciclo <- 24 / ciclos_dia  # horas
  
  # Volumen requerido (V = Q*DBO/carga)
  Q_m3d <- Q * 86.4
  volumen <- (Q_m3d * DBO / 1000) / carga_volumetrica
  
  # Dividir en 2 reactores (mínimo para operación alternada)
  volumen_por_reactor <- volumen / 2
  
  # Dimensiones para reactor circular:
  diametro <- round(sqrt(volumen_por_reactor / 4 * 4 / pi), 1)  # Relación h/D = 1
  
  # Requerimiento de oxígeno (considerando nitrificación)
  RO <- (1.5 * (DBO / 1000) + 4.6 * (N_total / 1000)) * Q_m3d
  
  return(list(
    tipo = "SBR para lácteos",
    n_reactores = 2,
    volumen_total = round(volumen, 2),
    volumen_reactor = round(volumen_por_reactor, 2),
    diametro = diametro,
    profundidad = diametro,  # Relación 1:1
    MLSS = MLSS,
    ciclos_dia = ciclos_dia,
    tiempo_ciclo = tiempo_ciclo,
    requerimiento_oxigeno = round(RO, 2)  # kgO2/d
  ))
}

# Función para tratamiento de lodos industriales
calcular_lodos_lacteos <- function(Q, SST, Grasas, DBO) {
  # Lodos primarios (DAF)
  lodos_primarios <- Q * 86.4 * (SST * 0.85 + Grasas * 0.95) / 1000  # kg/d
  
  # Lodos secundarios (SBR)
  lodos_secundarios <- 0.6 * Q * 86.4 * (DBO * 0.95) / 1000  # kg/d
  
  # Lodos totales con 4% sólidos
  lodos_totales <- lodos_primarios + lodos_secundarios
  volumen_lodos <- lodos_totales / (0.04 * 1000)  # m³/d (4% sólidos)
  
  # Espesamiento por gravedad
  area_espesador <- lodos_totales / 25  # 25 kg/m²/d
  
  # Digestión anaerobia (alta producción de biogás)
  volumen_digestor <- lodos_totales / 2.5  # 2.5 kgVS/m³/d
  produccion_biogas <- lodos_totales * 0.8  # m³/d (0.8 m³/kgDQO)
  
  return(list(
    lodos_primarios = round(lodos_primarios, 2),
    lodos_secundarios = round(lodos_secundarios, 2),
    lodos_totales = round(lodos_totales, 2),
    volumen_lodos = round(volumen_lodos, 2),
    espesador = list(
      area = round(area_espesador, 2),
      carga = 25
    ),
    digestor = list(
      volumen = round(volumen_digestor, 2),
      carga_organica = 2.5,
      produccion_biogas = round(produccion_biogas, 2)
    ),
    deshidratacion = list(
      capacidad = round(lodos_totales / 24, 2),  # kg/h
      polimero = round(lodos_totales * 8 / 1000, 2)  # kg/d (mayor dosis para lodos grasos)
    )
  ))
}

# -----------------------------------
# 4. CÁLCULOS PRINCIPALES
# -----------------------------------

# Pretratamiento
trampa_grasas <- calcular_trampa_grasas(datos_industriales$caudal$promedio, 
                                        datos_industriales$calidad_agua$Grasas)

# Corrección de pH (neutralización)
dosificacion_cal <- (250000 * (7 - 5) * 0.02)  # L/día * ΔpH * factor

# Tratamiento primario
daf <- calcular_daf(datos_industriales$caudal$promedio,
                    datos_industriales$calidad_agua$SST,
                    datos_industriales$calidad_agua$Grasas)

# Calidad después de DAF
DBO_daf <- datos_industriales$calidad_agua$DBO * (1 - 0.35)
SST_daf <- datos_industriales$calidad_agua$SST * (1 - 0.85)
Grasas_daf <- datos_industriales$calidad_agua$Grasas * (1 - 0.95)

# Tratamiento secundario
sbr <- calcular_sbr(datos_industriales$caudal$promedio,
                    DBO_daf,
                    datos_industriales$calidad_agua$N_total)

# Tratamiento terciario (filtración rápida)
area_filtracion <- (datos_industriales$caudal$diario / 24) / 5  # 5 m³/m²/h

# Manejo de lodos
lodos <- calcular_lodos_lacteos(datos_industriales$caudal$promedio,
                                datos_industriales$calidad_agua$SST,
                                datos_industriales$calidad_agua$Grasas,
                                datos_industriales$calidad_agua$DBO)

# Resultados de tratamiento
resultados_tratamiento <- data.frame(
  Etapa = c("Entrada", "DAF", "SBR", "Filtración", "Límite"),
  DBO = c(datos_industriales$calidad_agua$DBO,
          DBO_daf,
          DBO_daf * (1 - 0.95),
          DBO_daf * (1 - 0.95) * 0.9,
          datos_industriales$normativas$DBO),
  SST = c(datos_industriales$calidad_agua$SST,
          SST_daf,
          SST_daf * (1 - 0.95),
          SST_daf * (1 - 0.95) * 0.8,
          datos_industriales$normativas$SST),
  Grasas = c(datos_industriales$calidad_agua$Grasas,
             Grasas_daf,
             Grasas_daf * (1 - 0.98),
             Grasas_daf * (1 - 0.98) * 0.9,
             datos_industriales$normativas$Grasas)
)

# -----------------------------------
# 5. VISUALIZACIÓN DE RESULTADOS
# -----------------------------------

# Función para tablas profesionales
crear_tabla_industrial <- function(datos, titulo) {
  datos %>%
    gt() %>%
    tab_header(title = titulo) %>%
    fmt_number(columns = where(is.numeric), decimals = 2) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
    ) %>%
    tab_options(
      table.width = pct(90),
      column_labels.font.size = 12,
      table.font.size = 11
    )
}

# Gráficos de evolución
datos_grafico <- resultados_tratamiento %>%
  mutate(Etapa = factor(Etapa, levels = c("Entrada", "DAF", "SBR", "Filtración", "Límite")))

g_dbo_industrial <- ggplot(datos_grafico, aes(x = Etapa, y = DBO, group = 1)) +
  geom_line(color = "red", linewidth = 1.5) +
  geom_point(size = 3, color = "red") +
  geom_hline(yintercept = datos_industriales$normativas$DBO, linetype = "dashed", color = "darkred") +
  labs(title = "Evolución de DBO en PTAR Láctea",
       subtitle = paste("Límite normativo =", datos_industriales$normativas$DBO, "mg/L"),
       y = "DBO (mg/L)") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))

g_grasas_industrial <- ggplot(datos_grafico, aes(x = Etapa, y = Grasas, group = 1)) +
  geom_line(color = "orange", linewidth = 1.5) +
  geom_point(size = 3, color = "orange") +
  geom_hline(yintercept = datos_industriales$normativas$Grasas, linetype = "dashed", color = "darkorange") +
  labs(title = "Evolución de Grasas en PTAR Láctea",
       subtitle = paste("Límite normativo =", datos_industriales$normativas$Grasas, "mg/L"),
       y = "Grasas y Aceites (mg/L)") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))

# -----------------------------------
# 6. DIAGRAMA DE FLUJO INDUSTRIAL
# -----------------------------------
diagrama_ptar_industrial <- grViz("
  digraph PTAR_Industrial {
    graph [layout = dot, rankdir = LR, nodesep = 0.5, ranksep = 0.8]
    
    node [shape = rectangle, style = filled, fillcolor = lightblue, fontname = Arial, fontsize = 12]
    influente [label = 'Agua Residual Láctea\\nDBO: 1,500 mg/L\\nGrasas: 300 mg/L\\nSST: 800 mg/L']
    neutralizacion [label = 'Neutralización\\nDosificación de cal\\nAjuste a pH 7']
    trampa_grasas [label = 'Trampa de Grasas\\nRemoción 95% grasas']
    daf [label = 'DAF\\nFlotación por Aire\\nRemoción 85% SST']
    sbr [label = 'Reactores SBR (2)\\nCarga: 1.2 kgDBO/m³/d\\nMLSS: 4,000 mg/L']
    filtracion [label = 'Filtración Rápida\\n5 m³/m²/h']
    uv [label = 'Desinfección UV\\nDosis: 30 mJ/cm²']
    efluente [label = 'Efluente Tratado\\nCumple normativas']
    
    node [shape = rectangle, fillcolor = lightgrey, fontsize = 11]
    lodos_daf [label = 'Lodos Primarios\\n4% sólidos']
    espesador [label = 'Espesador\\n25 kg/m²/d']
    digestor [label = 'Digestor Anaerobio\\nProducción biogás\\n0.8 m³/kgDQO']
    deshidratacion [label = 'Centrífuga\\n+ Polímero\\n25% sólidos']
    
    // Flujo principal
    influente -> neutralizacion -> trampa_grasas -> daf -> sbr -> filtracion -> uv -> efluente
    
    // Manejo de lodos
    daf -> lodos_daf -> espesador -> digestor -> deshidratacion
    sbr -> espesador [label = 'Lodos Secundarios']
    
    // Jerarquía visual
    {rank = same; influente neutralizacion trampa_grasas}
    {rank = same; daf sbr}
    {rank = same; filtracion uv efluente}
    {rank = same; espesador digestor deshidratacion}
  }
")

# -----------------------------------
# 7. REPORTE FINAL INDUSTRIAL
# -----------------------------------
cat("\n=== DISEÑO DE PTAR PARA INDUSTRIA LÁCTEA ===\n")
cat(paste("Capacidad:", datos_industriales$capacidad, "\n"))
cat(paste("Caudal promedio:", datos_industriales$caudal$promedio, "L/s\n"))
cat(paste("Caudal diario:", datos_industriales$caudal$diario, "m³/d\n\n"))

# Tablas de diseño
cat("\n=== COMPONENTES PRINCIPALES ===\n")

crear_tabla_industrial(data.frame(
  Componente = c("Trampa de grasas", "Sistema DAF", "Reactores SBR", "Filtración", "UV"),
  "Tamaño/Configuración" = c(
    paste(trampa_grasas$largo, "m x", trampa_grasas$ancho, "m"),
    paste(daf$area, "m²,", daf$presion, "kPa"),
    paste(sbr$n_reactores, "reactores de", sbr$volumen_reactor, "m³"),
    paste(round(area_filtracion, 2), "m² @ 5 m³/m²/h"),
    "Sistema de 30 mJ/cm²"
  ),
  Eficiencia = c(
    paste(trampa_grasas$eficiencia, "% grasas"),
    "85-95% SST/Grasas",
    "95% DBO, 90% N",
    "10-20% remoción adicional",
    "99.9% patógenos"
  )
), "Resumen de Componentes")

# Resultados de tratamiento
cat("\n=== RESULTADOS DE TRATAMIENTO ===\n")
print(resultados_tratamiento)

# Producción de lodos y biogás
cat("\n=== PRODUCCIÓN DE LODOS Y BIOGÁS ===\n")
print(data.frame(
  "Parámetro" = c("Lodos totales", "Volumen lodos", "Biogás producido", "Polímero requerido"),
  "Valor" = c(
    paste(lodos$lodos_totales, "kg/d"),
    paste(lodos$volumen_lodos, "m³/d"),
    paste(lodos$digestor$produccion_biogas, "m³/d"),
    paste(lodos$deshidratacion$polimero, "kg/d")
  )
))

# Mostrar gráficos
print(g_dbo_industrial)
print(g_grasas_industrial)

# Mostrar diagrama de flujo
print(diagrama_ptar_industrial)

