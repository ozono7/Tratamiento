# =============================================
# DISEÑO DE PLANTA DE TRATAMIENTO DE AGUAS RESIDUALES (PTAR)
# =============================================
# Este script calcula y diseña los componentes principales de una PTAR para 5,000 habitantes
# incluyendo pretratamiento, tratamiento primario/secundario, manejo de lodos y desinfección.
# Basado en normativas NOM-001-SEMARNAT-2021 y estándares EPA.
# =============================================

# -----------------------------------
# 1. INSTALACIÓN Y CARGA DE PAQUETES
# -----------------------------------
# Paquetes necesarios para análisis, visualización y modelado
if (!require("tidyverse")) install.packages("tidyverse")  # Manipulación de datos
if (!require("ggplot2")) install.packages("ggplot2")      # Gráficos avanzados
if (!require("DiagrammeR")) install.packages("DiagrammeR")# Diagramas de flujo
if (!require("deSolve")) install.packages("deSolve")      # Ecuaciones diferenciales (modelos biológicos)
if (!require("gt")) install.packages("gt")                # Tablas formateadas profesionalmente
if (!require("scales")) install.packages("scales")        # Escalas para gráficos

library(tidyverse)   # Carga dplyr, ggplot2, tidyr, etc.
library(ggplot2)     # Visualización de datos
library(DiagrammeR)  # Creación de diagramas de flujo
library(deSolve)     # Resolución de ecuaciones diferenciales
library(gt)          # Creación de tablas con formato
library(scales)      # Funciones para escalas en gráficos

# -----------------------------------
# 2. DATOS DE ENTRADA BÁSICOS
# -----------------------------------
# Definición de parámetros base para el diseño de la PTAR
datos_entrada <- list(
  poblacion = 5000,       # Número de habitantes servidos
  caudal_Ls = 15,         # Caudal promedio en litros/segundo
  caudal_m3d = 1300,      # Caudal equivalente en m³/día (15 L/s * 86400 s/d / 1000 L/m³)
  
  # Características del agua residual (valores típicos para aguas domésticas)
  calidad_agua = list(
    DBO = 250,           # Demanda Bioquímica de Oxígeno (mg/L) - Indicador de materia orgánica
    DQO = 400,           # Demanda Química de Oxígeno (mg/L) - Versión química de DBO
    SST = 150,           # Sólidos Suspendidos Totales (mg/L) - Materia en suspensión
    coliformes = 1e6,    # NMP/100mL - Indicador de contaminación fecal
    N_total = 45,        # Nitrógeno total (mg/L) - Importante para eutrofización
    P_total = 12         # Fósforo total (mg/L) - Nutriente clave para crecimiento algal
  )
)

# -----------------------------------
# 3. FUNCIONES DE DISEÑO
# -----------------------------------
# Cada función calcula dimensiones y parámetros operativos para un componente específico de la PTAR

# Función para calcular dimensiones de rejas gruesas
# Objetivo: Remover sólidos gruesos (ramas, plásticos) que podrían dañar equipos
calcular_rejas <- function(Q) {
  # Parámetros de diseño (basados en Metcalf & Eddy y normativas):
  ancho_min <- 0.6       # Ancho mínimo del canal (m) - Permite mantenimiento
  velocidad <- 0.6       # Velocidad óptima (m/s) - Evita sedimentación (mín 0.3) y arrastre (máx 1.0)
  espaciamiento <- 0.05  # Separación entre barras (m = 50 mm) - Típico para rejas gruesas
  
  # Cálculo del área requerida (Q = V*A → A = Q/V)
  Q_m3s <- Q / 1000      # Conversión L/s → m³/s
  area <- Q_m3s / velocidad
  
  # Ajustar dimensiones del canal:
  profundidad <- area / ancho_min
  
  # Validar profundidad dentro de rangos típicos (0.3-1.0 m):
  if (profundidad < 0.3) profundidad <- 0.3  # Mínimo para evitar turbulencia
  if (profundidad > 1.0) {
    profundidad <- 1.0    # Máximo para facilitar limpieza manual
    ancho <- area / profundidad  # Ajustar ancho si profundidad supera límite
  } else {
    ancho <- ancho_min    # Usar ancho mínimo si profundidad está en rango
  }
  
  return(list(
    ancho = round(ancho, 2),              # Ancho del canal (m)
    profundidad = round(profundidad, 2),  # Profundidad del agua (m)
    espaciamiento = espaciamiento * 1000, # Convertir a mm para especificaciones
    velocidad = velocidad,                # Velocidad del flujo (m/s)
    perdida_carga = 0.15                  # Pérdida de carga estimada (m) - Para diseño de bombeo
  ))
}

# Función para calcular desarenador
# Objetivo: Remover arena y partículas minerales que erosionan equipos
calcular_desarenador <- function(Q) {
  # Parámetros de diseño:
  tiempo_ret <- 60        # Tiempo de retención (segundos) - Suficiente para sedimentar arena
  velocidad <- 0.3        # Velocidad horizontal (m/s) - Crítica para eficiencia
  profundidad <- 1.2      # Profundidad útil (m) - Permite acumulación de arena
  
  # Volumen requerido (V = Q * t):
  volumen <- Q * tiempo_ret / 1000  # m³
  
  # Sección transversal (A = Q/V):
  area_transversal <- (Q / 1000) / velocidad
  ancho <- area_transversal / profundidad
  
  # Longitud del canal (L = V * t):
  longitud <- velocidad * tiempo_ret
  
  return(list(
    volumen = round(volumen, 2),      # Volumen total (m³)
    longitud = round(longitud, 2),    # Longitud del desarenador (m)
    ancho = round(ancho, 2),          # Ancho del canal (m)
    profundidad = profundidad,        # Profundidad útil (m)
    velocidad = velocidad,            # Velocidad de flujo (m/s)
    tiempo_ret = tiempo_ret / 60      # Tiempo retención en minutos
  ))
}

# Función para sedimentador primario
# Objetivo: Remover sólidos sedimentables por gravedad (reduce ~60% SST)
calcular_sed_primario <- function(Q, carga_superficial = 1.2) {
  # Parámetros:
  tiempo_ret <- 2         # Tiempo retención (horas) - Típico 1.5-2.5 h
  profundidad <- 3.5      # Profundidad útil (m) - Para zona de lodos y clarificación
  
  # Convertir caudal a m³/h:
  Q_m3h <- Q * 3.6  # 15 L/s * 3.6 = 54 m³/h
  
  # Área superficial (A = Q/Carga_superficial):
  area <- Q_m3h / carga_superficial  # m²
  
  # Dimensiones para tanque rectangular (relación largo/ancho = 2:1):
  ancho <- sqrt(area / 2)  # Cálculo basado en relación de aspecto
  largo <- 2 * ancho       # Flujo longitudinal mejora eficiencia
  
  # Volumen total (V = A * h):
  volumen <- area * profundidad
  
  return(list(
    tipo = "Rectangular",            # Configuración más común
    area = round(area, 2),           # Área superficial (m²)
    largo = round(largo, 2),         # Longitud del tanque (m)
    ancho = round(ancho, 2),         # Ancho del tanque (m)
    profundidad = profundidad,       # Profundidad útil (m)
    volumen = round(volumen, 2),     # Volumen total (m³)
    carga_superficial = carga_superficial,  # m³/m²/h - Crítico para diseño
    tiempo_ret = tiempo_ret          # Horas
  ))
}

# Función para reactor biológico (lodos activados)
# Objetivo: Degradación de materia orgánica (DBO) mediante microorganismos
calcular_reactor <- function(Q, DBO, F_M = 0.3, MLSS = 2500, edad_lodos = 10) {
  # Parámetros clave:
  # F_M: Relación alimento/microorganismo (kgDBO/kgMLSS·d) - Controla crecimiento bacteriano
  # MLSS: Mixed Liquor Suspended Solids (mg/L) - Concentración de biomasa
  # edad_lodos: Tiempo promedio que los microorganismos permanecen en el sistema (días)
  
  # Convertir caudal a m³/d:
  Q_m3d <- Q * 86.4  # 15 L/s * 86,400 s/d / 1,000 L/m³ = 1,296 m³/d
  
  # Volumen del reactor (V = Q*DBO / (F_M*MLSS)):
  volumen <- (Q_m3d * DBO) / (F_M * MLSS)  # m³
  
  # Dimensiones sugeridas (relación 3:2:1 para mezcla óptima):
  profundidad <- 4  # Profundidad típica para sistemas de aireación
  ancho <- round(sqrt(volumen / (profundidad * 1.5)), 1)  # Cálculo geométrico
  largo <- round(1.5 * ancho, 1)
  
  # Volumen ajustado a dimensiones reales:
  volumen_ajustado <- largo * ancho * profundidad
  
  return(list(
    volumen = round(volumen_ajustado, 1),  # m³
    largo = largo,                         # m
    ancho = ancho,                         # m
    profundidad = profundidad,             # m
    MLSS = MLSS,                           # mg/L
    edad_lodos = edad_lodos,               # días
    relacion_FM = F_M                      # kgDBO/kgMLSS·d
  ))
}

# Función para sedimentador secundario
# Objetivo: Separar los lodos biológicos del efluente tratado
calcular_sed_secundario <- function(Q, SST, carga_solida = 4) {
  # Parámetros:
  # carga_solida: kgSS/m²/h - Determina el área necesaria para sedimentación
  
  # Convertir caudal a m³/h:
  Q_m3h <- Q * 3.6  # 15 L/s * 3.6 = 54 m³/h
  
  # Calcular carga másica (M = Q * SST):
  carga_masica_kgd <- Q_m3h * 24 * SST / 1000  # kgSS/d
  
  # Calcular área requerida (A = Q*SST / carga_solida):
  area <- (Q_m3h * SST) / (carga_solida * 1000)
  
  # Diámetro para tanque circular (A = πr² → D = 2*sqrt(A/π)):
  diametro <- round(sqrt(area * 4 / pi), 1)
  
  # Profundidad típica para clarificadores:
  profundidad <- 3.8  # m (incluye zonas de lodos y clarificación)
  
  # Volumen total:
  volumen <- area * profundidad
  
  return(list(
    tipo = "Circular",            # Configuración más común para secundarios
    diametro = diametro,          # Diámetro del tanque (m)
    area = round(area, 2),        # Área superficial (m²)
    profundidad = profundidad,    # Profundidad útil (m)
    volumen = round(volumen, 2),  # Volumen total (m³)
    carga_solida = carga_solida   # kgSS/m²/h
  ))
}

# Función para sistema de aireación
# Objetivo: Proporcionar oxígeno para el crecimiento de microorganismos
calcular_aireacion <- function(Q, DBO, eficiencia = 0.08) {
  # Parámetros:
  # eficiencia: kgO2/kWh - Eficiencia de transferencia de oxígeno (típico 0.08-0.12)
  
  # Requerimiento teórico de oxígeno (1.5 kgO2/kgDBO):
  RO <- 1.5 * Q * 86.4 * DBO / 1000  # kgO2/d
  
  # Potencia requerida (P = RO / (24 * eficiencia)):
  potencia_kW <- RO / (24 * eficiencia)
  
  # Caballos de fuerza del motor (1 kW ≈ 1.341 hp):
  hp <- potencia_kW / 0.75 * 1.341  # Considerando 75% eficiencia del motor
  
  # Caudal de aire (asumiendo 0.23 kgO2/m³ de aire):
  caudal_aire_m3h <- RO / (0.23 * 24)
  
  return(list(
    requerimiento_oxigeno = round(RO, 2),  # kgO2/d
    potencia = round(potencia_kW, 2),      # kW
    hp = round(hp, 1),                    # hp
    caudal_aire = round(caudal_aire_m3h, 2),  # m³/h
    eficiencia = eficiencia * 100          # %
  ))
}

# Función para cloración
# Objetivo: Desinfección del efluente para cumplir con normativas microbiológicas
calcular_cloracion <- function(Q, dosis = 8, tiempo_contacto = 30) {
  # Parámetros:
  # dosis: mg/L - Depende de calidad del efluente y normativas
  # tiempo_contacto: minutos - Crítico para efectividad de desinfección
  
  # Volumen de contacto (V = Q * t):
  volumen <- Q * tiempo_contacto * 60 / 1000  # m³
  
  # Consumo diario de cloro (C = Q * dosis):
  consumo_cloro <- Q * 86.4 * dosis / 1000  # kg/d
  
  return(list(
    volumen = round(volumen, 2),      # m³
    dosis = dosis,                    # mg/L
    tiempo_contacto = tiempo_contacto, # minutos
    consumo_cloro = round(consumo_cloro, 2)  # kg/d
  ))
}

# Función para manejo de lodos
# Objetivo: Tratamiento y disposición de lodos generados en el proceso
calcular_lodos <- function(Q, SST, DBO, eficiencia_primario = 0.6, eficiencia_secundario = 0.95) {
  # Parámetros:
  # eficiencia_primario: % remoción de SST en primario
  # eficiencia_secundario: % remoción de DBO en secundario
  
  # Convertir caudal a m³/d:
  Q_m3d <- Q * 86.4  # m³/d
  
  # Lodos primarios (60% remoción SST):
  lodos_primarios <- Q_m3d * SST * eficiencia_primario / 1000  # kgSS/d
  
  # Lodos secundarios (0.5 kgSS/kgDBO removida):
  dbo_removida <- DBO * (1 - (1 - eficiencia_primario) * (1 - eficiencia_secundario))
  lodos_secundarios <- 0.5 * Q_m3d * dbo_removida / 1000  # kgSS/d
  
  # Lodos totales:
  lodos_totales <- lodos_primarios + lodos_secundarios
  
  # Diseño del espesador (carga sólida 50 kg/m²/d):
  area_espesador <- lodos_totales / 50  # m²
  diametro_espesador <- round(sqrt(area_espesador * 4 / pi), 1)
  
  # Diseño del digestor (carga orgánica 1.5 kgDBO/m³/d):
  dbo_lodos <- lodos_primarios * 0.5 + lodos_secundarios * 0.4  # Estimación DBO en lodos
  volumen_digestor <- dbo_lodos / 1.5  # m³
  tiempo_ret_digestor <- 25  # días (óptimo para digestión anaerobia)
  
  # Ajustar volumen para cumplir tiempo de retención:
  volumen_digestor_ajustado <- max(volumen_digestor, lodos_totales * tiempo_ret_digestor / 30)
  
  return(list(
    lodos_primarios = round(lodos_primarios, 2),  # kgSS/d
    lodos_secundarios = round(lodos_secundarios, 2),  # kgSS/d
    lodos_totales = round(lodos_totales, 2),      # kgSS/d
    espesador = list(
      diametro = diametro_espesador,              # m
      area = round(area_espesador, 2),            # m²
      carga_solida = 50                           # kg/m²/d
    ),
    digestor = list(
      volumen = round(volumen_digestor_ajustado, 2),  # m³
      tiempo_ret = tiempo_ret_digestor,           # días
      carga_organica = 1.5                        # kgDBO/m³/d
    ),
    deshidratacion = list(
      capacidad = round(lodos_totales / 24, 2),   # kgSS/h
      polimero = round(lodos_totales * 5 / 1000, 2)  # kg/d (dosis típica 5 g/kgSS)
    )
  ))
}

# -----------------------------------
# 4. CÁLCULOS PRINCIPALES
# -----------------------------------
# Aplicación de las funciones de diseño a los datos de entrada

# Pretratamiento (rejas y desarenador)
rejas <- calcular_rejas(datos_entrada$caudal_Ls)
desarenador <- calcular_desarenador(datos_entrada$caudal_Ls)

# Tratamiento primario (sedimentación)
sed_primario <- calcular_sed_primario(datos_entrada$caudal_Ls)
DBO_primario <- datos_entrada$calidad_agua$DBO * (1 - 0.35)  # 35% remoción de DBO
SST_primario <- datos_entrada$calidad_agua$SST * (1 - 0.6)    # 60% remoción de SST

# Tratamiento secundario (lodos activados)
reactor <- calcular_reactor(datos_entrada$caudal_Ls, DBO_primario)
sed_secundario <- calcular_sed_secundario(datos_entrada$caudal_Ls, SST_primario)

# Sistema de aireación (para lodos activados)
aireacion <- calcular_aireacion(datos_entrada$caudal_Ls, DBO_primario)

# Desinfección (cloración)
cloracion <- calcular_cloracion(datos_entrada$caudal_Ls)

# Manejo de lodos (espesamiento, digestión, deshidratación)
lodos <- calcular_lodos(datos_entrada$caudal_Ls, 
                        datos_entrada$calidad_agua$SST, 
                        datos_entrada$calidad_agua$DBO)

# Resultados de tratamiento (calidad del agua por etapa)
resultados_tratamiento <- data.frame(
  Etapa = c("Entrada", "Primario", "Secundario", "Límite"),
  DBO = c(datos_entrada$calidad_agua$DBO, 
          DBO_primario, 
          DBO_primario * (1 - 0.95),  # 95% remoción en secundario
          30),                         # Límite normativo
  SST = c(datos_entrada$calidad_agua$SST,
          SST_primario,
          SST_primario * (1 - 0.9),    # 90% remoción en secundario
          30),                         # Límite normativo
  Coliformes = c(datos_entrada$calidad_agua$coliformes,
                 datos_entrada$calidad_agua$coliformes * 0.8,  # 20% remoción en primario
                 datos_entrada$calidad_agua$coliformes * 0.8 * 0.001,  # 99.9% remoción en secundario
                 1000)                        # Límite normativo
)

# -----------------------------------
# 5. VISUALIZACIÓN DE RESULTADOS
# -----------------------------------

# Función para crear tablas de diseño con formato profesional
crear_tabla_diseño <- function(datos, titulo) {
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

# Gráfico de calidad de agua por etapa
datos_grafico <- resultados_tratamiento %>%
  mutate(Etapa = factor(Etapa, levels = c("Entrada", "Primario", "Secundario", "Límite")))

# Gráfico de evolución de DBO
g_dbo <- ggplot(datos_grafico, aes(x = Etapa, y = DBO, group = 1)) +
  geom_line(color = "red", linewidth = 1.5) +
  geom_point(size = 3, color = "red") +
  geom_hline(yintercept = 30, linetype = "dashed", color = "darkred") +
  labs(title = "Evolución de DBO a través del tratamiento",
       subtitle = "Límite normativo = 30 mg/L (línea roja discontinua)",
       y = "DBO (mg/L)") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))

# Gráfico de evolución de SST
g_sst <- ggplot(datos_grafico, aes(x = Etapa, y = SST, group = 1)) +
  geom_line(color = "blue", linewidth = 1.5) +
  geom_point(size = 3, color = "blue") +
  geom_hline(yintercept = 30, linetype = "dashed", color = "darkblue") +
  labs(title = "Evolución de SST a través del tratamiento",
       subtitle = "Límite normativo = 30 mg/L (línea azul discontinua)",
       y = "SST (mg/L)") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))

# Gráfico de evolución de coliformes (escala logarítmica)
g_coliformes <- ggplot(datos_grafico, aes(x = Etapa, y = Coliformes, group = 1)) +
  geom_line(color = "darkgreen", linewidth = 1.5) +
  geom_point(size = 3, color = "darkgreen") +
  scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
  geom_hline(yintercept = 1000, linetype = "dashed", color = "darkgreen") +
  labs(title = "Evolución de Coliformes Fecales",
       subtitle = "Límite normativo = 1,000 MPN/100mL (línea verde discontinua)",
       y = "MPN/100mL (escala logarítmica)") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))

# -----------------------------------
# 6. DIAGRAMA DE FLUJO
# -----------------------------------
# Diagrama esquemático del proceso de tratamiento
diagrama_ptar <- grViz("
  digraph PTAR {
    graph [layout = dot, rankdir = LR, nodesep = 0.5, ranksep = 0.8]
    
    node [shape = rectangle, style = filled, fillcolor = lightblue, fontname = Arial, fontsize = 12]
    influente [label = 'Agua Residual\\nQ = 15 L/s\\nDBO = 250 mg/L\\nSST = 150 mg/L']
    rejas [label = 'Rejas Gruesas\\nEspaciamiento: 50 mm']
    desarenador [label = 'Desarenador\\nV = 3.6 m³\\nTR = 1 min']
    sed_primario [label = 'Sed. Primario\\n50 m²\\nTR = 2 h']
    reactor [label = 'Reactor Biológico\\n432 m³\\nMLSS = 2,500 mg/L']
    sed_secundario [label = 'Sed. Secundario\\nØ = 8 m\\nTR = 2.5 h']
    clorador [label = 'Cloración\\nDosis: 8 mg/L\\nTC = 30 min']
    efluente [label = 'Efluente Tratado\\nDBO < 30 mg/L\\nSST < 30 mg/L']
    
    node [shape = rectangle, fillcolor = lightgrey, fontsize = 11]
    lodos_primarios [label = 'Lodos Primarios\\n60% SST removidos']
    espesador [label = 'Espesador\\nØ = 5 m\\nTR = 12 h']
    digestor [label = 'Digestor\\n60 m³\\nTR = 25 días']
    deshidratacion [label = 'Deshidratación\\nFiltros banda\\n75-80% humedad']
    lodos_exceso [label = 'Lodos en Exceso\\n30% del caudal']
    retorno [label = 'Retorno de Lodos\\n70% del caudal']
    
    // Flujo principal
    influente -> rejas -> desarenador -> sed_primario -> reactor -> sed_secundario -> clorador -> efluente
    
    // Manejo de lodos primarios
    sed_primario -> lodos_primarios -> espesador -> digestor -> deshidratacion
    
    // Manejo de lodos secundarios
    sed_secundario -> retorno [label = '70%', fontsize = 10]
    sed_secundario -> lodos_exceso [label = '30%', fontsize = 10]
    retorno -> reactor [arrowhead = none]
    
    // Jerarquía visual
    {rank = same; influente rejas desarenador}
    {rank = same; sed_primario reactor sed_secundario}
    {rank = same; clorador efluente}
    {rank = same; espesador digestor deshidratacion}
  }
")

# -----------------------------------
# 7. REPORTE FINAL
# -----------------------------------
# Impresión de resultados en consola
cat("\n=== DISEÑO COMPLETO DE PTAR PARA 5,000 HABITANTES ===\n")

# 1. Datos de entrada
cat("\n1. DATOS DE ENTRADA:\n")
print(data.frame(
  Parámetro = c("Población", "Caudal", "DBO", "DQO", "SST", "Coliformes", "N Total", "P Total"),
  Valor = c("5,000 hab", "15 L/s (1,300 m³/d)", "250 mg/L", "400 mg/L", "150 mg/L", 
            "1,000,000 MPN/100mL", "45 mg/L", "12 mg/L")
))

# 2. Resultados de tratamiento
cat("\n2. RESULTADOS DE TRATAMIENTO:\n")
print(resultados_tratamiento)

# 3. Producción de lodos
cat("\n3. PRODUCCIÓN DE LODOS:\n")
print(data.frame(
  Tipo = c("Primarios", "Secundarios", "Total"),
  kgSS_día = c(lodos$lodos_primarios, lodos$lodos_secundarios, lodos$lodos_totales),
  Ton_mes = c(lodos$lodos_primarios * 30 / 1000, 
              lodos$lodos_secundarios * 30 / 1000, 
              lodos$lodos_totales * 30 / 1000)
))

# 4. Consumo de energía
cat("\n4. CONSUMO DE ENERGÍA:\n")
print(data.frame(
  Componente = c("Soplante aireación", "Bombas recirculación", "Mezcladores", "Total"),
  Potencia = c(paste(aireacion$potencia, "kW"), 
               "5 kW", 
               "3 kW", 
               paste(aireacion$potencia + 5 + 3, "kW")),
  Consumo_diario = c(paste(aireacion$potencia * 24, "kWh/d"), 
                     "120 kWh/d", 
                     "72 kWh/d", 
                     paste(aireacion$potencia * 24 + 120 + 72, "kWh/d"))
))

# Mostrar gráficos
cat("\n=== GRÁFICOS DE RESULTADOS ===\n")
print(g_dbo)
print(g_sst)
print(g_coliformes)

# Mostrar diagrama de flujo
cat("\n=== DIAGRAMA DE FLUJO DE LA PTAR ===\n")
print(diagrama_ptar)

# Mostrar tablas de diseño detallado
cat("\n=== TABLAS DE DISEÑO DETALLADO ===\n")

# Pretratamiento
crear_tabla_diseño(data.frame(
  Parámetro = c("Ancho", "Profundidad", "Espaciamiento", "Velocidad", "Pérdida de carga"),
  Valor = c(paste(rejas$ancho, "m"), paste(rejas$profundidad, "m"), 
            paste(rejas$espaciamiento, "mm"), paste(rejas$velocidad, "m/s"), 
            paste(rejas$perdida_carga, "m"))
), "Diseño de Rejas Gruesas")

crear_tabla_diseño(data.frame(
  Parámetro = c("Volumen", "Longitud", "Ancho", "Profundidad", "Velocidad", "TR"),
  Valor = c(paste(desarenador$volumen, "m³"), paste(desarenador$longitud, "m"),
            paste(desarenador$ancho, "m"), paste(desarenador$profundidad, "m"),
            paste(desarenador$velocidad, "m/s"), paste(desarenador$tiempo_ret, "min"))
), "Diseño de Desarenador")

# Tratamiento primario
crear_tabla_diseño(data.frame(
  Parámetro = c("Tipo", "Largo", "Ancho", "Profundidad", "Área", "Volumen", "Carga superficial", "TR"),
  Valor = c(sed_primario$tipo, paste(sed_primario$largo, "m"), paste(sed_primario$ancho, "m"),
            paste(sed_primario$profundidad, "m"), paste(sed_primario$area, "m²"),
            paste(sed_primario$volumen, "m³"), paste(sed_primario$carga_superficial, "m³/m²/h"),
            paste(sed_primario$tiempo_ret, "h"))
), "Diseño de Sedimentador Primario")

# Tratamiento secundario
crear_tabla_diseño(data.frame(
  Parámetro = c("Volumen", "Largo", "Ancho", "Profundidad", "MLSS", "Edad de lodos", "Relación F/M"),
  Valor = c(paste(reactor$volumen, "m³"), paste(reactor$largo, "m"), paste(reactor$ancho, "m"),
            paste(reactor$profundidad, "m"), paste(reactor$MLSS, "mg/L"),
            paste(reactor$edad_lodos, "días"), paste(reactor$relacion_FM, "kgDBO/kgMLSS·d"))
), "Diseño de Reactor Biológico")

crear_tabla_diseño(data.frame(
  Parámetro = c("Tipo", "Diámetro", "Área", "Profundidad", "Volumen", "Carga sólida"),
  Valor = c(sed_secundario$tipo, paste(sed_secundario$diametro, "m"), 
            paste(sed_secundario$area, "m²"), paste(sed_secundario$profundidad, "m"),
            paste(sed_secundario$volumen, "m³"), paste(sed_secundario$carga_solida, "kg/m²/h"))
), "Diseño de Sedimentador Secundario")

# Sistema de aireación
crear_tabla_diseño(data.frame(
  Parámetro = c("Requerimiento O₂", "Potencia", "Motor", "Caudal aire", "Eficiencia"),
  Valor = c(paste(aireacion$requerimiento_oxigeno, "kgO₂/día"), 
            paste(aireacion$potencia, "kW"), 
            paste(aireacion$hp, "hp"),
            paste(aireacion$caudal_aire, "m³/h"),
            paste(aireacion$eficiencia, "%"))
), "Diseño de Sistema de Aireación")

# Manejo de lodos
crear_tabla_diseño(data.frame(
  Parámetro = c("Diámetro", "Área", "Carga sólida", "TR"),
  Valor = c(paste(lodos$espesador$diametro, "m"), paste(lodos$espesador$area, "m²"),
            paste(lodos$espesador$carga_solida, "kg/m²/d"), "12 h")
), "Diseño de Espesador de Lodos")

crear_tabla_diseño(data.frame(
  Parámetro = c("Volumen", "TR", "Carga orgánica", "Producción biogás"),
  Valor = c(paste(lodos$digestor$volumen, "m³"), paste(lodos$digestor$tiempo_ret, "días"),
            paste(lodos$digestor$carga_organica, "kgDBO/m³·d"), "0.35 m³/kgDBO")
), "Diseño de Digestor Anaerobio")

crear_tabla_diseño(data.frame(
  Parámetro = c("Tipo", "Capacidad", "Polímero", "Humedad final"),
  Valor = c("Filtros banda", paste(lodos$deshidratacion$capacidad, "kgSS/h"),
            paste(lodos$deshidratacion$polimero, "kg/d"), "75-80%")
), "Diseño de Sistema de Deshidratación")

# Desinfección
crear_tabla_diseño(data.frame(
  Parámetro = c("Volumen", "Dosis", "Tiempo contacto", "Consumo cloro"),
  Valor = c(paste(cloracion$volumen, "m³"), paste(cloracion$dosis, "mg/L"),
            paste(cloracion$tiempo_contacto, "min"), paste(cloracion$consumo_cloro, "kg/d"))
), "Diseño de Sistema de Cloración")

