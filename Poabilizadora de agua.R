# =============================================
# DISEÑO DE PLANTA POTABILIZADORA DE AGUA
# =============================================
# Sistema convencional para 20,000 habitantes
# Proceso: Coagulación-Floculación-Sedimentación-Filtración-Desinfección
# =============================================

# -----------------------------------
# 1. INSTALACIÓN DE PAQUETES
# -----------------------------------
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("DiagrammeR")) install.packages("DiagrammeR")
if (!require("gt")) install.packages("gt")

library(tidyverse)
library(DiagrammeR)
library(gt)

# -----------------------------------
# 2. DATOS DE ENTRADA
# -----------------------------------
datos_agua <- list(
  poblacion = 20000,
  consumo = 200, # L/hab/día
  calidad_agua = list(
    turbiedad = list(
      promedio = 100, # NTU
      maximo = 500 # NTU
    ),
    color = 45, # UC
    pH = 7.0,
    alcalinidad = 40, # mg/L CaCO3
    coliformes = 5000 # UFC/100mL
  ),
  normativas = list(
    turbiedad = 1, # NTU
    color = 15, # UC
    coliformes = 0 # UFC/100mL
  )
)

# -----------------------------------
# 3. CÁLCULO DE CAUDALES
# -----------------------------------
calcular_caudales <- function(poblacion, consumo) {
  q_medio <- poblacion * consumo / 1000 # m³/día
  q_max_diario <- q_medio * 1.3
  q_max_horario <- q_medio * 1.8
  
  return(list(
    medio = round(q_medio, 2),
    max_diario = round(q_max_diario, 2),
    max_horario = round(q_max_horario, 2),
    medio_Ls = round(q_medio * 1000 / 86400, 2),
    max_diario_Ls = round(q_max_diario * 1000 / 86400, 2),
    max_horario_Ls = round(q_max_horario * 1000 / 86400, 2)
  ))
}

caudales <- calcular_caudales(datos_agua$poblacion, datos_agua$consumo)

# -----------------------------------
# 4. FUNCIONES DE DISEÑO
# -----------------------------------

# 4.1 Diseño de rejas
disenar_rejas <- function(Q) {
  # Q en L/s
  ancho_min <- 0.8 # m
  velocidad <- 0.6 # m/s
  espaciamiento <- 0.025 # m (25 mm)
  
  # Cálculo del área
  area <- (Q / 1000) / velocidad
  
  # Ajuste de dimensiones
  profundidad <- area / ancho_min
  if (profundidad < 0.5) profundidad <- 0.5
  if (profundidad > 1.2) {
    profundidad <- 1.2
    ancho <- area / profundidad
  } else {
    ancho <- ancho_min
  }
  
  return(list(
    tipo = "Rejas gruesas manuales",
    ancho = round(ancho, 2),
    profundidad = round(profundidad, 2),
    espaciamiento = espaciamiento * 1000, # mm
    velocidad = velocidad,
    perdida_carga = 0.10 # m
  ))
}

# 4.2 Diseño de desarenador
disenar_desarenador <- function(Q) {
  # Q en L/s
  tiempo_ret <- 60 # segundos
  velocidad <- 0.3 # m/s
  profundidad <- 1.5 # m
  
  # Volumen requerido
  volumen <- Q * tiempo_ret / 1000 # m³
  
  # Sección transversal
  area_transversal <- (Q / 1000) / velocidad
  ancho <- area_transversal / profundidad
  
  # Longitud
  longitud <- velocidad * tiempo_ret
  
  return(list(
    tipo = "Desarenador horizontal",
    volumen = round(volumen, 2),
    longitud = round(longitud, 2),
    ancho = round(ancho, 2),
    profundidad = profundidad,
    velocidad = velocidad
  ))
}

# 4.3 Diseño de mezcla rápida
disenar_mezcla_rapida <- function(Q) {
  # Q en L/s
  tiempo_ret <- 30 # segundos
  gradiente_velocidad <- 900 # s⁻¹
  
  # Volumen del tanque
  volumen <- Q * tiempo_ret / 1000 # m³
  
  # Dimensiones (tanque cuadrado)
  lado <- round(volumen^(1/3), 2)
  profundidad <- lado * 1.2 # Mayor profundidad
  
  # Potencia requerida
  mu <- 1.002e-3 # Pa·s (20°C)
  potencia <- mu * gradiente_velocidad^2 * volumen
  
  return(list(
    tipo = "Mezclador rápido hidráulico",
    volumen = round(volumen, 2),
    lado = round(lado, 2),
    profundidad = round(profundidad, 2),
    tiempo_ret = tiempo_ret,
    gradiente = gradiente_velocidad,
    potencia = round(potencia, 2) # W
  ))
}

# 4.4 Diseño de floculador
disenar_floculador <- function(Q) {
  # Q en L/s
  tiempo_ret <- 20 * 60 # 20 minutos
  gradientes <- c(50, 30, 20) # s⁻¹ (3 etapas)
  
  # Volumen total
  volumen <- Q * tiempo_ret / 1000 # m³
  
  # Configuración de canales (3 etapas)
  n_canales <- 3
  volumen_etapa <- volumen / n_canales
  
  # Dimensiones (asumiendo profundidad de 3 m)
  profundidad <- 3.0
  area_etapa <- volumen_etapa / profundidad
  lado <- round(sqrt(area_etapa), 2)
  
  # Potencias requeridas
  mu <- 1.002e-3 # Pa·s
  potencias <- mu * gradientes^2 * volumen_etapa
  
  return(list(
    tipo = "Floculador hidráulico de 3 etapas",
    volumen_total = round(volumen, 2),
    etapas = n_canales,
    volumen_etapa = round(volumen_etapa, 2),
    lado = round(lado, 2),
    profundidad = profundidad,
    gradientes = gradientes,
    potencias = round(potencias, 2) # W
  ))
}

# 4.5 Diseño de sedimentador
disenar_sedimentador <- function(Q, turbiedad_max) {
  # Q en L/s
  carga_superficial <- ifelse(turbiedad_max > 300, 0.7, 1.2) # m³/m²/h
  
  # Convertir caudal
  Q_m3h <- Q * 3.6
  
  # Área superficial
  area <- Q_m3h / carga_superficial
  
  # Configuración rectangular (2 unidades)
  n_unidades <- 2
  ancho <- 4.0 # m (estándar)
  largo <- (area / n_unidades) / ancho
  
  # Profundidad
  profundidad <- 3.5 # m
  
  # Volumen total
  volumen <- area * profundidad
  
  # Tiempo de retención
  tiempo_ret <- volumen / (Q_m3h / n_unidades)
  
  return(list(
    tipo = "Sedimentadores rectangulares",
    n_unidades = n_unidades,
    area_total = round(area, 2),
    area_unidad = round(area/n_unidades, 2),
    largo = round(largo, 2),
    ancho = ancho,
    profundidad = profundidad,
    volumen_total = round(volumen, 2),
    carga_superficial = carga_superficial,
    tiempo_ret = round(tiempo_ret, 2) # horas
  ))
}

# 4.6 Diseño de filtros rápidos
disenar_filtros <- function(Q) {
  # Q en L/s
  tasa_filtracion <- 5 # m³/m²/h
  
  # Convertir caudal
  Q_m3h <- Q * 3.6
  
  # Área total de filtración
  area_total <- Q_m3h / tasa_filtracion
  
  # Configurar 4 filtros
  n_filtros <- 4
  area_filtro <- area_total / n_filtros
  
  # Dimensiones (cuadrados)
  lado <- round(sqrt(area_filtro), 2)
  
  # Medios filtrantes
  medios <- data.frame(
    capa = c("Antracita", "Arena", "Grava"),
    espesor = c(0.5, 0.3, 0.2), # m
    tamaño_efectivo = c(1.0, 0.5, NA), # mm
    coeficiente_uniformidad = c(1.5, 1.3, NA)
  )
  
  return(list(
    tipo = "Filtros rápidos de tasa constante",
    n_filtros = n_filtros,
    area_total = round(area_total, 2),
    area_filtro = round(area_filtro, 2),
    lado = lado,
    tasa_filtracion = tasa_filtracion,
    medios = medios
  ))
}

# 4.7 Diseño de desinfección
disenar_cloracion <- function(Q) {
  # Q en L/s
  dosis_cloro <- 2.0 # mg/L
  tiempo_contacto <- 30 # minutos
  
  # Volumen de contacto
  volumen <- Q * tiempo_contacto * 60 / 1000 # m³
  
  # Consumo diario de cloro
  consumo <- Q * 86.4 * dosis_cloro / 1000 # kg/d
  
  return(list(
    tipo = "Cloración por gas",
    volumen_contacto = round(volumen, 2),
    dosis = dosis_cloro,
    tiempo_contacto = tiempo_contacto,
    consumo_diario = round(consumo, 2)
  ))
}

# 4.8 Diseño de tanque de agua tratada
disenar_tanque_almacenamiento <- function(Qmedio) {
  # Qmedio en m³/día
  capacidad <- 0.3 * Qmedio # 30% del caudal medio
  
  # Configuración circular
  profundidad <- 4.0 # m
  diametro <- round(sqrt(capacidad / (profundidad * pi/4)), 2)
  
  return(list(
    tipo = "Tanque de almacenamiento circular",
    capacidad = round(capacidad, 2),
    diametro = diametro,
    profundidad = profundidad
  ))
}

# -----------------------------------
# 5. CÁLCULOS PRINCIPALES
# -----------------------------------

# Pretratamiento
rejas <- disenar_rejas(caudales$max_horario_Ls)
desarenador <- disenar_desarenador(caudales$max_horario_Ls)

# Coagulación-Floculación
mezcla <- disenar_mezcla_rapida(caudales$max_diario_Ls)
floculador <- disenar_floculador(caudales$max_diario_Ls)

# Sedimentación
sedimentador <- disenar_sedimentador(caudales$max_diario_Ls, 
                                     datos_agua$calidad_agua$turbiedad$maximo)

# Filtración
filtros <- disenar_filtros(caudales$max_diario_Ls)

# Desinfección
cloracion <- disenar_cloracion(caudales$max_diario_Ls)

# Almacenamiento
tanque_agua <- disenar_tanque_almacenamiento(caudales$medio)

# -----------------------------------
# 6. VISUALIZACIÓN DE RESULTADOS
# -----------------------------------

# Función para tablas profesionales
crear_tabla_potabilizadora <- function(datos, titulo) {
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

# Diagrama de flujo
diagrama_planta <- grViz("
  digraph Potabilizadora {
    graph [layout = dot, rankdir = LR, nodesep = 0.5, ranksep = 0.8]
    
    node [shape = rectangle, style = filled, fillcolor = lightblue, fontname = Arial, fontsize = 12]
    captacion [label = 'Captación\\nrío']
    rejas [label = 'Rejas gruesas']
    desarenador [label = 'Desarenador']
    mezcla [label = 'Mezcla rápida\\n+ coagulante']
    floculador [label = 'Floculador']
    sedimentador [label = 'Sedimentadores']
    filtros [label = 'Filtros rápidos']
    cloracion [label = 'Cloración']
    tanque [label = 'Tanque\\nalmacenamiento']
    red [label = 'Red de\\ndistribución']
    
    // Flujo principal
    captacion -> rejas -> desarenador -> mezcla -> floculador -> sedimentador -> filtros -> cloracion -> tanque -> red
    
    // Jerarquía visual
    {rank = same; captacion rejas desarenador}
    {rank = same; mezcla floculador sedimentador}
    {rank = same; filtros cloracion tanque red}
  }
")

# -----------------------------------
# 7. REPORTE FINAL
# -----------------------------------
cat("\n=== DISEÑO DE PLANTA POTABILIZADORA ===\n")
cat(paste("Población atendida:", datos_agua$poblacion, "habitantes\n"))
cat(paste("Caudal medio:", caudales$medio, "m³/día (", caudales$medio_Ls, "L/s)\n"))
cat(paste("Caudal máximo diario:", caudales$max_diario_Ls, "L/s\n"))
cat(paste("Caudal máximo horario:", caudales$max_horario_Ls, "L/s\n\n"))

# Tablas de diseño
cat("\n=== COMPONENTES PRINCIPALES ===\n")

# Pretratamiento
crear_tabla_potabilizadora(
  data.frame(
    Componente = c("Rejas gruesas", "Desarenador"),
    Dimensiones = c(
      paste(rejas$ancho, "m ×", rejas$profundidad, "m"),
      paste(desarenador$longitud, "m ×", desarenador$ancho, "m ×", desarenador$profundidad, "m")
    ),
    Características = c(
      paste("Espaciamiento:", rejas$espaciamiento, "mm"),
      paste("TR:", desarenador$tiempo_ret, "min")
    )
  ), "Pretratamiento"
)

# Coagulación-Floculación
crear_tabla_potabilizadora(
  data.frame(
    Componente = c("Mezcla rápida", "Floculador"),
    Dimensiones = c(
      paste(mezcla$lado, "m ×", mezcla$lado, "m ×", mezcla$profundidad, "m"),
      paste("3 etapas de", floculador$lado, "m ×", floculador$lado, "m ×", floculador$profundidad, "m")
    ),
    Parámetros = c(
      paste("G:", mezcla$gradiente, "s⁻¹"),
      paste("G:", paste(floculador$gradientes, collapse = ", "), "s⁻¹ por etapa")
    )
  ), "Coagulación-Floculación"
)

# Sedimentación-Filtración
crear_tabla_potabilizadora(
  data.frame(
    Componente = c("Sedimentadores", "Filtros rápidos"),
    Cantidad = c(
      sedimentador$n_unidades,
      filtros$n_filtros
    ),
    Dimensiones = c(
      paste(sedimentador$largo, "m ×", sedimentador$ancho, "m ×", sedimentador$profundidad, "m"),
      paste(filtros$lado, "m ×", filtros$lado, "m")
    ),
    Carga = c(
      paste(sedimentador$carga_superficial, "m³/m²/h"),
      paste(filtros$tasa_filtracion, "m³/m²/h")
    )
  ), "Sedimentación y Filtración"
)

# Desinfección-Almacenamiento
crear_tabla_potabilizadora(
  data.frame(
    Componente = c("Cloración", "Tanque almacenamiento"),
    Parámetros = c(
      paste("Dosis:", cloracion$dosis, "mg/L, Contacto:", cloracion$tiempo_contacto, "min"),
      paste("Capacidad:", tanque_agua$capacidad, "m³, Ø:", tanque_agua$diametro, "m")
    )
  ), "Desinfección y Almacenamiento"
)

# Mostrar diagrama de flujo
print(diagrama_planta)
