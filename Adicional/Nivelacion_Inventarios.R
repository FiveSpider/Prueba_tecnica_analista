# TRANSFERENCIAS BASADAS EN DÍAS DE INVENTARIO

library(readxl)
library(dplyr)

# Parámetros
archivo <- "C:/Users/juanl/Downloads/Compras 2025.08.09 1.1.xlsx"
rango <- "B9:CO19689"
dias_objetivo <- 14
k <- 1.5
fecha <- format(Sys.Date(), "%Y%m%d")

# Definición de columnas por sucursal
sucursales <- c("Polanco", "Tecamachalco", "Interlomas", "CMD")

inventario_cols <- c(
  Polanco = "Stock_Polanco",
  Tecamachalco = "Stock_Teca",
  Interlomas = "Stock_Inter",
  CMD = "Stock_CMD"
)

prom_dia_cols <- c(
  Polanco = "PromDiario_Polanco",
  Tecamachalco = "PromDiario_Teca",
  Interlomas = "PromDiario_Inter",
  CMD = "PromDiario_CMD"
)

desviacion_cols <- c(
  Polanco = "SD_Polanco",
  Tecamachalco = "SD_Tecamachalco",
  Interlomas = "SD_Interlomas",
  CMD = "SD_CMD"
)

# Leer archivo
df <- read_excel(archivo, range = rango)

df <- df %>%
  filter(Status == "ACTIVO")


clasificar_productos <- function(df, inventario_cols, prom_dia_cols,
                                 umbral_min = 15, umbral_max = 30) {
  
  clasificaciones <- list()
  
  for (i in 1:nrow(df)) {
    prod <- df[i, ]
    
    inv <- sapply(inventario_cols, function(col) prod[[col]])
    prom <- sapply(prom_dia_cols, function(col) prod[[col]])
    dias_inv <- inv / prom
    
    if (any(is.na(dias_inv) | is.infinite(dias_inv))) next
    
    num_bajo <- sum(dias_inv < umbral_min)
    num_alto <- sum(dias_inv > umbral_max)
    
    clasificacion <- case_when(
      num_bajo >= 3 ~ "Falta crónica",
      num_alto >= 3 ~ "Exceso crónico",
      TRUE ~ "Balanceado"
    )
    
    clasificaciones[[length(clasificaciones) + 1]] <- data.frame(
      Codigo = prod$codigo,
      Producto = prod$producto,
      Categoria = prod$Categoria,
      Proveedor = prod$Proveedor,
      Dias_Polanco = dias_inv["Polanco"],
      Dias_Tecamachalco = dias_inv["Tecamachalco"],
      Dias_Interlomas = dias_inv["Interlomas"],
      Dias_CMD = dias_inv["CMD"],
      Clasificacion = clasificacion
    )
  }
  
  return(bind_rows(clasificaciones))
}

tabla_clasificada <- clasificar_productos(df, inventario_cols, prom_dia_cols)
write.csv(tabla_clasificada, paste0("clasificacion_productos_", fecha, ".csv"), row.names = FALSE)


# Inicializar listas
transferencias <- list()
logs <- list()

# Iterar por producto
for (i in 1:nrow(df)) {
  prod <- df[i, ]
  
  # Extraer inventario, promedio y desviación por sucursal
  inv <- sapply(inventario_cols, function(col) prod[[col]])
  prom <- sapply(prom_dia_cols, function(col) prod[[col]])
  desv <- sapply(desviacion_cols, function(col) prod[[col]])
  
  dias_inv <- inv / prom
  
  base_log <- data.frame(
    Codigo = prod$codigo,
    Producto = prod$producto,
    Categoria = prod$Categoria,
    Proveedor = prod$Proveedor,
    Dias_Polanco = dias_inv["Polanco"],
    Dias_Tecamachalco = dias_inv["Tecamachalco"],
    Dias_Interlomas = dias_inv["Interlomas"],
    Dias_CMD = dias_inv["CMD"]
  )
  
  # Validar datos válidos
  if (any(is.na(dias_inv) | is.infinite(dias_inv))) {
    logs[[length(logs) + 1]] <- cbind(base_log, Motivo = "Datos faltantes")
    next
  }
  
  # Determinar stock objetivo por sucursal
  stock_objetivo <- prom * dias_objetivo + k * desv
  
  # Detectar excedentes y faltantes
  donantes <- which(inv > stock_objetivo)
  receptoras <- which(inv < stock_objetivo)
  
  if (length(donantes) == 0 || length(receptoras) == 0) {
    logs[[length(logs) + 1]] <- cbind(base_log, Motivo = "Sin faltante o excedente")
    next
  }
  
  # Generar transferencias
  hubo_transferencia <- FALSE
  
  for (d in donantes) {
    for (r in receptoras) {
      exceso_piezas <- inv[d] - stock_objetivo[d]
      faltante_piezas <- stock_objetivo[r] - inv[r]
      cantidad_transferir <- round(min(exceso_piezas, faltante_piezas))
      
      if (cantidad_transferir > 0) {
        transferencias[[length(transferencias) + 1]] <- data.frame(
          Codigo = prod$codigo,
          Producto = prod$producto,
          Categoria = prod$Categoria,
          Proveedor = prod$Proveedor,
          De = names(inv)[d],
          A = names(inv)[r],
          Cantidad_sugerida = cantidad_transferir,
          Prom_Dia_De = prom[d],
          Prom_Dia_A = prom[r],
          SD_De = desv[d],
          SD_A = desv[r],
          Dias_Inv_De = dias_inv[d],
          Dias_Inv_A = dias_inv[r]
        )
        hubo_transferencia <- TRUE
      }
    }
  }
  
  motivo <- if (hubo_transferencia) "Transferencia sugerida" else "Sin cantidad suficiente para transferir"
  logs[[length(logs) + 1]] <- cbind(base_log, Motivo = motivo)
}

# Exportar archivos

# Maestro
transferencias_df <- bind_rows(transferencias)
write.csv(transferencias_df, paste0("transferencias_dias_inv_", fecha, ".csv"), row.names = FALSE)

# Logs
log_df <- bind_rows(logs)
write.csv(log_df, paste0("log_dias_inv_", fecha, ".csv"), row.names = FALSE)

# Por sucursal emisora (columna "De")
for (suc in sucursales) {
  archivo_suc <- transferencias_df %>% filter(De == suc)
  
  if (nrow(archivo_suc) > 0) {
    nombre_archivo <- paste0("transferencias_", suc, "_emisora_", fecha, ".csv")
    write.csv(archivo_suc, nombre_archivo, row.names = FALSE)
    cat("Archivo guardado para emisora", suc, "→", nombre_archivo, "\n")
  } else {
    cat("No hay transferencias desde", suc, "\n")
  }
}


detectar_cronicos <- function(df, inventario_cols, prom_dia_cols, desviacion_cols,
                              umbral_min = 15, umbral_max = 30) {
  exceso_cronico <- list()
  falta_cronica <- list()
  
  for (i in 1:nrow(df)) {
    prod <- df[i, ]
    
    inv <- sapply(inventario_cols, function(col) prod[[col]])
    prom <- sapply(prom_dia_cols, function(col) prod[[col]])
    dias_inv <- inv / prom
    
    if (any(is.na(dias_inv) | is.infinite(dias_inv))) next
    
    # Contar sucursales con exceso o falta
    num_exceso <- sum(dias_inv > umbral_max)
    num_falta <- sum(dias_inv < umbral_min)
    
    if (num_exceso >= 1) {  # Ajuste del umbral
      exceso_cronico[[length(exceso_cronico) + 1]] <- data.frame(
        Codigo = prod$codigo,
        Producto = prod$producto,
        Categoria = prod$Categoria,
        Proveedor = prod$Proveedor,
        Dias_Inv = t(dias_inv)
      )
    }
    
    if (num_falta >= 1) {
      falta_cronica[[length(falta_cronica) + 1]] <- data.frame(
        Codigo = prod$codigo,
        Producto = prod$producto,
        Categoria = prod$Categoria,
        Proveedor = prod$Proveedor,
        Dias_Inv = t(dias_inv)
      )
    }
  }
  
  list(
    exceso_cronico = bind_rows(exceso_cronico),
    falta_cronica = bind_rows(falta_cronica)
  )
}

resultados_cronicos <- detectar_cronicos(
  df,
  inventario_cols = inventario_cols,
  prom_dia_cols = prom_dia_cols,
  desviacion_cols = desviacion_cols,
  umbral_min = 15,
  umbral_max = 30
)

write.csv(resultados_cronicos$exceso_cronico, paste0("productos_exceso_cronico_", fecha, ".csv"), row.names = FALSE)
write.csv(resultados_cronicos$falta_cronica, paste0("productos_falta_cronica_", fecha, ".csv"), row.names = FALSE)
