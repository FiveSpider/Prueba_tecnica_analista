# -------------------------------------------------
# Librerías


library(readxl)
library(dplyr)
library(stringr)
library(stringi)
library(tidyr)
library(stringdist)

quitar_acentos <- function(texto){
  texto %>%
    stringi::stri_trans_nfd() %>%
    str_replace_all("\\p{Mn}", "") %>%
    stringi::stri_trans_nfc()
}


# 1 Leer la base de datos

ruta <- "C:/Users/juanl/Downloads/Base para prueba (1) (1).xlsx"
datos <- read_excel(ruta)

# 2 Limpieza inicial

datos <- datos %>%
  mutate(
    
    # limpiar correos
    Email = str_to_lower(str_trim(Email)),
    Email = str_replace_all(Email, "[^a-z0-9ñ@._-]", ""),
    
    # reemplazar NA en asistencias por 0
    across(starts_with("asistencia"), ~replace_na(.x, 0))
    
  )

# 3 Crear participantes únicos (usando el correo)


participantes <- datos %>%
  group_by(Email) %>%
  summarise(
    
    Nombre = first(Nombre),
    `Primer apellido` = first(`Primer apellido`),
    `Segundo apellido` = first(`Segundo apellido`),
    
    # consolidar asistencia
    across(starts_with("asistencia"), max),
    
    .groups = "drop"
    
  )


# 4 Crear nombre completo limpio

participantes <- participantes %>%
  mutate(
    
    nombre_completo = paste(
      Nombre,
      `Primer apellido`,
      `Segundo apellido`
    ),
    
    # quitar acentos pero conservar ñ
    nombre_completo = str_replace_all(nombre_completo, "[áàäâ]", "a"),
    nombre_completo = str_replace_all(nombre_completo, "[éèëê]", "e"),
    nombre_completo = str_replace_all(nombre_completo, "[íìïî]", "i"),
    nombre_completo = str_replace_all(nombre_completo, "[óòöô]", "o"),
    nombre_completo = str_replace_all(nombre_completo, "[úùüû]", "u"),
    
    nombre_completo = str_replace_all(nombre_completo, "[ÁÀÄÂ]", "A"),
    nombre_completo = str_replace_all(nombre_completo, "[ÉÈËÊ]", "E"),
    nombre_completo = str_replace_all(nombre_completo, "[ÍÌÏÎ]", "I"),
    nombre_completo = str_replace_all(nombre_completo, "[ÓÒÖÔ]", "O"),
    nombre_completo = str_replace_all(nombre_completo, "[ÚÙÜÛ]", "U"),
    
    # eliminar caracteres inválidos (pero permitir ñ)
    nombre_completo = str_replace_all(nombre_completo, "[^a-zA-ZñÑ ]", ""),
    
    # limpiar espacios
    nombre_completo = str_squish(nombre_completo),
    
    # Title Case
    nombre_completo = str_to_title(nombre_completo)
    
  )

# 5 Generar correo esperado a partir del nombre

participantes <- participantes %>%
  mutate(
    
    correo_generado = paste0(
      str_to_lower(str_replace_all(nombre_completo, " ", "")),
      "@gmail.com"
    ),
    
    # distancia entre correos
    distancia_email = stringdist(Email, correo_generado),
    
    # posible error tipográfico
    posible_error_email = distancia_email > 0
    
  )

# 6 Calcular desempeño

participantes <- participantes %>%
  mutate(
    
    total_asistencia = rowSums(
      select(., starts_with("asistencia"))
    )
    
  )

# 7 Identificar participantes con menor desempeño


menor_desempeno <- participantes %>%
  filter(total_asistencia == min(total_asistencia))

menor_desempeno

participantes %>%
  summarise(
    participantes = n(),
    correos_sospechosos = sum(posible_error_email),
    asistencia_promedio = mean(total_asistencia)
  )

n_distinct(participantes$Email) == nrow(participantes)

participantes %>%
  count(Email) %>%
  filter(n > 1)

colSums(is.na(participantes))

participantes %>%
  filter(str_detect(nombre_completo, "[áéíóúÁÉÍÓÚ]"))

participantes %>%
  filter(nombre_completo != str_to_title(nombre_completo))

participantes %>%
  filter(str_detect(nombre_completo, "[^A-Za-zñÑ ]"))

participantes %>%
  select(starts_with("asistencia")) %>%
  summarise_all(~all(. %in% c(0,1)))

error_de_mail <- participantes %>%
  filter(posible_error_email)
