# Prueba_tecnica_analista

Prueba_tecnica

Procesamiento y Limpieza de Datos de Participantes (R)
Este proyecto contiene un script en R diseñado para automatizar la limpieza, 
consolidación y validación de una base de datos de participantes proveniente de un archivo Excel.
El flujo se centra en la normalización de correos electrónicos, nombres y el cálculo de métricas
de asistencia.
Descripción del Flujo
El script realiza las siguientes operaciones principales:
Limpieza de Emails: 
Convierte a minúsculas, elimina espacios y retira caracteres especiales no permitidos 
en direcciones de correo.
Consolidación de Registros: 
Agrupa los datos por Email para asegurar que cada participante sea único, tomando sus nombres 
y el máximo valor de asistencia registrado.
Normalización de Nombres: 
Genera un nombre_completo unificado.
Elimina acentos (preservando la "ñ").
Aplica formato Title Case (Mayúsculas Iniciales).
Validación de Identidad:
Crea un correo electrónico teórico basado en el nombre y calcula la distancia de string 
(stringdist) para identificar posibles errores de dedo (typos) en el correo real.
Análisis de Desempeño: 
Calcula el total de asistencias y detecta a los participantes con menor participación. 

Requisitos
Para ejecutar este script, necesitas tener instaladas las siguientes librerías de R:
Rinstall.packages(c("readxl", "dplyr", "stringr", "stringi", "tidyr", "stringdist"))
Estructura de los Datos
El script espera un archivo Excel con las siguientes columnas mínimas:
Email: 
Identificador principal.
Nombre, Primer apellido, Segundo apellido.
Columnas que comiencen con el prefijo asistencia (ej. asistencia_v1, asistencia_v2).
Resumen de Salidas
Al finalizar la ejecución, el script genera:
Un dataframe participantes con datos limpios y estandarizados.
Un reporte de menor_desempeno.
Un resumen estadístico que incluye:
Conteo total de participantes únicos.
Número de correos con sospecha de error tipográfico.
Promedio general de asistencia.
Validaciones Incluidas
El código incluye un bloque de auditoría para asegurar que:
No existan correos duplicados.
No queden valores NA en campos críticos.
La normalización de texto se haya aplicado correctamente (sin acentos residuales).
Los valores de asistencia sean binarios (0 o 1).