# Cargar librerías necesarias
library(dplyr)
library(tidyr)
library(janitor)
library(readxl)
library(writexl)

# 1️⃣ Cargar el archivo Excel
ENCUESTA <-read_excel("C:/Users/ACER/Downloads/ENCUESTA SOBRE EL IMPACTO DE LA CONSTRUCCIÓN DE LA ESTACION E15 DEL METRO(1-35).xlsx")

# 2️⃣ Eliminar las primeras 6 columnas que no se necesitan
base <- ENCUESTA[ , -1:-6]

# 3️⃣ Limpiar nombres de columnas (quita tildes, espacios, signos y los pone en minúsculas)
base <- base %>% janitor::clean_names()

# 4️⃣ Reemplazar "Sí"/"No" por 1 y 0
base[] <- lapply(base, function(x) {
  if (is.character(x) || is.factor(x)) {
    x <- ifelse(x == "Si", 1, ifelse(x == "No", 0, x))
  }
  return(x)
})

# 5️⃣ Filtrar solo los que respondieron “Sí” a universitarios
base <- base %>%
  filter(una_parte_de_sus_clientes_son_universitarios != 0)

# 6️⃣ Crear variables numéricas limpias
base <- base %>%
  mutate(
    empleados = as.numeric(numero_de_empleados),
    cambio_perfil = as.numeric(cambio_su_perfil_de_clientes_este_primer_semestre),
    guejas = as.numeric(ha_recibido_algun_comentario_o_queja_acerca_de_la_construccion_del_metro),
    afluencia_1_10 = as.numeric(de_1_a_10_donde_1_es_muy_malo_y_10_muy_bueno_como_calificaria_el_nivel_de_afluencia_de_clientes_en_el_primer_semestre_de_2025_comparado_con_periodos_anteriores),
    redes = as.numeric(tiene_presencia_en_redes_sociales)
  )

# 7️⃣ Recodificar variables de tiempo
base <- base %>%
  mutate(
    hace_cuanto = case_when(
      hace_cuanto_esta_en_esta_ubicacion == "Hace 1 año" ~ 1,
      hace_cuanto_esta_en_esta_ubicacion == "Hace 2 año" ~ 2,
      hace_cuanto_esta_en_esta_ubicacion == "Hace 3 año" ~ 3,
      hace_cuanto_esta_en_esta_ubicacion == "Hace 4 año" ~ 4,
      hace_cuanto_esta_en_esta_ubicacion == "Hace 5 años o mas" ~ 5,
      TRUE ~ NA_real_
    ),
    comienzo = case_when(
      cuando_comenzo_este_negocio_en_esta_ubicacion == "Hace 1 año" ~ 1,
      cuando_comenzo_este_negocio_en_esta_ubicacion == "Hace 2 año" ~ 2,
      cuando_comenzo_este_negocio_en_esta_ubicacion == "Hace 3 año" ~ 3,
      cuando_comenzo_este_negocio_en_esta_ubicacion == "Hace 4 año" ~ 4,
      cuando_comenzo_este_negocio_en_esta_ubicacion == "Hace 5 años o mas" ~ 5,
      TRUE ~ NA_real_
    ),
    translado = case_when(
      el_negocio_ha_cambiado_de_direccion_durante_el_tiempo_que_lleva_funcionando == "Sí, se movió" ~ 1,
      el_negocio_ha_cambiado_de_direccion_durante_el_tiempo_que_lleva_funcionando == "No se ha translado" ~ 0,
      TRUE ~ NA_real_
    )
  )

# 8️⃣ Separar las respuestas múltiples de “perfil de clientes”
base_larga <- base %>%
  separate_rows(cual_es_el_perfil_principal_de_sus_clientes_marque_todas_las_que_apliquen, sep = ";") %>%
  mutate(cual_es_el_perfil_principal_de_sus_clientes_marque_todas_las_que_apliquen = 
           trimws(cual_es_el_perfil_principal_de_sus_clientes_marque_todas_las_que_apliquen)) %>%
  filter(cual_es_el_perfil_principal_de_sus_clientes_marque_todas_las_que_apliquen != "")

# 9️⃣ Convertir a formato ancho con prefijo "t_"
base <- base_larga %>%
  mutate(valor = 1) %>%
  pivot_wider(
    names_from = cual_es_el_perfil_principal_de_sus_clientes_marque_todas_las_que_apliquen,
    values_from = valor,
    values_fill = list(valor = 0),
    names_prefix = "t_"
  )

# 🔟 Repetir para “Horario de atención habitual”
base_larga2 <- base %>%
  separate_rows(horario_de_atencion_habitual, sep = ";") %>%
  mutate(horario_de_atencion_habitual = trimws(horario_de_atencion_habitual)) %>%
  filter(horario_de_atencion_habitual != "")

base <- base_larga2 %>%
  mutate(valor = 1) %>%
  pivot_wider(
    names_from = horario_de_atencion_habitual,
    values_from = valor,
    values_fill = list(valor = 0),
    names_prefix = "h_"
  )

#Descargar base de datos
write_xlsx(base, "base.xlsx")
