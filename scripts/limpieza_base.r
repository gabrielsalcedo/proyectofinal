# Cargar librerías necesarias
library(dplyr)
library(tidyr)
library(janitor)
library(readxl)
library(writexl)

# 1️⃣ Cargar el archivo Excel
ENCUESTA <-read_excel("D:/universidad/ENCUESTA SOBRE EL IMPACTO DE LA CONSTRUCCIÓN DE LA ESTACION E15 DEL METRO(1-35).xlsx")

# 2️⃣ Eliminar las primeras 6 columnas que no se necesitan
base <- ENCUESTA[ , -1:-6]

# 3️⃣ Limpiar nombres de columnas (quita tildes, espacios, signos y los pone en minúsculas)
base <- base %>% janitor::clean_names()


# Elimina columnas por nombre
base <- base %>%
  select(-el_establecimiento_esta_abierto_al_momento_de_la_visita, -hace_cuanto_esta_en_esta_ubicacion, -habitualmente_este_negocio_funciona, -horario_de_atencion_habitual
         , -horario_de_atencion_habitual_lunes_a_viernes_de_a, -horario_de_atencion_habitual_sabados_de_a, -horario_de_atencion_habitual_domingos_de_a,
         -cual_de_estas_es_la_mas_frecuente, -en_que_meses_del_primer_semestre_de_2025_noto_mayor_afectacion_en_la_afluencia_de_clientes_marque_todos_los_que_apliquen, 
         -de_1_a_10_donde_1_es_muy_malo_y_10_muy_bueno_como_calificaria_el_nivel_de_afluencia_de_clientes_en_el_primer_semestre_de_2025_comparado_con_periodos_anteriores,
         -como_cambiaron_sus_ganancias_netas_despues_de_descontar_gastos_durante_el_primer_semestre_de_2025, -de_los_aspectos_anteriores_cual_considera_que_ha_sido_el_mas_perjudicial_para_su_negocio,
         -ha_experimentado_cortes_o_interrupciones_de_servicios_publicos_debido_a_las_obras, -en_caso_afirmativo_que_servicios, -ha_implementado_alguna_estrategia_para_mitigar_el_impacto_de_la_construccion_del_metro_marque_todas_las_que_apliquen,
         -cual_de_estas_estrategias_ha_sido_la_mas_efectiva, -tipo_de_apoyo, -tiene_presencia_en_redes_sociales, -ha_aumentado_el_uso_de_redes_sociales_o_plataformas_digitales_durante_el_primer_semestre_de_2025
         )



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


base <- base %>%
  rename(
    validez = una_parte_de_sus_clientes_son_universitarios,
    empleados = numero_de_empleados,
    comienzo = cuando_comenzo_este_negocio_en_esta_ubicacion,
    translado = el_negocio_ha_cambiado_de_direccion_durante_el_tiempo_que_lleva_funcionando,
    motivo_t = por_que_el_cambio,
    p_principal = cual_es_el_perfil_principal_de_sus_clientes_marque_todas_las_que_apliquen,
    p_cambio = cambio_su_perfil_de_clientes_este_primer_semestre,
    p_new = por_cual,
    distancia = a_que_distancia_aproximada_se_encuentra_su_negocio_de_la_zona_de_construccion_de_la_estacion_e15,
    queja = ha_recibido_algun_comentario_o_queja_acerca_de_la_construccion_del_metro,
    afluencia = durante_el_primer_semestre_de_2025_enero_junio_como_cambio_el_numero_de_clientes_que_visitaban_su_establecimiento_en_comparacion_con_el_mismo_periodo_del_ano_anterior_2024,
    u_afluencia = usted_considera_la_asistencia_de_clientes_estudiantes_universitarios_durante_el_primer_semestre_de_2025,
    ingresos = durante_el_primer_semestre_de_2025_como_cambiaron_sus_ingresos_promedio_mensual_porcentaje_en_comparacion_con_el_mismo_periodo_de_2024,
    afectaciones = cuales_de_los_siguientes_aspectos_relacionados_con_la_construccion_del_metro_han_afectado_su_negocio_marque_todos_los_que_apliquen,
    metro = metro_de_bogota,
    alcaldia = alcaldia_de_bogota,
    camara = camara_de_comercio,
    cerrar = ha_considerado_cerrar_el_negocio_o_mudarse_a_otra_ubicacion_debido_al_impacto_de_las_obras,
    futuro = como_cree_que_sera_el_nivel_de_ingresos_de_su_negocio_una_vez_finalice_la_construccion_del_metro_y_entre_en_operacion_la_estacion_e15
  )


base <- base %>%
  mutate(
    ingresos= case_when(
      ingresos == "Disminuyó mucho (más del 50%)" ~ 1,
      ingresos == "Disminuyó moderadamente (entre 25% y 50%)" ~ 2,
      ingresos == "Disminuyó levemente (entre 10% y 25%)" ~ 3,
      ingresos == "Se mantuvo igual" ~ 4,
      ingresos == "Aumentó levemente (entre 10% y 25%)" ~ 5,
      ingresos == "Aumentó moderadamente (entre 25% y 50%)" ~ 6,
      ingresos == "Aumentó mucho (más del 50%)" ~ 7,
      TRUE ~ NA_real_
    )
  )

base <- base %>%
  mutate(
    afluencia = case_when(
      afluencia == "Disminuyó mucho (más del 50%)" ~ 1,
      afluencia == "Disminuyó moderadamente (entre 25% y 50%)" ~ 2,
      afluencia == "Disminuyó levemente (entre 10% y 25%)" ~ 3,
      afluencia == "Se mantuvo igual" ~ 4,
      afluencia == "Aumentó levemente (entre 10% y 25%)" ~ 5,
      afluencia == "Aumentó moderadamente (entre 25% y 50%)" ~ 6,
      afluencia == "Aumentó mucho (más del 50%)" ~ 7,
      TRUE ~ NA_real_
    )
  )



#
base <- base %>%
  mutate(u_afluencia = case_when(
    u_afluencia == "Disminuyó significativamente" ~ 1,
    u_afluencia == "Disminuyó moderadamente"      ~ 2,
    u_afluencia == "Se mantuvo igual"             ~ 3,
    u_afluencia == "Aumentó moderadamente"        ~ 4,
    u_afluencia == "Aumentó significativamente"   ~ 5,
  TRUE ~ NA_real_
  )
 )







# 6️⃣ Crear variables numéricas limpias
base <- base %>%
  mutate(
    empleados = as.numeric(empleados),
    p_cambio = as.numeric(p_cambio),
    guejas = as.numeric(guejas),
    redes = as.numeric(redes)
  )

# 7️⃣ Recodificar variables de tiempo
base <- base %>%
  mutate(
    comienzo = case_when(
      comienzo == "Hace 1 año" ~ 1,
      comienzo == "Hace 2 año" ~ 2,
      comienzo == "Hace 3 año" ~ 3,
      comienzo == "Hace 4 año" ~ 4,
      comienzo == "Hace 5 años o mas" ~ 5,
      TRUE ~ NA_real_
    ),
    translado = case_when(
      translado == "Sí, se movió" ~ 1,
      translado == "No se ha translado" ~ 0,
      TRUE ~ NA_real_
    ),
    
  )

#######################################################################

# 8️⃣ Separar las respuestas múltiples de “perfil de clientes”
base_larga <- base %>%
  separate_rows(p_principal, sep = ";") %>%
  mutate(p_principal = 
           trimws(p_principal)) %>%
  filter(p_principal != "")

# 9️⃣ Convertir a formato ancho con prefijo "t_"
base <- base_larga %>%
  mutate(valor = 1) %>%
  pivot_wider(
    names_from = p_principal,
    values_from = valor,
    values_fill = list(valor = 0),
    names_prefix = "P_"
  )


base_larga <- base %>%
  separate_rows(afectaciones, sep = ";") %>%
  mutate(afectaciones = 
           trimws(afectaciones)) %>%
  filter(afectaciones != "")

# 9️⃣ Convertir a formato ancho con prefijo "t_"
base <- base_larga %>%
  mutate(valor = 1) %>%
  pivot_wider(
    names_from = afectaciones,
    values_from = valor,
    values_fill = list(valor = 0),
    names_prefix = "a_"
  )





#Descargar base de datos
write_xlsx(base, "base.xlsx")

