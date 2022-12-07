library(readr)
library (dplyr)

# Cargamos la tabla "registro_regional_de_sida.csv" dentro de la carpeta "DATA" de nuestro repositorio
registro_regional_de_sida <- read_delim("DATA/registro-regional-de-sida.csv",
                                        delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Cargamos la tabla "registro_nuevas_infecciones_por_vih.csv" dentro de la carpeta "DATA" de nuestro repositorio
registro_nuevas_infecciones_por_vih <- read_delim("DATA/registro-nuevas-infecciones-por-vih.csv", 
                                                  delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Cargamos la tabla "opinion_transmision.csv" dentro de la carpeta "DATA" de nuestro repositorio
opinion_transmision <- read_delim("DATA/opinion_transmision.csv",
                                  delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Cargamos la tabla "medidas_prevencion_eficacia.csv" dentro de la carpeta "DATA" de nuestro repositorio
medidas_prevencion_eficacia <- read_delim("DATA/medidas_prevencion_eficacia.csv", 
                                          delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Cargamos la tabla "salidas_riesgo_drogas.csv" dentro de la carpeta "DATA" de nuestro repositorio
salidas_riesgo_drogas <- read_delim("DATA/salidas_riesgo_drogas.csv",
                                    delim = ";", escape_double = FALSE, trim_ws = TRUE)


# Renombramos sintácticamente la tabla registro_nuevas_infecciones_por_VIH y los atributos y eliminamos las filas que no nos interesan
registroNuevasInfecciones <- rename(.data = registro_nuevas_infecciones_por_vih, Provincia = "Provincia residencia", Pais = "País de nacimiento (si no es España)", Codigo_Riesgo = "Código Grupo de riesgo asignado", Grupo_Riesgo = "grupo riesgo", Conducta_Sexual = "Conducta sexual", Año = "VIH")%>%
  relocate(., Conducta_Sexual, .before = "Estado clínico de la infección por VIH" )%>%
  select(., Provincia:Conducta_Sexual)%>%
  arrange(Año)


#View(registroNuevasInfecciones)

# Renombramos sintácticamente la tabla registro_regional_de_sida y los atributos y eliminamos las filas que no nos interesan
registroRegionalSida <- rename(.data = registro_regional_de_sida, Año = "Año de diagnóstico", Edad = "Edad años", Codigo_Riesgo = "Código Grupo de riesgo", Grupo_Riesgo = "Grupo de riesgo", Provincia = "provincia", Pais = "País de origen")%>%
  relocate(., "Edad meses", .after = Grupo_Riesgo)%>%
  select(., Año:Grupo_Riesgo)%>%
  arrange(Año)

#View(registroRegionalSida)

# Renombramos sintácticamente la tabla salidas_riesgo_drogas y los atributos y eliminamos las filas que no nos interesan
Salidas_riesgo_y_drogas <- rename (.data = salidas_riesgo_drogas, Frecuencia_salidas = "Frecuencia de salidas nocturnas en los últimos 12 meses", Riesgo_Drogas_Inyectables = "Percepción del riesgo de infección de VIH/sida por drogas inyectables") %>%
  filter (., Sexo != "Ambos sexos")

#View(Salidas_riesgo_y_drogas)

# Renombramos sintácticamente la tabla medidas_prevencion_eficacia y los atributos y eliminamos las filas que no nos interesan
Medidas_prevencion <- rename(.data = medidas_prevencion_eficacia, Medidas_prevencion = "Medidas de prevención del VIH/sida", Opinion_eficacia = "Opinión sobre su eficacia", Frecuencia_salidas = "Frecuencia de salidas nocturnas en los últimos 12 meses") %>%
  filter (., Sexo != "Ambos sexos")

#View (Medidas_prevencion)

# Renombramos sintácticamente la tabla opinion_transmision y los atributos y eliminamos las filas que no nos interesan
opinionTransmision <- rename(.data = opinion_transmision, Mecanismos_tansmision = "Mecanismos de transmisión del VIH/sida", opinion_posibilidad_transmision = "Opinión sobre la posibilidad de transmisión", Frecuencia_salidas = "Frecuencia de salidas nocturnas en los últimos 12 meses")%>%
  filter(., Sexo != "Ambos sexos")

# View(opinionTransmision) lo comentamos porque al pasar a HTML no lo va a saber procesar y nos va a salir un error


# Vamos a empezar a buscar los niveles de las columnas que parecen tener en común las tablas para luego proceder a su unión
levels(factor(opinionTransmision$Sexo))
levels(factor(opinionTransmision$Frecuencia_salidas))

levels(factor(Medidas_prevencion$Sexo))
levels(factor(Medidas_prevencion$Frecuencia_salidas))

levels(factor(Salidas_riesgo_y_drogas$Sexo))
levels(factor(Salidas_riesgo_y_drogas$Frecuencia_salidas))

levels(factor(registroRegionalSida$Sexo))
levels(factor(registroRegionalSida$Provincia))
levels(factor(registroRegionalSida$Pais))
levels(factor(registroRegionalSida$Grupo_Riesgo))

levels(factor(registroNuevasInfecciones$Sexo))
levels(factor(registroNuevasInfecciones$Provincia)) # Esán escritos en mayúsculas
levels(factor(registroNuevasInfecciones$Pais)) # No tiene el mismo numero de niveles en las dos tablas
levels(factor(registroNuevasInfecciones$Grupo_Riesgo)) # No tiene el mismo numero de niveles en las dos tablas



# Creo que estas uniones no tienen sentido asi porque nos sale un numero desmedido de filas y las relaciones no tienen mucho sentido

# a1 <- full_join(x = Medidas_prevencion, y = opinionTransmision, by = c("Sexo", "Frecuencia_salidas"))

# a2 <- full_join(x = a1, y = Salidas_riesgo_y_drogas, by = c("Sexo", "Frecuencia_salidas"))

# b1 <- full_join(x = registroRegionalSida, y = registroNuevasInfecciones)



# Tabla con la media de edad y año de contagio para cada sexo y grupo de contagio desde 1985 hasta 2021.

c1 <- registroRegionalSida%>%
  dplyr::filter(Año < 2009)%>%
  group_by(Sexo, Grupo_Riesgo, Provincia)%>%
  filter(n() > 1)%>%
  summarise(across(c(Año,Edad), ~ mean(.x, na.rm = TRUE)))%>%
  mutate(Tipo = "Tipo 1")%>%
  relocate(Tipo , .before = Sexo)%>%
  relocate(Provincia , .before = Sexo)
# View(c1)

# Tabla con la media de edad y año de contagio para cada sexo y grupo de contagio.

c2 <- registroNuevasInfecciones%>%
  group_by(Sexo, Grupo_Riesgo, Provincia)%>%
  filter(n() > 1)%>%
  summarise(across(c(Año,Edad), ~ mean(.x, na.rm = TRUE)))%>%
  mutate(Tipo = "Tipo 2")%>%
  relocate(Tipo , .before = Sexo)%>%
  relocate(Provincia , .before = Sexo)

# View(c2)

Union <- full_join(x = c1, y = c2)
# View(Union)
library(ggplot2)


