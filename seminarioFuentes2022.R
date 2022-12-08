library(readr)
library (dplyr)
# Cargamos la tabla "registro_regional_de_sida.csv" dentro de la carpeta "DATA" de nuestro repositorio
registro_regional_de_sida <- read_delim("DATA/registro-regional-de-sida.csv",
                                        delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Cargamos la tabla "registro_nuevas_infecciones_por_vih.csv" dentro de la carpeta "DATA" de nuestro repositorio
registro_nuevas_infecciones_por_vih <- read_delim("DATA/registro-nuevas-infecciones-por-vih.csv", 
                                                  delim = ";", escape_double = FALSE, trim_ws = TRUE)

poblacionCyL <- read_delim("DATA/04001.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Renombramos sintácticamente la tabla registro_nuevas_infecciones_por_VIH y los atributos y eliminamos las filas que no nos interesan
registroNuevasInfecciones <- rename(.data = registro_nuevas_infecciones_por_vih, Provincia = "Provincia residencia", Codigo_Riesgo = "Código Grupo de riesgo asignado", Grupo_Riesgo = "grupo riesgo", Año = "VIH")%>%
  relocate(., "País de nacimiento (si no es España)", .before = "Estado clínico de la infección por VIH")%>%
  select(., Provincia:Grupo_Riesgo)%>%
  arrange(Año)
#View(registroNuevasInfecciones)

# Renombramos sintácticamente la tabla registro_regional_de_sida y los atributos y eliminamos las filas que no nos interesan
registroRegionalSida <- rename(.data = registro_regional_de_sida, Año = "Año de diagnóstico", Edad = "Edad años", Codigo_Riesgo = "Código Grupo de riesgo", Grupo_Riesgo = "Grupo de riesgo", Provincia = "provincia")%>%
  relocate(., "Edad meses", .after = Grupo_Riesgo)%>%
  relocate(.,"País de origen", .after = Grupo_Riesgo)%>%
  select(., Año:Grupo_Riesgo)%>%
  arrange(Año)
#View(registroRegionalSida)

# View(poblacion)
poblacion <- rename(.data = poblacionCyL, Provincia = "Provincias", Edad = "Edad (3 grupos de edad)")%>%
  filter(., Sexo != "Ambos sexos", Provincia %in% c("05 Ávila", "09 Burgos", "León", "Zamora", "Salamanca", "Segovia", "Valladolid", "Palencia", "Soria"))%>%
  select(., Provincia, Sexo, Edad, Año, Total)%>%
  arrange(Provincia)

# Vamos a empezar a buscar los niveles de las columnas que parecen tener en común las tablas para luego proceder a su unión

levels(factor(registroRegionalSida$Sexo))
levels(factor(registroRegionalSida$Provincia))
levels(factor(registroRegionalSida$Grupo_Riesgo))

levels(factor(registroNuevasInfecciones$Sexo))
levels(factor(registroNuevasInfecciones$Provincia)) # Esán escritos en mayúsculas
levels(factor(registroNuevasInfecciones$Grupo_Riesgo)) 



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


