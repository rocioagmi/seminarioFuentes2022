library(readr)
library (dplyr)

registro_regional_de_sida <- read_delim("DATA/registro-regional-de-sida.csv",
                                        delim = ";", escape_double = FALSE, trim_ws = TRUE)

registro_nuevas_infecciones_por_vih <- read_delim("DATA/registro-nuevas-infecciones-por-vih.csv", 
                                                  delim = ";", escape_double = FALSE, trim_ws = TRUE)

# poblacionCyL <- read_delim("DATA/04001.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

registroNuevasInfecciones <- rename(.data = registro_nuevas_infecciones_por_vih, Provincia = "Provincia residencia", Codigo_Riesgo = "Código Grupo de riesgo asignado", Grupo_Riesgo = "grupo riesgo", Año = "VIH")%>%
  relocate(., "País de nacimiento (si no es España)", .before = "Estado clínico de la infección por VIH")%>%
  select(., Provincia:Grupo_Riesgo)%>%
  mutate(Tipo = "2")%>%
  arrange(Año)
#View(registroNuevasInfecciones)

registroRegionalSida <- rename(.data = registro_regional_de_sida, Año = "Año de diagnóstico", Edad = "Edad años", Codigo_Riesgo = "Código Grupo de riesgo", Grupo_Riesgo = "Grupo de riesgo", Provincia = "provincia")%>%
  relocate(., "Edad meses", .after = Grupo_Riesgo)%>%
  relocate(.,"País de origen", .after = Grupo_Riesgo)%>%
  dplyr::filter(Año < 2009)%>%
  select(., Año:Grupo_Riesgo)%>%
  arrange(Año)%>%
  mutate(Tipo = "1")%>%
  mutate(Provincia = factor(Provincia, levels = c("Burgos", "León", "Ávila", "Zamora", "Salamanca", "Segovia", "Valladolid", "Palencia", "Soria"), labels = c("BURGOS", "LEON", "AVILA", "ZAMORA", "SALAMANCA", "SEGOVIA", "VALLADOLID", "PALENCIA", "SORIA")))#View(registroRegionalSida)

union <- full_join(x = registroRegionalSida, y = registroNuevasInfecciones)
union


mujeres <- union%>%
  dplyr::filter(Sexo == "M")%>%
  group_by(Grupo_Riesgo)%>%
  arrange(Año)
mujeres
# View(mujeres)

hombres <- union%>%
  dplyr::filter(Sexo == "H")%>%
  arrange(Año)

mujeres %>% 
  pivot_longer(data = ., names_to = "Variable", values_to = "Valores", cols = c())

# View(hombres)

#poblacionCyL <- rename(.data = poblacionCyL, Provincia = "Provincias", Edad = "Edad (3 grupos de edad)")%>%
  # mutate(Provincia = factor(Provincia, levels = c("09 Burgos", "24 León", "05 Ávila", "49 Zamora", "37 Salamanca", "40 Segovia", "47 Valladolid", "34 #Palencia", "42 Soria"), labels = c("BURGOS", "LEON", "AVILA", "ZAMORA", "SALAMANCA", "SEGOVIA", "VALLADOLID", "PALENCIA", "SORIA")))%>%
  #dplyr::filter(., Sexo != "Ambos sexos", Edad != "TOTAL EDADES", Provincia %in% c("09 Burgos", "24 León", "05 Ávila", "49 Zamora", "37 Salamanca", "40 Segovia", "47 Valladolid", "34 Palencia", "42 Soria"))%>%
  #select(., Provincia, Sexo, Edad, Año, Total)

levels(factor(registroRegionalSida$Sexo))
levels(factor(registroRegionalSida$Provincia))
levels(factor(registroRegionalSida$Grupo_Riesgo))

levels(factor(registroNuevasInfecciones$Sexo))
levels(factor(registroNuevasInfecciones$Provincia)) # Esán escritos en mayúsculas forzamos con factor para que sean iguales en las dos tablas
levels(factor(registroNuevasInfecciones$Grupo_Riesgo)) 


c1 <- registroRegionalSida%>%
  group_by(Sexo, Grupo_Riesgo, Provincia)%>%
  summarise(across(c(Año,Edad), ~ mean(.x, na.rm = TRUE)))%>%
  relocate(Tipo , .before = Sexo)%>%
  relocate(Provincia , .before = Sexo)
# View(c1)

c2 <- registroNuevasInfecciones%>%
  group_by(Sexo, Grupo_Riesgo, Provincia)%>%
  filter(n() > 1)%>%
  summarise(across(c(Año,Edad), ~ mean(.x, na.rm = TRUE)))%>%
  relocate(Tipo , .before = Sexo)%>%
  relocate(Provincia , .before = Sexo)
# View(c2)


#promedioAntiguos <- registroRegionalSida%>%
#  dplyr::filter(Año < 2009)%>%
#  group_by(Año, Provincia)%>%
#  dplyr::filter(n() > 1)%>%
#  summarise(across(c(Sexo), ~ length(unique(.x))))%>%
#  mutate(Tipo = "Tipo 1")%>%
#  relocate(Tipo , .before = Sexo)%>%
#  relocate(Provincia , .before = Sexo)
# View(promedioAntiguos)



# install_github("ropenspain/climaemet")
library(climaemet)
# aemet_api_key("eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJyYW0xMDEwQGFsdS51YnUuZXMiLCJqdGkiOiI3ZDQyYjFlNS02ZjMwLTRhNGUtODM0Ny1hZTI5ODU3YzUwYzgiLCJpc3MiOiJBRU1FVCIsImlhdCI6MTY3MDYwODI1MywidXNlcklkIjoiN2Q0MmIxZTUtNmYzMC00YTRlLTgzNDctYWUyOTg1N2M1MGM4Iiwicm9sZSI6IiJ9.N3jVd2A9en72Xh22QTWpTJ9omGlVcl-xcmfuyh5UQ1w", install = TRUE)
