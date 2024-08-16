#en este script se cargan los datos de la casen preprocesados en casen2022_procesar.r, 
#y se seleccionan las variables a utilizar en el visualizador, calculando las medidas 
#necesarias para optimizar el rendimiento en vivo de la app.

library(dplyr)
library(purrr)
library(stringr)
library(haven)
library(readr)

#cargar datos de casen2022_procesar.r
casen2022 <- readr::read_rds("casen/datos_procesados/casen2022.rds")

# variables ----

# lista de variables de la casen a considerar
variables_casen <- c(
  "comuna",
  "cut_comuna",
  "region",
  "area",
  "expc",                    # factor de expansión comunal
  "expr",                    # factor de expansión regional
  "pco1",                    # jefe de hogar
  "sexo",                    # género
  "esc",                     # años de escolaridad
  "edad",                    # edad
  "educ",                    # nivel de escolaridad
  "pobreza",                 # pobreza
  "pobreza_multi_5d"         # pobreza multidimensional
)

variables_numericas_personas <- c(
  "ytotcor",                 # Ingreso total corregido
  "yoprcor",                 # Ingreso ocupación principal
  "ytrabajocor",             # ingreso del trabajo
  "y2803",                   # Jubilación o pensión de vejez
  "y0101",                   # Asalariados principal - Sueldos y salarios monetario
  "ytot",                    # Ingreso total
  "dau",                     # Decil autónomo nacional
  "qaut",                    # Quintil autónomo nacional
  "dautr",                   # Decil autónomo regional
  "qautr"                    # Quintil autónomo regional
)

# variables_numericas_hogar <- c(
#   "ytotcorh",                # Ingreso total del hogar corregido
#   "ytrabajocorh",            # ingreso del trabajo del hogar
#   "ypchautcor",              # Ingreso autónomo per cápita
#   "ypc",                     # Ingreso total per cápita del hogar corregido
#   "numper"                   # numero de personas en el hogar
# )

# filtrar variables y aplicar factor de expansión ----

# modificar formato de algunas variables, de manera preliminar
casen2022 <- casen2022 |> 
  mutate(pobreza = as_factor(pobreza),
         pobreza_multi_5d = as_factor(pobreza_multi_5d),
         educ = as_factor(educ)) |> 
  filter(ytot > 0)

casen_region <- casen2022 |> 
  select(any_of(c(variables_casen, variables_numericas_personas))) |> 
  tidyr::uncount(weights = expr) |> #factor de expansión
  mutate(nivel = "región")

# calcular numéricas ----

casen_region_numericos_personas <- casen_region |> 
  group_by(region) |> 
  summarize(across(any_of(variables_numericas_personas), ~median(.x, na.rm = TRUE)),
            across(c(edad, esc), ~mean(.x, na.rm = TRUE)),
            .groups = "drop") |> 
  ungroup()


# calcular conteo ----
#para toda la población, por comunas, se evalúan expresiones, por ejemplo, si la persona pertenece a pueblos originarios, 
#y se obtiene el conteo de personas en la comuna que cumple esa característica,
#de modo que, en el siquiente paso, se transforme esa cifra de personas en un porcentaje comunal


casen_region_conteo_personas <- casen_region |> 
  group_by(region) |> 
  summarize(poblacion = n(),
            pobreza = sum(pobreza == "Pobreza extrema" | pobreza == "Pobreza no extrema", na.rm = TRUE),
            pobreza_multi = sum(pobreza_multi_5d == "Pobreza", na.rm=TRUE)
  ) |> 
  ungroup()

# casen_region |> 
#   group_by(region) |> 
#   select(educ) |> 
#   mutate(educ2 = as.numeric(educ)) |> 
#   count(educ, educ2) |> 
#   select(-educ) |> 
#   group_by(region) |> 
#   mutate(p = n/sum(n)) |> 
#   select(-n) |> 
#   tidyr::pivot_wider(names_from = educ2, values_from = p, names_prefix = "educ_")


## ingresos ----
# percentiles de ingresos
casen_region |> 
  select(ytotcor) |> 
  pull() |> 
  quantile(probs = seq(.01, .99, .01))

## nivel educacional ----
# porcentaje de la población con un nivel de educación menor
### nacional ----
casen_region |> 
  # group_by(region) |> 
  select(educ) |> 
  mutate(educ_orden = as.numeric(educ)) |> 
  count(educ, educ_orden) |> 
  # eliminar distinción entre tipos de educación media
  mutate(educ = str_remove(educ, "humanista |técnica profesional "),
         educ_orden = case_when(educ_orden == 6 ~ 5,
                                educ_orden == 8 ~ 7, .default = educ_orden)) |> 
  # reordenar niveles
  mutate(educ_orden = case_when(educ == "Posgrado incompleto" ~ 13,
                                educ == "Profesional completo" ~ 12,
                                .default = educ_orden)) |> 
  group_by(educ, educ_orden) |> 
  summarize(n = sum(n)) |> 
  ungroup() |> 
  arrange(educ_orden) |> 
  # calcular sumas acumuladas
  mutate(n_sum = cumsum(n)) |> 
  mutate(p = n_sum/sum(n_sum)) |> 
  mutate(p_sum = cumsum(p)) |>  #porcentaje de la población con ese nivel o menor
  mutate(p_sum_sup = p_sum - p) #porcentaje de la población con menor nivel


### por región ----
casen_region |> 
  mutate(educ_orden = as.numeric(educ)) |> 
  group_by(region) |>
  count(educ, educ_orden) |> 
  # eliminar distinción entre tipos de educación media
  mutate(educ = str_remove(educ, "humanista |técnica profesional "),
         educ_orden = case_when(educ_orden == 6 ~ 5,
                                educ_orden == 8 ~ 7, .default = educ_orden)) |> 
  # reordenar niveles
  mutate(educ_orden = case_when(educ == "Posgrado incompleto" ~ 13,
                                educ == "Profesional completo" ~ 12,
                                .default = educ_orden)) |> 
  group_by(region, educ, educ_orden) |> 
  summarize(n = sum(n)) |> 
  ungroup() |> 
  arrange(region, educ_orden) |> 
  # calcular sumas acumuladas
  group_by(region) |> 
  mutate(n_sum = cumsum(n)) |> 
  mutate(p = n_sum/sum(n_sum)) |> 
  mutate(p_sum = cumsum(p)) |>  #porcentaje de la población con ese nivel o menor
  mutate(p_sum_sup = p_sum - p) #porcentaje de la población con menor nivel


### por sexo ----
casen_region |> 
  mutate(educ_orden = as.numeric(educ)) |> 
  group_by(sexo) |>
  count(educ, educ_orden) |> 
  # eliminar distinción entre tipos de educación media
  mutate(educ = str_remove(educ, "humanista |técnica profesional "),
         educ_orden = case_when(educ_orden == 6 ~ 5,
                                educ_orden == 8 ~ 7, .default = educ_orden)) |> 
  # reordenar niveles
  mutate(educ_orden = case_when(educ == "Posgrado incompleto" ~ 13,
                                educ == "Profesional completo" ~ 12,
                                .default = educ_orden)) |> 
  group_by(sexo, educ, educ_orden) |> 
  summarize(n = sum(n)) |> 
  ungroup() |> 
  arrange(sexo, educ_orden) |> 
  # calcular sumas acumuladas
  group_by(sexo) |> 
  mutate(n_sum = cumsum(n)) |> 
  mutate(p = n_sum/sum(n_sum)) |> 
  mutate(p_sum = cumsum(p)) |>  #porcentaje de la población con ese nivel o menor
  mutate(p_sum_sup = p_sum - p) #porcentaje de la población con menor nivel



#consolidar base ----
# casen_region_numericos_personas
# casen_comuna_numericos_personas
# 
# casen_comuna_conteo_personas
# casen_region_conteo_personas


# calcular nacional ----
casen_pais_conteo_personas <- casen_region_conteo_personas |> 
  ungroup() |> 
  summarize(across(where(is.numeric), sum)) |> 
  select(-region)

casen_pais_numericos_personas <- casen_region_numericos_personas |> 
  ungroup() |> 
  summarize(across(where(is.numeric), mean)) |> 
  select(-region)


# porcentajes ----
# se obtienen los conteos de personas por comuna, y se transforman en porcentajes, para poder comparar comunas

#porcentaje en relación a población
casen_region_conteo_personas_p <- casen_region_conteo_personas |> 
  mutate(across(where(is.numeric),
                ~.x/poblacion, .names = "{.col}_p")) |> 
  select(-poblacion_p)

casen_comuna_conteo_personas_p <- casen_comuna_conteo_personas |> 
  mutate(across(where(is.numeric),
                ~.x/poblacion, .names = "{.col}_p")) |> 
  select(-poblacion_p)


casen_pais_conteo_personas_p <- casen_pais_conteo_personas |> 
  mutate(across(where(is.numeric),
                ~.x/poblacion, .names = "{.col}_p")) |> 
  select(-poblacion_p)



# seleccionar ----
# dejar solo las columnas necesarias
pobreza_pais <- casen_pais_conteo_personas_p |> 
  select(ends_with("_p"))

pobreza_comuna <- casen_comuna_conteo_personas_p |> 
  select(cut_comuna, comuna, ends_with("_p")) |> 
  mutate(across(where(is.labelled), as.numeric))

pobreza_region <- casen_region_conteo_personas_p |> 
  select(region, ends_with("_p")) |> 
  mutate(across(where(is.labelled), as.numeric))

ingresos_pais <- casen_pais_numericos_personas

ingresos_comuna <- casen_comuna_numericos_personas |> 
  mutate(across(where(is.labelled), as.numeric))

ingresos_region <- casen_region_numericos_personas |> 
  mutate(across(where(is.labelled), as.numeric))


# guardar ----
pobreza_pais |> write_rds("casen/datos_procesados/casen_pobreza_pais.rds")
pobreza_comuna |> write_rds("casen/datos_procesados/casen_pobreza_comuna.rds")
pobreza_region |> write_rds("casen/datos_procesados/casen_pobreza_region.rds")
ingresos_pais |> write_rds("casen/datos_procesados/casen_ingresos_pais.rds")
ingresos_comuna |> write_rds("casen/datos_procesados/casen_ingresos_comuna.rds")
ingresos_region |> write_rds("casen/datos_procesados/casen_ingresos_region.rds")

pobreza_pais |> write_csv2("casen/datos_procesados/casen_pobreza_pais.csv")
pobreza_comuna |> write_csv2("casen/datos_procesados/casen_pobreza_comuna.csv")
pobreza_region |> write_csv2("casen/datos_procesados/casen_pobreza_region.csv")
ingresos_pais |> write_csv2("casen/datos_procesados/casen_ingresos_pais.csv")
ingresos_comuna |> write_csv2("casen/datos_procesados/casen_ingresos_comuna.csv")
ingresos_region |> write_csv2("casen/datos_procesados/casen_ingresos_region.csv")

# traspasar a la app
read_rds("casen/datos_procesados/casen_pobreza_pais.rds") |> write_rds("app/datos/casen_pobreza_pais.rds")
read_rds("casen/datos_procesados/casen_pobreza_comuna.rds") |> write_rds("app/datos/casen_pobreza_comuna.rds")
read_rds("casen/datos_procesados/casen_pobreza_region.rds") |> write_rds("app/datos/casen_pobreza_region.rds")