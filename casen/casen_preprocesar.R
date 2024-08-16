# en este script se carga la base de datos original de la Casen 2022 y se deja guardada en un formato más eficiente

library(dplyr)
library(haven)


message("cargando casen 2022...")

ruta_datos_casen = "casen/datos_originales/"

# comunas ----
casen2022comunas_0 <- haven::read_dta(paste0(ruta_datos_casen, "Base de datos provincia y comuna Casen 2022 STATA.dta"))

casen2022comunas <- casen2022comunas_0 |> 
  mutate(cut_comuna = as.character(comuna)) |> 
  mutate(comuna = haven::as_factor(comuna))


# cargar base ----
# casen2022 <- readstata13::read.dta13("datos/Base de datos Casen 2022 STATA.dta" , generate.factors = T) |> as_tibble()
casen2022 <- haven::read_dta(paste0(ruta_datos_casen, "Base de datos Casen 2022 STATA.dta"))

## unir base con comunas ----
casen2022_2 <- casen2022 |> 
  # anexar información de comunas y factores de expansión
  left_join(casen2022comunas, join_by(folio, id_persona)) |> 
  # ordenar
  select(names(casen2022comunas), everything()) |> 
  # remover variables que no usaremos
  select(-id_vivienda, -folio, -id_persona, -cod_upm, -estrato, -varstrat, -varunit, -fecha_entrev)

# guardar ----
# arrow::write_parquet(casen2022_2, "datos/casen2022.parquet")
readr::write_rds(casen2022_2, "casen/datos_procesados/casen2022.rds", compress = "gz")
# readr::read_rds("casen/datos_procesados/casen2022.rds")
