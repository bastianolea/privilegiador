# Documento Metodológico Encuesta Suplementaria de Ingresos (ESI) 2022
# https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/metodologia/documento-metodológico/documento-metodológico---esi-2022.pdf?sfvrsn=daf887d8_10

library(survey) #Para estimaciones a partir de diseño muestral complejo
library(magrittr) #Para usar operador %<>%
library(dplyr) #Para funciones de manejo de datos

# datos ----
#a. Leer base y ordenar en base al vector oficial de orden ESI
esi_2022 <- readr::read_rds("esi/datos_originales/esi-2022---personas.rds")
esi_2022 <- esi_2022 %>% arrange(id_mediana)

# diseño ----
#b. Generación del diseño muestral: 
diseno <- svydesign(
  id = ~ conglomerado_correlativo,
  strata = ~ estrato,
  check.strata = TRUE,
  weights = ~ fact_cal_esi,
  data = esi_2022
)

options(survey.lonely.psu = "remove")

#c. Se fijan las semillas
set.seed(234262762)

#d. Se realiza el diseño repetido de sub-bootstrap para 2.000 réplicas
diseno_rep <- as.svrepdesign(diseno, type = "subbootstrap", replicates = 2000)

#e. Se ejecuta la función modificada correspondiente a la regla de interpolación del método N°5.
interpolacionMetodo5 <- function(x, w, p) {
  if (any(zero <- w == 0)) {
    w <- w[!zero]
    x <- x[!zero]
  }
  
  n <- length(x)
  ii <- order(x)
  x <- x[ii]
  w <- w[ii]
  cumw <- cumsum(w)
  pk <- (cumw - w / 2) / (cumw[n])
  
  #Modificación: si x es sólo 1 valor, muéstralo; en caso contrario; generar interpolación:
  if (length(x) == 1) {
    x
  } else{
    approx(pk, x, p, method = "linear", rule = 2)$y
  }
}

#f. Se realiza la estimación especificando en el parámetro "qrule" la función definida en el paso anterior
estimacion <- svyby(
  ~ ing_t_p,
  by = ~ region + sexo,
  data = esi_2022,
  subset(diseno_rep, ocup_ref == 1),
  svyquantile,
  quantiles = c(0.5),
  interval.type = "quantile",
  vartype = c("se"),
  ci = TRUE,
  na.rm.all = FALSE,
  qrule = interpolacionMetodo5,
  multicore = TRUE
)

#g. Mostrar el resultado de la estimación
estimacion



# calcular ----

## deciles ----

### deciles nacional ----
deciles <- svyquantile(~ ing_t_p, data = esi_2022,
  subset(diseno_rep, ocup_ref == 1),
  quantiles = seq(0.1, 0.9, 0.1),
  vartype = c("se"),
  ci = TRUE,
  na.rm.all = FALSE,
  qrule = interpolacionMetodo5,
  multicore = TRUE
)

deciles_2 <- deciles[[1]] |> 
  # as_tibble() |> 
  as.data.frame() |>
  select(ingreso = 1) |> 
  tibble::rownames_to_column("quantile") |> 
  as_tibble()

### deciles sexo ----
deciles_sexo <- svyby(~ ing_t_p, by = ~ sexo, data = esi_2022,
                 subset(diseno_rep, ocup_ref == 1),
                 svyquantile, quantiles = seq(0.1, 0.9, 0.1),
                 interval.type = "quantile", vartype = c("se"),
                 ci = TRUE, na.rm.all = FALSE,
                 qrule = interpolacionMetodo5,
                 multicore = TRUE)

deciles_sexo_2 <- deciles_sexo |> 
  tidyr::pivot_longer(cols = starts_with("ing"), names_to = "quantile", values_to = "ingreso") |> 
  select(sexo, quantile, ingreso) |> 
  mutate(quantile = str_extract(quantile, "\\d+\\.\\d+") |> as.numeric())


### deciles region ----
deciles_region <- svyby(~ ing_t_p, by = ~ region, data = esi_2022,
                      subset(diseno_rep, ocup_ref == 1),
                      svyquantile, quantiles = seq(0.1, 0.9, 0.1),
                      interval.type = "quantile", vartype = c("se"),
                      ci = TRUE, na.rm.all = FALSE,
                      qrule = interpolacionMetodo5,
                      multicore = TRUE)

deciles_region_2 <- deciles_region |> 
  tidyr::pivot_longer(cols = starts_with("ing"), names_to = "quantile", values_to = "ingreso") |> 
  select(region, quantile, ingreso) |> 
  mutate(quantile = str_extract(quantile, "\\d+\\.\\d+") |> as.numeric())


## percentiles ----
### percentil nacional ----
percentiles <- svyquantile(~ ing_t_p, data = esi_2022,
  subset(diseno_rep, ocup_ref == 1),
  quantiles = seq(0.01, 0.99, 0.01),
  vartype = c("se"),
  ci = TRUE,
  na.rm.all = FALSE,
  qrule = interpolacionMetodo5,
  multicore = TRUE
)

percentiles_2 <- percentiles[[1]] |> 
  # as_tibble() |> 
  as.data.frame() |>
  select(ingreso = 1) |> 
  tibble::rownames_to_column("quantile") |> 
  as_tibble()


### percentil sexo ----
tictoc::tic()
percentiles_sexo <- svyby(~ ing_t_p, by = ~ sexo, data = esi_2022,
                      subset(diseno_rep, ocup_ref == 1),
                      svyquantile, quantiles = seq(0.01, 0.99, 0.01),
                      interval.type = "quantile", vartype = c("se"),
                      ci = TRUE, na.rm.all = FALSE,
                      qrule = interpolacionMetodo5,
                      multicore = TRUE)
tictoc::toc()

percentiles_sexo_2 <- percentiles_sexo |> 
  tidyr::pivot_longer(cols = starts_with("ing"), names_to = "quantile", values_to = "ingreso") |> 
  select(sexo, quantile, ingreso) |> 
  mutate(quantile = str_extract(quantile, "\\d+\\.\\d+") |> as.numeric())


### percentil región ----
tictoc::tic()
percentiles_region <- svyby(~ing_t_p, by = ~ region, data = esi_2022,
                          subset(diseno_rep, ocup_ref == 1),
                          svyquantile, quantiles = seq(0.01, 0.99, 0.01),
                          interval.type = "quantile", vartype = c("se"),
                          ci = TRUE, na.rm.all = FALSE,
                          qrule = interpolacionMetodo5,
                          multicore = TRUE)
tictoc::toc()

percentiles_region_2 <- percentiles_region |> 
  tidyr::pivot_longer(cols = starts_with("ing"), names_to = "quantile", values_to = "ingreso") |> 
  select(region, quantile, ingreso) |> 
  mutate(quantile = str_extract(quantile, "\\d+\\.\\d+") |> as.numeric())


# guardar ----
resultados <- list("deciles_pais" = deciles_2,
"deciles_sexo" = deciles_sexo_2,
"deciles_region" = deciles_region_2,
"percentiles_pais" = percentiles_2,
"percentiles_region" = percentiles_region_2,
"percentiles_sexo" = percentiles_sexo_2)

readr::write_rds(resultados, "esi/datos_procesados/esi_cuantiles_ingreso.rds")
