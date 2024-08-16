download.file("https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/rdata/2022/esi-2022---personas.rds",
              destfile = "esi/datos_originales/esi-2022---personas.rds")


esi <- readr::read_rds("esi/datos_originales/esi-2022---personas.rds")

esi
