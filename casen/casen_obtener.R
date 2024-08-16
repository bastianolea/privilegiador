# descargar encuesta Casen 2022 y descomprimirla

options(timeout = max(300, getOption("timeout")))

ruta_datos_casen = "casen/datos_originales/"

# descargar base
download.file(url = "https://observatorio.ministeriodesarrollosocial.gob.cl/storage/docs/casen/2022/Base%20de%20datos%20Casen%202022%20STATA.dta.zip",
              destfile = paste0(ruta_datos_casen, "Casen2022.zip"),
              method = "libcurl")

download.file(url = "https://observatorio.ministeriodesarrollosocial.gob.cl/storage/docs/casen/2022/Base%20de%20datos%20provincia%20y%20comuna%20Casen%202022%20STATA.dta.zip",
              destfile = paste0(ruta_datos_casen, "Casen2022comunal.zip"),
              method = "libcurl")

# descomprimir
unzip(zipfile = paste0(ruta_datos_casen, "Casen2022.zip"), exdir = ruta_datos_casen)
unzip(zipfile = paste0(ruta_datos_casen, "Casen2022comunal.zip"), exdir = ruta_datos_casen)

# eliminar archivos comprimidos
file.remove(paste0(ruta_datos_casen, "Casen2022.zip"))
file.remove(paste0(ruta_datos_casen, "Casen2022comunal.zip"))
file.remove(paste0(ruta_datos_casen, "__MACOSX"))


# descargar manuales
download.file("https://observatorio.ministeriodesarrollosocial.gob.cl/storage/docs/casen/2022/Libro_de_Códigos_Base_Casen_2022_v20oct23.xlsx",
              destfile = paste0(ruta_datos_casen, "Libro_de_Códigos_Base_Casen_2022_v20oct23.xlsx"))

download.file("https://observatorio.ministeriodesarrollosocial.gob.cl/storage/docs/casen/2022/Libro%20de%20codigos%20Base%20de%20datos%20provincia%20y%20comuna%20Casen%202022.xlsx",
              destfile = paste0(ruta_datos_casen, "Libro_de_codigos_Base_de_datos_provincia_y_comuna_Casen_2022.xlsx"))



