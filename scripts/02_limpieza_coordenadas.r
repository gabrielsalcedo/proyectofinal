# 🧩 Instalar (solo la primera vez)
install.packages("tidyverse")
install.packages("readxl")

# 📚 Cargar librerías
library(tidyverse)
library(readxl)
library(writexl)

# 1️⃣ Leer el archivo Excel

url_raw <- "https://raw.githubusercontent.com/gabrielsalcedo/proyectofinal/0bbfd9b1c0b2f6eef4d5fa202eb3d9f019e16ff6/data/raw/Estudio%20Chapinero%20Central-%20Lugares%20propuestos.xlsx"

# Descargar archivo temporalmente

temp <- tempfile(fileext = ".xlsx")
download.file(url_raw, temp, mode = "wb")

# Leer sin convertir primera fila en nombres de columnas
base <- read_xlsx(temp, col_names = FALSE)

# 2️⃣ Renombrar columnas si aparecen como ...1, ...2, etc.
# (de esta forma trabajamos con nombres más claros)
base <- base %>%
  rename(
    coordenadas = ...1,
    nombre = ...2
  )

# 3️⃣ Separar la columna de coordenadas en longitud y latitud
base <- base %>%
  separate(col = coordenadas, into = c("longitud", "latitud"), sep = ";") %>%
  mutate(
    longitud = as.numeric(longitud),
    latitud = as.numeric(latitud)
  )

#Descargar base
write_xlsx(base,'base_coordenadas.xlsx')
