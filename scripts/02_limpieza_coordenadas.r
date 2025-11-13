# 🧩 Instalar (solo la primera vez)
install.packages("tidyverse")
install.packages("readxl")

# 📚 Cargar librerías
library(tidyverse)
library(readxl)
library(writexl)
# 1️⃣ Leer el archivo Excel
base <- read_excel(
  "D:/universidad/Estudio Chapinero Central- Lugares propuestos.xlsx",
  sheet = "Hoja1",
  col_names = FALSE   # cambia a FALSE si la primera fila NO tiene nombres
)

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
