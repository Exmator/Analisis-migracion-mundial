# Transformaciones de las bases
# Grupo: Migración bajo la Lupa
# --------------------------------------------------------------------------------------------------------------

# ------------------------------------------Implementación inicial--------------------------------------
# Instalación de paquetes (En caso de no tenerlos instalados previamente, eliminar el #, en caso contrario, dejar como esta)
# install.packages("readxl")
# install.packages("tidyr")
# install.packages("openxlsx")

# Invocación de las librerias usadas
library("readxl")
library(dplyr)
library(tidyr)
library(openxlsx)

# ----------------------------------------------Stock migratorio--------------------------------------------------
# Leer la base Stock Migratorio
Stock_migratorio <- read_excel("Stock_migratorio.xlsx")

# Eliminar columnas con nombre de los paises, notas de migrantes y renombrar las columnas de acuerdo al diagrama
Stock_migratorio <- Stock_migratorio %>% select(
  -`Region, development group, country or area of destination`:-`Data type`,
  -`Region, development group, country or area of origin`) %>%
  rename(
    `ID_stock` = `Index`,
    `ID_pais_destino` = `Location code of destination`,
    `ID_pais_origen` = `Location code of origin`
  )

# Crear 3 nuevas bases segun sexo de los migrantes
Stock_migratorio_mixto <- Stock_migratorio %>% select(-`1990...16`:-`2024...31`)
Stock_migratorio_masculino <- Stock_migratorio %>% select(-`1990...8`:-`2024...15`,-`1990...24`:-`2024...31`)
Stock_migratorio_femenino <- Stock_migratorio %>% select(-`1990...8`:-`2024...23`)

# Renombrar las columnas de los años
Stock_migratorio_mixto <- Stock_migratorio_mixto %>% rename(
  `1990` = `1990...8`,
  `1995` = `1995...9`,
  `2000` = `2000...10`,
  `2005` = `2005...11`,
  `2010` = `2010...12`,
  `2015` = `2015...13`,
  `2020` = `2020...14`,
  `2024` = `2024...15`
)
Stock_migratorio_masculino <- Stock_migratorio_masculino %>% rename(
  `1990` = `1990...16`,
  `1995` = `1995...17`,
  `2000` = `2000...18`,
  `2005` = `2005...19`,
  `2010` = `2010...20`,
  `2015` = `2015...21`,
  `2020` = `2020...22`,
  `2024` = `2024...23`
)
Stock_migratorio_femenino <- Stock_migratorio_femenino %>% rename(
  `1990` = `1990...24`,
  `1995` = `1995...25`,
  `2000` = `2000...26`,
  `2005` = `2005...27`,
  `2010` = `2010...28`,
  `2015` = `2015...29`,
  `2020` = `2020...30`,
  `2024` = `2024...31`
)

# Se pasan las columnas de los años a filas, bajo la columna Año y Cantidad
Stock_migratorio_mixto <- pivot_longer(Stock_migratorio_mixto, cols = `1990`:`2024`, names_to = "Año", values_to = "Cantidad")
Stock_migratorio_masculino <- pivot_longer(Stock_migratorio_masculino, cols = `1990`:`2024`, names_to = "Año", values_to = "Cantidad")
Stock_migratorio_femenino <- pivot_longer(Stock_migratorio_femenino, cols = `1990`:`2024`, names_to = "Año", values_to = "Cantidad")

#-----------------------------------------------Politicas migratorias----------------------------------------
# Leer la base Politicas Migratorias
Politicas_migratorias <- read_excel("Politicas_migratorias.xlsx")

# Crear columna de politicas
politica <- c()
for(k in 1:48){ 
  for(i in 1:5){ # Con las primeras 5 politicas
    for(j in 1:4){ # Agregar el id de cada una de estas 4 veces
      politica <- c(politica,i) # Al vector politica
    }
  }
  for (i in 6:30) { # Y apartir de la politica 6 hasta la 30
    for(j in 1:3){ # Agregar cada una 3 veces
      politica <- c(politica,i) # Al vector politica
    }
  }
} # Y repetir por cada uno de los grupos dentro de la BD

# Se pasan las columnas de los diferentes atributos a filas
Politicas_migratorias <- Politicas_migratorias %>% pivot_longer(cols = `Yes, regardless of immigration status...2`:`Data not available...96`, names_to = "Atributo", values_to = "Cantidad_paises")

# Se agrega la columna de politicas contruida con anterioridad a la base
Politicas_migratorias <- cbind(Politicas_migratorias, Id_politica = politica)

# Se re organiza la tabla
Politicas_migratorias <- Politicas_migratorias %>% relocate(Id_politica, .before = Atributo)

# Elimina "...número" al final de los textos en la columna 'Atributo' que pone R de forma automatica
Politicas_migratorias$Atributo <- gsub("\\.\\.\\.\\d+$", "", Politicas_migratorias$Atributo)

# Se traducen los atributos a español
Politicas_migratorias <- Politicas_migratorias %>%
  mutate(Atributo = case_when(
    Atributo == "Yes, regardless of immigration status" ~ "Sí, independientemente del estatus migratorio",
    Atributo == "Only for those with legal immigration status" ~ "Solo para quienes tienen estatus migratorio legal",
    Atributo == "No" ~ "No",
    Atributo == "Yes" ~ "Sí",
    Atributo == "Data not available" ~ "Datos no disponibles",
    TRUE ~ Atributo  # Para mantener lo que no coincida
  ))

# Se cambia el nombre de la primera columna a id_grupo
names(Politicas_migratorias)[1] <- "Id_grupo"

#------------------------------------------------Paises-------------------------------------------------------

# Se leen los paises dentro de cada base dada
Base1 <- read_excel("Politicas_migratorias_o.xlsx")
Base2 <- read_excel("Stock_migratorio_o.xlsx")
Base3 <- read_excel("Flujo_migratorio_o.xlsx")

# Se crean dos bases diferentes en base a la base de stock migratorio
Base4 <- Base2[, c("Region, development group, country or area of destination", "Location code of destination")]
Base2 <- Base2[, c("Region, development group, country or area of origin", "Location code of origin")]

# Se eliminan los duplicados de todas las bases
Base1 <- distinct(Base1)
Base2 <- distinct(Base2)
Base3 <- distinct(Base3)
Base4 <- distinct(Base4)

# Renombrar todas las bases a columnas iguales
Base1 <- rename(Base1, pais = `Region or development group`, id_pais = `Region or development group code`)
Base2 <- rename(Base2, pais = `Region, development group, country or area of origin`, id_pais = `Location code of origin`)
Base3 <- rename(Base3, pais = `CntName`)
Base4 <- rename(Base4, pais = `Region, development group, country or area of destination`, id_pais = `Location code of destination`)

# Unir todos los paises en una misma base
Paises <- bind_rows(Base1,Base2,Base3,Base4)

# Normalizar cada nombre
Paises$pais <- gsub("\\*$", "", Paises$pais) # Eliminar el * final que tienen varios paises
Paises <- Paises %>%
  mutate(pais = recode(pais,
                       "North Macedonia" = "The former Yugoslav Republic of Macedonia",
                       "Czechia" = "Czech Republic",
                       "United Kingdom" = "United Kingdom of Great Britain and Northern Ireland",
                       "WORLD" = "World")) # Cambiar el nombre algunos países con diferentes nombres entre tablas a un nombre común 

# Eliminar duplicados generales, por nombre y luego por id
Paises <- distinct(Paises)
Paises <- distinct(Paises, pais, .keep_all = T)
Paises <- distinct(Paises, id_pais, .keep_all = T)

# Crear base de grupos
Grupos <- Paises[Paises$id_pais >= 900, ]

# Crear base de paises
Paises <- Paises[Paises$id_pais < 900, ]

#-------------------------------------------Flujos migratorios------------------------------------------------

# Cargar la base de Flujos migratorios
flujo_migratorio <- read_excel("undesa_pd_2015_migration_flow_totals_Totals_.xlsx")

# Eliminar columna id
flujo_migratorio <- flujo_migratorio %>% select(-ID_flujo)

# Cambiar los nombres de los paises por los ids correspondientes en la tabla Paises
flujo_migratorio <- flujo_migratorio %>%
  left_join(
    Paises %>% select(pais, id_pais),
    by = c("CntName" = "pais")
  ) %>%
  select(-CntName) %>%
  relocate(id_pais, .before = Criterio)

# Pasar los años de columnas a filas
flujo_migratorio <- pivot_longer(flujo_migratorio, cols = `1980`:`2013`, names_to = "Año", values_to = "Cantidad")

#-------------------------------------------Excel final------------------------------------------------
# Guardar en excel las bases
write.xlsx(
  list(Paises = Paises, 
       Grupos = Grupos, 
       Politicas_migratorias = Politicas_migratorias, 
       Stock_migratorio_femenino = Stock_migratorio_femenino, 
       Stock_migratorio_masculino = Stock_migratorio_masculino, 
       Stock_migratorio_mixto = Stock_migratorio_mixto, 
       flujo_migratorio = flujo_migratorio),
  file = "BD_proyecto_Visualizacion.xlsx"
)
