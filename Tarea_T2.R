# Autor: Sebastián Navarrete Poblete
# Fecha: 22-09-2026   
# objetivo: Utilizar los datos casen reducidos para responder la siguiente 
# pregunta de investigacion economica: Los sectores mejor pagados 
# concentran a la gente con mayor educacion?

library(dplyr)
# 1 - pregunta economica: Los sectores mejor pagados 
# concentran a la gente con mayor educacion?

# 2. carga de datos casen 
casen <- read.csv("data/raw/casen_reducido.csv") # desde la raíz del proyecto
ingresos <- read.csv("data/raw/casen_ingresos.csv") # base de apoyo (punto 2b)

# revisar los datos variables dimesiones y NA°s

str(casen)    # variables
dim(casen)    # dimesiones de 60 y 6
summary(casen$ingreso)  # Datos minimos mediiana media quartil maximos y NA°s
sum(is.na(casen$ingreso))  # cantidad de NA°s

# 2b. Elige columnas por patrón

names(ingresos) # seleccion de columnas con ingresos 

names(select(ingresos, starts_with("ing")))  # seleccion columnas con "ing"
names(select(ingresos,  where(is.numeric)))  # seleccion por el tipo de contenido
# en este caso numerico

### 3. Usa los cinco verbos — al menos una vez cada uno

casen <- casen |>
  mutate(
    experiencia = pmax(edad - educ - 6, 0),
    superior = if_else(educ > 12, 1 ,0 ))

### 3b. Clasifica con `case_when()` 

casen <- casen |>
  mutate(
    nivel_educ = case_when(
      educ <  12 ~ "Sin media completa",   # crear mutate de nivel_educ 
      educ == 12 ~ "media completa ",      # para responder la pregunta
      TRUE       ~ "Superior"
    )
  )

summary(casen$experiencia)
table(casen$nivel_educ, useNA = "ifany") # Se verifica que este bien creado

# 4. Agrega con `group_by()`

resultado <- casen |>
  ungroup() |> 
  filter(!is.na(ingreso) & !is.na(educ))  |>           # ¿a quiénes necesitas? 
  group_by(sector,nivel_educ) |>          # ¿por qué grupo se parte la pregunta?
  summarise(
    n   = n() ,             # NUNCA lo omitas
    ingreso_medio = mean(ingreso) ) |> 
  arrange(desc(ingreso_medio))

resultado

### 5. Interpreta (esto es lo que más pesa)

# INTERPRETACIÓN:
# Los 3 sectores con mayores ingresos son educacion, servicios (nivel superior)
# y servicios (nivel media completa) con ingresos medios de 908857 CLP, 
# 866285 CLP y 802500 CLP respectivamente. y vemos que dentro del top 5 sectores
# con mayor cantidad de ingresos promedio 4 de estos pertenecen a personas con
# educacion superior por lo que se asocia una relacion correlativa entre
# los sectores que mayor ganan junto con los niveles de educacion
#
# LIMITACIÓN: La muestra no es muy representativa del total ya que todos los 
# valores n son menores a 8.

dir.create("data/processed", showWarnings = FALSE) # crear carpeta donde dejar
# los resultados
write.csv(resultado, "data/processed/casen_s5_derivadas.csv", row.names = FALSE) #
# guardos los nuevos datos casen

file.exists("data/processed/casen_s5_derivadas.csv") # comprobar que se haya creado


