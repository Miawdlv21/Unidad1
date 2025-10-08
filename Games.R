#Archivo Kaggle - vgsales
vgventas <- read.csv("vgsales.csv")
head(vgventas)
str(vgventas)
summary(vgventas)

install.packages("dplyr")
library(dplyr)
#Géneros más vendidos
vgventas %>%
  group_by(Genre) %>%
  summarise(Ventas_Totales = sum(Global_Sales)) %>%
  arrange(desc(Ventas_Totales)) %>%
  head(5)

#Plataformas más vendidas
vgventas %>%
  group_by(Platform) %>%
  summarise(Ventas_Totales = sum(Global_Sales)) %>%
  arrange(desc(Ventas_Totales)) %>%
  head(5)

#Publisher más vendidos
vgventas %>%
  group_by(Publisher) %>%
  summarise(Ventas_Totales = sum(Global_Sales)) %>%
  arrange(desc(Ventas_Totales)) %>%
  head(5)

install.packages("ggplot2")
library(ggplot2)

# Primero, calculamos las ventas por año
ventas_por_ano <- vgventas %>%
  group_by(Year) %>%
  summarise(Ventas_Globales_Anuales = sum(Global_Sales))

# Luego, creamos el gráfico
ggplot(data = ventas_por_ano, aes(x = Year, y = Ventas_Globales_Anuales)) +
  geom_line() + # Gráfico de líneas
  geom_point() + # Puntos en cada año
  labs(title = "Evolución de las Ventas Globales de Videojuegos",
       x = "Año",
       y = "Ventas Globales (en millones)")
