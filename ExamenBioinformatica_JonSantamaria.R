# cat genes.txt | wc -l


###########   RELLENAR



library(nycflights13)
library(tidyverse)

tiempo=nycflights13::weather
a=dim(tiempo) # tambien se puede hacer con nrow(tiempo) y ncol(tiempo)
print(paste0("El número de registros es ", a[1], " y el número de columnas es ", a[2]))


table(tiempo$origin)



LGA= tiempo[tiempo$origin== "LGA",]
median_LGA_wind_speed= median(LGA$wind_speed)
media_LGA_pressure= mean(LGA$pressure, na.rm = T)
print(paste0("En LGA, la mediana de wind_speed es ", median_LGA_wind_speed, " y la media de pressure es ", media_LGA_pressure))



tiempo_NA_gust= tiempo[complete.cases(tiempo$wind_gust),]

Medias_mes= tiempo_NA_gust %>%
  group_by(month)%>%
  summarise(media_wind_speed= mean(wind_speed),
            media_wind_gust= mean(wind_gust),
            n_casos=n())

View(Medias_mes)
print("En la data Medias_mes podemos observar la media de wind_speed y de wind_gunt para cada mes")



EWR= tiempo[tiempo$origin== "EWR",]
JFK= tiempo[tiempo$origin== "JFK",]
LGA= tiempo[tiempo$origin== "LGA",]

par(mfrow = c(1,3))
boxplot(EWR$temp~EWR$month, xlab = "Months", ylab = "ºC", main = "EWR", col = 2, pch=23)
boxplot(JFK$temp~JFK$month, xlab = "Months", ylab = "ºC", main = "JFK", col = 3,  pch=23)
boxplot(LGA$temp~LGA$month, xlab = "Months", ylab = "ºC", main = "LGA", col = 4,  pch=23)  



plot_meteo <- function(dat, meteo, mes, titulo, unidades, color)
{
  dat <- data.frame(dat)
  x <- dat[,meteo]
  y<- dat[,mes]
  boxplot(x~y, xlab = "Months", ylab = unidades, main = titulo, pch=23, col=color)
  media<- c(mean(EWR$temp, na.rm=T), mean(JFK$temp, na.rm=T), mean(LGA$temp, na.rm=T))
  print(media)
}

plot_meteo(dat=EWR, meteo ="temp", mes= "month",titulo = "EWR", unidades = "ºC", color=2)
plot_meteo(dat=JFK, meteo ="temp", mes= "month",titulo = "JFK", unidades = "ºC", color=3)
plot_meteo(dat=LGA, meteo ="temp", mes= "month",titulo = "LGA", unidades = "ºC", color=4)



cump_EWR= EWR[EWR$month== 9 & EWR$day== 29,]
cump_JFK= JFK[JFK$month== 9 & JFK$day== 29,]
cump_LGA= LGA[LGA$month== 9 & LGA$day== 29,]



cor(cump_EWR$temp, cump_EWR$humid)
ggplot(cump_EWR)+
  geom_point(aes(cump_EWR$temp, cump_EWR$humid))
print("podemos observar tanto graficamente como mediante el test de correlacion que hay una correlación inversa, es decir, que a mayor temperatura hay menor humedad")


cor(cump_JFK$temp, cump_JFK$humid)
ggplot(cump_JFK)+
  geom_point(aes(cump_JFK$temp, cump_JFK$humid))
print("podemos observar tanto graficamente como mediante el test de correlacion que hay una correlación inversa, es decir, que a mayor temperatura hay menor humedad")




cor(cump_LGA$temp, cump_LGA$humid)
ggplot(cump_LGA)+
  geom_point(aes(cump_LGA$temp, cump_LGA$humid))
print("podemos observar tanto graficamente como mediante el test de correlacion que hay una correlación inversa, es decir, que a mayor temperatura hay menor humedad")

print("finalmente observamos que en todos los origin hay una correlacion inversa entre temperatura y humedad el día 29 de semptiembre")


par(mfrow = c(1,1))
T_test=t.test(cump_JFK$temp, cump_LGA$temp)
Pvalor_T=T_test$p.value
boxplot(cump_JFK$temp,cump_LGA$temp, main= "Temperatura JFK vs LGA (29/09/2013)", ylab= "Temp", xlab="JFK                             LGA")
print(paste0("Como podemos ver las medias son diferentes siendo este hecho estadisticamente significativo debiado a que el p-valor es ", Pvalor_T))


# El Fc más grande en valores absolutos es el punto que se encuentra más a la izquierda a la altura del 5 del eje de las "y"
# El gen sobreexpresado mas significativamente es el Rbp1
