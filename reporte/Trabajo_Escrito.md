# Proyecto Final - Bioestadística

- Vícto Samayoa - 175750
- Saúl Caballero - 133930
- Delia Del Águila - 167188

## Introducción
Se obtuvo la base de datos usada para la competencia de desafío de pronóstico en la Conferencia Internacional sobre pronóstico y gestión de la salud (PHM08). [1]

La base consiste en múltiples series de tiempo multivariadas. Cada serie es de un motor diferente pero de un mismo tipo. Cada motor arranca con diferentes grados de desgaste inicial y variación de fabricación que es desconocido para el usuario. Este desgaste y variación se considera normal, es decir, no se considera una condición de falla. Hay tres configuraciones operativas que tienen un efecto sustancial en el rendimiento del motor, además se cuentan con medidas de 211 sensores en cada ciclo.
El motor funciona normalmente al inicio de cada serie de tiempo y comienza a degradarse en algún momento durante la serie. El objetivo será crear la función de supervivencia para los motores donde el evento de falla será cuando el motor no pueda seguir funcionando y tenga que ser mandado a mantenimiento.

## Análisis de Datos
Nuestra base de datos cuenta con 75,738 registros correspondientes a 436 motores, donde cada registro corresponde a un ciclo de un motor en específico e incluye las configuraciones iniciales del ciclo así como las mediciones de los sensores.

Contamos con 50% de censura por la derecha, donde si el motor no tiene censura su último registro será del último ciclo antes de mantenimiento. En la siguiente gráfica podemos observar la distribución del tiempo en que cada motor está en funcionamiento o en qué ciclo se detuvieron las mediciones, es decir en qué ciclo la censura comenzó.







Fig 1. Ciclos en funcionamiento hasta censura


Para el caso de motores sin censura, en la siguiente gráfica podemos observar la distribución del tiempo en que cada motor está en funcionamiento y en qué ciclo es mandado a mantenimiento dada una falla.







Fig 2. Ciclos en funcionamiento hasta mantenimiento


Nuestra base de datos también incluye 3 configuraciones que hacen los operadores al inicio de cada ciclo. Cada configuración tiene un rango distinto, la configuración uno va de 0 a 42, la dos de 0 a 0.84 y la tres de 0 a 100. La siguiente gráfica muestra de ejemplo al motor e5 y de las distintas configuraciones definidas en cada ciclo.


Fig 3. Configuraciones por ciclo para motor e5


Cada configuración tiene mediciones en la siguiente escala:

Configuración 1: De 0 a 42.1
Configuración 2: De 0 a 0.842
Configuración 3: Valores discretos de 0 a 100 con saltos de 20 puntos 


Fig 4. Boxplot para las configuraciones de cada ciclo


Además contamos con las lecturas de 21 sensores en cada ciclo. La siguiente gráfica muestra de nuevo el ejemplo del motor e5 pero con las distintas lecturas de los sensores en cada ciclo.







Fig 5. Lectura de sensores por ciclo para motor e5


En general cada sensor tiene mediciones en la siguiente escala:

Sensor 16: Cuenta con dos valores 0.02 y 0.03
Sensor 10: De 0.93 a 1.3
Sensores 5, 6 y 15: De 3.91 a 21.61
Sensores 11, 20 y 21: De 6.124 a 48.39
Sensor 19: Cuenta con sólo dos valores 84.93 y 100
Sensores 1, 2, 7 y 12:  De 129.2 a 644.4
Sensor 17: Con valores enteros entre 303 y 398
Sensores 3 y 4: De 1029 a 1615
Sensores 8 y 13: De 1915 a 2391
Sensor 18: Con valores enteros entre 1915 y 2388
Sensores 9 y 14: De 7852 a 9217


Fig 6. Boxplot para todas las lecturas de sensores

## Bibliografía

 [1] https://ti.arc.nasa.gov/tech/dash/groups/pcoe/prognostic-data-repository/
 
 [2] Package ‘TraMineR’.  2019. http://traminer.unige.ch/preview.shtml



## Apéndice

#Análisis de Datos

#Cargo de librerias
library(dplyr)
library(sqldf)
library(ggplot2)
library(ggthemes)

#Contamos con 75,738 registros de ciclos de motores
datos <- as.data.frame(select(readRDS("datos.rds"), "id":"sensor_21", "delta"))   
nrow(datos)

#No tenemos registros con NA's
datos[is.na(datos),]

#Tampoco tenemos registros duplicados
datos[duplicated(datos), ]

#Nuestro archivo cuenta con 29 columnas:
#- ID: El identificador único por motor
#- Ciclo: Indica de qué ciclo son las leturas pertenecientes a la motor indicada en el campo "ID"
#- Conf_[n]: Con n = 1,2,3. Configuración hecha por el operador al inicio del ciclo de la motor
#- Sensor_[m]: Con m = 1,2,...,21. Lectura de sensores
#- Delta: Indica si los registros contienen censura (0) o no (1)
glimpse(datos)

#Analizando cada variable.
#ID: Nuestra base cuenta con registros de 436 motores
datos$id <- as.factor(datos$id)
length(levels(datos$id))

#Delta: De las 436 motores, el 50% (218) cuenta con censura a la derecha y el otro 50% de motores tiene datos exactos
sqldf("select count(distinct id) as maquinas
      from datos 
      where delta=1")

#Ciclo: Analizando solo los datos exactos vemos que la cantidad mìnima de ciclos es 128 y la máxima es 357, además su distribución está sesgada a la derecha alrededor de 209 ciclos
ciclos <- datos %>% filter(delta==1) %>% count(id) %>% select(maquina=id, ciclos=n)
summary(ciclos)

ggplot(ciclos) + 
  geom_histogram(aes(x = ciclos, y = (..count..)/sum(..count..)), 
                 bins = 15, fill = "#6497b1", colour = "black", ) +
  theme_gdocs() +
  labs(title = "Número de ciclos hasta mantenimiento",
       subtitle = "Datos exactos (Sin censura)",
       x = "Número de ciclos hasta mantenimiento", 
       y = "Frecuencia Relativa")  

#Veamos que como es esperado las maquinas con censura tienen una cantidad de ciclos menor a la de los registros sin censura 
tciclos <- sqldf("select distinct delta, id as maquina, count(distinct ciclo) as ciclos
                 from datos
                 group by 1,2")

rep.col<-function(x,n){
   matrix(rep(x,each=n), ncol=n, byrow=TRUE)
}

ciclo_max <- max(tciclos$ciclos)
nmaquinas <- nrow(tciclos)

ciclos_detalle <- cbind(tciclos$ciclos[[1]],tciclos$delta[[1]],tciclos$maquina[[1]],as.data.frame(rep.col(1, tciclos$ciclos[[1]])),as.data.frame(rep.col(0,ciclo_max-tciclos$ciclos[[1]])))
colnames(ciclos_detalle) <- c("ciclos","delta","maquina",paste("ciclo_",1:ciclo_max,sep="")) 

for (i in 2:nmaquinas) {
  if(tciclos$ciclos[[i]]!=ciclo_max) {
    aux <- cbind(tciclos$ciclos[[i]],tciclos$delta[[i]],tciclos$maquina[[i]],as.data.frame(rep.col(1, tciclos$ciclos[[i]])),as.data.frame(rep.col(0,ciclo_max-tciclos$ciclos[[i]]))) }
  else {
    aux <- cbind(tciclos$ciclos[[i]],tciclos$delta[[i]],tciclos$maquina[[i]],as.data.frame(rep.col(1, tciclos$ciclos[[i]])))
  }
  colnames(aux) <- c("ciclos","delta","maquina",paste("ciclo_",1:ciclo_max,sep="")) 
  ciclos_detalle <- rbind(ciclos_detalle, aux)
}

ciclos_seq <- seqdef(ciclos_detalle, 4:(ciclo_max+3), labels = c('censura','funcionando'), 
                     cpal = c("#b3cde0", "#011f4b"))

seqiplot(ciclos_seq, idxs=0, sortv=ciclos_detalle$ciclos, border=NA, space=0, with.legend=F, 
         group=ciclos_detalle$delta, main='Ciclos en funcionamiento') 
seqlegend(ciclos_seq) 

#Conf_[n]: Los operadores realizan tres configuraciones antes de cada ciclo.
summary(datos %>% select(conf_1, conf_2, conf_3))

ggplot(datos %>% filter(id=='e5'), aes(ciclo)) +
  geom_line(aes(y = conf_1, colour = "conf_1")) + 
  geom_line(aes(y = conf_2, colour = "conf_2")) + 
  geom_line(aes(y = conf_3, colour = "conf_3")) + 
  theme_gdocs() +
  labs(title = "Configuraciones previas al ciclo",
       subtitle = "Ejemplo: motor e5",
       x = "Ciclo", 
       y = "Configuración")  

conf1 <- melt(datos, id.vars='id', measure.vars=c('conf_1','conf_3'))

ggplot(conf1, aes(x=variable, y=value)) + 
  geom_boxplot(fill = "#6497b1") +
  theme_gdocs() +
  coord_flip() +
  labs(title = "Configuración 1 y 3",
       subtitle = "Todos los ciclos",
       x = "Tipo de configuración", 
       y = "Valor establecido para la configuración")

conf2 <- melt(datos, id.vars='id', measure.vars=c('conf_2'))

ggplot(conf2, aes(x=variable, y=value)) + 
  geom_boxplot(fill = "#6497b1") +
  theme_gdocs() +
  coord_flip() +
  labs(title = "Configuración 2",
       subtitle = "Todos los ciclos",
       x = "Tipo de configuración", 
       y = "Valor establecido para la configuración")

#Sensores
ggplot(datos %>% filter(id=='e5'), aes(ciclo)) +
  geom_line(aes(y = sensor_1, colour = "sensor_1")) +
  geom_line(aes(y = sensor_2, colour = "sensor_2")) +
  geom_line(aes(y = sensor_3, colour = "sensor_3")) +
  geom_line(aes(y = sensor_4, colour = "sensor_4")) +
  geom_line(aes(y = sensor_5, colour = "sensor_5")) +
  geom_line(aes(y = sensor_6, colour = "sensor_6")) +
  geom_line(aes(y = sensor_7, colour = "sensor_7")) +
  geom_line(aes(y = sensor_8, colour = "sensor_8")) +
  geom_line(aes(y = sensor_9, colour = "sensor_9")) +
  geom_line(aes(y = sensor_10, colour = "sensor_10")) +
  geom_line(aes(y = sensor_11, colour = "sensor_11")) +
  geom_line(aes(y = sensor_12, colour = "sensor_12")) +
  geom_line(aes(y = sensor_13, colour = "sensor_13")) +
  geom_line(aes(y = sensor_14, colour = "sensor_14")) +
  geom_line(aes(y = sensor_15, colour = "sensor_15")) +
  geom_line(aes(y = sensor_16, colour = "sensor_16")) +
  geom_line(aes(y = sensor_17, colour = "sensor_17")) +
  geom_line(aes(y = sensor_18, colour = "sensor_18")) +
  geom_line(aes(y = sensor_19, colour = "sensor_19")) +
  geom_line(aes(y = sensor_20, colour = "sensor_20")) +
  geom_line(aes(y = sensor_21, colour = "sensor_21")) +
  theme_gdocs() +
  labs(title = "Lectura de sensores por ciclo",
       subtitle = "Ejemplo: motor e5",
       x = "Ciclo", 
       y = "Lectura de Sensor")  

sensor1 <- melt(datos, id.vars='id', measure.vars=colnames(datos)[6:26])

ggplot(sensor1, aes(x=variable, y=value)) + 
  geom_boxplot(fill = "#6497b1") +
  theme_gdocs() +
  coord_flip() +
  labs(title = "Medición de sensores",
       subtitle = "Todos los ciclos",
       x = "Sensor", 
       y = "Lectura del Sensor")

sensor2 <- melt(datos, id.vars='id', measure.vars=c('sensor_16'))

ggplot(sensor2, aes(x=variable, y=value)) + 
  geom_boxplot(fill = "#6497b1") +
  theme_gdocs() +
  coord_flip() +
  labs(title = "Medición de sensor 16",
       subtitle = "Todos los ciclos",
       x = "Sensor", 
       y = "Lectura del Sensor")

sensor3 <- melt(datos, id.vars='id', measure.vars=c('sensor_10'))

ggplot(sensor3, aes(x=variable, y=value)) + 
  geom_boxplot(fill = "#6497b1") +
  theme_gdocs() +
  coord_flip() +
  labs(title = "Medición de sensor 10",
       subtitle = "Todos los ciclos",
       x = "Sensor", 
       y = "Lectura del Sensor")

sensor4 <- melt(datos, id.vars='id', measure.vars=c('sensor_5', 'sensor_6', 'sensor_15'))

ggplot(sensor4, aes(x=variable, y=value)) + 
  geom_boxplot(fill = "#6497b1") +
  theme_gdocs() +
  coord_flip() +
  labs(title = "Medición de sensores 5, 6 y 15",
       subtitle = "Todos los ciclos",
       x = "Sensor", 
       y = "Lectura del Sensor")

sensor5 <- melt(datos, id.vars='id', measure.vars=c('sensor_11', 'sensor_20', 'sensor_21'))

ggplot(sensor5, aes(x=variable, y=value)) + 
  geom_boxplot(fill = "#6497b1") +
  theme_gdocs() +
  coord_flip() +
  labs(title = "Medición de sensores 11, 20 y 21",
       subtitle = "Todos los ciclos",
       x = "Sensor", 
       y = "Lectura del Sensor")

sensor6 <- melt(datos, id.vars='id', measure.vars=c('sensor_19'))

ggplot(sensor6, aes(x=variable, y=value)) + 
  geom_boxplot(fill = "#6497b1") +
  theme_gdocs() +
  coord_flip() +
  labs(title = "Medición de sensor 19",
       subtitle = "Todos los ciclos",
       x = "Sensor", 
       y = "Lectura del Sensor")

sensor7 <- melt(datos, id.vars='id', measure.vars=c('sensor_1', 'sensor_2', 'sensor_7', 'sensor_12', 'sensor_17'))

ggplot(sensor7, aes(x=variable, y=value)) + 
  geom_boxplot(fill = "#6497b1") +
  theme_gdocs() +
  coord_flip() +
  labs(title = "Medición de sensores 1, 2, 7, 12 y 17",
       subtitle = "Todos los ciclos",
       x = "Sensor", 
       y = "Lectura del Sensor")

sensor8 <- melt(datos, id.vars='id', measure.vars=c('sensor_3', 'sensor_4'))

ggplot(sensor8, aes(x=variable, y=value)) + 
  geom_boxplot(fill = "#6497b1") +
  theme_gdocs() +
  coord_flip() +
  labs(title = "Medición de sensores 3 y 4",
       subtitle = "Todos los ciclos",
       x = "Sensor", 
       y = "Lectura del Sensor")

sensor9 <- melt(datos, id.vars='id', measure.vars=c('sensor_8', 'sensor_13', 'sensor_18'))

ggplot(sensor9, aes(x=variable, y=value)) + 
  geom_boxplot(fill = "#6497b1") +
  theme_gdocs() +
  coord_flip() +
  labs(title = "Medición de sensores 8, 13 y 18",
       subtitle = "Todos los ciclos",
       x = "Sensor", 
       y = "Lectura del Sensor")

sensor10 <- melt(datos, id.vars='id', measure.vars=c('sensor_9', 'sensor_14'))

ggplot(sensor10, aes(x=variable, y=value)) + 
  geom_boxplot(fill = "#6497b1") +
  theme_gdocs() +
  coord_flip() +
  labs(title = "Medición de sensores 9 y 14",
       subtitle = "Todos los ciclos",
       x = "Sensor", 
       y = "Lectura del Sensor")




