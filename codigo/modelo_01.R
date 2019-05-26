library(survival)
# library(KMsurv)
# library(actuar)
# library(BGPhazard)
# library(rmutil)
library(tidyverse)

datos_path <- "../datos/"

datos <- read_rds(paste0(datos_path, "datos.rds"))

datos_sel <- datos %>% 
             select(-c(sensor_02, sensor_04, sensor_05, sensor_06, sensor_07, sensor_09, sensor_10,
                       sensor_11, sensor_12, sensor_14, sensor_15, sensor_16, sensor_17, sensor_19,
                       sensor_20, sensor_21))

datos_norm <- datos_sel %>%
              gather(key = variable, value = medicion, c(starts_with("conf"), starts_with("sensor"))) %>% 
              group_by(variable) %>% 
              mutate(media = mean(medicion),
                     desv_s = sd(medicion),
                     medicion = (medicion - media)/desv_s) %>%
              ungroup() %>% 
              select(-c(media, desv_s)) %>% 
              spread(key = variable, value = medicion)

max_ciclo <- datos %>% 
              group_by(id) %>% 
              summarise(max_ciclo = max(ciclo),
                        delta = max(delta)) %>% 
              arrange(max_ciclo, delta)

datos_agg <- max_ciclo %>%
             left_join(datos, by = c("id", "max_ciclo" = "ciclo", "delta"))




#-Extraccion de tiempos de fallo y supervivencia-
tj <- xfit$time
sj <- xfit$surv
k <- length(tj)
sje <- 0.5 * sj + 0.5 * c(1, sj[-k])

tj2 <- c(0, tj[-k])
sj2 <- c(1, sj[-k])

#-Verificacin de supuesto exponencial-
plot(tj,log(sje))

#-Verificacin de supuesto weibull-
plot(log(tj),log(-log(sje)))

#-Verificacin de supuesto lognormal-
qqnorm(log(tj))



#################################################################################################
# Ajuste del modelo vida acelerada
xfit_rW1 <- survreg(tiempo ~ datos_agg$conf_1 + datos_agg$conf_2 + datos_agg$conf_3, dist = "weibull")
summary(xfit_rW1)

xfit_rW2 <- survreg(tiempo ~ 
                     datos_agg$sensor_01 + datos_agg$sensor_03 + datos_agg$sensor_08 +
                     datos_agg$sensor_13 + datos_agg$sensor_18,
                    #control = list(maxiter = 5000, outer.max = 100, rel.tolerance = 1e-05, toler.chol = 1e-06),
                    dist = "weibull")
summary(xfit_rW2)

xfit_rloglog1 <- survreg(tiempo ~ datos_agg$conf_1 + datos_agg$conf_2 + datos_agg$conf_3, dist = "loglogistic")
summary(xfit_rloglog1)

xfit_rloglog2 <- survreg(tiempo ~ 
                     datos_agg$sensor_01 + datos_agg$sensor_03 + datos_agg$sensor_08 +
                     datos_agg$sensor_13 + datos_agg$sensor_18,
                    #control = list(maxiter = 5000, outer.max = 100, rel.tolerance = 1e-05, toler.chol = 1e-06),
                    dist = "loglogistic")
summary(xfit_rloglog2)

xfit_rlognorm1 <- survreg(tiempo ~ datos_agg$conf_1 + datos_agg$conf_2 + datos_agg$conf_3, dist = "lognormal")
summary(xfit_rlognorm1)

xfit_rlognorm2 <- survreg(tiempo ~ 
                     datos_agg$sensor_01 + datos_agg$sensor_03 + datos_agg$sensor_08 +
                     datos_agg$sensor_13 + datos_agg$sensor_18,
                    #control = list(maxiter = 5000, outer.max = 100, rel.tolerance = 1e-05, toler.chol = 1e-06),
                    dist = "lognormal")
summary(xfit_rlognorm2)






datos %>% 
  gather(key = variable, value = medicion, c(starts_with("conf"), starts_with("sensor"))) %>% 
  ggplot(aes(x = medicion)) +
  geom_density() +
  facet_wrap(vars(variable), scales = "free")





xfitp <- survreg(tiempo ~ 1, dist = "lognormal")
#xpredp<-predict(xfitp)
pct <- 1:98/100
xpredp<-predict(xfitp,type="quantile",p=pct, se=TRUE)

ajuste <- tibble(ciclo = xpredp$fit[1,],
                 weibull = 1-pct,
                 inferior = xpredp$fit[1,]-2*xpredp$se.fit[1,],
                 superior = xpredp$fit[1,]+2*xpredp$se.fit[1,])

ajuste %>% 
  ggplot(aes(y = weibull)) +
  geom_line(data = ajuste, aes(x = ciclo), color = "firebrick", size = 1) +
  geom_line(data = ajuste, aes(x = inferior), color = "firebrick", size = 1, linetype = 2) +
  geom_line(data = ajuste, aes(x = superior), color = "firebrick", size = 1, linetype = 2) +
  labs(title = "Función de supervivencia",
       x = "Ciclos",
       y = "Probabilidad")






ajuste <- tibble(ciclo = xpredp$fit[1,],
                 ajuste = 1-pct,
                 inferior = xpredp$fit[1,]-2*xpredp$se.fit[1,],
                 superior = xpredp$fit[1,]+2*xpredp$se.fit[1,])

xfit_tbl %>%   
  ggplot(aes(x = time, y = surv)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_line(color = "steelblue", size = 1) +
  geom_line(data = ajuste, aes(x = ciclo, y = ajuste), color = "firebrick", size = 1) +
  geom_line(data = ajuste, aes(x = inferior, y = ajuste), color = "firebrick", linetype = 2) +
  geom_line(data = ajuste, aes(x = superior, y = ajuste), color = "firebrick", linetype = 2) +
  labs(title = "Función de supervivencia",
       x = "Ciclos",
       y = "Probabilidad")






tabla$table %>% 
  as_tibble(rownames = "coef")
