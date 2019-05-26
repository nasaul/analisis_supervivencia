library(survival)
# library(KMsurv)
# library(actuar)
# library(BGPhazard)
# library(rmutil)
library(tidyverse)

datos_path <- "../datos/"

datos <- read_rds(paste0(datos_path, "datos.rds"))

max_ciclo <- datos %>% 
              group_by(id) %>% 
              summarise(max_ciclo = max(ciclo),
                        delta = max(delta)) %>% 
              arrange(max_ciclo)

tiempo <- Surv(max_ciclo$max_ciclo, max_ciclo$delta)

xfit <- survfit(tiempo ~ 1, conf.type = "log-log")

xfit_tbl <- with(xfit, tibble(time, n.risk, n.event, n.censor, surv, lower, upper))

xfit_tbl %>%   
  ggplot(aes(x = time, y = surv)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_line(color = "steelblue", size = 1) +
  labs(title = "Función de supervivencia",
       x = "Ciclos",
       y = "Probabilidad") +
  theme(plot.title = element_text(hjust = 0.5))
  
datos %>%
  gather(key = sensor, value = medicion, starts_with("sensor"))
