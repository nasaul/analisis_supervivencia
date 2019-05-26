library(tidyverse)

datos_path <- "../datos/"

variables <- c("id", "ciclo",
               paste0("conf_", 1:3),
               paste0("sensor_", str_pad(1:23, width = 2, side = "left", pad = "0")))

train <- read_delim(paste0(datos_path, "train.txt"), delim = " ", col_names = variables,
                    col_types = cols(.deafult = col_double())) %>% 
         mutate(id = paste0("e", id),
                delta = 1L)

test <- read_delim(paste0(datos_path, "test.txt"), delim = " ", col_names = variables,
                    col_types = cols(.deafult = col_double())) %>% 
        mutate(id = paste0("c", id),
               delta = 0L)

bind_rows(train, test) %>% 
  select(-c(sensor_22, sensor_23)) %>% 
  write_rds(paste0(datos_path, "datos.rds"))
        