library(readr)
library(dplyr)
library(rstan)
rstan_options(auto_write = TRUE)

variables <- c("id", "ciclo",
               paste0("conf_", 1:3),
               paste0("sensor_", 1:23))

train <- read_delim(
  paste0("datos/train.txt"),
  delim = " ",
  col_names = variables,
  col_types = cols(.deafult = col_double())
) 

size_df <- train %>% 
  group_by(id) %>% 
  summarise(ciclo = max(ciclo)) %>% 
  mutate(y = 1L)

df <- left_join(
  train,
  size_df
) %>% 
  select(
    -c(sensor_22, sensor_23)
  ) %>% 
  mutate(
    y = if_else(is.na(y), 0, 1)
  )


model <- stan_model(
  here::here("stan/model.stan")
)

model_estimation <- sampling(
  model,
  data = list(
    N     = max(size_df$id),
    max_t = max(size_df$ciclo),
    dim_y = length(df$y),
    Time  = as.integer(size_df$ciclo),
    Y     = as.integer(df$y)
  ),
  iter   = 500,
  warmup = 200,
  chains = 1,
  seed   = 12345 
)

model_estimation
