library(dplyr)
library(readxl)

recetas <- read_excel('./data/recetas.xlsx')
recetas_ing <- read_excel('./data/recetas-ing.xlsx')
recetas_prohibidas <- read_csv('./data/Recetas prohibidas - Hoja 1.csv', 
                               col_names = FALSE)

#summary(recetas)

dedeptoRank <- data_frame(
  depto = unique(recetas$depto)
) %>%
  mutate(deptoRank = 1:nrow(.))

recetas <- recetas %>%
  mutate(tiempo_mins = ifelse(is.na(tiempo_mins) & !is.na(tiempo_dias), 0, tiempo_mins),
         tiempo_dias = ifelse(is.na(tiempo_dias) & !is.na(tiempo_mins), 0, tiempo_dias),
         tiempo_mins = tiempo_mins + tiempo_dias * 24 * 60,
         price = runif(nrow(recetas), 1, 100)) %>%
  left_join(recetas_ing, by = 'uid') %>%
  select(uid, price, name = name.x, region, depto, instruc, dificultad, tiempo_mins, ing) %>%
  left_join(dedeptoRank, by = 'depto') %>%
  mutate(prohibida = ifelse(.$name %in% recetas_prohibidas$X1, TRUE, FALSE))

saveRDS(recetas, file = "data/recetas.Rda")


