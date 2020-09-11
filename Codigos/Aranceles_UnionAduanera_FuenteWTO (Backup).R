# Proyecto JSP (ult. act 10sep20)
# Codigo para estimar comercio NA que ocurre con aranceles iguales (Union aduanera) 
# de base de Tariff Download System y uniendo base de comercio

gc()
library(tidyverse)
library(readxl)
library(xlsx)

##### Lectura de bases de datos #####

WTO <- read_excel("Inputs/Tariff_Download_System_2019.xlsx", skip = 4)

##### Procesamiento inicial de base #####

WTO_proc <- WTO %>% 
  select(-c(`14`:`17`)) %>% 
  filter(HS_LEVEL == 6)

##### Base ancha para comparar tariffs #####

WTO_wide <- WTO_proc %>% 
  select(REP, HS_CODE, S_AVG) %>% 
  pivot_wider(names_from = REP, values_from = S_AVG) %>% 
  rename(USA = `United States of America`) %>% 
  replace_na(list(Canada = 0, Mexico = 0, USA = 0)) %>% 
  mutate(ind_CAN_MX = ifelse(Canada == Mexico, 1, 0),
         ind_CAN_USA = ifelse(Canada == USA, 1, 0),
         ind_MX_USA = ifelse(Mexico == USA, 1, 0)) %>% 
  mutate(ind_iguales = 
           case_when(ind_CAN_MX == 1 & ind_CAN_USA == 1 & ind_MX_USA == 1 ~ 'Iguales',
                     TRUE ~ 'Distintos'))

##### Tabla resumen #####

T_resum <- WTO_wide %>% 
  group_by(ind_iguales) %>% 
  summarise(n = n()) %>% 
  mutate(p_n = n/sum(n))

