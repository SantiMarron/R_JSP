# Proyecto JSP (ult. act 09sep20)
# Codigo para estimar comercio NA que ocurre con aranceles iguales (Union aduanera)

gc()
library(tidyverse)
library(readxl)
library(xlsx)

##### Lectura de bases de datos #####

WITS <- read_excel("Inputs/WITS_Tariff_2018_MEX-USA-CAN.xlsx")

##### Procesamiento inicial de base #####

WITS_proc <- WITS %>% 
  transmute(YEAR = `Tariff Year`, REP = `Reporter Name`, PAR = `Partner Name`, PROD = `Product Name`, 
         S_AVG = `Simple Average`, W_AVG = `Weighted Average`, T_Lines = `Nbr of Total Lines`,
         IMP_VAL = `Imports Value in 1000 USD`) 

##### Comparacion de simple avg tariffs #####

WITS_wide <- WITS_proc %>% 
  select(REP, PROD, S_AVG, IMP_VAL) %>% 
  pivot_wider(names_from = REP, values_from = c(S_AVG, IMP_VAL)) %>%
  replace_na(list(S_AVG_Mexico = 0, S_AVG_Canada = 0, `S_AVG_United States` = 0, 
                  IMP_VAL_Mexico = 0, IMP_VAL_Canada = 0,
                  `IMP_VAL_United States` = 0)) %>% #pongo cero a NAS, hace que no los pondere esos
  mutate(IMP_VAL = IMP_VAL_Mexico + IMP_VAL_Canada + `IMP_VAL_United States`) %>% 
  select(-IMP_VAL_Mexico, -IMP_VAL_Canada, -`IMP_VAL_United States`) %>%
  mutate(ind_MX_EU = ifelse(S_AVG_Mexico == `S_AVG_United States`, 1, 0),
         ind_MX_CAN = ifelse(S_AVG_Mexico == S_AVG_Canada, 1, 0),
         ind_EU_CAN = ifelse(`S_AVG_United States` == S_AVG_Canada, 1, 0)) %>% 
  mutate(ind_iguales =
           case_when(
             ind_MX_EU == 1 & ind_MX_CAN == 1 & ind_EU_CAN == 1 ~ 'Iguales',
             TRUE ~ 'Distintos'))

##### Tabla resumen #####

T_resum <- WITS_wide %>% 
  group_by(ind_iguales) %>% 
  summarise(NUM = n(),
            IMP_VAL = sum(IMP_VAL)) %>% 
  mutate(p_NUM = NUM/sum(NUM),
         p_VAL = IMP_VAL/sum(IMP_VAL))
