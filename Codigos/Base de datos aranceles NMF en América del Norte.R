# Proyecto JSP (ult. act 14 sep20)
# Codigo para conformar base de datos de aranceles NMF entre paises de NA 

gc()
library(tidyverse)
library(readxl)
library(xlsx)

##### Lectura de bases de datos #####

WITS <- read_excel("Inputs/WITS_Tariff_2018_MEX-USA-CAN.xlsx")

# Clasificaciones de HS:  https://www.google.com/search?q=which+is+the+h5+classification+in+comtrade&oq=which+is+the+h5+classification+in+comtrade&aqs=chrome..69i57j33.13902j1j7&sourceid=chrome&ie=UTF-8
Clasif <- read_excel("Inputs/UN Comtrade Commodity Classifications.xlsx") %>% 
  filter(Classification %in% c('H4', 'H5')) %>%  #solo HS 2012 y 2017
  select(-Classification) %>% 
  distinct()

##### Procesamiento inicial de bases #####

WITS_proc <- WITS %>% 
  transmute(YEAR = `Tariff Year`, REP = `Reporter Name`, PAR = `Partner Name`, PROD_C = Product,
            PROD_N = `Product Name`, S_AVG = `Simple Average`, W_AVG = `Weighted Average`, 
            T_Lines = `Nbr of Total Lines`, IMP_VAL = `Imports Value in 1000 USD`) 

Lista_6dig <- Clasif %>% 
  filter(Level == '6') %>% 
  select(Code, Description)

Lista_2dig <- Clasif %>% 
  filter(Level == '2' & `Code Parent` == 'TOTAL') %>% 
  select(Code, Description) %>% 
  rename(Code_Group = Code, Group_Description = Description)

##### Conformacion de la base horizontal #####

WITS_wide <- WITS_proc %>% 
  select(REP, PROD_C, S_AVG) %>% 
  pivot_wider(names_from = REP, values_from = S_AVG) %>% 
  rename(MEX = Mexico, CAN = Canada, USA = `United States`) %>% 
  mutate(Code = ifelse(str_length(PROD_C) == 5,
                       str_c('0', PROD_C),
                       str_c(PROD_C))) %>% select(-PROD_C) %>% 
  distinct() %>% 
  left_join(Lista_6dig) %>% 
  mutate(Code_Group = str_sub(Code, 1, 2)) %>% 
  left_join(Lista_2dig) %>% 
  select(Code_Group, Group_Description, Code, Description, MEX, USA, CAN) %>% 
  replace_na(list(MEX = 'n.a', USA = 'n.a.', CAN = 'n.a.'))

write.csv(WITS_wide, 'Outputs/BD de aranceles NMF en NA (2018).csv', row.names = F)  


