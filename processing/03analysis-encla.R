# Code 3: Analysis ENCLA 
# Gran parte de los códigos son reproducidos a partir del github del investigador Nicolás Ratto 

# 1. Cargar librerias ----
if (!require("pacman"))install.packages("pacman")

pacman::p_load(tidyverse, rvest, xml2, lubridate, openxlsx, readxl, ggrepel, tibble, writexl, haven,
               dplyr, car, summarytools, ggpubr, sjmisc, sjlabelled, stargazer, srvyr)

options(scipen=999)

# 2. Cargar datos ----

load("C:/Users/PC/Downloads/enclas.RData")

# 3. Procesamiento ----

# 3.1. ENCLA 1998 ----

# var. tamaño original: 1=micro, 2=pequeña, 3=mediana, 4=gran
encla1998_e$tamaño <- as_factor(encla1998_e$tamaño)
encla1998_e <- encla1998_e %>% mutate(tamaño2 = case_when(tamaño == 1 ~ "Pequeña",
                                                          tamaño == 2 ~ "Pequeña",
                                                          tamaño == 3 ~ "Mediana",
                                                          tamaño == 4 ~ "Gran"))

encla1998_e <- encla1998_e %>% mutate(rama2 = case_when(rama == NA ~ "Actividad no especificada",
                                                        rama == 1 ~ "Agricultura y pesca", 
                                                        rama == 2 ~ "Mineria", 
                                                        rama == 3 ~ "Industria",
                                                        rama == 4 ~ "Electricidad, gas y agua", 
                                                        rama == 5 ~ "Construccion", 
                                                        rama == 6 ~ "Comercio y hoteleria", 
                                                        rama == 7 ~ "Transporte y comunicaciones", 
                                                        rama == 8 ~ "Establecimientos financieros",
                                                        rama == 9 ~ "Servicios comunales"))


## Cantidad empresa
encla1998_e %>% group_by(sindicato) %>% tally() %>% 
  pivot_wider(names_from =sindicato,values_from = n) %>% 
  rename(sin_sindicato=`1`, con_sindicato=`0`)%>%
  select(sin_sindicato, con_sindicato) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato),
         ano = "1998")

## tamaño
a <- encla1998_e %>% group_by(sindicato, tamaño2) %>% tally() %>% 
  pivot_wider(names_from =sindicato,values_from = n) %>% 
  rename(sin_sindicato=`1`, con_sindicato=`0`)%>%
  select(sin_sindicato, con_sindicato, tamaño2) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato),
         ano = "1998")

## sector y tamaño

## pequeña
a_pequena <- encla1998_e %>% group_by(sindicato, rama2)  %>% filter(tamaño2 == 'Pequeña')  %>% tally() %>% 
  pivot_wider(names_from =sindicato,values_from = n) %>% 
  rename(sin_sindicato=`1`, con_sindicato=`0`)%>%
  select(sin_sindicato, con_sindicato, rama2) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato),
         ano = "1998")

## mediana
a_mediana <- encla1998_e %>% group_by(sindicato, rama2) %>% filter(tamaño2 == 'Mediana') %>% 
  tally() %>% pivot_wider(names_from =sindicato,values_from = n) %>% 
  rename(sin_sindicato=`1`, con_sindicato=`0`)%>%
  select(sin_sindicato, con_sindicato, rama2) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato),
         ano = "1998")

## gran
a_gran <- encla1998_e %>% group_by(sindicato, rama2) %>% filter(tamaño2 == 'Gran') %>% 
  tally() %>% pivot_wider(names_from =sindicato,values_from = n) %>% 
  rename(sin_sindicato=`1`, con_sindicato=`0`)%>%
  select(sin_sindicato, con_sindicato, rama2) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato),
         ano = "1998")


# 3.2. ENCLA 1999 ----

# var. tamaño original: 1=micro, 2=pequeña, 3=mediana, 4=gran
encla1999_e$tamaño <- as_factor(encla1999_e$tamaño)
encla1999_e <- encla1999_e %>% mutate(tamaño2 = case_when(tamaño == 1 ~ "Pequeña",
                                                          tamaño == 2 ~ "Pequeña",
                                                          tamaño == 3 ~ "Mediana",
                                                          tamaño == 4 ~ "Gran"))

encla1999_e <- encla1999_e %>% mutate(rama2 = case_when(rama == NA ~ "Actividad no especificada",
                                                        rama == 1 ~ "Agricultura y pesca", 
                                                        rama == 2 ~ "Mineria", 
                                                        rama == 3 ~ "Industria",
                                                        rama == 4 ~ "Electricidad, gas y agua", 
                                                        rama == 5 ~ "Construccion", 
                                                        rama == 6 ~ "Comercio y hoteleria", 
                                                        rama == 7 ~ "Transporte y comunicaciones", 
                                                        rama == 8 ~ "Establecimientos financieros",
                                                        rama == 9 ~ "Servicios comunales"))

# Cantidad en empresa

encla1999_e %>% group_by(sindical) %>% 
  tally() %>% pivot_wider(names_from =sindical,values_from = n)%>% 
  rename(sin_sindicato=`0`,con_sindicato=`1`,sin_respuesta=`NA`) %>% 
  mutate(sin_respuesta=as.numeric(sin_respuesta)) %>% 
  mutate(sin_respuesta=if_else(is.na(sin_respuesta),0,sin_respuesta)) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato+sin_respuesta),
         ano = "1999")

# tamaño
b <- encla1999_e %>% group_by(sindical, tamaño2) %>% 
  tally() %>% pivot_wider(names_from =sindical,values_from = n)%>% 
  rename(sin_sindicato=`0`,con_sindicato=`1`,sin_respuesta=`NA`) %>% 
  mutate(sin_respuesta=as.numeric(sin_respuesta)) %>% 
  mutate(sin_respuesta=if_else(is.na(sin_respuesta),0,sin_respuesta)) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato+sin_respuesta),
         ano = "1999") %>% select(-sin_respuesta)

## sector y tamaño

## pequeña
b_pequena <- encla1999_e %>% group_by(sindical, rama2) %>% filter(tamaño2 == 'Pequeña') %>%
  tally() %>% pivot_wider(names_from =sindical,values_from = n)%>% 
  rename(sin_sindicato=`0`,con_sindicato=`1`,sin_respuesta=`NA`) %>% 
  mutate(sin_respuesta=as.numeric(sin_respuesta)) %>% 
  mutate(sin_respuesta=if_else(is.na(sin_respuesta),0,sin_respuesta)) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato+sin_respuesta),
         ano = "1999") %>% select(-sin_respuesta)

## mediana
b_mediana <- encla1999_e %>% group_by(sindical, rama2) %>% filter(tamaño2 == 'Mediana') %>%
  tally() %>% pivot_wider(names_from =sindical,values_from = n)%>% 
  rename(sin_sindicato=`0`,con_sindicato=`1`) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato),
         ano = "1999")


## gran
b_gran <- encla1999_e %>% group_by(sindical, rama2) %>% filter(tamaño2 == 'Gran') %>%
  tally() %>% pivot_wider(names_from =sindical,values_from = n)%>% 
  rename(sin_sindicato=`0`,con_sindicato=`1`,sin_respuesta=`NA`) %>% 
  mutate(sin_respuesta=as.numeric(sin_respuesta)) %>% 
  mutate(sin_respuesta=if_else(is.na(sin_respuesta),0,sin_respuesta)) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato+sin_respuesta),
         ano = "1999") %>% select(-sin_respuesta)


# 3.3. ENCLA 2002 ----
# var. tamaño: 1=micro, 2=pequeña, 3=mediana, 4=grande
encla2002_e$tamaño <- as_factor(encla2002_e$tamaño)
encla2002_e <- encla2002_e %>% mutate(tamaño2 = case_when(tamaño == 1 ~ "Pequeña",
                                                          tamaño == 2 ~ "Pequeña",
                                                          tamaño == 3 ~ "Mediana",
                                                          tamaño == 4 ~ "Gran"))

encla2002_e <- encla2002_e %>% mutate(rama2 = case_when(rama == NA ~ "Actividad no especificada",
                                                        rama == 1 ~ "Agricultura y pesca", 
                                                        rama == 2 ~ "Mineria", 
                                                        rama == 3 ~ "Industria",
                                                        rama == 4 ~ "Electricidad, gas y agua", 
                                                        rama == 5 ~ "Construccion", 
                                                        rama == 6 ~ "Comercio y hoteleria", 
                                                        rama == 7 ~ "Transporte y comunicaciones", 
                                                        rama == 8 ~ "Establecimientos financieros",
                                                        rama == 9 ~ "Servicios comunales"))

# Cantidad en empresa
encla2002_e %>% group_by(sindical) %>% 
  tally() %>% pivot_wider(names_from =sindical,values_from = n) %>% 
  rename(sin_sindicato=`0`,con_sindicato=`1`) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato),
         ano = "2002")

# tamaño
c <- encla2002_e %>% group_by(sindical, tamaño2) %>% 
  tally() %>% pivot_wider(names_from =sindical,values_from = n) %>% 
  rename(sin_sindicato=`0`,con_sindicato=`1`) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato),
         ano = "2002") %>% filter(!is.na(tamaño2))

# sector y tamaño

## pequeña
c_pequena <- encla2002_e %>% group_by(sindical, rama2) %>% filter(tamaño2 == 'Pequeña') %>%
  tally() %>% pivot_wider(names_from =sindical,values_from = n) %>% 
  rename(sin_sindicato=`0`,con_sindicato=`1`) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato),
         ano = "2002")

## mediana
c_mediana <- encla2002_e %>% group_by(sindical, rama2) %>% filter(tamaño2 == 'Mediana') %>%
  tally() %>% pivot_wider(names_from =sindical,values_from = n) %>% 
  rename(sin_sindicato=`0`,con_sindicato=`1`) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato),
         ano = "2002")

## gran
c_grande <- encla2002_e %>% group_by(sindical, rama2) %>% filter(tamaño2 == 'Gran') %>%
  tally() %>% pivot_wider(names_from =sindical,values_from = n) %>% 
  rename(sin_sindicato=`0`,con_sindicato=`1`) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato),
         ano = "2002")

# 3.4. ENCLA 2004 ----
# var. tamaño: 1=micro, 2=pequeña, 3=mediana, 4=grande
encla2004_a$tamaño <- as_factor(encla2004_a$tamaño)
encla2004_a <- encla2004_a %>% mutate(tamaño3 = case_when(tamaño == 1 ~ "Pequeña",
                                                          tamaño == 2 ~ "Pequeña",
                                                          tamaño == 3 ~ "Mediana",
                                                          tamaño == 4 ~ "Gran"))

encla2004_a <- encla2004_a %>% mutate(rama2 = case_when(rama == NA ~ "Actividad no especificada",
                                                        rama == 1 ~ "Agricultura y pesca", 
                                                        rama == 2 ~ "Mineria", 
                                                        rama == 3 ~ "Industria",
                                                        rama == 4 ~ "Electricidad, gas y agua", 
                                                        rama == 5 ~ "Construccion", 
                                                        rama == 6 ~ "Comercio y hoteleria", 
                                                        rama == 7 ~ "Transporte y comunicaciones", 
                                                        rama == 8 ~ "Establecimientos financieros",
                                                        rama == 9 ~ "Servicios comunales"))

# Cantidad en empresa
encla2004_a %>% group_by(sindicat) %>% 
  tally() %>% pivot_wider(names_from =sindicat,values_from = n) %>% 
  rename(sin_sindicato=`2`,con_sindicato=`1`) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato),
         ano = "2004")

# tamaño
d <- encla2004_a %>% group_by(sindicat, tamaño3) %>% 
  tally() %>% pivot_wider(names_from =sindicat,values_from = n) %>% 
  rename(sin_sindicato=`2`,con_sindicato=`1`, tamaño2= `tamaño3`) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato),
         ano = "2004")

# sector y tamaño

## pequeña
d_pequena <- encla2004_a %>% group_by(sindicat, rama2) %>% filter(tamaño3 == 'Pequeña') %>% 
  tally() %>% pivot_wider(names_from =sindicat,values_from = n) %>% 
  rename(sin_sindicato=`2`,con_sindicato=`1`) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato),
         ano = "2004")

## mediana
d_mediana <- encla2004_a %>% group_by(sindicat, rama2) %>% filter(tamaño3 == 'Mediana') %>% 
  tally() %>% pivot_wider(names_from =sindicat,values_from = n) %>% 
  rename(sin_sindicato=`2`,con_sindicato=`1`) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato),
         ano = "2004")

## gran
d_gran <- encla2004_a %>% group_by(sindicat, rama2) %>% filter(tamaño3 == 'Gran') %>% 
  tally() %>% pivot_wider(names_from =sindicat,values_from = n) %>% 
  rename(sin_sindicato=`2`,con_sindicato=`1`) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato),
         ano = "2004")


# 3.5. ENCLA 2006 ----
encla2006_a$Tamaño <- as_factor(encla2006_a$Tamaño)
encla2006_a <- encla2006_a %>% mutate(tamaño2 = case_when(Tamaño == 1 ~ "Pequeña",
                                                          Tamaño == 2 ~ "Pequeña",
                                                          Tamaño == 3 ~ "Mediana",
                                                          Tamaño == 4 ~ "Gran"))

encla2006_a <- encla2006_a %>% mutate(rama2 = case_when(Rama_antigua  == NA ~ "Actividad no especificada",
                                                        Rama_antigua  == 1 ~ "Agricultura y pesca", 
                                                        Rama_antigua  == 2 ~ "Mineria", 
                                                        Rama_antigua  == 3 ~ "Industria",
                                                        Rama_antigua  == 4 ~ "Electricidad, gas y agua", 
                                                        Rama_antigua  == 5 ~ "Construccion", 
                                                        Rama_antigua  == 6 ~ "Comercio y hoteleria", 
                                                        Rama_antigua  == 7 ~ "Transporte y comunicaciones", 
                                                        Rama_antigua  == 8 ~ "Establecimientos financieros",
                                                        Rama_antigua  == 9 ~ "Servicios comunales"))

# Cantidad empresa
encla2006_a %>% group_by(Sindicat) %>% 
  tally() %>% pivot_wider(names_from =Sindicat,values_from = n) %>% 
  rename(sin_sindicato=`0`,con_sindicato=`1`) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato),
         ano = "2006")

# tamaño
e <- encla2006_a %>% group_by(Sindicat, tamaño2) %>% 
  tally() %>% pivot_wider(names_from =Sindicat,values_from = n) %>% 
  rename(sin_sindicato=`0`,con_sindicato=`1`) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato),
         ano = "2006")
# sector y tamaño

## pequeña
e_pequena <- encla2006_a %>% group_by(Sindicat, rama2) %>% filter(tamaño2 == 'Pequeña') %>%
  tally() %>% pivot_wider(names_from =Sindicat,values_from = n) %>% 
  rename(sin_sindicato=`0`,con_sindicato=`1`) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato),
         ano = "2006")

## mediana
e_mediana <- encla2006_a %>% group_by(Sindicat, rama2) %>% filter(tamaño2 == 'Mediana') %>%
  tally() %>% pivot_wider(names_from =Sindicat,values_from = n) %>% 
  rename(sin_sindicato=`0`,con_sindicato=`1`) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato),
         ano = "2006")

## gran
e_gran <- encla2006_a %>% group_by(Sindicat, rama2) %>% filter(tamaño2 == 'Gran') %>%
  tally() %>% pivot_wider(names_from =Sindicat,values_from = n) %>% 
  rename(sin_sindicato=`0`,con_sindicato=`1`) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato),
         ano = "2006")

# 4. Union data ----

# 4.1. Cantidad total
tot98_99 <- full_join(a, b, by = NULL)
tot02_04 <- full_join(c, d, by = NULL)
tot98_04 <- full_join(tot98_99, tot02_04, by = NULL)
total_tam <- full_join(tot98_04, e, by = NULL)

# 4.2. Tamaño pequeña empresa
peq98_99 <- full_join(a_pequena, b_pequena, by = NULL)
peq02_04 <- full_join(c_pequena, d_pequena, by = NULL)
peq98_04 <- full_join(peq98_99, peq02_04, by = NULL)
peq_tot <- full_join(peq98_04, e_pequena,  by = NULL)

# 4.3. Tamaño mediana empresa
med98_99 <- full_join(a_mediana, b_mediana, by = NULL)
med02_04 <- full_join(c_mediana, d_mediana, by = NULL)
med98_04 <- full_join(med98_99, med02_04, by = NULL)
med_tot <- full_join(med98_04, e_mediana,  by = NULL)

# 4.4. Tamaño gran empresa
gran98_99 <- full_join(a_gran, b_gran, by = NULL)
gran02_04 <- full_join(c_grande, d_gran, by = NULL)
gran98_04 <- full_join(gran98_99, gran02_04, by = NULL)
gran_tot <- full_join(gran98_04, e_gran,  by = NULL)

# 5. Guardar y exportar ----
save(total_tam, file = "output/total_tamaño_1998_2006.RData")
save(peq_tot, file = "output/pequeña_empresa_1998_2006.RData")
save(med_tot, file = "output/mediana_empresa_1998_2006.RData")
save(gran_tot, file = "output/gran_empresa_1998_2006.RData")

write_xlsx(total_tam,"output/tabla1.empresas_sindicato_por_tamano_1998_2006.xlsx", col_names = TRUE,format_headers = TRUE)
write_xlsx(list("Pequeña"=peq_tot,
                "Mediana"=med_tot,
                "Gran"=gran_tot),"output/tabla2.empresas_sindicato_sector_y_tamaño_1998_2006.xlsx", col_names = TRUE,format_headers = TRUE)