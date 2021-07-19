# Code 4: Analysis CASEN 

# 1. Cargar librerias ----
if (!require("pacman"))install.packages("pacman")

pacman::p_load(tidyverse, rvest, xml2, lubridate, openxlsx, readxl, ggrepel, tibble, writexl, haven,
               dplyr, car, summarytools, ggpubr, sjmisc, sjlabelled, stargazer, srvyr)

options(scipen=999)

# 2. Cargar datos ----
load("input/casen20.RData")
load("input/casen03.RData")
load("input/casen09.RData")
load("input/casen13.RData")
load("input/casen15.RData")
load("input/casen17.RData")

# 3. Procesamiento ----

# recodificar tamaño empresa, afiliacion y condicion asalariada en cada base


# 2000
casen20 %>% group_by(o13) %>% tally() 
casen20$o13<- as_factor(casen20$o13)
casen20$o13[casen20$o13 == ""] <- NA
casen20 <- casen20 %>% mutate(tamaño = case_when(o13 == "C" ~ "Pequeña",
                                                 o13 == "D" ~ "Pequeña",
                                                 o13 == "E" ~ "Mediana",
                                                 o13 == "F" ~ "Gran",
                                                 TRUE ~ NA_character_))

casen20 <- casen20 %>% mutate(sindicato = if_else(p18 == 18, 1, 0))

casen20 <- casen20 %>% mutate(asalariado = case_when(o10 == 1 ~ "No asalariado",
                                                     o10 == 2 ~ "No asalariado",
                                                     o10 == 3 ~ "Asalariado",
                                                     o10 == 4 ~ "Asalariado",
                                                     o10 == 5 ~ "No asalariado",
                                                     o10 == 6 ~ "No asalariado",
                                                     o10 == 7 ~ "No asalariado"))

# 2003
casen03 %>% group_by(o14) %>% tally() 
casen03$o14<- as_factor(casen03$o14)
casen03$o14[casen03$o14 == ""] <- NA
casen03 <- casen03 %>% mutate(tamaño = case_when(o14 == "C" ~ "Pequeña",
                                                 o14 == "D" ~ "Pequeña",
                                                 o14 == "E" ~ "Mediana",
                                                 o14 == "F" ~ "Gran",
                                                 TRUE ~ NA_character_))

casen03 <- casen03 %>% mutate(sindicato = if_else(r18 == 20, 1, 0))
casen03 <- casen03 %>% mutate(asalariado = case_when(o9 == 1 ~ "No asalariado",
                                                     o9 == 2 ~ "No asalariado",
                                                     o9 == 3 ~ "Asalariado",
                                                     o9 == 4 ~ "Asalariado",
                                                     o9 == 5 ~ "No asalariado",
                                                     o9 == 6 ~ "No asalariado",
                                                     o9 == 7 ~ "No asalariado"))
# 2009
casen09 %>% group_by(o14) %>% tally() 
casen09$o14<- as_factor(casen09$o14)
casen09$o14[casen09$o14 == ""] <- NA
casen09 <- casen09 %>% mutate(tamaño = case_when(o14 == "C" ~ "Pequeña",
                                                 o14 == "D" ~ "Pequeña",
                                                 o14 == "E" ~ "Mediana",
                                                 o14 == "F" ~ "Gran",
                                                 TRUE ~ NA_character_))

casen09 <- casen09 %>% mutate(sindicato = if_else(afiliado == "Sindicato", 1, 0))
casen09 <- casen09 %>% mutate(asalariado = case_when(o23 == 1 ~ "No asalariado",
                                                     o23 == 2 ~ "No asalariado",
                                                     o23 == 3 ~ "Asalariado",
                                                     o23 == 4 ~ "Asalariado",
                                                     o23 == 5 ~ "No asalariado",
                                                     o23 == 6 ~ "No asalariado",
                                                     o23 == 7 ~ "No asalariado"))

# 2013
casen13 %>% group_by(o24) %>% tally()
casen13$o24<- as_factor(casen13$o24)
casen13$o24[casen13$o24 == ""] <- NA
casen13 <- casen13 %>% mutate(tamaño = case_when(o24 == "C" ~ "Pequeña",
                                                 o24 == "D" ~ "Pequeña",
                                                 o24 == "E" ~ "Mediana",
                                                 o24 == "F" ~ "Gran",
                                                 TRUE ~ NA_character_))

casen13<- casen13 %>% mutate(sindicato = if_else(r9 == 12, 1, 0))
casen13 <- casen13 %>% mutate(asalariado = case_when(o15 == 1 ~ "No asalariado",
                                                     o15 == 2 ~ "No asalariado",
                                                     o15 == 3 ~ "Asalariado",
                                                     o15 == 4 ~ "Asalariado",
                                                     o15 == 5 ~ "No asalariado",
                                                     o15 == 6 ~ "No asalariado",
                                                     o15 == 7 ~ "No asalariado"))

# 2015
casen15 %>% group_by(o23) %>% tally()
casen15$o23<- as_factor(casen15$o23)
casen15$o23[casen15$o23 == ""] <- NA
casen15 <- casen15 %>% mutate(tamaño = case_when(o23 == "C" ~ "Pequeña",
                                                 o23 == "D" ~ "Pequeña",
                                                 o23 == "E" ~ "Mediana",
                                                 o23 == "F" ~ "Gran",
                                                 TRUE ~ NA_character_))

casen15 <- casen15 %>% mutate(sindicato = case_when(o24a == 1 ~ 1,
                                                    o24a == 2 ~ 0,
                                                    o24b == 1 ~ 1,
                                                    o24b == 2 ~ 0,
                                                    o24d == 1 ~ 1, 
                                                    o24d == 2 ~ 0,
                                                    TRUE ~ NA_real_))

casen15 <- casen15 %>% mutate(asalariado = case_when(o15 == 1 ~ "No asalariado",
                                                     o15 == 2 ~ "No asalariado",
                                                     o15 == 3 ~ "Asalariado",
                                                     o15 == 4 ~ "Asalariado",
                                                     o15 == 5 ~ "No asalariado",
                                                     o15 == 6 ~ "No asalariado",
                                                     o15 == 7 ~ "No asalariado"))

# 2017
casen17 %>% group_by(o23) %>% tally()
casen17$o23<- as_factor(casen17$o23)
casen17$o23[casen17$o23 == ""] <- NA
casen17 <- casen17 %>% mutate(tamaño = case_when(o23 == "C" ~ "Pequeña",
                                                 o23 == "D" ~ "Pequeña",
                                                 o23 == "E" ~ "Mediana",
                                                 o23 == "F" ~ "Gran",
                                                 TRUE ~ NA_character_))

casen17 <- casen17 %>% mutate(sindicato = case_when(o24a == 1 ~ 1,
                                                    o24a == 2 ~ 0,
                                                    o24b == 1 ~ 1,
                                                    o24b == 2 ~ 0,
                                                    o24d == 1 ~ 1, 
                                                    o24d == 2 ~ 0,
                                                    TRUE ~ NA_real_))

casen17 <- casen17 %>% mutate(asalariado = case_when(o15 == 1 ~ "No asalariado",
                                                     o15 == 2 ~ "No asalariado",
                                                     o15 == 3 ~ "Asalariado",
                                                     o15 == 4 ~ "Asalariado",
                                                     o15 == 5 ~ "No asalariado",
                                                     o15 == 6 ~ "No asalariado",
                                                     o15 == 7 ~ "No asalariado"))
# 4. Estimacion ----

# 4.1. CASEN 2000 ----

# Cantidad 
casen20 %>% group_by(sindicato) %>% filter(asalariado == "Asalariado")%>%
  summarise(FACTOR=sum(expr)) %>% 
  pivot_wider(names_from = sindicato, values_from = FACTOR) %>% 
  rename(no_afiliados=`0`, afiliados=`1`)%>%
  select(no_afiliados, afiliados) %>%
  mutate(porcentaje_afiliados = afiliados/(no_afiliados+afiliados),
         ano = "2000") 


# tamaño
a <- casen20 %>% group_by(sindicato, tamaño, na.rm = T) %>% filter(asalariado == "Asalariado") %>%
  summarise(FACTOR=sum(expr)) %>% 
  pivot_wider(names_from = sindicato, values_from = FACTOR) %>% 
  rename(no_afiliados=`0`, afiliados=`1`)%>%
  select(no_afiliados, afiliados) %>%
  mutate(porcentaje_afiliados = afiliados/(no_afiliados+afiliados),
         ano = "2000") 

# sector y tamaño

## pequeña
a_pequena <- casen20 %>% group_by(sindicato, rama2, na.rm = T) %>% 
  filter(tamaño == 'Pequeña' & asalariado == "Asalariado") %>% summarise(FACTOR=sum(expr)) %>% 
  pivot_wider(names_from = sindicato, values_from = FACTOR) %>% 
  rename(no_afiliados=`0`, afiliados=`1`)%>%
  select(no_afiliados, afiliados) %>%
  mutate(porcentaje_afiliados = afiliados/(no_afiliados+afiliados),
         ano = "2000") 

## mediana
a_mediana <- casen20 %>% group_by(sindicato, rama2, na.rm = T) %>% 
  filter(tamaño == 'Mediana' & asalariado == "Asalariado") %>%
  summarise(FACTOR=sum(expr)) %>% 
  pivot_wider(names_from = sindicato, values_from = FACTOR) %>% 
  rename(no_afiliados=`0`, afiliados=`1`)%>%
  select(no_afiliados, afiliados) %>%
  mutate(porcentaje_afiliados = afiliados/(no_afiliados+afiliados),
         ano = "2000") 

## gran
a_gran <- casen20 %>% group_by(sindicato, rama2, na.rm = T) %>% 
  filter(tamaño == 'Gran' & asalariado == "Asalariado") %>%
  summarise(FACTOR=sum(expr)) %>% 
  pivot_wider(names_from = sindicato, values_from = FACTOR) %>% 
  rename(no_afiliados=`0`, afiliados=`1`)%>%
  select(no_afiliados, afiliados) %>%
  mutate(porcentaje_afiliados = afiliados/(no_afiliados+afiliados),
         ano = "2000") 

# 4.2. CASEN 2003 ----

# Cantidad 
casen03 %>% group_by(sindicato) %>% filter(asalariado == "Asalariado")%>%
  summarise(FACTOR=sum(expr)) %>% 
  pivot_wider(names_from = sindicato, values_from = FACTOR) %>% 
  rename(no_afiliados=`0`, afiliados=`1`)%>%
  select(no_afiliados, afiliados) %>%
  mutate(porcentaje_afiliados = afiliados/(no_afiliados+afiliados),
         ano = "2003") 

# tamaño
b <- casen03 %>% group_by(sindicato, tamaño, na.rm = T) %>% filter(asalariado == "Asalariado")%>%
  summarise(FACTOR=sum(expr)) %>% 
  pivot_wider(names_from = sindicato, values_from = FACTOR) %>% 
  rename(no_afiliados=`0`, afiliados=`1`)%>%
  select(no_afiliados, afiliados) %>%
  mutate(porcentaje_afiliados = afiliados/(no_afiliados+afiliados),
         ano = "2003") 

# sector y tamaño 

## pequeña
b_pequena <- casen03 %>% group_by(sindicato, rama2, na.rm = T) %>% 
  filter(tamaño == 'Pequeña' & asalariado == "Asalariado")%>%
  summarise(FACTOR=sum(expr)) %>% 
  pivot_wider(names_from = sindicato, values_from = FACTOR) %>% 
  rename(no_afiliados=`0`, afiliados=`1`)%>%
  select(no_afiliados, afiliados) %>%
  mutate(porcentaje_afiliados = afiliados/(no_afiliados+afiliados),
         ano = "2003") %>% filter(!is.na(rama2))

## mediana
b_mediana <- casen03 %>% group_by(sindicato, rama2, na.rm = T) %>% 
  filter(tamaño == 'Mediana' & asalariado == "Asalariado")%>%
  summarise(FACTOR=sum(expr)) %>% 
  pivot_wider(names_from = sindicato, values_from = FACTOR) %>% 
  rename(no_afiliados=`0`, afiliados=`1`)%>%
  select(no_afiliados, afiliados) %>%
  mutate(porcentaje_afiliados = afiliados/(no_afiliados+afiliados),
         ano = "2003") %>% filter(!is.na(rama2))

## gran
b_gran <- casen03 %>% group_by(sindicato, rama2, na.rm = T) %>% 
  filter(tamaño == 'Gran' & asalariado == "Asalariado")%>%
  summarise(FACTOR=sum(expr)) %>% 
  pivot_wider(names_from = sindicato, values_from = FACTOR) %>% 
  rename(no_afiliados=`0`, afiliados=`1`)%>%
  select(no_afiliados, afiliados) %>%
  mutate(porcentaje_afiliados = afiliados/(no_afiliados+afiliados),
         ano = "2003") 

# 4.3. CASEN 2009 ----

# cantidad
casen09 %>% group_by(sindicato) %>% filter(asalariado == "Asalariado")%>%
  summarise(FACTOR=sum(expr_p)) %>% 
  pivot_wider(names_from = sindicato, values_from = FACTOR) %>% 
  rename(no_afiliados=`0`, afiliados=`1`)%>%
  select(no_afiliados, afiliados) %>%
  mutate(porcentaje_afiliados = afiliados/(no_afiliados+afiliados),
         ano = "2009") 

# tamaño
c <- casen09 %>% group_by(sindicato, tamaño, na.rm=T) %>% filter(asalariado == "Asalariado")%>%
  summarise(FACTOR=sum(expr_p)) %>% 
  pivot_wider(names_from = sindicato, values_from = FACTOR) %>% 
  rename(no_afiliados=`0`, afiliados=`1`)%>%
  select(no_afiliados, afiliados) %>%
  mutate(porcentaje_afiliados = afiliados/(no_afiliados+afiliados),
         ano = "2009") 

# sector y tamaño

## pequeña
c_pequena <- casen09 %>% group_by(sindicato, rama2, na.rm=T) %>% 
  filter(tamaño == 'Pequeña' & asalariado == "Asalariado")%>%
  summarise(FACTOR=sum(expr_p)) %>% 
  pivot_wider(names_from = sindicato, values_from = FACTOR) %>% 
  rename(no_afiliados=`0`, afiliados=`1`)%>%
  select(no_afiliados, afiliados) %>%
  mutate(porcentaje_afiliados = afiliados/(no_afiliados+afiliados),
         ano = "2009")

## mediana
c_mediana <- casen09 %>% group_by(sindicato, rama2, na.rm=T) %>% 
  filter(tamaño == 'Mediana' & asalariado == "Asalariado")%>%
  summarise(FACTOR=sum(expr_p)) %>% 
  pivot_wider(names_from = sindicato, values_from = FACTOR) %>% 
  rename(no_afiliados=`0`, afiliados=`1`)%>%
  select(no_afiliados, afiliados) %>%
  mutate(porcentaje_afiliados = afiliados/(no_afiliados+afiliados),
         ano = "2009")

## gran
c_gran <- casen09 %>% group_by(sindicato, rama2, na.rm=T) %>% 
  filter(tamaño == 'Gran' & asalariado == "Asalariado")%>%
  summarise(FACTOR=sum(expr_p)) %>% 
  pivot_wider(names_from = sindicato, values_from = FACTOR) %>% 
  rename(no_afiliados=`0`, afiliados=`1`)%>%
  select(no_afiliados, afiliados) %>%
  mutate(porcentaje_afiliados = afiliados/(no_afiliados+afiliados),
         ano = "2009")

# 4.4. CASEN 2013 ----

# cantidad
casen13 %>% group_by(sindicato) %>% filter(asalariado == "Asalariado")%>%
  summarise(FACTOR=sum(expr)) %>% 
  pivot_wider(names_from = sindicato, values_from = FACTOR) %>% 
  rename(no_afiliados=`0`, afiliados=`1`)%>%
  select(no_afiliados, afiliados) %>%
  mutate(porcentaje_afiliados = afiliados/(no_afiliados+afiliados),
         ano = "2013") 

# tamaño
d <- casen13 %>% group_by(sindicato, tamaño, na.rm = T) %>% filter(asalariado == "Asalariado")%>%
  summarise(FACTOR=sum(expr)) %>% 
  pivot_wider(names_from = sindicato, values_from = FACTOR) %>% 
  rename(no_afiliados=`0`, afiliados=`1`)%>%
  select(no_afiliados, afiliados) %>%
  mutate(porcentaje_afiliados = afiliados/(no_afiliados+afiliados),
         ano = "2013") 

# sector y tamaño

## pequeña
d_pequena <- casen13 %>% group_by(sindicato, rama2, na.rm = T) %>% 
  filter(tamaño == 'Pequeña' & asalariado == "Asalariado")%>%
  summarise(FACTOR=sum(expr)) %>% 
  pivot_wider(names_from = sindicato, values_from = FACTOR) %>% 
  rename(no_afiliados=`0`, afiliados=`1`)%>%
  select(no_afiliados, afiliados) %>%
  mutate(porcentaje_afiliados = afiliados/(no_afiliados+afiliados),
         ano = "2013")

## mediana
d_mediana <- casen13 %>% group_by(sindicato, rama2, na.rm = T) %>% 
  filter(tamaño == 'Mediana' & asalariado == "Asalariado")%>%
  summarise(FACTOR=sum(expr)) %>% 
  pivot_wider(names_from = sindicato, values_from = FACTOR) %>% 
  rename(no_afiliados=`0`, afiliados=`1`)%>%
  select(no_afiliados, afiliados) %>%
  mutate(porcentaje_afiliados = afiliados/(no_afiliados+afiliados),
         ano = "2013")

## gran
d_gran <- casen13 %>% group_by(sindicato, rama2, na.rm = T) %>% 
  filter(tamaño == 'Gran' & asalariado == "Asalariado")%>%
  summarise(FACTOR=sum(expr)) %>% 
  pivot_wider(names_from = sindicato, values_from = FACTOR) %>% 
  rename(no_afiliados=`0`, afiliados=`1`)%>%
  select(no_afiliados, afiliados) %>%
  mutate(porcentaje_afiliados = afiliados/(no_afiliados+afiliados),
         ano = "2013")

# 4.5. CASEN 2015 ----

# cantidad
casen15 %>% group_by(sindicato) %>% filter(asalariado == "Asalariado")%>%
  summarise(FACTOR=sum(expr)) %>% 
  pivot_wider(names_from = sindicato, values_from = FACTOR) %>% 
  rename(no_afiliados=`0`, afiliados=`1`)%>%
  select(no_afiliados, afiliados) %>%
  mutate(porcentaje_afiliados = afiliados/(no_afiliados+afiliados),
         ano = "2015") 

# tamaño
e <- casen15 %>% group_by(sindicato, tamaño, na.rm = T) %>% filter(asalariado == "Asalariado")%>%
  summarise(FACTOR=sum(expr)) %>% 
  pivot_wider(names_from = sindicato, values_from = FACTOR) %>% 
  rename(no_afiliados=`0`, afiliados=`1`)%>%
  select(no_afiliados, afiliados) %>%
  mutate(porcentaje_afiliados = afiliados/(no_afiliados+afiliados),
         ano = "2015") 

# sector y tamaño

## pequeña
e_pequena <- casen15 %>% group_by(sindicato, rama2, na.rm = T) %>% 
  filter(tamaño == 'Pequeña' & asalariado == "Asalariado")%>%
  summarise(FACTOR=sum(expr)) %>% 
  pivot_wider(names_from = sindicato, values_from = FACTOR) %>% 
  rename(no_afiliados=`0`, afiliados=`1`)%>%
  select(no_afiliados, afiliados) %>%
  mutate(porcentaje_afiliados = afiliados/(no_afiliados+afiliados),
         ano = "2015") 

## mediana
e_mediana <- casen15 %>% group_by(sindicato, rama2, na.rm = T) %>% 
  filter(tamaño == 'Mediana' & asalariado == "Asalariado")%>%
  summarise(FACTOR=sum(expr)) %>% 
  pivot_wider(names_from = sindicato, values_from = FACTOR) %>% 
  rename(no_afiliados=`0`, afiliados=`1`)%>%
  select(no_afiliados, afiliados) %>%
  mutate(porcentaje_afiliados = afiliados/(no_afiliados+afiliados),
         ano = "2015")

## gran
e_gran <- casen15 %>% group_by(sindicato, rama2, na.rm = T) %>% 
  filter(tamaño == 'Gran' & asalariado == "Asalariado")%>%
  summarise(FACTOR=sum(expr)) %>% 
  pivot_wider(names_from = sindicato, values_from = FACTOR) %>% 
  rename(no_afiliados=`0`, afiliados=`1`)%>%
  select(no_afiliados, afiliados) %>%
  mutate(porcentaje_afiliados = afiliados/(no_afiliados+afiliados),
         ano = "2015")

# 4.6. CASEN 2017 ----

# cantidad
casen17 %>% group_by(sindicato) %>% filter(asalariado == "Asalariado")%>%
  summarise(FACTOR=sum(expr)) %>% 
  pivot_wider(names_from = sindicato, values_from = FACTOR) %>% 
  rename(no_afiliados=`0`, afiliados=`1`)%>%
  select(no_afiliados, afiliados) %>%
  mutate(porcentaje_afiliados = afiliados/(no_afiliados+afiliados),
         ano = "2017") 

# tamaño
f <- casen17 %>% group_by(sindicato, tamaño, na.rm = T) %>% filter(asalariado == "Asalariado")%>%
  summarise(FACTOR=sum(expr)) %>% 
  pivot_wider(names_from = sindicato, values_from = FACTOR) %>% 
  rename(no_afiliados=`0`, afiliados=`1`)%>%
  select(no_afiliados, afiliados) %>%
  mutate(porcentaje_afiliados = afiliados/(no_afiliados+afiliados),
         ano = "2017") 

# sector y tamaño

## pequeña
f_pequena <- casen17 %>% group_by(sindicato, rama2, na.rm = T) %>% 
  filter(tamaño == 'Pequeña' & asalariado == "Asalariado")%>%
  summarise(FACTOR=sum(expr)) %>% 
  pivot_wider(names_from = sindicato, values_from = FACTOR) %>% 
  rename(no_afiliados=`0`, afiliados=`1`)%>%
  select(no_afiliados, afiliados) %>%
  mutate(porcentaje_afiliados = afiliados/(no_afiliados+afiliados),
         ano = "2017") 

## mediana
f_mediana <- casen17 %>% group_by(sindicato, rama2, na.rm = T) %>% 
  filter(tamaño == 'Mediana' & asalariado == "Asalariado")%>%
  summarise(FACTOR=sum(expr)) %>% 
  pivot_wider(names_from = sindicato, values_from = FACTOR) %>% 
  rename(no_afiliados=`0`, afiliados=`1`)%>%
  select(no_afiliados, afiliados) %>%
  mutate(porcentaje_afiliados = afiliados/(no_afiliados+afiliados),
         ano = "2017")

## gran
f_gran <- casen17 %>% group_by(sindicato, rama2, na.rm = T) %>% 
  filter(tamaño == 'Gran' & asalariado == "Asalariado")%>%
  summarise(FACTOR=sum(expr)) %>% 
  pivot_wider(names_from = sindicato, values_from = FACTOR) %>% 
  rename(no_afiliados=`0`, afiliados=`1`)%>%
  select(no_afiliados, afiliados) %>%
  mutate(porcentaje_afiliados = afiliados/(no_afiliados+afiliados),
         ano = "2017")

# 5. Unir data ----
# cantidad total
tot20_03 <- full_join(a, b, by = NULL)
tot09_13 <- full_join(c, d, by = NULL)
tot15_17 <- full_join(e, f, by = NULL)
tot20_13 <- full_join(tot20_03, tot09_13, by = NULL)
total_2000_2017 <- full_join(tot20_13, tot15_17, by = NULL)

# total pequeña empresa
peq20_03 <- full_join(a_pequena, b_pequena, by = NULL)
peq09_13 <- full_join(c_pequena, d_pequena, by = NULL)
peq15_17 <- full_join(e_pequena, f_pequena, by = NULL)
peq20_13 <- full_join(peq20_03, peq09_13, by = NULL)
peq_total <- full_join(peq20_13, peq15_17, by = NULL)

# total mediana empresa
med20_03 <- full_join(a_mediana, b_mediana, by = NULL)
med09_13 <- full_join(c_mediana, d_mediana, by = NULL)
med15_17 <- full_join(e_mediana, f_mediana, by = NULL)
med20_13 <- full_join(med20_03, med09_13, by = NULL)
med_total <- full_join(med20_13, med15_17, by = NULL)

# total gran empresa
gran20_03 <- full_join(a_gran, b_gran, by = NULL)
gran09_13 <- full_join(c_gran, d_gran, by = NULL)
gran15_17 <- full_join(e_gran, f_gran, by = NULL)
gran20_13 <- full_join(gran20_03, gran09_13, by = NULL)
gran_total <- full_join(gran20_13, gran15_17, by = NULL)

# 6. Guardar y exportar ----
save(total_2000_2017, file = "output/casen.total_afiliados_tamaño_2000_2017.RData")
save(peq_total, file = "output/casen.afiliados_pequeña_empresa_2000_2017.RData")
save(med_total, file = "output/casen.afiliados_mediana_empresa_2000_2017.RData")
save(gran_total, file = "output/casen.afiliados_gran_empresa_2000_2017.RData")

write_xlsx(total_2000_2017,"output/casen.total_afiliados_tamano_2000_2017.xlsx", col_names = TRUE,format_headers = TRUE)
write_xlsx(list("Pequeña"=peq_total,
                "Mediana"=med_total,
                "Gran"=gran_total),"output/casen.total_afiliados_sector_tamaño_2000_2017.xlsx", col_names = TRUE,format_headers = TRUE)