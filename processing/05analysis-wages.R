# Code 5: Analysis CASEN 

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
                                                     o10 == 3 ~ "Públicos",
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
                                                     o9 == 3 ~ "Públicos",
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
                                                     o23 == 3 ~ "Públicos",
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
                                                     o15 == 3 ~ "Públicos",
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
                                                     o15 == 3 ~ "Públicos",
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
                                                     o15 == 3 ~ "Públicos",
                                                     o15 == 4 ~ "Asalariado",
                                                     o15 == 5 ~ "No asalariado",
                                                     o15 == 6 ~ "No asalariado",
                                                     o15 == 7 ~ "No asalariado"))

# 4. Estimacion afiliados ----

# 4.1. 2000 ----
db20_pond <- casen20 %>% as_survey_design(ids = 1, strata = estrato, weights=expr)

# ocupados
ocup_2000 <- db20_pond %>% group_by(rama2) %>% filter(asalariado == "Asalariado") %>%
  summarise(salario_promed_ocup = survey_mean(yopraj, vartype = NULL, na.rm = T)) %>%
  mutate(ano = "2000")

# afiliados
afi_20 <- db20_pond %>% group_by(rama2) %>% filter(sindicato == 1 & asalariado == "Asalariado") %>%
  summarise(salario_promed_afiliado = survey_mean(yopraj, vartype = NULL, na.rm = T)) %>%
  mutate(ano = "2000")

# no afiliados
noafi_20 <- db20_pond %>% group_by(rama2) %>% filter(sindicato == 0 & asalariado == "Asalariado") %>%
  summarise(salario_promed_no_afi = survey_mean(yopraj, vartype = NULL, na.rm = T)) %>%
  mutate(ano = "2000")

a <- full_join(afi_20, noafi_20, by = c("rama2", "ano"))
tot_2000 <- full_join(a, ocup_2000, by = c("rama2", "ano"))

# 4.2. 2003 ----
db03_pond <- casen03 %>% as_survey_design(ids = 1, 
                                          strata = estrato, 
                                          weights= expr)

# ocupados
ocup_2003 <- db03_pond %>% group_by(rama2) %>% filter(asalariado == "Asalariado") %>% 
  summarise(salario_promed_ocup = survey_mean(yopraj, vartype = NULL, na.rm = T)) %>%
  mutate(ano = "2003") %>% filter(!is.na(rama2))

# afiliados
afi_03 <- db03_pond %>% group_by(rama2) %>% filter(sindicato == 1 & asalariado == "Asalariado") %>% 
  summarise(salario_promed_afiliado = survey_mean(yopraj, vartype = NULL, na.rm = T)) %>% 
  mutate(ano = "2003")

# no afiliados 
noafi_03 <- db03_pond %>% group_by(rama2) %>% filter(sindicato == 0 & asalariado == "Asalariado") %>% 
  summarise(salario_promed_no_afi = survey_mean(yopraj, vartype = NULL, na.rm = T)) %>% 
  mutate(ano = "2003") %>% filter(!is.na(rama2))

b <- full_join(afi_03, noafi_03, by = c("rama2", "ano"))
tot_2003 <- full_join(b, ocup_2003, by = c("rama2", "ano"))

# 4.2. 2009 ----
db09_pond <- casen09 %>% as_survey_design(ids = 1,
                                          strata = estrato,
                                          weights = expr_p)

# ocupados
ocup_2009 <- db09_pond %>% group_by(rama2) %>% filter(asalariado == "Asalariado") %>% 
  summarise(salario_promed_ocup = survey_mean(yopraj, vartype = NULL, na.rm = T)) %>% 
  mutate(ano = "2009")

# afiliados
afi_09 <- db09_pond %>% group_by(rama2) %>% filter(sindicato == 1 & asalariado == "Asalariado") %>% 
  summarise(salario_promed_afiliado = survey_mean(yopraj, vartype = NULL, na.rm = T)) %>% 
  mutate(ano = "2009")

# no afiliados
noafi_09 <- db09_pond %>% group_by(rama2) %>% filter(sindicato == 0 & asalariado == "Asalariado") %>% 
  summarise(salario_promed_no_afi = survey_mean(yopraj, vartype = NULL, na.rm = T)) %>% 
  mutate(ano = "2009")

c <- full_join(afi_09, noafi_09, by = c("rama2", "ano"))
tot_2009 <- full_join(c, ocup_2009, by = c("rama2", "ano"))

# 4.3. 2013 ----
db13_pond <- casen13 %>% as_survey_design(ids =1, 
                                          strata = varstrat, 
                                          weights = expr)

# ocupados
ocup_2013 <- db13_pond %>% group_by(rama2) %>% filter(asalariado == "Asalariado") %>% 
  summarise(salario_promed_ocup = survey_mean(yoprcor, vartype = NULL, na.rm = T)) %>% 
  mutate(ano = "2013")

# afiliados
afi_13 <- db13_pond %>% group_by(rama2) %>% filter(sindicato == 1 & asalariado == "Asalariado") %>% 
  summarise(salario_promed_afiliado = survey_mean(yoprcor, vartype = NULL, na.rm = T)) %>% 
  mutate(ano = "2013")

# no afiliados
noafi_13 <- db13_pond %>% group_by(rama2) %>% filter(sindicato == 0 & asalariado == "Asalariado") %>% 
  summarise(salario_promed_no_afi = survey_mean(yoprcor, vartype = NULL, na.rm = T)) %>% 
  mutate(ano = "2013")

d <- full_join(afi_13, noafi_13, by = c("rama2", "ano")) 
tot_2013 <- full_join(d, ocup_2013, by = c("rama2", "ano"))

# 4.4. 2015 ----
db15_pond <- casen15 %>% as_survey_design(ids = 1, 
                                          strata = varstrat,
                                          weights = expr)

# ocupados
ocup_2015 <- db15_pond %>% group_by(rama2) %>% filter(asalariado == "Asalariado") %>% 
  summarise(salario_promed_ocup = survey_mean(yoprcor, vartype = NULL, na.rm = T)) %>% 
  mutate(ano = "2015")

# afiliado
afi_15 <- db15_pond %>% group_by(rama2) %>% filter(sindicato == 1 & asalariado == "Asalariado") %>% 
  summarise(salario_promed_afiliado = survey_mean(yoprcor, vartype = NULL, na.rm = T)) %>% 
  mutate(ano = "2015")

# no afiliados
noafi_15 <- db15_pond %>% group_by(rama2) %>% filter(sindicato == 0 & asalariado == "Asalariado") %>% 
  summarise(salario_promed_no_afi = survey_mean(yoprcor, vartype = NULL, na.rm = T)) %>% 
  mutate(ano = "2015")

e <- full_join(afi_15, noafi_15, by = c("rama2", "ano"))
tot_2015 <- full_join(e, ocup_2015, by = c("rama2", "ano"))
# 4.5. 2017 ----
db17_pond <- casen17 %>% as_survey_design(ids = 1, 
                                          strata = varstrat, 
                                          weights = expr)

# ocupados
ocup_2017 <- db17_pond %>% group_by(rama2) %>% filter(asalariado == "Asalariado") %>% 
  summarise(salario_promed_ocup = survey_mean(yoprcor, vartype = NULL, na.rm = T)) %>% 
  mutate(ano = "2017")

# afiliados 
afi_17 <- db17_pond %>% group_by(rama2) %>% filter(sindicato == 1 & asalariado == "Asalariado") %>% 
  summarise(salario_promed_afiliado = survey_mean(yoprcor, vartype = NULL, na.rm = T)) %>% 
  mutate(ano = "2017")

# no afiliado
noafi_17 <- db17_pond %>% group_by(rama2) %>% filter(sindicato == 0 & asalariado == "Asalariado") %>% 
  summarise(salario_promed_no_afi = survey_mean(yoprcor, vartype = NULL, na.rm = T)) %>% 
  mutate(ano = "2017")

f <- full_join(afi_17, noafi_17, by = c("rama2", "ano"))
tot_2017 <- full_join(f, ocup_2017, by = c("rama2", "ano"))

# 5. Unir data ----

salarios20_03 <- full_join(tot_2000, tot_2003, by = NULL)
salarios09_13 <- full_join(tot_2009, tot_2013, by = NULL)
salarios15_17 <- full_join(tot_2015, tot_2017, by = NULL)
salarios20_13 <- full_join(salarios20_03, salarios09_13, by = NULL)
salarios_afiliados_sector_2000_2017 <- full_join(salarios20_13, salarios15_17, by = NULL)  

# 6. Estimacion tamaño ----
## no es necesario reconocer el diseño muestral por año

# 6.1. 2000 ----

tam2000<- db20_pond %>% group_by(rama2, tamaño) %>% filter(asalariado == "Asalariado") %>%
  summarise(salario_promed_ocup = survey_mean(yopraj, vartype = NULL, na.rm = T)) %>%
  mutate(ano = "2000") %>% filter(!is.na(tamaño))

# 6.2. 2003 ---
tam2003 <- db03_pond %>% group_by(rama2, tamaño) %>% filter(asalariado == "Asalariado") %>% 
  summarise(salario_promed_ocup = survey_mean(yopraj, vartype = NULL, na.rm = T)) %>%
  mutate(ano = "2003") %>% filter(!is.na(tamaño) & !is.na(rama2))

# 6.3. 2009 ----
tam2009 <- db09_pond %>% group_by(rama2, tamaño) %>% filter(asalariado == "Asalariado") %>% 
  summarise(salario_promed_ocup = survey_mean(yopraj, vartype = NULL, na.rm = T)) %>% 
  mutate(ano = "2009") %>% filter(!is.na(tamaño))

# 6.4. 2013 ----
tam2013 <- db13_pond %>% group_by(rama2, tamaño) %>% filter(asalariado == "Asalariado") %>% 
  summarise(salario_promed_ocup = survey_mean(yoprcor, vartype = NULL, na.rm = T)) %>% 
  mutate(ano = "2013") %>% filter(!is.na(tamaño))

# 6.5. 2015 ----
tam2015 <- db15_pond %>% group_by(rama2, tamaño) %>% filter(asalariado == "Asalariado") %>% 
  summarise(salario_promed_ocup = survey_mean(yoprcor, vartype = NULL, na.rm = T)) %>% 
  mutate(ano = "2015") %>% filter(!is.na(tamaño))

# 6.6. 2017 ----
tam2017 <- db17_pond %>% group_by(rama2, tamaño) %>% filter(asalariado == "Asalariado") %>% 
  summarise(salario_promed_ocup = survey_mean(yoprcor, vartype = NULL, na.rm = T)) %>% 
  mutate(ano = "2017") %>% filter(!is.na(tamaño))

# 7. Unir data ----

tam20_03 <- full_join(tam2000, tam2003, by = NULL)
tam09_13 <- full_join(tam2009, tam2013, by = NULL)
tam15_17 <- full_join(tam2015, tam2017, by = NULL)
tam20_13 <- full_join(tam20_03, tam09_13, by = NULL)
salarios_tamano_sector_2000_2017 <- full_join(tam20_13, tam15_17, by = NULL)

# 8. Guardar y exportar tablas ----

# Guardar en RData
save(salarios_afiliados_sector_2000_2017, file = "output/salarios_afiliados_sector_2000_2017.RData")
save(salarios_tamano_sector_2000_2017, file = "output/salarios_tamano_sector_2000_2017.RData")

# Exportar en excel
write_xlsx(salarios_afiliados_sector_2000_2017,"output/salarios_afiliados_sector_2000_2017.xlsx", col_names = TRUE,format_headers = TRUE)
write_xlsx(salarios_tamano_sector_2000_2017,"output/salarios_tamano_sector_2000_2017.xlsx", col_names = TRUE,format_headers = TRUE)