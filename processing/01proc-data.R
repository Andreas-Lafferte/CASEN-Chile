# Code 1: Process CASEN 

# 1. Cargar librerias ----
if (!require("pacman"))install.packages("pacman")

pacman::p_load(tidyverse, rvest, xml2, lubridate, openxlsx, readxl, ggrepel, tibble, writexl, haven,
               dplyr, car, summarytools, ggpubr, sjmisc, sjlabelled, stargazer, srvyr)

options(scipen=999)

# 2. Cargar datos ----
casen96 <- read_dta("C:/Users/PC/Documents/CASEN/casen1996.dta")
casen98 <- read_dta("C:/Users/PC/Documents/CASEN/casen1998.dta")
casen20 <- read_dta("C:/Users/PC/Documents/CASEN/casen2000_Stata.dta")
casen03 <- read_dta("C:/Users/PC/Documents/CASEN/casen2003.dta")
casen06 <- read_dta("C:/Users/PC/Documents/CASEN/casen2006.dta")
casen09 <- read_dta("C:/Users/PC/Documents/CASEN/casen2009stata.dta")
casen11 <- read_dta("C:/Users/PC/Documents/CASEN/casen2011stata_03122013stata.dta")
casen13 <- read_dta("C:/Users/PC/Documents/CASEN/casen_2013_mn_b_principal.dta")
casen15 <- read_dta("C:/Users/PC/Documents/CASEN/Casen 2015.dta")  
casen17 <- read_dta("C:/Users/PC/Documents/CASEN/Casen 2017.dta")
  
# 3. Procesamiento ----

# CASEN 96
casen96$o8 <- as_factor(casen96$o8)
casen96$activ <- as_factor(casen96$activ)
casen96$estrato <- (casen96$r + casen96$p + casen96$c + casen96$z)
casen96 <- casen96 %>% dplyr::mutate(rama2 = case_when(rama == 0 ~ "Actividad no especificada",
                                                 rama == 1 ~ "Agricultura y pesca", 
                                                 rama == 2 ~ "Mineria", 
                                                 rama == 3 ~ "Industria",
                                                 rama == 4 ~ "Electricidad, gas y agua", 
                                                 rama == 5 ~ "Construccion", 
                                                 rama == 6 ~ "Comercio y hoteleria", 
                                                 rama == 7 ~ "Transporte y comunicaciones", 
                                                 rama == 8 ~ "Establecimientos financieros",
                                                 rama == 9 ~ "Servicios comunales"))

casen96 <- casen96 %>% select(o8, activ, rama2, expr, estrato, o10)

# CASEN 98
casen98$o8 <- as_factor(casen98$o8)
casen98$o21 <- as_factor(casen98$o21)
casen98 <- casen98 %>% dplyr::mutate(rama2 = case_when(rama == 0 ~ "Actividad no especificada",
                                                       rama == 1 ~ "Agricultura y pesca", 
                                                       rama == 2 ~ "Mineria", 
                                                       rama == 3 ~ "Industria",
                                                       rama == 4 ~ "Electricidad, gas y agua", 
                                                       rama == 5 ~ "Construccion", 
                                                       rama == 6 ~ "Comercio y hoteleria", 
                                                       rama == 7 ~ "Transporte y comunicaciones", 
                                                       rama == 8 ~ "Establecimientos financieros",
                                                       rama == 9 ~ "Servicios comunales"))

casen98 <- casen98 %>% select(o21, o8, estrato, expr, rama2, oficio, o10)

# CASEN 2000
casen20$o10 <- car::recode(casen20$o10, "1 = 1; 2 = 2; c(3,4) = 3; 5 = 4; c(6,7) = 5; 8 = 6; 9 = 7")# 3 = publicos/ 4=privados (sin trb domest)
casen20$o10 <- as_factor(casen20$o10)
casen20$o8 <- car::recode(casen20$o8, "1110:1143 = 1100; 1210:1239 = 1200; 1310:1319 = 1300; 2111:2149 = 2100; 
                          2211:2230 = 2200; 2310:2359 = 2300; 2410:2460 = 2400; 3110:3152 = 3100; 3210:3242 = 3200;
                          3310:3340 = 3300; 3410:3480 = 3400; 4110:4190 = 4100; 4211:4223 = 4200; 5111:5169 = 5100;
                          5210:5230 = 5200; 6110:6154 = 6100; 6210 = 6200; 7111:7143 = 7100; 7210:7245 = 7200; 
                          7311:7346 = 7300; 7411:7442 = 7400; 8111:8172 = 8100; 8211:8290 = 8200; 8311:8340 = 8300;
                          9110:9162 = 9100; 9210:9213 = 9200; 9310:9333 = 9300; 110 = 110;  9999 = NA", as.factor = T)
casen20 <- casen20 %>% dplyr::mutate(ciuo = case_when(o8 == 1100 | o8 == 1200 | o8 == 1300 ~ "Grupo1",
                                                      o8 == 2100 | o8 == 2200 | o8 == 2400 ~ "Grupo2",
                                                      o8 == 3100 | o8 == 3200 | o8 == 3400 ~ "Grupo3",
                                                      o8 == 4100 | o8 == 4200 ~ "Grupo4",
                                                      o8 == 5100 | o8 == 5200 ~ "Grupo5",
                                                      o8 == 6100 | o8 == 6200 ~ "Grupo6",
                                                      o8 == 7100 | o8 == 7200 | o8 == 7300 | o8 == 7400 ~ "Grupo7",
                                                      o8 == 8100 | o8 == 8200 | o8 == 8300 ~ "Grupo8",
                                                      o8 == 9100 | o8 == 9200 | o8 == 9300 ~ "Grupo9",
                                                      o8 == 110 ~ "FFAA",
                                                      o8 == 2300 | o8 == 3300 ~ "Ensenanza",
                                                      TRUE ~ NA_character_))
casen20$ciuo <- as_factor(casen20$ciuo)
casen20$activ <- as_factor(casen20$activ)
casen20$p18 <- as_factor(casen20$p18)
casen20 <- casen20 %>% dplyr::mutate(rama2 = case_when(rama == 0 ~ "Actividad no especificada",
                                                       rama == 1 ~ "Agricultura y pesca", 
                                                       rama == 2 ~ "Mineria", 
                                                       rama == 3 ~ "Industria",
                                                       rama == 4 ~ "Electricidad, gas y agua", 
                                                       rama == 5 ~ "Construccion", 
                                                       rama == 6 ~ "Comercio y hoteleria", 
                                                       rama == 7 ~ "Transporte y comunicaciones", 
                                                       rama == 8 ~ "Establecimientos financieros",
                                                       rama == 9 ~ "Servicios comunales"))


casen20 <- casen20 %>% select(activ, o10, p18, estrato, expr, rama2, ciuo, o13)


# CASEN 2003
casen03$o9 <- car::recode(casen03$o9, "1 = 1; 2 = 2; c(3,4) = 3; 5 = 4; c(6,7) = 5; 8 = 6; 9 = 7") # 3 = publicos/ 4=privados (sin trb domest)
casen03$o9 <- as_factor(casen03$o9)
casen03$o7 <- car::recode(casen03$o7, "1110:1143 = 1100; 1210:1239 = 1200; 1310:1319 = 1300; 2111:2149 = 2100; 
                          2211:2230 = 2200; 2310:2359 = 2300; 2410:2460 = 2400; 3110:3152 = 3100; 3210:3242 = 3200;
                          3310:3340 = 3300; 3410:3480 = 3400; 4110:4190 = 4100; 4211:4223 = 4200; 5111:5169 = 5100;
                          5210:5230 = 5200; 6110:6154 = 6100; 6210 = 6200; 7111:7143 = 7100; 7210:7245 = 7200; 
                          7311:7346 = 7300; 7411:7442 = 7400; 8111:8172 = 8100; 8211:8290 = 8200; 8311:8340 = 8300;
                          9110:9162 = 9100; 9210:9212 = 9200; 9310:9333 = 9300; 110 = 110;  9999 = NA", as.factor = T)
casen03 <- casen03 %>% dplyr::mutate(ciuo = case_when(o7 == 1100 | o7 == 1200 | o7 == 1300 ~ "Grupo1",
                                                      o7 == 2100 | o7 == 2200 | o7 == 2400 ~ "Grupo2",
                                                      o7 == 3100 | o7 == 3200 | o7 == 3400 ~ "Grupo3",
                                                      o7 == 4100 | o7 == 4200 ~ "Grupo4",
                                                      o7 == 5100 | o7 == 5200 ~ "Grupo5",
                                                      o7 == 6100 | o7 == 6200 ~ "Grupo6",
                                                      o7 == 7100 | o7 == 7200 | o7 == 7300 | o7 == 7400 ~ "Grupo7",
                                                      o7 == 8100 | o7 == 8200 | o7 == 8300 ~ "Grupo8",
                                                      o7 == 9100 | o7 == 9200 | o7 == 9300 ~ "Grupo9",
                                                      o7 == 110 ~ "FFAA",
                                                      o7 == 2300 | o7 == 3300 ~ "Ensenanza",
                                                      TRUE ~ NA_character_))
casen03$ciuo <- as_factor(casen03$ciuo)
casen03$activ <- as_factor(casen03$activ)
casen03$r18 <- as_factor(casen03$r18)
casen03 <- casen03 %>% dplyr::mutate(rama2 = case_when(rama == 0 ~ "Actividad no especificada",
                                                       rama == 1 ~ "Agricultura y pesca", 
                                                       rama == 2 ~ "Mineria", 
                                                       rama == 3 ~ "Industria",
                                                       rama == 4 ~ "Electricidad, gas y agua", 
                                                       rama == 5 ~ "Construccion", 
                                                       rama == 6 ~ "Comercio y hoteleria", 
                                                       rama == 7 ~ "Transporte y comunicaciones", 
                                                       rama == 8 ~ "Establecimientos financieros",
                                                       rama == 9 ~ "Servicios comunales"))


casen03 <- casen03 %>% select(activ, o9, ciuo, rama2, r18, o14, estrato, expr)

# CASEN 2006
casen06$o19 <- car::recode(casen06$o19, "1 = 1; 2 = 2; c(3,4) = 3; 5 = 4; c(6,7) = 5; 8 = 6; 9 = 7") # 3 = publicos/ 4=privados (sin trb domest)
casen06$o19 <- as_factor(casen06$o19)
casen06$activ <- as_factor(casen06$activ)
casen06 <- casen06 %>% dplyr::mutate(rama2 = case_when(rama == 0 ~ "Actividad no especificada",
                                                       rama == 1 ~ "Agricultura y pesca", 
                                                       rama == 2 ~ "Mineria", 
                                                       rama == 3 ~ "Industria",
                                                       rama == 4 ~ "Electricidad, gas y agua", 
                                                       rama == 5 ~ "Construccion", 
                                                       rama == 6 ~ "Comercio y hoteleria", 
                                                       rama == 7 ~ "Transporte y comunicaciones", 
                                                       rama == 8 ~ "Establecimientos financieros",
                                                       rama == 9 ~ "Servicios comunales"))

casen06 <- casen06 %>% select(activ, rama2, o19, expr, estrato, o13) 

# CASEN 2009
casen09$activ <- as_factor(casen09$activ)
casen09$o23 <- car::recode(casen09$o23, "1 = 1; 2 = 2; c(3,4) = 3; 5 = 4; c(6,7) = 5; 8 = 6; 9 = 7") # 3 publico 4 privados
casen09$o23 <- as_factor(casen09$o23)
casen09$t18a <- as_factor(casen09$t18a)
casen09$t18b <- as_factor(casen09$t18b)
casen09$c_o12 <- as_factor(casen09$c_o12)
casen09$c_o12 <- car::recode(casen09$c_o12, recodes = "1100:1319='Grupo1'; 2100:2230='Grupo2'; 2300:2359 ='Ensenanza';
                          2400:2460='Grupo2'; 3100:3242='Grupo3'; 3300:3340='Ensenanza'; 3400:3480='Grupo3'; 
                          4100:4223='Grupo4'; 5100:5230='Grupo5'; 6100:6210='Grupo6'; 7100:7442='Grupo7'; 
                          8100:8340='Grupo8'; 9100:9333='Grupo9'; 110='FFAA';  9999 = NA")
casen09 <- casen09 %>% dplyr::mutate(rama2 = case_when(rama == 0 ~ "Actividad no especificada",
                                                       rama == 1 ~ "Agricultura y pesca", 
                                                       rama == 2 ~ "Mineria", 
                                                       rama == 3 ~ "Industria",
                                                       rama == 4 ~ "Electricidad, gas y agua", 
                                                       rama == 5 ~ "Construccion", 
                                                       rama == 6 ~ "Comercio y hoteleria", 
                                                       rama == 7 ~ "Transporte y comunicaciones", 
                                                       rama == 8 ~ "Establecimientos financieros",
                                                       rama == 9 ~ "Servicios comunales"))
casen09 <- casen09[!is.na(casen09$expr_p),]
casen09 <- casen09 %>% mutate(afiliado = case_when(t18b == 1 ~ "Vecinal",
                                                   t18b == 2 ~ "Deportivo",
                                                   t18b == 3 ~ "Estudiantil",
                                                   t18b == 4 ~ "Voluntariado",
                                                   t18b == 5 ~ "Asociacion",
                                                   t18b == 6 ~ "Sindicato", 
                                                   t18b == 7 ~ "PP",
                                                   t18b == 8 ~ "AsoProd",
                                                   t18b == 9 ~ "Indigena",
                                                   t18b == 10 ~ "Discapacidad",
                                                   t18b == 11 ~ "AdultoMayor", 
                                                   t18b == 12 ~ "Juvenil",
                                                   is.na(t18b)&t18a == 1 ~ "Vecinal",
                                                   is.na(t18b)&t18a == 2 ~ "Deportivo",
                                                   is.na(t18b)&t18a == 3 ~ "Estudiantil",
                                                   is.na(t18b)&t18a == 4 ~ "Voluntariado",
                                                   is.na(t18b)&t18a == 5 ~ "Asociacion",
                                                   is.na(t18b)&t18a == 6 ~ "Sindicato",
                                                   is.na(t18b)&t18a == 7 ~ "PP",
                                                   is.na(t18b)&t18a == 8 ~ "AsoProd",
                                                   is.na(t18b)&t18a == 9 ~ "Indigena",
                                                   is.na(t18b)&t18a == 10 ~ "Discapacidad",
                                                   is.na(t18b)&t18a == 11 ~ "AdultoMayor",
                                                   is.na(t18b)&t18a == 12 ~ "Juvenil"))


casen09 <- casen09 %>% select(activ, o23, afiliado, c_o12, rama2, o14, expr_p, estrato)


# CASEN 2011 
casen11$activ <- as_factor(casen11$activ)
casen11$o15 <- car::recode(casen11$o15, "1 = 1; 2 = 2; c(3,4) = 3; 5 = 4; c(6,7) = 5; 8 = 6; 9 = 7") # 3 publico 4 privados
casen11$o15 <- as_factor(casen11$o15)
casen11 <- casen11 %>% mutate(rama2 = case_when(rama1 == 1 | rama1 == 2 ~ "Agricultura y pesca",
                                                rama1 == 3 ~ "Mineria",
                                                rama1 == 4 ~ "Industria",
                                                rama1 == 5 ~ "Electricidad, gas y agua", 
                                                rama1 == 6 ~ "Construccion",
                                                rama1 == 7 | rama1 == 8 ~ "Comercio y hoteleria",
                                                rama1 == 9 ~ "Transporte y comunicaciones",
                                                rama1 == 10 | rama1 == 11 ~ "Establecimientos financieros",
                                                rama1 == 12 | rama1 == 13 | rama1 == 14 | rama1 == 15 | rama1 == 16 |rama1 ==17 ~ "Servicios comunales",
                                                rama1 == 18 ~ "Actividad no especificada"))

casen11 <- casen11 %>% select(activ, o15, rama2, o24, varstrat, expr_r2)

# CASEN 2013
casen13$activ <- as_factor(casen13$activ)
casen13$o15 <- as_factor(casen13$o15)
casen13$o15 <- car::recode(casen13$o15, "1 = 1; 2 = 2; c(3,4) = 3; 5 = 4; c(6,7) = 5; 8 = 6; 9 = 7") # 3 publico 4 privados
casen13$r9 <- as_factor(casen13$r9)
casen13$oficio4_n  <- as_factor(casen13$oficio4_n)
casen13$oficio4_n <- car::recode(casen13$oficio4_n, recodes = "1110:1319='Grupo1'; 2111:2230='Grupo2'; 2310:2359 ='Ensenanza';
                          2410:2460='Grupo2'; 3111:3242='Grupo3'; 3310:3340='Ensenanza'; 3411:3480='Grupo3'; 
                          4110:4223='Grupo4'; 5111:5230='Grupo5'; 6100:6210='Grupo6'; 7110:7442='Grupo7'; 
                          8111:8340='Grupo8'; 9111:9333='Grupo9'; 110='FFAA';  9999 = NA")
casen13 <- casen13 %>% mutate (rama2 = case_when(rama1 == 1 | rama1 == 2 ~ "Agricultura y pesca",
                                                 rama1 == 3 ~ "Mineria",
                                                 rama1 == 4 ~ "Industria",
                                                 rama1 == 5 ~ "Electricidad, gas y agua", 
                                                 rama1 == 6 ~ "Construccion",
                                                 rama1 == 7 | rama1 == 8 ~ "Comercio y hoteleria",
                                                 rama1 == 9 ~ "Transporte y comunicaciones",
                                                 rama1 == 10 | rama1 == 11 ~ "Establecimientos financieros",
                                                 rama1 == 12 | rama1 == 13 | rama1 == 14 | rama1 == 15 | rama1 == 16 |rama1 ==17 ~ "Servicios comunales",
                                                 rama1 == 99 ~ "Actividad no especificada"))

casen13 <- casen13 %>% select(activ, o15, rama2, o24, r9, oficio4_n, varstrat, expr)

# CASEN 2015
casen15$activ <- as_factor(casen15$activ)
casen15$o15 <- as_factor(casen15$o15)
casen15$o15 <- car::recode(casen15$o15, "1 = 1; 2 = 2; c(3,4) = 3; 5 = 4; c(6,7) = 5; 8 = 6; 9 = 7")# 3 publico 4 privados
casen15$o24a <- as_factor(casen15$o24a)
casen15$o24b <- as_factor(casen15$o24b)
casen15$o24d <- as_factor(casen15$o24d)
casen15 <- casen15 %>% mutate (rama2 = case_when(rama1 == 1 | rama1 == 2 ~ "Agricultura y pesca",
                                                 rama1 == 3 ~ "Mineria",
                                                 rama1 == 4 ~ "Industria",
                                                 rama1 == 5 ~ "Electricidad, gas y agua", 
                                                 rama1 == 6 ~ "Construccion",
                                                 rama1 == 7 | rama1 == 8 ~ "Comercio y hoteleria",
                                                 rama1 == 9 ~ "Transporte y comunicaciones",
                                                 rama1 == 10 | rama1 == 11 ~ "Establecimientos financieros",
                                                 rama1 == 12 | rama1 == 13 | rama1 == 14 | rama1 == 15 | rama1 == 16 |rama1 ==17 ~ "Servicios comunales",
                                                 rama1 == 99 ~ "Actividad no especificada"))
casen15$oficio4[casen15$oficio4 == ""] <- NA
casen15$oficio4 <- as_factor(casen15$oficio4)
casen15$oficio4 <- car::recode(casen15$oficio4, recodes = "1110:1319='Grupo1'; 2111:2230='Grupo2'; 2310:2359 ='Ensenanza';
                          2410:2460='Grupo2'; 3111:3242='Grupo3'; 3310:3340='Ensenanza'; 3411:3480='Grupo3'; 
                          4110:4223='Grupo4'; 5111:5230='Grupo5'; 6100:6210='Grupo6'; 7110:7442='Grupo7'; 
                          8111:8340='Grupo8'; 9111:9333='Grupo9'; 0110='FFAA';  9999 = NA")

casen15 <- casen15 %>% select(activ, o15, rama2, o23, o24a, o24b, o24d, varstrat, expr, oficio4)

# CASEN 2017
casen17$o15 <- as_factor(casen17$o15)
casen17$o15 <- car::recode(casen17$o15, "1 = 1; 2 = 2; c(3,4) = 3; 5 = 4; c(6,7) = 5; 8 = 6; 9 = 7")# 3 publico 4 privados
casen17$activ <- as_factor(casen17$activ)
casen17$o24a <- as_factor(casen17$o24a)
casen17$o24b <- as_factor(casen17$o24b)
casen17$o24d <- as_factor(casen17$o24d)
casen17$oficio4 <- as_factor(casen17$oficio4)
casen17$oficio4 <- car::recode(casen17$oficio4, recodes = "1110:1319='Grupo1'; 2111:2230='Grupo2'; 2310:2359 ='Ensenanza';
                          2410:2460='Grupo2'; 3111:3242='Grupo3'; 3310:3340='Ensenanza'; 3411:3480='Grupo3'; 
                          4110:4223='Grupo4'; 5111:5230='Grupo5'; 6100:6210='Grupo6'; 7110:7442='Grupo7'; 
                          8111:8340='Grupo8'; 9111:9333='Grupo9'; 110='FFAA';  9999 = NA")
casen17 <- casen17 %>% mutate (rama2 = case_when(rama1 == 1 | rama1 == 2 ~ "Agricultura y pesca",
                                                 rama1 == 3 ~ "Mineria",
                                                 rama1 == 4 ~ "Industria",
                                                 rama1 == 5 ~ "Electricidad, gas y agua", 
                                                 rama1 == 6 ~ "Construccion",
                                                 rama1 == 7 | rama1 == 8 ~ "Comercio y hoteleria",
                                                 rama1 == 9 ~ "Transporte y comunicaciones",
                                                 rama1 == 10 | rama1 == 11 ~ "Establecimientos financieros",
                                                 rama1 == 12 | rama1 == 13 | rama1 == 14 | rama1 == 15 | rama1 == 16 |rama1 ==17 ~ "Servicios comunales",
                                                 rama1 == 999 ~ "Actividad no especificada"))

casen17 <- casen17 %>% select(activ, o15, rama2, oficio4, o23, o24a, o24b, o24d, varstrat, expr)

# 4. Guardar ----
save(casen96, file = "input/casen96.RData")
save(casen98, file = "input/casen98.RData")
save(casen20, file = "input/casen20.RData")
save(casen03, file = "input/casen03.RData")
save(casen06, file = "input/casen06.RData")
save(casen09, file = "input/casen09.RData")
save(casen11, file = "input/casen11.RData")
save(casen13, file = "input/casen13.RData")
save(casen15, file = "input/casen15.RData")
save(casen17, file = "input/casen17.RData")