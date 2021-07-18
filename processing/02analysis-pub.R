# Code 2: Analysis CASEN 

# 1. Cargar librerias ----
if (!require("pacman"))install.packages("pacman")

pacman::p_load(tidyverse, rvest, xml2, lubridate, openxlsx, readxl, ggrepel, tibble, writexl, haven,
               dplyr, car, summarytools, ggpubr, sjmisc, sjlabelled, stargazer, srvyr)

options(scipen=999)

# 2. Cargar datos ----
load("input/casen96.RData")
load("input/casen98.RData")
load("input/casen20.RData")
load("input/casen03.RData")
load("input/casen06.RData")
load("input/casen09.RData")
load("input/casen11.RData")
load("input/casen13.RData")
load("input/casen15.RData")
load("input/casen17.RData")


# 3. Diseño muestral ----

# CASEN 96
db96 <- casen96 %>% select(o8, activ, rama2, expr, estrato)

db96_pond <- db96 %>% as_survey_design(ids = 1, strata = estrato, weights = expr)  

db96 %>% filter(activ == 1 & o8 == 3) %>% select(expr) %>% sum() # COMPROBAR SIMPLE

db96_pond %>% filter(activ == 1 & o8 == 3) %>% 
  summarise(survey_total(vartype = c("ci", "cv", "var"), level = 0.95, na.rm = T)) #COMPROBAR CI

a <- db96_pond %>% filter(activ == 1 & o8 == 3) %>% 
  summarise(ocup = survey_total(na.rm = T)) %>% 
  mutate(ano = "1996") 

# CASEN 98
db98 <- casen98 %>% select(o21, o8, estrato, expr, rama2, oficio)

db98_pond <- db98 %>% as_survey_design(ids = 1, strata = estrato, weights = expr)

casen98 %>% filter(o21 == 1 & (oficio == 1 & o8 ==3)) %>% select(expr) %>% sum() # COMPROBAR SIMPLE / SE FILTRA POR OFICIO

db98_pond %>% filter(o21 == 1 & (oficio == 1 & o8 ==3)) %>%
  summarise(survey_total(vartype = c("ci", "cv", "var"), level = 0.95, na.rm = T)) # COMRPOBAR CI

b <- db98_pond %>% filter(o21 == 1 & (oficio == 1 & o8 ==3)) %>%
  summarise(ocup= survey_total(na.rm = T)) %>% 
  mutate(ano = "1998")

# CASEN 2000

db20 <- casen20 %>% select(activ, o10, p18, estrato, expr, rama2, ciuo, o13)

db20_pond <- db20 %>% as_survey_design(ids = 1, strata = estrato, weights = expr)

## TOTAL OCC PUB
casen20 %>% filter(activ == 1 & o10==3) %>% select(expr) %>% sum() # TOTAL OCC PUBLICOS SIMPLE

db20_pond %>% filter(activ == 1 & o10==3) %>%
  summarise(survey_total(vartype = c("ci", "cv", "var"), level = 0.95, na.rm = T)) # TOTAL OCC PUB CI

c <- db20_pond %>% filter(activ == 1 & o10==3) %>%
  summarise(ocup = survey_total(na.rm = T)) %>% 
  mutate(ano = "2000") # GUARDAR

## AFILIADOS OCC PUB 

casen20 %>% filter(activ == 1 & (o10==3 & p18 == 18)) %>% 
  select(expr) %>% sum() # TOTAL OCC AFILIADOS SINDICATOS PUBLICOS

casen20 %>% filter(ciuo == "Ensenanza" & (o10 == 3 & p18 == 10))%>% 
  select(expr) %>% sum() # PUBLICOS DE ENSENANZA AFILIADOS A ASOCIACION

casen20 %>% filter((o10==3 & p18 ==18) | ((ciuo == "Ensenanza" & o10==3) & p18==10)) %>%
  select(expr) %>% sum() #  TOTAL DE AFILIADOS PUBLICOS (INCLUIDO ENSEÑANZA) EN SINDICATOS Y ASOCIACIONES (SOLO DE ENSEÑANZA)

db20_pond %>% filter((o10==3 & p18 ==18) | ((ciuo == "Ensenanza" & o10==3) & p18==10)) %>%
  summarise(survey_total(vartype = c("ci", "cv", "var"), level = 0.95, na.rm = T)) # CON CI

c$ocup_afi <- db20_pond %>% filter((o10==3 & p18 ==18) | ((ciuo == "Ensenanza" & o10==3) & p18==10)) %>%
  summarise(survey_total(vartype = NULL, na.rm = T)) # GUARDAR (se guardan afiliados publicos incluidos ensenanza)

# CASEN 2003

db03 <- casen03 %>% select(activ, o9, ciuo, rama2, r18, o14, estrato, expr)

db03_pond <- db03 %>% as_survey_design(ids = 1, strata = estrato, weights = expr)

casen03 %>% filter(activ == 1 & o9 == 3) %>% select(expr) %>% sum() # TOTAL OCC PUBLICOS SIMPLE  

db03_pond %>% filter(activ == 1 & o9 == 3) %>%
  summarise(survey_total(vartype = c("ci", "cv", "var"), level = 0.95, na.rm = T)) # TOTAL CI

d <- db03_pond %>% filter(activ == 1 & o9 == 3) %>%
  summarise(ocup = survey_total(na.rm = T)) %>% 
  mutate(ano = "2003") # GUARDAR

casen03 %>% filter((o9 == 3 & r18 == 20) | ((ciuo == "Ensenanza" & o9 == 3) & r18 == 11)) %>% 
  select(expr) %>% sum() # TOTAL AFI PUB (incluye ensenñanza para colegios profesionales)

db03_pond %>% filter((o9 == 3 & r18 == 20) | ((ciuo == "Ensenanza" & o9 == 3) & r18 == 11)) %>% 
  summarise(survey_total(vartype = c("ci", "cv", "var"), level = 0.95, na.rm = T)) # CON CI

d$ocup_afi <- db03_pond %>% filter((o9 == 3 & r18 == 20) | ((ciuo == "Ensenanza" & o9 == 3) & r18 == 11)) %>%
  summarise(survey_total(vartype = NULL, na.rm = T)) # GUARDAR (se guardan afiliados publicos incluidos ensenanza)

# CASEN 2006
db06 <- casen06 %>% select(activ, rama2, o19, expr, estrato, o13)

db06_pond <- db06 %>% as_survey_design(ids = 1, strata = estrato, weights = expr)

casen06 %>% filter(activ == 1 & o19 == 3) %>% select(expr) %>% sum()

db06_pond %>% filter(activ == 1 & o19 == 3) %>% 
  summarise(survey_total(vartype = c("ci", "cv", "var"), level = 0.95, na.rm = T)) #COMPROBAR CI

e <- db06_pond %>% filter(activ == 1 & o19 == 3) %>% 
  summarise(ocup = survey_total(na.rm = T)) %>% 
  mutate(ano = "2006") # GUARDAR

# CASEN 2009
db09 <- casen09 %>% select(activ, o23, afiliado, c_o12, rama2, o14, expr_p, estrato)

db09_pond <- db09 %>% as_survey_design(ids = 1, strata = estrato,  weights = expr_p)

casen09 %>% filter(activ == 1 & o23 == 3) %>% select(expr_p) %>% sum() # TOTAL OCUP PUB 

db09_pond %>% filter(activ == 1 & o23 == 3) %>%
  summarise(survey_total(vartype = c("ci", "cv", "var"), level = 0.95, na.rm = T)) #COMPROBAR CI

f <- db09_pond %>% filter(activ == 1 & o23 == 3) %>%
  summarise(ocup = survey_total(na.rm = T)) %>% 
  mutate(ano = "2009") # GUARDAR

casen09 %>% filter((o23 == 3 & afiliado== "Sindicato") | ((o23 == 3 & c_o12 == "Ensenanza") & afiliado == "Asociacion")) %>% 
  select(expr_p) %>% sum() #COMPROBAR SIMPLE

db09_pond %>% filter((o23 == 3 & afiliado== "Sindicato") | ((o23 == 3 & c_o12 == "Ensenanza") & afiliado == "Asociacion")) %>% 
  summarise(survey_total(vartype = c("ci", "cv", "var"), level = 0.95, na.rm = T)) #COMPROBAR CI

f$ocup_afi <- db09_pond %>% filter((o23 == 3 & afiliado== "Sindicato") | ((o23 == 3 & c_o12 == "Ensenanza") & afiliado == "Asociacion")) %>%
  summarise(survey_total(vartype = NULL, na.rm = T))

# CASEN 2011

db11 <- casen11 %>% select(activ, o15, rama2, o24, varstrat, expr_r2)

db11_pond <- db11 %>% as_survey_design(ids = 1, strata = varstrat, weights = expr_r2)

casen11 %>% filter(activ == 1 & o15 == 3) %>% select(expr_r2) %>% sum() # COMPROBAR SIMPLE

db11_pond %>% filter(activ == 1 & o15 == 3) %>% 
  summarise(survey_total(vartype = c("ci", "cv", "var"), level = 0.95, na.rm = T)) #COMPROBAR CI

g <- db11_pond %>% filter(activ == 1 & o15 == 3) %>%
  summarise(ocup = survey_total(na.rm = T)) %>% 
  mutate(ano = "2011") # GUARDAR

# CASEN 2013
db13 <- casen13 %>% select(activ, o15, rama2, o24, r9, oficio4_n, varstrat, expr)

db13_pond <- db13 %>% as_survey_design(ids = 1, strata = varstrat, weights = expr)

casen13 %>% filter(activ==1 & o15==3) %>% select(expr) %>% sum()

db13_pond %>% filter(activ==1 & o15==3) %>% 
  summarise(survey_total(vartype = c("ci", "cv", "var"), level = 0.95, na.rm = T)) #COMPROBAR CI

h <- db13_pond %>% filter(activ==1 & o15==3) %>%
  summarise(ocup = survey_total(na.rm = T)) %>% 
  mutate(ano = "2013") # GUARDAR

casen13 %>% filter(o15==3 & r9==12) %>% select(expr) %>% sum()

db13_pond %>% filter(o15==3 & r9==12) %>%
  summarise(survey_total(vartype = c("ci", "cv", "var"), level = 0.95, na.rm = T)) #COMPROBAR CI

h$ocup_afi <- db13_pond %>% filter(o15==3 & r9==12) %>%
  summarise(survey_total(vartype = NULL, na.rm = T))

# CASEN 2015

db15 <- casen15 %>% select(activ, o15, rama2, o23, o24a, o24b, o24d, varstrat, expr, oficio4) # se exlcuye aso gremial ya que da 0

db15_pond <- db15 %>% as_survey_design(ids = 1, strata = varstrat, weights = expr)

casen15 %>% filter(activ == 1 & o15 ==3) %>% select(expr) %>% sum()

db15_pond %>% filter(activ == 1 & o15 ==3) %>% 
  summarise(survey_total(vartype = c("ci", "cv", "var"), level = 0.95, na.rm = T))

i <- db15_pond %>% filter(activ == 1 & o15 ==3) %>%
  summarise(ocup = survey_total(na.rm = T)) %>% 
  mutate(ano = "2015") # GUARDAR

casen15 %>% filter((o15 == 3 & (o24a == 1 | o24b == 1)) | (o15==3 & oficio4 == "Ensenanza") & (o24d==1)) %>% 
  select(expr) %>% sum() #AFI

db15_pond %>% filter((o15 == 3 & (o24a == 1 | o24b == 1)) | (o15==3 & oficio4 == "Ensenanza") & (o24d==1)) %>% 
  summarise(survey_total(vartype = c("ci", "cv", "var"), level = 0.95, na.rm = T)) 

i$ocup_afi <- db15_pond %>% filter((o15 == 3 & (o24a == 1 | o24b == 1)) | (o15==3 & oficio4 == "Ensenanza") & (o24d==1)) %>% 
  summarise(survey_total(vartype = NULL, na.rm = T))

# CASEN 2017

db17 <- casen17 %>% select(activ, o15, rama2, oficio4, o23, o24a, o24b, o24d, varstrat, expr)

db17_pond <- db17 %>% as_survey_design(ids = 1, strata = varstrat, weights = expr)

casen17 %>% filter(activ == 1 & o15 ==3) %>% select(expr) %>% sum()

db17_pond %>% filter(activ == 1 & o15 ==3) %>% 
  summarise(survey_total(vartype = c("ci", "cv", "var"), level = 0.95, na.rm = T))

j <- db17_pond %>% filter(activ == 1 & o15 ==3) %>% 
  summarise(ocup = survey_total(na.rm = T)) %>% 
  mutate(ano = "2017") # GUARDAR

casen17 %>% filter((o15==3 & (o24a == 1 | o24b ==1)) | ((o15==3 & oficio4== "Ensenanza") & o24d ==1)) %>%
  select(expr) %>% sum()

db17_pond %>% filter((o15==3 & (o24a == 1 | o24b ==1)) | ((o15==3 & oficio4== "Ensenanza") & o24d ==1)) %>%
  summarise(survey_total(vartype = c("ci", "cv", "var"), level = 0.95, na.rm = T))

j$ocup_afi <- db17_pond %>% filter((o15==3 & (o24a == 1 | o24b ==1)) | ((o15==3 & oficio4== "Ensenanza") & o24d ==1)) %>%
  summarise(survey_total(vartype = NULL, na.rm = T))

# 4. Limpiar y unir data ----

# 4.1. Limpiar data ponderada
a <- a[-c(2)]
b <- b[-c(2)]
c <- c[-c(2)]
d <- d[-c(2)]
e <- e[-c(2)]
f <- f[-c(2)]
g <- g[-c(2)]
h <- h[-c(2)]
i <- i[-c(2)]
j <- j[-c(2)]


# 5.2. Unir 
a <- as_factor(a)
b <- as_factor(b)
c <- as_factor(c)
d <- as_factor(d)
e <- as_factor(e)
f <- as_factor(f)
g <- as_factor(g)
h <- as_factor(h)
i <- as_factor(i)
j <- as_factor(j)

cuadro96_98 <- full_join(a, b, by = NULL)
cuadro20_03 <- full_join(c, d, by = NULL)
cuadro06_09<- full_join(e, f, by = NULL)
cuadro11_13 <- full_join(g, h, by = NULL)
cuadro15_17 <- full_join(i, j, by = NULL)

cuadro96_03 <- full_join(cuadro96_98, cuadro20_03, by = NULL)
cuadro06_13 <- full_join(cuadro06_09, cuadro11_13, by = NULL)
cuadro96_13 <- full_join(cuadro96_03, cuadro06_13, by = NULL)
cuadrofinal <- full_join(cuadro96_13, cuadro15_17, by = NULL)

# 5. Guardar y exportar ----
serie_ocup_pub <- cuadrofinal
save(serie_ocup_pub, file = "output/serie_pub_1996_2017.RData")
write_xlsx(serie_ocup_pub, "output/serie_pub_1996_2017.xlsx", col_names = TRUE,format_headers = TRUE)