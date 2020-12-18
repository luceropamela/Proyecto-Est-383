#### LIMPIEZA DE LAS BASES DE DATOS

library(foreign)
library(dplyr)
library(tidyr)
setwd("C:\\Users\\JULIA\\Documents\\PAMELA\\R Segundo semestre\\BASES DE DATOS\\violencia mujer")
dir()
mc<-read.spss("EPCVCM_Casadas.sav",to.data.frame = T)
mp<-read.spss("EPCVCM_Persona.sav",to.data.frame = T)
ms<-read.spss("EPCVCM_Separadas.sav",to.data.frame = T)
msol<-read.spss("EPCVCM_Solteras.sav",to.data.frame = T)
mviv<-read.spss("EPCVCM_Vivienda.sav",to.data.frame = T)


### BASE DE DATOS DE PERSONAS
bdper<-mp %>% mutate(edad=s2_03,sexo=s2_02,parentesco=s2_05,grado=s3_01,estado=s2_10) %>% filter(sexo=="2. Mujer" & edad >=15) %>% 
  select(folio,depto,sexo,edad,grado,estado)
# Nivel de instruccion
ninguno<-c("10. NINGUNO Y NO SABE LEER Y ESCRIBIR","11. NINGUNO PERO SABE LEER Y ESCRIBIR")
prim_incom<-c("13. EDUCACIÓN INICIAL O PRE-ESCOLAR (PRE KINDER/KINDER)","21. BÁSICO (1 A 5 AÑOS) - SISTEMA ESCOLAR ANTIGUO","22. INTERMEDIO (1 A 3 AÑOS) - SISTEMA ESCOLAR ANTIGUO","23. MEDIO (1 A 4 AÑOS) - SISTEMA ESCOLAR ANTIGUO","51. EDUCACIÓN BÁSICA DE ADULTOS (EBA) - EDUCACIÓN DE ADULTOS (Sistema Antiguo)          
[25] 75. POSTGRADO DOCTORADO - EDUCACIÓN SUPERIOR","51. EDUCACIÓN BÁSICA DE ADULTOS (EBA) - EDUCACIÓN DE ADULTOS (Sistema Antiguo)","52. CENTRO DE EDUCACIÓN MEDIA DE ADULTOS (CEMA) - EDUCACIÓN DE ADULTOS (Sistema Antiguo)")
prim_completa<-c("31. PRIMARIA (1 A 8 AÑOS) - SISTEMA ESCOLAR ANTERIOR","41. PRIMARIA (1 A 6 AÑOS) - SISTEMA ESCOLAR ACTUAL","62. EDUCACIÓN PRIMARIA DE ADULTOS (EPA) - EDUCACIÓN ALTERNATIVA Y ESPECIAL")
secun_incompl<-c("52. CENTRO DE EDUCACIÓN MEDIA DE ADULTOS (CEMA) - EDUCACIÓN DE ADULTOS (Sistema Antiguo)
","61. EDUCACIÓN JUVENIL ALTERNATIVA (EJA) - EDUCACIÓN ALTERNATIVA Y ESPECIAL")
secun_com<-c("32. SECUNDARIA (1 A 4 AÑOS) - SISTEMA ESCOLAR ANTERIOR","42. SECUNDARIA (1 A 6 AÑOS) - SISTEMA ESCOLAR ACTUAL","63. EDUCACIÓN SECUNDARIA DE ADULTOS (ESA) - EDUCACIÓN ALTERNATIVA Y ESPECIAL")
superior<-c("71. NORMAL (ESCUELA SUP. DE FORMACIÒN DE MAESTROS) - EDUCACIÓN SUPERIOR","72. UNIVERSIDAD - EDUCACIÓN SUPERIOR","73. POSTGRADO DIPLOMADO - EDUCACIÓN SUPERIOR","74. POSTGRADO MAESTRÍA - EDUCACIÓN SUPERIOR","75. POSTGRADO DOCTORADO - EDUCACIÓN SUPERIOR","76. TÉCNICO DE UNIVERSIDAD - EDUCACIÓN SUPERIOR","77. TÉCNICO DE INSTITUTO (Duración mayor o igual a 1 año) - EDUCACIÓN SUPERIOR","78. INSTITUTOS DE FORMACIÓN MILITAR Y POLICIAL - EDUCACIÓN SUPERIOR","79. EDUCACIÓN TÉCNICA DE ADULTOS (ETA) - EDUCACIÓN SUPERIOR")
otros<-c("12. CURSO DE ALFABETIZACIÓN","64. PROGRAMA NACIONAL DE POST ALFABETIZACIÓN - EDUCACIÓN ALTERNATIVA Y ESPECIAL","65. EDUCACIÓN ESPECIAL - EDUCACIÓN ALTERNATIVA Y ESPECIAL")
names(bdper)
bdper<- bdper %>% mutate(ninguno=(grado %in% ninguno),primaria_incompleta=(grado %in% prim_incom),
                         primaria_completa=(grado %in% prim_completa),secundaria_incompleta=(grado %in% secun_incompl),
                         secundaria_completa=(grado %in% secun_com),superior=(grado %in% superior),
                         otros=(grado %in% otros))

# volviendole en una variable de nivel de instruccion
aux<-names(bdper)[7:13]
j<-0
for (i in aux) {
  j<-j+1
  bdper[,i]<-as.character(bdper[,i])
  bdper[grepl("TRUE",bdper[,i]),i]<-j
  bdper[grepl("FALSE",bdper[,i]),i]<-0
  }

for (i in aux) {
  bdper[,i]<-as.numeric(bdper[,i])
}
bdper<- bdper %>% mutate(gedad=cut(edad,c(14,28,39,49,59,100),labels = c("15 a 28","29 a 39","40 a 49","50 a 59","Mayor a 60")))
bdper<-bdper %>% mutate(Nivel_instruccion = (select(., ninguno:otros) %>% apply(1, sum, na.rm=TRUE)))
bdper$Nivel_instruccion<-factor(bdper$Nivel_instruccion,levels = 1:7,labels = c("Ninguno","Primaria incompleta","Primaria completa","Secundaria incompleta","Secundaria completa","Superior","Otros"))
bdper<-bdper %>% select(-c(7:13))

### BASE DE DATOS DE CASADAS
viol_psicologica<-c("S3_10_01","S3_10_02","S3_10_03","S3_10_04","S3_10_05","S3_10_06",
                    "S3_10_17")
viol_fisica<-c("S3_10_12","S3_10_13","S3_10_14","S3_10_15","S3_10_16","S3_10_18",
               "S3_10_19")
viol_sexual<-c("S3_10_20","S3_10_21","S3_10_22")
viol_econom<-c("S3_10_07","S3_10_08","S3_10_09","S3_10_10","S3_10_11")
violencia<-c("1. Muchas veces","2. Pocas veces","3. Una vez")
no_violencia<-c("4. No ocurrió","5. No recuerda","6. No aplica")

bd_casada1<-bdper %>% filter(estado=="2. CASADO/A"| estado=="3. CONVIVIENTE O CONCUBINO/A")
bd_casada<-mc %>% select(folio,area,depto,viol_psicologica,viol_fisica,
                         viol_sexual,viol_econom) %>% mutate(psi1=(S3_10_01 %in% violencia)*1,
                        psi2=(S3_10_02 %in% violencia)*1,psi3=(S3_10_03 %in% violencia)*1,
                        psi4=(S3_10_04 %in% violencia)*1,psi5=(S3_10_05 %in% violencia)*1,
                        psi6=(S3_10_06 %in% violencia)*1,psi7=(S3_10_17%in% violencia)*1,
                        fis1=(S3_10_12 %in% violencia)*1,fis2=(S3_10_13 %in% violencia)*1,
                        fis3=(S3_10_14 %in% violencia)*1,fis4=(S3_10_15 %in% violencia)*1,
                        fis5=(S3_10_16 %in% violencia)*1,fis6=(S3_10_18 %in% violencia)*1,
                        fis7=(S3_10_19 %in% violencia)*1,sex1=(S3_10_20 %in% violencia)*1,
                        sex2=(S3_10_21 %in% violencia)*1,sex3=(S3_10_22 %in% violencia)*1,
                        econ1=(S3_10_07 %in% violencia)*1,econ2=(S3_10_08 %in% violencia)*1,
                        econ3=(S3_10_09 %in% violencia)*1,econ4=(S3_10_10 %in% violencia)*1,
                        econ5=(S3_10_11 %in% violencia)*1) %>% 
  mutate(v_psi = (select(., psi1:psi7) %>% apply(1, sum, na.rm=TRUE))>1,
         v_fis = (select(., fis1:fis7) %>% apply(1, sum, na.rm=TRUE))>1,
         v_sex = (select(., sex1:sex3) %>% apply(1, sum, na.rm=TRUE))>1,
         v_econ = (select(., econ1:econ5) %>% apply(1, sum, na.rm=TRUE))>1) %>% 
  select(-c(4:47))

casada<-merge(bd_casada1,bd_casada)

## BASE DE DATOS SOLTERA
viol_psicologica<-c("S3_10_01","S3_10_02","S3_10_03","S3_10_04","S3_10_05","S3_10_06",
                    "S3_10_17")
viol_fisica<-c("S3_10_12","S3_10_13","S3_10_14","S3_10_15","S3_10_16","S3_10_18",
               "S3_10_19")
viol_sexual<-c("S3_10_20","S3_10_21","S3_10_22")
viol_econom<-c("S3_10_07","S3_10_08","S3_10_09","S3_10_10","S3_10_11")
violencia<-c("1. Muchas veces","2. Pocas veces","3. Una vez")
no_violencia<-c("4. No ocurrió","5. No recuerda","6. No aplica")
unique(bdper$estado)
bd_soltera1<-bdper %>% filter(estado=="1. SOLTERO/A")
bd_soltera<-msol %>% select(folio,area,depto,viol_psicologica,viol_fisica,viol_sexual,viol_econom) %>% mutate(psi1=(S3_10_01 %in% violencia)*1,
                         psi2=(S3_10_02 %in% violencia)*1,psi3=(S3_10_03 %in% violencia)*1,
                         psi4=(S3_10_04 %in% violencia)*1,psi5=(S3_10_05 %in% violencia)*1,
                         psi6=(S3_10_06 %in% violencia)*1,psi7=(S3_10_17%in% violencia)*1,
                         fis1=(S3_10_12 %in% violencia)*1,fis2=(S3_10_13 %in% violencia)*1,
                         fis3=(S3_10_14 %in% violencia)*1,fis4=(S3_10_15 %in% violencia)*1,
                         fis5=(S3_10_16 %in% violencia)*1,fis6=(S3_10_18 %in% violencia)*1,
                         fis7=(S3_10_19 %in% violencia)*1,sex1=(S3_10_20 %in% violencia)*1,
                         sex2=(S3_10_21 %in% violencia)*1,sex3=(S3_10_22 %in% violencia)*1,
                         econ1=(S3_10_07 %in% violencia)*1,econ2=(S3_10_08 %in% violencia)*1,
                         econ3=(S3_10_09 %in% violencia)*1,econ4=(S3_10_10 %in% violencia)*1,
                         econ5=(S3_10_11 %in% violencia)*1) %>% 
  mutate(v_psi = (select(., psi1:psi7) %>% apply(1, sum, na.rm=TRUE))>1,
         v_fis = (select(., fis1:fis7) %>% apply(1, sum, na.rm=TRUE))>1,
         v_sex = (select(., sex1:sex3) %>% apply(1, sum, na.rm=TRUE))>1,
         v_econ = (select(., econ1:econ5) %>% apply(1, sum, na.rm=TRUE))>1) %>% 
  select(-c(4:47))

soltera<-merge(bd_soltera1,bd_soltera)

## BASE DE DATOS DIVORCIADAS 

bd_separado1<-bdper %>% filter(estado=="4. SEPARADO/A"| estado=="6. VIUDO/A"| estado=="5. DIVORCIADO/A")
bd_separado<-ms %>% select(folio,area,depto,viol_psicologica,viol_fisica,viol_sexual,viol_econom) %>% mutate(psi1=(S3_10_01 %in% violencia)*1,
                         psi2=(S3_10_02 %in% violencia)*1,psi3=(S3_10_03 %in% violencia)*1,
                         psi4=(S3_10_04 %in% violencia)*1,psi5=(S3_10_05 %in% violencia)*1,
                         psi6=(S3_10_06 %in% violencia)*1,psi7=(S3_10_17%in% violencia)*1,
                         fis1=(S3_10_12 %in% violencia)*1,fis2=(S3_10_13 %in% violencia)*1,
                         fis3=(S3_10_14 %in% violencia)*1,fis4=(S3_10_15 %in% violencia)*1,
                         fis5=(S3_10_16 %in% violencia)*1,fis6=(S3_10_18 %in% violencia)*1,
                         fis7=(S3_10_19 %in% violencia)*1,sex1=(S3_10_20 %in% violencia)*1,
                         sex2=(S3_10_21 %in% violencia)*1,sex3=(S3_10_22 %in% violencia)*1,
                         econ1=(S3_10_07 %in% violencia)*1,econ2=(S3_10_08 %in% violencia)*1,
                         econ3=(S3_10_09 %in% violencia)*1,econ4=(S3_10_10 %in% violencia)*1,
                         econ5=(S3_10_11 %in% violencia)*1) %>% 
  mutate(v_psi = (select(., psi1:psi7) %>% apply(1, sum, na.rm=TRUE))>1,
         v_fis = (select(., fis1:fis7) %>% apply(1, sum, na.rm=TRUE))>1,
         v_sex = (select(., sex1:sex3) %>% apply(1, sum, na.rm=TRUE))>1,
         v_econ = (select(., econ1:econ5) %>% apply(1, sum, na.rm=TRUE))>1) %>% 
  select(-c(4:47))

separada<-merge(bd_separado1,bd_separado)

## GUARDANDO LAS BASES DE DATOS
setwd("C:\\Users\\JULIA\\Documents\\PAMELA\\R Segundo semestre\\PROYECTO\\PROYECTO EST 383")
save(casada,soltera,separada,file = "BD_Mujeres.RData")


  
  