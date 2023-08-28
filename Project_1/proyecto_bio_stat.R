library(dplyr)
library(readr)
library(ggplot2)
library(XLConnect)
install.packages("XLConnect")
  library(tidyr)

covid_19 <- read_delim("C:/Users/jorge/Downloads/datos_abiertos_covid19/230228COVID19MEXICO.csv")
covid_19 <- covid_19 %>% filter(CLASIFICACION_FINAL %in% c(1,2,3))
covid_19 <- head(covid_19,n=100000)
glimpse(covid_19)
sex <- covid_19 %>% mutate(SEXO= case_when(
  .$SEXO==1 ~ "Mujer",
  .$SEXO==2 ~ "Hombre",
  .$SEXO==99 ~ "No Especificado")) %>% group_by(SEXO) %>% summarise(Total=n())
ggplot(sex,aes(x=1,y=Total,fill=SEXO)) + geom_col() + coord_polar(theta="y") + theme_void()
pie_labels=paste0(sex$SEXO,",","","n=",sex$Total)
pie(sex$Total,labels = pie_labels)
#2
Catalogos<- loadWorkbook("C:/Users/jorge/Downloads/Catalogos_071020.xlsx")
Entidad_RED <- readWorksheet(Catalogos,sheet=9) 
Entidad_RES <- as_tibble(Entidad_RED)
Entidades <- covid_19 %>% 
  left_join(Entidad_RES,by=c('ENTIDAD_RES'='CLAVE_ENTIDAD'))
Entidades %>% ggplot(aes(ENTIDAD_FEDERATIVA)) +
  geom_bar() +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45,hjust=1)) + ylab("Total de casos") +
  xlab('Estado')
#3
Poblacion<- loadWorkbook("C:/Users/jorge/Downloads/Poblacion2.xlsx")
Poblacion <- readWorksheet(Poblacion,sheet=1)
Entidades2<- Entidades %>%  
  group_by(ENTIDAD_FEDERATIVA) %>% summarise(Total_casos=n()) %>%
  left_join(Poblacion,by='ENTIDAD_FEDERATIVA') %>% 
  mutate(casos_X100000=n()*100000/POBLACION)

Entidades2 %>% ggplot(aes(ENTIDAD_FEDERATIVA,casos_X100000)) +
  geom_col()+ 
  theme_classic() +
  theme(axis.text.x = element_text(angle=45,hjust=1)) + 
  ylab("Casos por cada 100 mil habitantes") + xlab('Estado')

#3
paciente <- covid_19 %>% mutate(TIPO_PACIENTE=case_when(
  .$TIPO_PACIENTE==1 ~ "Ambulatorio",
  .$TIPO_PACIENTE==2 ~ "Hospitalizado",
  .$TIPO_PACIENTE==99 ~ "No especificado"
)) %>% group_by(TIPO_PACIENTE) %>% summarise(Total=n())

pie_labels<-paste0(paciente$TIPO_PACIENTE, ",", "","n=",paciente$Total)
pie(paciente$Total,labels = pie_labels)
                                            
#4
Resultados<- covid_19 %>% 
  mutate(RESULTADO_LAB= case_when(
    .$RESULTADO_LAB==1 ~ "Positivo",
    .$RESULTADO_LAB==2 ~ "Negativo",
    .$RESULTADO_LAB==3 ~ "Pendiente",
    .$RESULTADO_LAB==4 ~ "No adecuado",
    .$RESULTADO_LAB==97 ~ "Sin muestra"
  ), RESULTADO_ANTIGENO= case_when(
    .$RESULTADO_ANTIGENO==1 ~ "Positivo",
    .$RESULTADO_ANTIGENO==2 ~ "Negativo",
    .$RESULTADO_ANTIGENO==3 ~ "Pendiente",
    .$RESULTADO_ANTIGENO==4 ~ "No adecuado",
    .$RESULTADO_ANTIGENO==97 ~ "Sin muestra"
  ))
Resultados1 <- Resultados %>% 
  select(RESULTADO_LAB,RESULTADO_ANTIGENO) %>%
  gather()
colnames(Resultados1) <- c('Prueba','Resultado')
Resultados1 <- Resultados1 %>%
  filter(Resultado=="Positivo") %>% 
  group_by(Prueba) %>%
  summarize(Total=n())
pie_labels <- paste0("POSITIVO"," ",
                     Resultados1$Prueba,","," ",
                     "n=",Resultados1$Total)  
pie(Resultados1$Total,labels = pie_labels)

#5
hist(covid_19$EDAD,breaks=11,
     main=c("Casos por edad"),
     xlab=c("Rango de edad"))
#6
Covid_19_edades <- covid_19 %>% mutate(EDAD= case_when(
  .$EDAD<10 ~ "0 a 9 años",
  .$EDAD>=10 & .$EDAD<20 ~ "10 a 19 años",
  .$EDAD>=20 & .$EDAD<30 ~ "20 a 29 años",
  .$EDAD>=30 & .$EDAD<40 ~ "30 a 39 años",
  .$EDAD>=40 & .$EDAD<50 ~ "40 a 49 años",
  .$EDAD>=50 & .$EDAD<60 ~ "50 a 59 años",
  .$EDAD>=60 & .$EDAD<70 ~ "60 a 69 años",
  .$EDAD>=70 & .$EDAD<80 ~ "70 a 79 años",
  .$EDAD>=80 & .$EDAD<90 ~ "80 a 89 años",
  .$EDAD>=90 & .$EDAD<100 ~ "90 a 99 años",
  .$EDAD>100 ~ "mas de 100 años",
  !is.na(.$EDAD) ~ "No Especificado"
)) %>% mutate(EDAD=as.factor(EDAD)) 

Pob_edades<- loadWorkbook("C:/Users/jorge/Downloads/Total_edades2.xlsx")
Pob_edades <- readWorksheet(Pob_edades,sheet=1)
casos_por_edad <- Covid_19_edades %>% group_by(EDAD) %>% summarize(Total=n()) %>%
  inner_join(Pob_edades,by='EDAD',suffix=c("_casos","_poblacion")) %>%
  mutate(casos_x10000=Total_casos*10000/Total_poblacion)
casos_por_edad %>% ggplot(aes(EDAD,casos_x10000)) + geom_bar(stat=
                                              "identity") +
  theme_classic() + theme(axis.text.x=element_text(hjust=1,
                                                   angle=45)) +
  ylab("Casos por cada 10mil habitantes")

#7

muertes_por_grupo<- Covid_19_edades %>% count(EDAD,FECHA_DEF) %>%
  filter(!is.na(FECHA_DEF)) %>% 
  group_by(EDAD) %>% summarise(Total=sum(n))

muertes_por_grupo %>% ggplot(aes(EDAD,Total)) + geom_bar(stat='identity') +
  theme_classic() + theme(axis.text.x = element_text(hjust=1,
                                                     angle=45))

#8

Letalidad <- casos_por_edad %>% select(EDAD,Total_casos) %>%
  inner_join(muertes_por_grupo,by='EDAD') %>% transmute(
    EDAD,Total_casos,Total_muertes=Total,indice_letalidad=
      Total/Total_casos
  )
Letalidad %>% ggplot(aes(EDAD,indice_letalidad)) +
  geom_col() + theme_classic() + 
  theme(axis.text.x = element_text(hjust=1,angle=45)) +
  ylab("Tasa de letalidad")

#9

Mortalidad <- muertes_por_grupo %>% 
  inner_join(Pob_edades, by='EDAD',suffix=c(
    "_muertes","_poblacion")) %>% mutate(Tasa_mortalidad=
                                           Total_muertes*10000/Total_poblacion)

Mortalidad %>% ggplot(aes(EDAD,Tasa_mortalidad)) + geom_col()+
  theme_classic() + theme(axis.text.x = element_text(hjust=1,angle=45)) +
  ylab("Muertes por cada 10mil habitantes")
                                            