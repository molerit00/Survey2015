source("libraries.r",encoding = "UTF-8")




Survey.Data<-
  read_sav("EncuestaLimpia.sav") %>% 
  as.data.table() %>% 
  
  rename(
    Gender                          = Genero,
    Education                      = Educacion,
    Married                      = Matrimonio,
    Economic.Index             = Indice_eco,
    Vote                        = Voto1 ,
    Employment.Status       =          Estatus_de_Empleo,
    Religion                =       Religion,
    Previous.Election           = Eleccionesprevias,
    Macri.Ideology              = Macri_ideo,
    Scioli.Ideology             = Scioli_Ideo,
    Massa.Ideology              = Massa_ideo,
    Carrio.Ideology             = Carrio_ideo,
    Sanz.Ideology               = Sanz__ideo,
    Stolbizer.Ideology          = Stolbizer,
    Autoperception.Ideology       = Autopercepcion,
    Que.Partido                   =      A_que_par,
    Year.Born                  = Ano_nacimiento
  ) %>% 
  mutate(
    Vote = factor(
      Vote, 
      levels = c(1,2,3,4,5,6,7, 9, 10,11, 73),
      labels = c(
        "FPV", "PRO", "UNA", "PRO", "PRO", "UNA", "UNA", "UNA", "Indeciso", "UNA",
        "Indeciso"
      ), 
      
    ),
    Gender = factor(
      Gender,
      levels = c(1,2),
      labels = c("Hombre", "Mujer") 
    ),
    Religion = factor(
      Religion, 
      levels = c(1000, 1101, 1200, 1300, 2000, 8300, 9600),
      labels = c("Ateo", "Católico Románo", "Ateo","Ateo","Ateo","Ateo","Ateo")
    ),
    Employment.Status  = factor(
      Employment.Status , 
      levels = c(1, 5, 6, 7, 8, 10, 97),
      labels = c("Empleado", "Empleado", "Empleado","Desempleado","Desempleado","Desempleado","Desempleado")
    ),
    
    
    Education = factor(
      Education, 
      levels = c(1,2,3,4,5,6,7,8),
      labels = c(
        "Primario", "Primario", "Secundario", "Secundario",
        "Secundario", "Secundario", "Secundario", "Secundario"
      ),
      exclude = 97
    ),

    
    Married = factor(
      Married, 
      levels = c(1,1,1,2), 
      labels = c("Casado","Casado", "Casado", "Soltero"),
      exclude = 7
    ),

    
    Economic.Index = factor( 
      Economic.Index,
      levels = c(1,3,5), 
      labels = c(
        "Mejorando", "Mejorando", "Empeora"
      ),
      exclude = c(7,8)
    ),
    
    Previous.Election = factor(
      Previous.Election, 
      levels = c(1,2,3,4,5,6,7, 90, 93, 97, 98),
      labels = c(
        "Cristina K", "No", "No", 
        "No","No", "No", "No", "No", "No", "No", "No"
      ), 
      exclude = 99
    ),
    
    
    Que.Partido = factor(
      Que.Partido, 
      levels = c(1,2,3,5,6,7,8, 9, 90, 97, 98, 99 ), 
      labels = c(
        "FPV", "FPV", "UNA",
        "UNA", "UNA", "PRO",
        "PRO", "NoTiene", "PRO"
        , "NoTiene", "NoTiene", "NoTiene"
      ) 
    ),
    
    
    ####Como remover valores en numericas ####
    
    Macri.Ideology = as.numeric(Macri.Ideology),
    Scioli.Ideology = as.numeric(Scioli.Ideology),
    Massa.Ideology = as.numeric(Massa.Ideology),
    Carrio.Ideology = as.numeric(Carrio.Ideology),
    Sanz.Ideology = as.numeric(Sanz.Ideology),
    Stolbizer.Ideology = as.numeric(Stolbizer.Ideology),
    Autoperception.Ideology = as.numeric(Autoperception.Ideology),
    Year.Born = as.numeric(Year.Born)
  ) %>% 
  mutate(
    Macri.Ideology = ifelse(Macri.Ideology > 10, NA, Macri.Ideology),
    Scioli.Ideology = ifelse(Scioli.Ideology > 10, NA, Scioli.Ideology  ),
    Massa.Ideology = ifelse(Massa.Ideology > 10, NA, Massa.Ideology  ),
    Carrio.Ideology = ifelse(Carrio.Ideology > 10, NA,  Carrio.Ideology  ),
    Sanz.Ideology = ifelse(Sanz.Ideology > 10, NA, Sanz.Ideology  ),
    Stolbizer.Ideology = ifelse(Stolbizer.Ideology > 10, NA, Stolbizer.Ideology  ),
    Autoperception.Ideology = ifelse(Autoperception.Ideology > 10, NA,  Autoperception.Ideology ),
    Year.Born = as.numeric(Year.Born)
  )



Survey.Data$Year.Born <- 2015 - Survey.Data$Year.Born
summary(Survey.Data)
#Survey.Data$Year.Born<- cut(Survey.Data$Year.Born, breaks = c(
#  15, 25, 45, 81), labels= c("Joven", "Adulto", "Jubilado"))


### Cut de variables that i dont want
Survey.Data<- tbl_df(Survey.Data[c(1,2,3,4,6,8, 14:18, 22, 25,26)] )

Survey.Data<- filter(Survey.Data, Vote %in% c( "FPV",  "PRO", "UNA", "Indeciso"))

Survey.Data$NoTiene.Ideology <- 5.75

levels(Survey.Data$Education)
levels(Survey.Data$Married)

#Survey.Data$BestCandidate <- 
#for(i in 1:nrow(Survey.Data)){
# if {
#    Survey.Data$Dir.Scioli[i] >  Survey.Data$Dir.Macri
#  }
#}
#FPV <- Survey.Data %>% filter( Vote == "FPV") 
#FPV$Que.Partido <- NULL
#FPV$Que.Partido <- "FPV"

#PRO <- Survey.Data %>% filter( Vote == "PRO") 
#PRO$Que.Partido <- NULL
#PRO$Que.Partido <- "PRO"

#UNA <- Survey.Data %>% filter( Vote == "UNA") 
#UNA$Que.Partido <- NULL
#UNA$Que.Partido <- "UNA"

#Indeciso <- Survey.Data %>% filter( Vote == "Indeciso") 
#Indeciso$Que.Partido <- NULL
#Indeciso$Que.Partido <- "Indeciso"
#table(Indeciso$Que.Partido)

#EY <- rbind(FPV,PRO,UNA,Indeciso)
#table(EY$Que.Partido)
#Survey.Data <- EY


#JEJE <- filter(Survey.Data, Que.Partido == "NoTiene" & Vote == "Indeciso" )
#summary(JEJE)

####For Directionality
table(Survey.Data$Que.Partido)

ALLL <- ggplot(data = Survey.Data, mapping = aes(Vote))
ALLL + geom_bar(aes(fill = factor(Que.Partido)), position = 'stack')


indecisx <- filter(Survey.Data.KNN, Vote == "Indeciso")
table(indecisx$Gender)
table(indecisx$Education)
table(indecisx$Married)
table(indecisx$Previous.Election)
table()
