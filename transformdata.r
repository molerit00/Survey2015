
source("libraries.r",encoding = "UTF-8")

source("global.r",encoding = "UTF-8")


  
#########   Divido Survey data en 2    #############
set.seed(44)
trIndexAleatorio <- sample(nrow(Survey.Data),0.7*nrow(Survey.Data),replace=F)
vaIndexAleatorio <- seq_len(nrow(Survey.Data))[!(seq_len(nrow(Survey.Data)) %in% trIndexAleatorio)]
Fit.Train <- Survey.Data[trIndexAleatorio,]
Fit.Test <- Survey.Data[vaIndexAleatorio,]

############ Rellenar Los NA Con kNN ###################
set.seed(44)
Fit.Train.KNN <- 
  kNN(Fit.Train, variable = c("Economic.Index", "Married"), k=4)
Fit.Train.KNN <-kNN(Fit.Train.KNN, variable = c("Education"), k=4) 
Fit.Train.KNN <- 
  kNN(Fit.Train.KNN, variable = c("Vote"), k=5)
Fit.Train.KNN <- 
  kNN(Fit.Train.KNN, variable = c("Employment.Status"), k=4)
Fit.Train.KNN <- 
  kNN(Fit.Train.KNN, variable = c("Que.Partido"), k=5)
Fit.Train.KNN <- 
  kNN(Fit.Train.KNN, variable = c("Previous.Election"), k=5)
Fit.Train.KNN <- 
  kNN(Fit.Train.KNN, variable = c("Religion"), k=5)

Fit.Train.KNN <- Fit.Train.KNN[1:15]
######Cambiar por la media los Na ######################
set.seed(34)

Fit.Train.KNN$Macri.Ideology[which(is.na(Fit.Train.KNN$Macri.Ideology))] <- 
  mean(Fit.Train.KNN$Macri.Ideology, na.rm=TRUE)

Fit.Train.KNN$Scioli.Ideology[which(is.na(Fit.Train.KNN$Scioli.Ideology))] <- 
  mean(Fit.Train.KNN$Scioli.Ideology, na.rm=TRUE)

Fit.Train.KNN$Massa.Ideology[which(is.na(Fit.Train.KNN$Massa.Ideology))] <- 

Fit.Train.KNN$Autoperception.Ideology[which(is.na(Fit.Train.KNN$Autoperception.Ideology))] <-
  mean(Fit.Train.KNN$Autoperception.Ideology, na.rm=TRUE)

Fit.Train.KNN<- Fit.Train.KNN %>% 
  mutate( 
    IDPA_FPV = if_else (Que.Partido == "FPV", 1, 0),
    IDPA_UNA = if_else (Que.Partido == "UNA", 1, 0),
    IDPA_PRO = if_else (Que.Partido == "PRO", 1, 0),
    IDPA_Indeciso = if_else (Que.Partido == "NoTiene", 1, 0)
  ) %>% mutate(
    IDPA_FPV = factor(IDPA_FPV, levels = c(0,1), 
                      labels = c("No" , "Si")
    ),
    IDPA_PRO = factor(IDPA_PRO, levels = c(0,1), 
                      labels = c("No" , "Si")
    ),
    IDPA_UNA = factor(IDPA_UNA, levels = c(0,1), 
                      labels = c("No" , "Si")
    ),
    IDPA_Indeciso = factor(IDPA_Indeciso, levels = c(0,1), 
                      labels = c("No" , "Si"))
    
  )
Fit.Train.KNN <- Fit.Train.KNN[-13]

Fit.Train.KNN$ID <- seq.int(nrow(Fit.Train.KNN))





##### Same With Fit.Test##########

set.seed(34)

Fit.Test.KNN <-
  kNN(Fit.Test, variable = c("Education", "Married"), k=4) 
Fit.Test.KNN <- 
  kNN(Fit.Test.KNN, variable = c("Economic.Index", "Employment.Status"), k=4)
Fit.Test.KNN <- 
  kNN(Fit.Test.KNN, variable = c("Que.Partido", "Vote" ), k=4)

Fit.Test.KNN <- 
  kNN(Fit.Test.KNN, variable = c("Previous.Election", "Employment.Status"), k=4)

Fit.Test.KNN <- 
  Fit.Test.KNN[1:15]

Fit.Test.KNN<- Fit.Test.KNN %>% 
  mutate( 
    IDPA_FPV     = if_else (Que.Partido == "FPV", 1, 0),
    IDPA_UNA         = if_else (Que.Partido == "UNA", 1, 0),
    IDPA_PRO            = if_else (Que.Partido == "PRO", 1, 0),
    IDPA_Indeciso = if_else (Que.Partido == "NoTiene", 1, 0)
  )%>% mutate(
    IDPA_FPV = factor(IDPA_FPV, levels = c(0,1), 
                            labels = c("No" , "Si")
    ),
    IDPA_PRO = factor(IDPA_PRO, levels = c(0,1), 
                      labels = c("No" , "Si")
    ),
    IDPA_UNA = factor(IDPA_UNA, levels = c(0,1), 
                        labels = c("No" , "Si")
    ),
    IDPA_Indeciso = factor(IDPA_Indeciso, levels = c(0,1), 
                          labels = c("No" , "Si"))
    
  )



Fit.Test.KNN <- Fit.Test.KNN[-13]
Fit.Test.KNN$ID <- seq.int(nrow(Fit.Test.KNN))


######Cambiar por la media los Na ######################

Fit.Test.KNN$Macri.Ideology[which(is.na(Fit.Test.KNN$Macri.Ideology))] <- 
  mean(Fit.Test.KNN$Macri.Ideology, na.rm=TRUE)

Fit.Test.KNN$Scioli.Ideology[which(is.na(Fit.Test.KNN$Scioli.Ideology))] <- 
  mean(Fit.Test.KNN$Scioli.Ideology, na.rm=TRUE)

Fit.Test.KNN$Massa.Ideology[which(is.na(Fit.Test.KNN$Massa.Ideology))] <- 
  mean(Fit.Test.KNN$Massa.Ideology, na.rm=TRUE)

Fit.Test.KNN$Autoperception.Ideology[which(is.na(Fit.Test.KNN$Autoperception.Ideology))] <-
  mean(Fit.Test.KNN$Autoperception.Ideology, na.rm=TRUE)



######  Para logistica ################################

Survey.Data.KNN <- rbind(Fit.Test.KNN, Fit.Train.KNN)


###Variables unicos data set

#Survey.Logist %>%  
 # count(factor(Vote))
#Survey.Logist %>% 
 # select(Scioli:Voto.Blanco) %>% 
  #map(~ count(data.frame(x= .x), x))

######## Transformación en train y testing ##########
set.seed(34)





###################################FILTER###################################


#levels(Survey.Data$Qyh++)
#DataSet que me deja solo obs que tengan NA en Que.Partido
#bmb <-Survey.Data %>% 
#  filter(!Que.Partido %in% c("PRO","Partido provinciales", "No Tiene", "Justicialista", "GEN", "Union Civica Radical", "Izquierda unida", "Union civica Radical", "Partidos provinciales"))
#Dataset filtrando los NA de Que.Partido
#bambita <- Survey.Data %>%  filter(Que.Partido %in% c("PRO","Partido provinciales", "No Tiene", "Justicialista", "GEN", "Union Civica Radical", "Izquierda unida", "Union civica Radical", "Partidos provinciales"))
#summary(bambita)
#summary(bmb)

##Imputar NA con KNN

#loquitabmb <- Fit.Train.KNN %>%  filter(Previous.Election == c("Alfonsin","Binner"))
#nrow(loquitabmb)
#summary(loquitabmb)


#Saco los que no que hayan votado a Cristina K en Previous.Election
#No_K <- bmb %>% filter(!Previous.Election %in% ("Cristina K"))
#view(No_K)








Fit.Train.KNN$Prox_Indeciso<- (Fit.Train.KNN$NoTiene.Ideology - Fit.Train.KNN$Autoperception.Ideology) ^2
Fit.Train.KNN$Prox_FPV<- (Fit.Train.KNN$Scioli.Ideology - Fit.Train.KNN$Autoperception.Ideology) ^2
Fit.Train.KNN$Prox_UNA<- (Fit.Train.KNN$Massa.Ideology-Fit.Train.KNN$Autoperception.Ideology)^2
Fit.Train.KNN$Prox_PRO<- (Fit.Train.KNN$Macri.Ideology-Fit.Train.KNN$Autoperception.Ideology)^2




Fit.Train.KNN$Macri.Ideology <-  Fit.Train.KNN$Macri.Ideology -5 
Fit.Train.KNN$Scioli.Ideology <-  Fit.Train.KNN$Scioli.Ideology -5 
Fit.Train.KNN$Massa.Ideology <-  Fit.Train.KNN$Massa.Ideology -5 
Fit.Train.KNN$Autoperception.Ideology <-  Fit.Train.KNN$Autoperception.Ideology -5 
Fit.Train.KNN$NoTiene.Ideology <-  Fit.Train.KNN$NoTiene.Ideology -5



Fit.Train.KNN$Dir_FPV <- 
  Fit.Train.KNN$Autoperception.Ideology*Fit.Train.KNN$Scioli.Ideology

Fit.Train.KNN$Dir_PRO<-
  Fit.Train.KNN$Macri.Ideology*Fit.Train.KNN$Autoperception.Ideology

Fit.Train.KNN$Dir_UNA<- 
  Fit.Train.KNN$Massa.Ideology*Fit.Train.KNN$Autoperception.Ideology

Fit.Train.KNN$Dir_Indeciso<- 
  Fit.Train.KNN$NoTiene.Ideology*Fit.Train.KNN$Autoperception.Ideology



Fit.Train.KNN <- Fit.Train.KNN[-c(9, 10,11,12,14)]



#######################






   
##Fit.Train.KNN.multilog<- Fit.Train.KNN.multilog %>% select(-c(1,2, 4:11,12,14))






Fit.Test.KNN$Prox_Indeciso<- (Fit.Test.KNN$NoTiene.Ideology - Fit.Test.KNN$Autoperception.Ideology) ^2
Fit.Test.KNN$Prox_FPV<- (Fit.Test.KNN$Scioli.Ideology - Fit.Test.KNN$Autoperception.Ideology) ^2
Fit.Test.KNN$Prox_UNA<- (Fit.Test.KNN$Massa.Ideology-Fit.Test.KNN$Autoperception.Ideology)^2
Fit.Test.KNN$Prox_PRO<- (Fit.Test.KNN$Macri.Ideology-Fit.Test.KNN$Autoperception.Ideology)^2

Fit.Test.KNN$Macri.Ideology <-  Fit.Test.KNN$Macri.Ideology -5 
Fit.Test.KNN$Scioli.Ideology <-  Fit.Test.KNN$Scioli.Ideology -5 
Fit.Test.KNN$Massa.Ideology <-  Fit.Test.KNN$Massa.Ideology -5 
Fit.Test.KNN$Autoperception.Ideology <-  Fit.Test.KNN$Autoperception.Ideology -5 
Fit.Test.KNN$NoTiene.Ideology <-  Fit.Test.KNN$NoTiene.Ideology -5



Fit.Test.KNN$Dir_FPV <- 
  Fit.Test.KNN$Autoperception.Ideology*Fit.Test.KNN$Scioli.Ideology

Fit.Test.KNN$Dir_PRO<-
  Fit.Test.KNN$Macri.Ideology*Fit.Test.KNN$Autoperception.Ideology

Fit.Test.KNN$Dir_UNA<- 
  Fit.Test.KNN$Massa.Ideology*Fit.Test.KNN$Autoperception.Ideology

Fit.Test.KNN$Dir_Indeciso<- 
  Fit.Test.KNN$NoTiene.Ideology*Fit.Test.KNN$Autoperception.Ideology

Fit.Test.KNN <- Fit.Test.KNN[-c(9, 10,11,12,14)]

summary(Fit.Train.KNN$Dir_Indeciso)

summary(Fit.Train.KNN)
