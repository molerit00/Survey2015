#### Correr global #########
source("libraries.r",encoding = "UTF-8")

source("global.r",encoding = "UTF-8")

### Read Fill_na_With_knn
source("transformdata.r", encoding = "UTF-8")


Survey.Data.KNN


set.seed(44)
table(Survey.Data.KNN$Vote)
Only.UNA.Indecisos <- Survey.Data.KNN %>% filter(Vote == c("UNA") | Vote == "Indeciso")
Only.FPV.PRO <- Survey.Data.KNN %>% filter(Vote == c("FPV") | Vote == "PRO" )
trIndexAleatorioFPV.PRO<- sample(nrow(Only.FPV.PRO),0.6*nrow(Only.FPV.PRO),replace=F)
Fit.Only.FPV.PRO <- Only.FPV.PRO[trIndexAleatorioFPV.PRO,]
table(Fit.Only.FPV.PRO$Vote)
Only.Mañosa <- rbind(Fit.Only.FPV.PRO,Only.UNA.Indecisos)
table(Only.Mañosa$Vote)




Only.Mañosa$Prox_Indeciso<- (Only.Mañosa$NoTiene.Ideology - Only.Mañosa$Autoperception.Ideology) ^2
Only.Mañosa$Prox_FPV<- (Only.Mañosa$Scioli.Ideology - Only.Mañosa$Autoperception.Ideology) ^2
Only.Mañosa$Prox_UNA<- (Only.Mañosa$Massa.Ideology-Only.Mañosa$Autoperception.Ideology)^2
Only.Mañosa$Prox_PRO<- (Only.Mañosa$Macri.Ideology-Only.Mañosa$Autoperception.Ideology)^2

Only.Mañosa$Macri.Ideology <-  Only.Mañosa$Macri.Ideology -5 
Only.Mañosa$Scioli.Ideology <-  Only.Mañosa$Scioli.Ideology -5 
Only.Mañosa$Massa.Ideology <-  Only.Mañosa$Massa.Ideology -5 
Only.Mañosa$Autoperception.Ideology <-  Only.Mañosa$Autoperception.Ideology -5 
Only.Mañosa$NoTiene.Ideology <-  Only.Mañosa$NoTiene.Ideology -5



Only.Mañosa$Dir_FPV <- 
  Only.Mañosa$Autoperception.Ideology*Only.Mañosa$Scioli.Ideology

Only.Mañosa$Dir_PRO<-
  Only.Mañosa$Macri.Ideology*Only.Mañosa$Autoperception.Ideology

Only.Mañosa$Dir_UNA<- 
  Only.Mañosa$Massa.Ideology*Only.Mañosa$Autoperception.Ideology

Only.Mañosa$Dir_Indeciso<- 
  Only.Mañosa$NoTiene.Ideology*Only.Mañosa$Autoperception.Ideology


Only.Mañosa <- Only.Mañosa[-c(9:12,14,19)]




set.seed(44)
Mañoño <- sample(nrow(Only.Mañosa),0.7*nrow(Only.Mañosa),replace=F)
Aletomaño<- seq_len(nrow(Only.Mañosa))[!(seq_len(nrow(Only.Mañosa)) %in%Mañoño)]
Fit.Train.Maño <- Only.Mañosa[Mañoño,]
Fit.Test.Maño <- Only.Mañosa[Aletomaño,]
