#### Correr global #########
source("libraries.r",encoding = "UTF-8")

source("global.r",encoding = "UTF-8")

### Read Fill_na_With_knn
source("transformdata.r", encoding = "UTF-8")

source("Only.Mañosa.R", encoding = "UTF-8")






###########    Regresión logistica    ##########################################################
Logis.Scioli<-(glm(Scioli~ Gender +
                         Education +
                         Economic.Index +
                         Previous.Election +
                         Macri.Ideology     + 
                         Scioli.Ideology   +
                         Massa.Ideology     + 
                         Autoperception.Ideology+
                     IDPA_Macri+ IDPA_Scioli+IDPA_Massa+
                         Year.Born,
                       data =  Fit.Train.Log, family = binomial("logit")))

train_control <- trainControl(method = "cv", number = 10)
Train_Control <- trainControl(method = "cv", number = 10)
model <- train(Scioli~Gender +
                 Education +
                 Economic.Index +
                 Previous.Election +
                 Macri.Ideology     + 
                 Scioli.Ideology   +
                 Massa.Ideology     + 
                 IDPA_Macri+ IDPA_Scioli+IDPA_Massa+
                 Autoperception.Ideology+
                Year.Born, data = Survey.Logist,
               trControl = Train_Control,
               method = "glm",
               family=binomial())

model %>%summary()

# Entrenar el modelo



Predict_Logi.Scioli <- 
  predict(Logis.Scioli,  Fit.Test.Log, type="response")
Predict_Logi.Scioli

levels(Predict_Logi.Scioli)= c("No Voto", "Voto")
#Predict_Logi.Scioli= as.factor(p>0.5)
aggregate(Predict_Logi.Scioli~  Fit.Test.Log$Scioli,data= Fit.Test.Log,FUN=mean) 

confusion.logis.Scioli <- table(pred = Predict_Logi.Scioli>0.5, CLASE =  Fit.Test.Log$Scioli)
confusion.logis.Scioli
precision.scioli <- confusion.logis.Scioli[2,2]/sum(confusion.logis.Scioli[2,])
precision.scioli
Test.prob.Log.Sci = predict(Logis.Scioli, newdata =  Fit.Test.KNN, arg = "link")
Test.prob.Log.Sci
test_roc = roc( Fit.Test.KNN$Vote ~ Test.prob.For, plot = TRUE, print.auc = TRUE)


#####################################
########## logistica MACRI ##########
#####################################
Logis.Macri<- glm(Macri~ Gender +
                    Education +
                    Economic.Index +
                    Previous.Election +
                    Macri.Ideology     + 
                    Scioli.Ideology   +
                    Massa.Ideology     + 
                    IDPA_Macri+ IDPA_Scioli+IDPA_Massa
                    Autoperception.Ideology+  +  
                    Year.Born,
                  data =  Fit.Train.Log, family = binomial("probit"))

summary(Logis.Macri)
Predict_Logi.Macri <- 
  predict(Logis.Macri,  Fit.Test.Log, type="response")

aggregate(Predict_Logi.Macri~  Fit.Test.Log$Macri,data= Fit.Test.Log,FUN=mean) 

confusion.logis.macri <- table(pred = Predict_Logi.Macri>0.5, CLASE =  Fit.Test.Log$Macri)
confusion.logis.macri
precision.macri <- confusion.logis.macri[2,2]/sum(confusion.logis.macri[2,])
precision.macri

recall <- confusion.logis.macri[2,2]/sum(confusion.logis.macri[,2])
recall

##### Multinomial Regresión ######


###### condicional vote ######


Fit.Train.KNN <- Fit.Train.KNN[-14]

a<- mlogit.data(Fit.Train.Maño, choice ="Vote",
                shape = c("wide"), drop.index = F, varying =  10:21,
                chid.var= "ID", alt.var = "alt", sep = "_", 
  alt.levels = c( "FPV","PRO","UNA", "Indeciso"))





Mlog_Prox <- mlogit(Vote ~ IDPA + Dir |Gender + Education + Married+
                      Previous.Election + Economic.Index + Religion +Employment.Status +
                      Year.Born,  reflevel = "FPV", method= "nr",  data = a)



Mlog_Prox %>% summary()

Mlog_Dir <- mlogit(Vote ~ IDPA + Dir |Gender + Education + Married+
                     Previous.Election + Economic.Index + Religion +Employment.Status +
  Year.Born,  reflevel = "FPV", method="nr",  data = a)

Mlog_Dir%>% summary() 
coef(Example)/coef(Example)[9]
str(Fit.Train.KNN$Gender)


Coef<- Model.mulnom$coefficients
Coef
Model.mulnom$model

###### Predict #####
logit2prob <- function(Coef){
  # this function converts logits to probabilities. Useful for interpretation of glm.
  # input: logit
  # output: probability of logit
  odds <- exp(Coef)
  prob <- odds / (1 + odds)
  return(prob)
}

logit2prob( 5.6743e-01)



predict_mnl <- function(model, products) {
  data.model <- model.matrix(update(model$formula, 0 ~ .),
                             data = products)[,-1]
  utility <- data.model%*%model$coef
  share <- exp(utility)/sum(exp(utility))
  cbind(share, products)
}

str(Fit.Train.KNN$Previous.Election)


Fit.Test.KNN <- Fit.Test.KNN[-14]



TransformFitTest<- mlogit.data(Fit.Test.Maño, choice ="Vote",
                shape = c("wide"), drop.index = F, varying =  10:21,
                chid.var= "ID", alt.var = "alt", sep = "_", 
                alt.levels = c( "FPV","PRO","UNA", "Indeciso"))


TransformFitTest


TomaYHacelo<- Fit.Test.Maño$Vote[1:290]
#########################
set.seed(44)
Predict_MLOG<- predict(Mlog_Dir, TransformFitTest)
AverSiSeDeja<- colnames(Predict_MLOG)[apply(Predict_MLOG,1,which.max)]
AverSiSeDeja<- as.data.frame(AverSiSeDeja)
AverSiSeDeja
bienmlo <- cbind(Predict_MLOG,AverSiSeDeja)
BMB<- confusionMatrix(Fit.Test.Maño$Vote , bienmlo$AverSiSeDeja)
BMB

nrow(bienmlo)

333*3


PrePA<- predict(Mlog_Dir, Fit.Test.KNN)

AverSiSeDeja<- colnames(Predict_MLOG)[apply(Predict_MLOG,1,which.max)]
AverSiSeDeja<- as.data.frame(AverSiSeDeja)
AverSiSeDeja
bienmlo <- cbind(Predict_MLOG,AverSiSeDeja)
BMB<- confusionMatrix(Fit.Test.KNN$Vote, AverSiSeDeja)
BMB


