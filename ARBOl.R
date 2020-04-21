set.seed(44)
arbol_Voto <- rpart(Vote~ Gender + Previous.Election+ Education +  Economic.Index + Year.Born + Religion + 
                      Employment.Status + IDPA_FPV + IDPA_PRO+ IDPA_UNA+
                       IDPA_Indeciso,
                    data =  Only.Mañosa,
                    method ='class',
                    parms = list(split = "gini" ),
                    control = rpart.control(minsplit = 2, minbucket = 4))



rpart.plot(arbol_Voto, roundint = F)

predicted.Arbol.Voto <-  
  predict(arbol_Voto,  Fit.Test.KNN, type= "class") #Puse prob para que quede igual a ver que pasa
predicted.Arbol.Voto

Matriz.Arbol.Voto <- 
  table( Fit.Test.KNN$Vote, predicted.Arbol.Voto)
Matriz.Arbol.Voto

predicted.ARBOl.Voto.Tabla.Real <- 
  predict(arbol_Voto, Fit.Test.KNN, type = "class")
matrizRfRealARB <- confusionMatrix( Fit.Test.KNN$Vote, predicted.Arbol.Voto)
matrizRfRealARB

