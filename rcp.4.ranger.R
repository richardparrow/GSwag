# libraries
library(ranger)

# loading
load("modelMatrixTrain_ranger.RData")



# Random Forest with ranger!
print(system.time(
  
rfModel <- ranger(dependent.variable.name = "transactionRevenue",
                  data = rangerXTrain,
                  num.trees = 10,
                  mtry = 500, # defaults to sqrt(p)
                  importance = "impurity") 

))



# plotting variable importance or die trying
rangerImp = importance(rfModel)

library(tidyverse)
varImp = cbind(names(rangerImp), as.numeric(rangerImp)) %>% 
  as.tibble() %>%
  mutate(varName = V1 %>% as.factor(),
         varImp = V2 %>% as.numeric() %>% scale()) %>%
  select(-V1, -V2)

# bisogna fare tutto sto sbatty beforehand cosi' il grafico viene fico
orderImp = varImp[order(varImp$varImp, decreasing = T), ]
orderImp$varName = factor(orderImp$varName, levels = orderImp$varName[order(orderImp$varImp)])

library(ggplot2)
firstK = 20 # scegli le k features piu' importanti da plottare
ggplot(orderImp[1:firstK, ]) + aes(x = varName, y = varImp, fill = varImp) + geom_bar(stat = "identity") + coord_flip()



# vediamo un po' come fitta
rfFitted = predict(rfModel, rangerXTrain)
plot(density(rangerXTrain[, "transactionRevenue"], from = 0, bw = 1))
lines(density(rfFitted$predictions, from = 0, bw = 1), col = "red")



