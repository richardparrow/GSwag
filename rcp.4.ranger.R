# libraries
library(ranger)


# loading
load("modelMatrixTrain_ranger.RData")


# Random Forest with ranger!
print(system.time(
  
rfModel <- ranger(dependent.variable.name = "transactionRevenue",
                  data = rangerXTrain,
                  num.trees = 100,
                  mtry = NULL, # defaults to sqrt(p)
                  importance = "impurity") 

)) # 2h ca.



# plotting variable importance or die trying
rangerImp = importance(rfModel)

library(tidyverse)
varImp = cbind(names(rangerImp), as.numeric(rangerImp)) %>% 
  as.tibble() %>%
  mutate(varName = V1 %>% as.factor(),
         varImp = V2 %>% as.numeric() %>% scale()) %>%
  select(-V1, -V2)

orderImp = varImp[order(varImp$varImp, decreasing = T), ]
orderImp$varName = factor(orderImp$varName, levels = orderImp$varName[order(orderImp$varImp)])

library(ggplot2)
ggplot(orderImp[1:20, ]) + aes(x = varName, y = varImp, fill = varImp) + geom_bar(stat = "identity") + coord_flip()
