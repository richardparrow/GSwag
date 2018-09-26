# libraries
library(ranger)

# loading
load("modelMatrixTrain_ranger.RData")



# Random Forest with ranger!

# model 1: num.trees = 1000; mtry = 35 ~ sqrt(p)  ---  2h 40m (num.threads = 6)
print(system.time(
  
rfOne <- ranger(dependent.variable.name = "transactionRevenue",
                data = rangerXTrain,
                num.trees = 1000,
                mtry = NULL, # defaults to sqrt(p)
                importance = "impurity",
                num.threads = 6) 

))

# model 2: num.trees = 1000; mtry = 100 > sqrt(p)  ---  6h (num.threads = 6)
print(system.time(
  
rfTwo <- ranger(dependent.variable.name = "transactionRevenue",
                data = rangerXTrain,
                num.trees = 1000,
                mtry = 100, # > sqrt(p)
                importance = "impurity",
                num.threads = 6)  
  
))

# model 3: num.trees = 1000; mtry = 10 < sqrt(p)  ---  1h 6m (num.threads = 6)
print(system.time(
  
rfThr <- ranger(dependent.variable.name = "transactionRevenue",
                data = rangerXTrain,
                num.trees = 1000,
                mtry = 10, # < sqrt(p)
                importance = "impurity",
                num.threads = 6)  
  
))



# OOB Prediction RMSEs:
# mtry =  10:  1.89
#         35:  1.75
#        100:  1.65  ---  Eureka?



# plotting variable importance or die trying
rangerImp = importance(rfTwo)

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
rfFitted = predict(rfTwo, rangerXTrain)
plot(density(rangerXTrain[, "transactionRevenue"], from = 0, bw = 1))
lines(density(rfFitted$predictions, from = 0, bw = 1), col = "red")



# Submitting!
load("modelMatrixTest_ranger.RData")
rfPreds = predict(rfTwo, rangerXTest)$predictions

library(dplyr)
predSub = cbind(testIds, rfPreds) %>% as.tibble() %>%
  group_by(fullVisitorId) %>%
  summarise(PredictedLogRevenue = log1p(sum(expm1(rfPreds))))

readr::write_csv(predSub, path = paste0(getwd(), "/sub_rf_mtry100.csv"))


