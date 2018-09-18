# libraries
library(ranger)


# loading
load("modelMatrixTrain_ranger.RData")


# Random Forest with ranger!
print(system.time(
  
rfModel <- ranger(dependent.variable.name = "transactionRevenue",
                  data = rangerXTrain,
                  num.trees = 1000,
                  mtry = 30)

)) # 90 seconds!
