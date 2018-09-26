# libraries
library(ranger)
library(dplyr)

# loading
load("modelMatrixTrain_ranger.RData")
load("modelMatrixTest_ranger.RData")


# Random Forest with ranger!

# model 1: num.trees = 1000; mtry = 100 > sqrt(p)  ---  6h (num.threads = 6)
print(system.time(
  
  rfOne <- ranger(dependent.variable.name = "transactionRevenue",
                  data = rangerXTrain,
                  num.trees = 1000,
                  mtry = 100, # defaults to sqrt(p)
                  importance = "impurity",
                  num.threads = 6) 
  
))

# model 2: num.trees = 1000; mtry = 200 > sqrt(p)  ---  ??? (num.threads = 6)
print(system.time(
  
  rfTwo <- ranger(dependent.variable.name = "transactionRevenue",
                  data = rangerXTrain,
                  num.trees = 1000,
                  mtry = 200, # > sqrt(p)
                  importance = "impurity",
                  num.threads = 6)  
  
))

gc()


# vediamo un po' come fitta
pdf(file="rcp.4c.fitComp.pdf")
rfFitOne = predict(rfOne, rangerXTrain)
rfFitOne = rfFitOne$predictions
rfFitTwo = predict(rfTwo, rangerXTrain)
rfFitTwo = rfFitTwo$predictions
plot(density(rangerXTrain[, "transactionRevenue"], from = 0, bw = 1))
lines(density(rfFitOne, from = 0, bw = 1), col = "red")
lines(density(rfFitTwo, from = 0, bw = 1), col = "blue")
dev.off()

gc()


# Submitting!
# RF One
rfPredsOne = predict(rfOne, rangerXTest)
rfPredsOne = rfPredsOne$predictions

predSubOne = cbind(testIds, rfPredsOne) %>% as.tibble() %>%
  group_by(fullVisitorId) %>%
  summarise(PredictedLogRevenue = log1p(sum(expm1(rfPredsOne))))

readr::write_csv(predSub, path = paste0(getwd(), "/sub_rf_mtry100.csv"))

# RF Two
rfPredsTwo = predict(rfTwo, rangerXTest)
rfPredsTwo = rfPredsTwo$predictions

predSubTwo = cbind(testIds, rfPredsTwo) %>% as.tibble() %>%
  group_by(fullVisitorId) %>%
  summarise(PredictedLogRevenue = log1p(sum(expm1(rfPredsTwo))))

readr::write_csv(predSub, path = paste0(getwd(), "/sub_rf_mtry200.csv"))

