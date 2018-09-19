# libraries
library(glmnet)
library(Matrix)
library(doMC)


# richiamo dati
load("modelMatrixTrain_xgb.RData")

# parallel stuff!
registerDoMC(cores = 5) # cores = detectCores() - 1



# ridge
print(system.time(
  
ridgeFit <- glmnet(x = XTrain,
                   y = yTrain,
                   alpha = 0,
                   standardize = T,
                   intercept = F,
                   family = "gaussian")

))

# CV
print(system.time(

ridgeCross <- cv.glmnet(x = XTrain,
                        y = yTrain,
                        nfolds = 5,
                        alpha = 0,
                        standardize = T,
                        intercept = F,
                        family = "gaussian",
                        type.measure = "mse",
                        parallel = T)

))

# plot!
plot(ridgeCross)
plot(ridgeFit, xvar = "lambda")
abline(v = log(ridgeCross$lambda.min))




####################
# BEST RIDGE MODEL #
####################

# best ridge I have
ridgeLambda = ridgeCross$lambda.min

ridgeBest = glmnet(x = XTrain,
                   y = yTrain,
                   alpha = 0,
                   lambda = ridgeLambda,
                   standardize = T,
                   intercept = F,
                   family = "gaussian")

ridgeFitted = predict.glmnet(ridgeBest, XTrain)

# plot!
plot(density(yTrain[, 1], bw = .5, from = 0))
lines(density(ridgeFitted, bw = .5, from = 0), col = "red")

# predict!
load("modelMatrixTest_xgb.RData")
ridgePreds = predict.glmnet(ridgeBest, XTest)

library(dplyr)
predSub = cbind(testIds, ridgePreds) %>% as.tibble() %>%
  group_by(fullVisitorId) %>%
  summarise(PredictedLogRevenue = log1p(sum(expm1(s0))))

readr::write_csv(predSub, path = paste0(getwd(), "/sub_ridgeProva.csv"))
