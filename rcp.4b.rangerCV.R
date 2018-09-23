# libraries
library(ranger)
library(foreach)
library(doParallel)

# test arguments
data = MASS::Boston
dependent.variable.name = "medv"

num.trees = c(500, 1000, 2000)
mtry = c(1, 3, 5)
min.node.size = c(1, 5, 10)

seed.CV = 137

cvRanger <- function(K = 5,
                     R = 1,
                     seed.CV = NULL,
                     num.threads.CV = 1,
                                          
                     data = NULL,
                     dependent.variable.name = NULL,
                     
                     num.trees = 500,
                     mtry = floor(sqrt(p)),
                     min.node.size = 5) {
  # set parameter grid
  tuneGrid <- expand.grid(num.trees, mtry, min.node.size)
  colnames(tuneGrid) <- c("num.trees", "mtry", "min.node.size")
  P <- nrow(tuneGrid)
  
  # extracting ids
  set.seed(seed.CV)
  cvIds <- lapply(1:R, function(x) sample(rep(1:K, length.out = nrow(data)), size = nrow(data), replace = F))
  
  cvResults = matrix(NA, nrow = P, ncol = 2)
  colnames(cvResults) = c("rmse", "sd")
  
  # setting up the cluster
  registerDoParallel(cores = num.threads.CV)
  
  # CV - Yikes!
  for(p in 1:P) {
    cat("\n")
    cat("*** Set of parameters:", p, "- num.trees =", tuneGrid[p, "num.trees"], " mtry =", tuneGrid[p, "mtry"], " min.node.size =", tuneGrid[p, "min.node.size"], "\n")
    roundResults = matrix(NA, nrow = R, ncol = K)
    
    for(r in 1:R) {
      foldResults <- numeric(K)
      
      for(k in 1:K) {
        foldTrain <- data[cvIds[, r] != k, ]
        foldTest <- data[cvIds[, r] == k, ]
        
        foldModel <- ranger(data = foldTrain,
                            dependent.variable.name = dependent.variable.name,
                          
                            num.trees = tuneGrid[p, "num.trees"],
                            mtry = tuneGrid[p, "mtry"],
                            min.node.size = tuneGrid[p, "num.trees"],
                          
                            importance = "impurity",
                            num.threads = 1)
        
        foldPreds <- predict(foldModel, foldTest)$predictions
        foldResults[k] = sqrt(mean((foldPreds - foldTest[, dependent.variable.name])^2))
      }
      
      roundResults[r, ] = foldResults
    }
    
    cvResults[p, "rmse"] = mean(roundResults)
    cvResults[p, "sd"] = sqrt(var(as.vector(roundResults)))
    
    cat(" ** ", "rmse:", cvResults[p, "rmse"], " sd:", cvResults[p, "sd"], "\n")
  }
  
  # stopping the cluster
  registerDoParallel(cores = 1)
  
  return(cbind(tuneGrid, cvResults)[order(cvResults[, "rmse"]), ])
}





#################
# F O R E A C H #
#################

library(foreach)
library(doParallel)
registerDoParallel(cores = 2)

prova <- foreach(i = 1:10, .combine = rbind) %dopar% {
  tabellina = rep(1:10)
  
  return(i*tabellina)
}

