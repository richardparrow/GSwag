# libraries
library(tidyverse)
library(jsonlite)


# loading
train = read_csv("train.csv")
test = read_csv("test.csv")

combi = rbind(train, test)


# per fare le prove combi = combi[1:10, ]


# JSONing ~ qua ci metterà un po'!
prepareJSON <- function(text) {
  return(paste("[", paste(text, collapse = ","), "]"))
}

deviceJSON = combi$device %>% prepareJSON() %>% fromJSON(flatten = T)
geoNetworkJSON = combi$geoNetwork %>% prepareJSON() %>% fromJSON(flatten = T)
totalsJSON = combi$totals %>% prepareJSON() %>% fromJSON(flatten = T)
trafficSourceJSON = combi$trafficSource %>% prepareJSON() %>% fromJSON(flatten = T)

combi = combi %>% 
  cbind(deviceJSON, geoNetworkJSON, totalsJSON, trafficSourceJSON) %>%
  select(-device, -geoNetwork, -totals, -trafficSource) %>%
  as.tibble()


# finding columns with only one unique value
uniqueValues = sapply(combi, function(x) length(unique(na.omit(x))))
oneValue = colnames(combi)[uniqueValues == 1]
combi = combi[, !(colnames(combi) %in% oneValue)]


# setting some variable types right
combi = combi %>%
  mutate(transactionRevenue = transactionRevenue %>% as.numeric()) %>%
  replace_na(list(transactionRevenue = 0)) %>%
  mutate(transactionRevenue = transactionRevenue %>% log1p()) %>%
  mutate(hits = hits %>% as.integer(),
         pageviews = pageviews %>% as.integer(),
         adwordsClickInfo.page = adwordsClickInfo.page %>% as.integer()) %>%
  mutate(browser = browser %>% as.factor(),
         operatingSystem = operatingSystem %>% as.factor(),
         deviceCategory = deviceCategory %>% as.factor(),
         continent = continent %>% as.factor(),
         subContinent = subContinent %>% as.factor(),
         country = country %>% as.factor(),
         region = region %>% as.factor(),
         metro = metro %>% as.factor(),
         networkDomain = networkDomain %>% as.factor(),
         campaign = campaign %>% as.factor(),
         source = source %>% as.factor(),
         medium = medium %>% as.factor(),
         keyword = keyword %>% as.factor(),
         #referralPath = referralPath %>% as.factor(), ~ non so che cacchio sia!
         adwordsClickInfo.slot = adwordsClickInfo.slot %>% as.factor(),
         adwordsClickInfo.gclId = adwordsClickInfo.gclId %>% as.factor(),
         adwordsClickInfo.adNetworkType = adwordsClickInfo.adNetworkType %>% as.factor())



############################


# re-split in train and test
isTrain = 1:nrow(train)
rm(train, test)
gc()

train = combi[isTrain, ]
test = combi[-isTrain, ]


# save objects as .RData
save(train, file = "trainPolished.RData")
save(test, file = "testPolished.RData")

