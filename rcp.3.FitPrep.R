# libraries
library(Matrix)


# loading
load("trainFeatEng.RData")
load("testFeatEng.RData")


# tolgo variabili problematiche (per ora)
train = train %>%
  select(-channelGrouping,
         -fullVisitorId,
         -sessionId,
         -visitId,
         -visitStartTime,
         -city,
         -date,
         -networkDomain,
         -source,
         -keyword,
         -referralPath,
         -adContent,
         -adwordsClickInfo.gclId, # too many levels
         -domain) %>% 
  mutate(browser = browser %>% as.character(),
         operatingSystem = operatingSystem %>% as.character(),
         deviceCategory = deviceCategory %>% as.character(),
         continent = continent %>% as.character(),
         subContinent = subContinent %>% as.character(),
         country = country %>% as.character(),
         region = region %>% as.character(),
         metro = metro %>% as.character(),
         campaign = campaign %>% as.character(),
         medium = medium %>% as.character(),
         adwordsClickInfo.slot = adwordsClickInfo.slot %>% as.character(),
         adwordsClickInfo.adNetworkType = adwordsClickInfo.adNetworkType %>% as.character()) %>%
  replace_na(list(browser = "Not Available",
                  operatingSystem = "Not Available",
                  deviceCategory = "Not Available",
                  continent = "Not Available",
                  subContinent = "Not Available",
                  country = "Not Available",
                  region = "Not Available",
                  metro = "Not Available",
                  campaign = "Not Available",
                  medium = "Not Available",
                  adwordsClickInfo.slot = "Not Available",
                  adwordsClickInfo.adNetworkType = "Not Available")) %>%
  mutate(browser = browser %>% as.factor(),
         operatingSystem = operatingSystem %>% as.factor(),
         deviceCategory = deviceCategory %>% as.factor(),
         continent = continent %>% as.factor(),
         subContinent = subContinent %>% as.factor(),
         country = country %>% as.factor(),
         region = region %>% as.factor(),
         metro = metro %>% as.factor(),
         campaign = campaign %>% as.factor(),
         medium = medium %>% as.factor(),
         adwordsClickInfo.slot = adwordsClickInfo.slot %>% as.factor(),
         adwordsClickInfo.adNetworkType = adwordsClickInfo.adNetworkType %>% as.factor()) %>%
  replace(is.na(.), -1)

test = test %>%
  select(-channelGrouping,
         -fullVisitorId,
         -sessionId,
         -visitId,
         -visitStartTime,
         -city,
         -date,
         -networkDomain,
         -source,
         -keyword,
         -referralPath,
         -adContent,
         -adwordsClickInfo.gclId,
         -domain,
         -transactionRevenue) %>%
  mutate(browser = browser %>% as.character(),
         operatingSystem = operatingSystem %>% as.character(),
         deviceCategory = deviceCategory %>% as.character(),
         continent = continent %>% as.character(),
         subContinent = subContinent %>% as.character(),
         country = country %>% as.character(),
         region = region %>% as.character(),
         metro = metro %>% as.character(),
         campaign = campaign %>% as.character(),
         medium = medium %>% as.character(),
         adwordsClickInfo.slot = adwordsClickInfo.slot %>% as.character(),
         adwordsClickInfo.adNetworkType = adwordsClickInfo.adNetworkType %>% as.character()) %>%
  replace_na(list(browser = "Not Available",
                  operatingSystem = "Not Available",
                  deviceCategory = "Not Available",
                  continent = "Not Available",
                  subContinent = "Not Available",
                  country = "Not Available",
                  region = "Not Available",
                  metro = "Not Available",
                  campaign = "Not Available",
                  medium = "Not Available",
                  adwordsClickInfo.slot = "Not Available",
                  adwordsClickInfo.adNetworkType = "Not Available")) %>%
  mutate(browser = browser %>% as.factor(),
         operatingSystem = operatingSystem %>% as.factor(),
         deviceCategory = deviceCategory %>% as.factor(),
         continent = continent %>% as.factor(),
         subContinent = subContinent %>% as.factor(),
         country = country %>% as.factor(),
         region = region %>% as.factor(),
         metro = metro %>% as.factor(),
         campaign = campaign %>% as.factor(),
         medium = medium %>% as.factor(),
         adwordsClickInfo.slot = adwordsClickInfo.slot %>% as.factor(),
         adwordsClickInfo.adNetworkType = adwordsClickInfo.adNetworkType %>% as.factor()) %>%
  replace(is.na(.), -1)


# preparo per modelli
rangerXTrain = sparse.model.matrix(~ 0 + ., train)
#rangerXTest = sparse.model.matrix(~ 0 + ., test) ~ interesting, test ha alcuni factor completamente in NA

save(rangerXTrain, file = "modelMatrixTrain_ranger.RData")

