library(readr)
library(dplyr)
library(readr)
library(psych)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(arules)
library(arulesViz)
library(proxy)
library(recommenderlab)
library(RColorBrewer)
library(fields)
library(ggplot2)
library(ggpubr)


if(.Platform$OS.type == "windows") withAutoprint({
  memory.size()
  memory.size(TRUE)
  memory.limit()
})
memory.limit(size=56000)

################################################################################
################################################################################
################################################################################



originaldata <- read_csv("users_brands.csv")
help <- read_csv("brand_features.csv")

users_brands<- as.tibble(originaldata)

users_brands<-users_brands %>% 
  rename(
    sequence_id = purchase_date
  )



#str(users_brands)
#str(help)

any(is.na(users_brands)) #false
any(is.na(help)) #false

summary(users_brands)
summary(help)

users_brands$brand_id <- as.factor(users_brands$brand_id)
users_brands$platform <- as.factor(users_brands$platform)
users_brands$sequence_id <- as.factor(users_brands$sequence_id)

help$brand_id  <- as.factor(help$brand_id)

length(unique(users_brands$user_id)) # 329213 users
length(unique(users_brands$brand_id)) #1771 brands
length(unique(users_brands$country))  #175 countries
length(unique(users_brands$sequence_id)) #231 sequences
length(unique(users_brands$platform)) #12


ggplot(users_brands %>% group_by(user_segment) %>%summarise(counts = n()), aes(x = user_segment, y = counts)) +
  geom_bar(fill = "#96a1e3", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean()

ggplot(users_brands %>% group_by(perc_sale) %>%summarise(counts = n()), aes(x = perc_sale, y = counts)) +
  geom_bar(fill = "#96a1e3", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean()

ggplot(users_brands %>% group_by(platform) %>%summarise(counts = n()), aes(x = platform, y = counts)) +
  geom_bar(fill = "#96a1e3", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean()


users_brands %>%
  ggplot(aes(user_segment)) +
  geom_bar(aes(fill = perc_sale))

users_brands %>%
  ggplot(aes(platform)) +
  coord_flip() +
  geom_bar(aes(fill = user_segment))

users_brands %>%
  ggplot(aes(platform)) +
  coord_flip() +
  geom_bar(aes(fill = perc_sale))



##################################################################################
############################### preparation ######################################
users_brands_n<- users_brands %>% select(user_id,brand_id)
users_brands_n$user_id<-as.factor(users_brands$user_id) 
users_brands_n$brand_id<-as.factor(users_brands$brand_id) 
data<-users_brands_n

TransList <- split(data$brand_id,data$user_id)
TransMat <- as(TransList, "transactions")
summary(TransMat)

itemFrequencyPlot(TransMat,topN=100,col=brewer.pal(8,'Pastel2'),cex.names=0.7,main='Relative Item Frequency Plot',type="relative",ylab="Item Frequency (Relative)")
itemFrequencyPlot(TransMat,topN=50,col=brewer.pal(8,'Pastel2'),cex.names=0.7,main='Relative Item Frequency Plot',type="relative",ylab="Item Frequency (Relative)")
x <- data %>% 
    group_by(brand_id) %>%
    summarise(n()) %>%
    top_n(50) %>%
  { filter(data, brand_id %in% .$brand_id) }


brm <- as(as.data.frame(x),"binaryRatingMatrix")
brm_offline<- brm[1:100000,]

user <- brm[500,]

getRatingMatrix(brm_offline)
inspect(getRatingMatrix(brm_offline))
#datanew <- getData.frame(brm_offline)
#image(brm_offline)
#colCounts(brm_offline)


##################################################################################
##################################################################################
##################################################################################
############################### popularity #######################################

modelPop <- Recommender(brm_offline, "POPULAR")

recsPOP1 <- predict(modelPop, user, n=1)
getList(recsPOP1)
recsPOP2 <- predict(modelPop, user, n=2)
getList(recsPOP2)
recsPOP6 <- predict(modelPop, user, n=6)
getList(recsPOP6)

##################################################################################
##################################################################################
##################################################################################
########################### ASSOCIATION RULES ####################################

modelar <- Recommender(brm_offline, "AR", param=list(supp=0.003, conf=0.05))
getModel(modelar)
rules <- getModel(modelar)$rule_base
inspect(rules)

recsAR1 <- predict(modelar, user, n=1)
getList(recsAR1)
recsAR2 <- predict(modelar, user, n=2)
getList(recsAR2)
recsAR6 <- predict(modelar, user, n=6)
getList(recsAR6)

##################################################################################
##################################################################################
##################################################################################
####################### COLLABORATIVE FILTERING ##################################

modelubcf <- Recommender(brm_offline, "UBCF",parameter=list(method="cosine",nn=100))

getModel(modelubcf)
recsUBCF1 <- predict(modelubcf, user, n=1)
getList(recsUBCF1)
recsUBCF2 <- predict(modelubcf, user, n=2)
getList(recsUBCF2)
recsUBCF6 <- predict(modelubcf, user, n=6)
getList(recsUBCF6)


modelibcf <- Recommender(brm_offline, "IBCF",parameter=list(method="cosine",k=100))
getModel(modelibcf)
recsIBCF1 <- predict(modelibcf, user, n=1)
getList(recsIBCF1)
recsIBCF2 <- predict(modelibcf, user, n=2)
getList(recsIBCF2)
recsIBCF6 <- predict(modelibcf, user, n=6)
getList(recsIBCF6)


##################################################################################
##################################################################################
##################################################################################
############################### EVALUATION #######################################



methods <- list(
  "popular" = list(name="POPULAR", param = NULL),
  "user-based CF" = list(name="UBCF", param = list(method="cosine",nn=100)),
  "item-based CF" = list(name="IBCF", param = list(method="cosine",k=100)),
  "ar" = list(name="AR", param = list(supp=0.003, conf=0.05))
)

#Tested for different rowCounts , the best result was with e3

set.seed(1)
brm <- brm[rowCounts(brm)>=4,]
e1 <- evaluationScheme(brm, method="split", train=0.80, given = 4)
results1 <- evaluate(e1, methods, type="topNList", n=c(1,2,6))
avg(results1)

set.seed(2)
brm <- brm[rowCounts(brm)>=3,]
e2 <- evaluationScheme(brm, method="split", train=0.80, given = 3)
results2 <- evaluate(e2, methods, type="topNList", n=c(1,2,6))
avg(results2)

set.seed(3)
brm <- brm[rowCounts(brm)>=1,]
e3 <- evaluationScheme(brm, method="split", train=0.80, given =1)
results3 <- evaluate(e3, methods, type="topNList", n=c(1,2,6))
avg(results3)

###################


model1 <- Recommender(getData(e3,"train"), "AR",param = list(supp=0.003, conf=0.05))
preds1 <- predict(model1,getData(e3,"known"),n=1)
getList(preds1)


model2 <- Recommender(getData(e3,"train"), "POPULAR")
preds2 <- predict(model2,getData(e3,"known"),n=1)
getList(preds2)


model3 <- Recommender(getData(e3,"train"), "UBCF",param = list(method="cosine",nn=10))
preds3 <- predict(model3,getData(e3,"known"),n=1)
getList(preds3)

model4 <- Recommender(getData(e3,"train"), "IBCF", param = list(method="cosine",k=10))
preds4 <- predict(model4,getData(e3,"known"),n=1)
getList(preds4)


plot(results3,annotate=TRUE)
plot(results3, "prec/rec", annotate=TRUE)


############################### TASK 3 #######################################
# limitar a database relativamente ao país com mais influencia, sales com mais influencia e plataforma

#Ver top 10 de countries
users_count<- users_brands %>% select(user_id,country)
users_count$user_id<-as.factor(users_brands$user_id) 
users_count$country<-as.factor(users_brands$country) 
data2<-users_count

TransList <- split(data2$country,data2$user_id)
TransMat <- as(TransList, "transactions")
summary(TransMat)

itemFrequencyPlot(TransMat,topN=50,col=brewer.pal(8,'Pastel2'),main='Absolute Item Frequency Plot',type="absolute",ylab="Item Frequency (Absolute)")
#conclusão , os top 10 country são dos mais influentes


#ver quais são as sales mais influentes
users_count<- users_brands %>% select(user_id,perc_sale)
users_count$user_id<-as.factor(users_brands$user_id) 
users_count$perc_sale<-as.factor(users_brands$perc_sale) 
data2<-users_count

TransList <- split(data2$perc_sale,data2$user_id)
TransMat <- as(TransList, "transactions")
summary(TransMat)

itemFrequencyPlot(TransMat,topN=5,col=brewer.pal(8,'Pastel2'),main='Absolute Item Frequency Plot',type="absolute",ylab="Item Frequency (Absolute)")
#pelo grafico concluimos que a aferencia a 0% e 25,50 são muito superiores, por isso top2


#Ver plataforma mais usada
users_count<- users_brands %>% select(user_id,platform)
users_count$user_id<-as.factor(users_brands$user_id) 
users_count$platform<-as.factor(users_brands$platform) 
data2<-users_count

TransList <- split(data2$platform,data2$user_id)
TransMat <- as(TransList, "transactions")
summary(TransMat)

itemFrequencyPlot(TransMat,topN=5,col=brewer.pal(8,'Pastel2'),main='Absolute Item Frequency Plot',type="absolute",ylab="Item Frequency (Absolute)")
#podemos observar que a plataforma mais usada é as 2 primeiras com bastante clareza, por isso, limitar para top2


### DATABASE REDUCTION  ###
users_final<- users_brands %>% select(user_id,brand_id,country,perc_sale,platform)
users_final$user_id<-as.factor(users_brands$user_id)
users_final$brand_id<-as.factor(users_brands$brand_id) 
users_final$country<-as.factor(users_brands$country)
users_final$perc_sale<-as.factor(users_brands$perc_sale)
users_final$platform<-as.factor(users_brands$platform) 


x <- users_final %>% 
  group_by(brand_id) %>%
  summarise(n()) %>%
  top_n(20) %>%
  { filter(users_final, brand_id %in% .$brand_id) } #top 20 brands

x <- x %>% 
  group_by(country) %>%
  summarise(n()) %>%
  top_n(10) %>%
  { filter(x, country %in% .$country) }  #top 10 country

x <- x %>% 
  group_by(perc_sale) %>%
  summarise(n()) %>%
  top_n(2) %>%
  { filter(x, perc_sale %in% .$perc_sale) } #top 2 sale

x <- x %>% 
  group_by(platform) %>%
  summarise(n()) %>%
  top_n(2) %>%
  { filter(x, platform %in% .$platform) } #top 2 platform


# ficar apenas com user_id e brand_id
x[3] <- NULL 
x[3] <- NULL 
x[3] <- NULL



#Testar se houve melhorias-> Houve melhorias

brm <- as(as.data.frame(x),"binaryRatingMatrix")

methods <- list(
  "popular" = list(name="POPULAR", param = NULL),
  "user-based CF" = list(name="UBCF", param = list(method="cosine",nn=100)),
  "item-based CF" = list(name="IBCF", param = list(method="cosine",k=100)),
  "ar" = list(name="AR", param = list(supp=0.003, conf=0.06))
)

#Tested for different rowCounts , the best result was with given = 1
set.seed(2)
brm <- brm[rowCounts(brm)>=3,]
e2 <- evaluationScheme(brm, method="split", train=0.80, given = 3)
results2 <- evaluate(e2, methods, type="topNList", n=c(1,2,6))
avg(results2)

set.seed(4)
brm <- brm[rowCounts(brm)>=2,]
e3 <- evaluationScheme(brm, method="split", train=0.80, given =2)
results3 <- evaluate(e3, methods, type="topNList", n=c(1,2,6))
avg(results3)

set.seed(3)
brm <- brm[rowCounts(brm)>=1,]
e3 <- evaluationScheme(brm, method="split", train=0.80, given =1)
results4 <- evaluate(e3, methods, type="topNList", n=c(1,2,6))
avg(results4)

#best results with n = 1

plot(results4,annotate=TRUE)
plot(results4, "prec/rec", annotate=TRUE)

