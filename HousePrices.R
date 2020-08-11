#https://www.kaggle.com/erikbruin/house-prices-lasso-xgboost-and-a-detailed-eda 
#https://www.kaggle.com/headsortails/shopping-for-insights-favorita-eda
#https://www.kaggle.com/erikbruin/google-analytics-eda-lightgbm-screenshots
#https://www.kaggle.com/rtatman/visualizing-data-with-ggplot2/comments#274543

setwd("C:/Users/user/Desktop/bysong/ADP/HousePrice")

# library
library(tidyverse) #metapackage with lots of helpful function

library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
library(randomForest)
library(psych)
library(car)

# 1. data loading
train_all <- read.csv("train.csv", header=TRUE)
test.df <- read.csv("test.csv", header=TRUE)

head(train_all)
tail(train_all)
str(train_all)

head(test.df)
tail(test.df)
str(test.df)

test.df$SalePrice <- NA
all <- rbind(train_all, test.df)
str(all)

# 2. Review of data-set (all)
sum(is.na(all[,-81]))
colSums(is.na(all))
summary(all)

##check the saleprice
summary(all$SalePrice)
  #mean = 180,921
  #max  = 755,000

##Replacing NA to None (fence, basement etc)
na_to_none <- function(variables)
{
  variable_char <- as.character(variables)
  variable_char[is.na(variable_char)] <- "None"
  variable_fac <- as.factor(variable_char)

  return(variable_fac)
}

factor_list <-c("Alley","BsmtQual", "BsmtCond", "BsmtExposure", "BsmtFinType1",
                "BsmtFinType2", "Electrical", "FireplaceQu","GarageType",
                "GarageFinish","GarageQual","GarageCond","PoolQC","Fence","MiscFeature")

for(i in factor_list)
{
  all[[i]] <- na_to_none(all[[i]])
}

str(all)
summary(all)
sum(is.na(all[,-81]))

## int -> factor
summary(all$MoSold)
summary(all$YrSold)
summary(all$MSSubClass)

all$MoSold <- as.factor(all$MoSold)
all$YrSold <- as.factor(all$YrSold)
all$MSSubClass <- as.factor(all$MSSubClass)

## Replace NA to average value
summary(all$LotFrontage)
filter(all, is.na(LotFrontage))
all$LotFrontage[is.na(all$LotFrontage)] <- mean(all$LotFrontage, na.rm=T)

summary(all$LotFrontage)

## MasVnrType : NA == None for MasVnrType and zero for MasVnrArea
all$MasVnrType[is.na(all$MasVnrType)] <- "None"
all$MasVnrArea[is.na(all$MasVnrArea)] <- 0

## GarageYrBlt : NA == no garage
all$GarageYrBlt[is.na(all$GarageYrBlt)] <- 0

## Utilities : NA <- mod value
## AllPub(All Public Utilities(E,G,W,S)
summary(all$Utilities)
all$Utilities[is.na(all$Utilities)] <- "AllPub"

## Functional, MSZoning : NA <- mod value
summary(all$Functional)
summary(all$MSZoning)

all$Functional[is.na(all$Functional)] <- "Typ"
all$MSZoning[is.na(all$MSZoning)] <- "RL"

## Bsmt : Na to 0 or mod
summary(all$BsmtFullBath)
summary(all$BsmtHalfBath)
all$BsmtHalfBath[is.na(all$BsmtHalfBath)] <- 0 # None 
all$BsmtFullBath[is.na(all$BsmtFullBath)] <- 0 # None 

summary(all$Exterior1st)
summary(all$Exterior2nd)
all$Exterior1st[is.na(all$Exterior1st)] <- "VinylSd" # assumed Metal  
all$Exterior2nd[is.na(all$Exterior2nd)] <- "VinylSd" # assumed Metal 

summary(all$BsmtFinSF1)
summary(all$BsmtFinSF2)
summary(all$BsmtUnfSF)
summary(all$TotalBsmtSF)
all$BsmtFinSF1[is.na(all$BsmtFinSF1)] <- 0   
all$BsmtFinSF2[is.na(all$BsmtFinSF2)] <- 0 
all$BsmtUnfSF[is.na(all$BsmtUnfSF)] <- 0 
all$TotalBsmtSF[is.na(all$TotalBsmtSF)] <- 0 

summary(all$KitchenQual)
summary(all$GarageCars)
summary(all$GarageArea)
summary(all$SaleType)
all$KitchenQual[is.na(all$KitchenQual)] <- 'TA'  # Typical/Average 
all$GarageCars[is.na(all$GarageCars)] <- 0 
all$GarageArea[is.na(all$GarageArea)] <- 0 
all$SaleType[is.na(all$SaleType)] <- 'WD' # Warranty Deed - Conventional

## Last Check number of NA
sum(is.na(all[,-81]))

## Distribution of sales price
summary(all$SalePrice)
ggplot(all[1:1460,], aes(SalePrice)) + geom_density()
ggplot(all[1:1460,], aes(SalePrice)) + geom_histogram(fill="red") +
  scale_x_continuous(name = "sale price", breaks = seq(0,800000,by=80000))
boxplot(all$SalePrice[1:1460])
ggplot(all[1:1460,], aes(as.character(YrSold), SalePrice)) +
  geom_boxplot()

## Review of correlation for numeric variables
str(all)
all <- all[,-c(39,47)] #TotalBsmtSF, GrLivArea: sum of other variables)
numvar_all <- sapply(all,is.numeric)
numvar.df <- all[,numvar_all]
numvar_scaled.df <- data.frame(scale(numvar.df))
str(numvar_scaled.df)

cor_result <- cor(numvar_scaled.df[-1], method=c("pearson"), use="complete.obs")

### above 0.5
cor_result.df <- as.data.frame(cor_result) %>% select(SalePrice) %>% 
  mutate(variables = rownames(cor_result)) %>% 
  arrange(desc(SalePrice)) %>% 
  filter(abs(SalePrice)>0.5)
cor_result.df
cor_variable <- cor_result.df$variables

pairs.panels(numvar_scaled.df[,cor_variable], pch = 10)

###OverallQual shows highest cor
plot(all$OverallQual, all$SalePrice)
ggplot(all, aes(as.factor(OverallQual), SalePrice)) +
  geom_boxplot()

## Review of factor variables
facvar_all <- sapply(all, is.factor)
facvar.df <- all[,facvar_all]
facvar.df <- cbind(Id=all$Id, facvar.df, SalePrice=all$SalePrice)
str(facvar.df)

facvar.df %>% group_by(YrSold, MoSold) %>% summarise(average_sale_price = mean(SalePrice, na.rm=T)) %>% 
  ggplot(aes(YrSold, average_sale_price))+geom_boxplot()

facvar.df %>% group_by(YrSold, MoSold) %>% summarise(average_sale_price = mean(SalePrice, na.rm=T)) %>% 
  ggplot(aes(MoSold, average_sale_price))+geom_boxplot()

facvar.df %>% group_by(HouseStyle, BsmtQual) %>% summarise(average_sale_price = mean(SalePrice, na.rm=T)) %>% 
  ggplot(aes(HouseStyle, average_sale_price))+geom_boxplot()

facvar.df %>% group_by(HouseStyle, BsmtQual) %>% summarise(average_sale_price = mean(SalePrice, na.rm=T)) %>% 
  ggplot(aes(BsmtQual, average_sale_price))+geom_boxplot()

facvar.df %>% group_by(YrSold, MoSold) %>% summarise(num = n()) %>% 
  ggplot(aes(MoSold, num))+geom_col()

facvar.df %>% group_by(YrSold, MoSold) %>% summarise(num = n()) %>% 
  ggplot(aes(YrSold, num))+geom_col()

facvar.df %>% group_by(HouseStyle, BsmtQual) %>% 
  summarise(num = n()) %>% ggplot(aes(HouseStyle, num))+
  geom_col()

facvar.df %>% group_by(HouseStyle, BsmtQual) %>% 
  summarise(n_sale = n()) %>% filter(n_sale>15) %>% 
  ggplot(aes(HouseStyle, n_sale)) + geom_col() + coord_flip()

### Less house sell in 2010, and it shows some seasonality (many house sell in May ~ July, less sale in Sep ~ Feb.
### Low sale price in year 2008~2010

## Finalize data set
all %>% filter(SalePrice>650000) #outlier
all_scaled.df <- cbind(facvar.df, numvar_scaled.df[-c(1,33)])
train.all.df <- all_scaled.df[1:1460,] %>% filter(SalePrice<650000)

set.seed(1)
index <- sample(nrow(train.all.df), nrow(train.all.df)*0.8)

### data set ready
train.df1 <- train.all.df[index,]
validation.df1 <- train.all.df[-index,]
test.df1 <- all_scaled.df[1461:2919,]

ggplot(train.all.df, aes(SalePrice)) + geom_histogram(fill="blue") +
  scale_x_continuous(name = "sale price", breaks = seq(0,800000,by=80000))


# 1) Random Forest
head(train.df1)
head(validation.df1)

str(train.df1)
rf <- randomForest(SalePrice~., data=train.df1[-c(1)], 
                   ntree=100, proximity=TRUE)

importance.df <- as.data.frame(importance(rf))
im.df <- add_rownames(importance.df, var="variables") %>%
          arrange(desc(IncNodePurity))
varImpPlot(rf)

rf.pred <- predict(rf, newdata=validation.df1[-c(1)])

result1.df <- cbind(rf.pred, va.result=validation.df1$SalePrice)
result <- as.data.frame(result1.df)

ggplot(as.data.frame(result1.df), aes(rf.pred, va.result)) + 
  geom_point() + geom_abline(aes(intercept=0, slope=1))

t.test(result$rf.pred, result$va.result, paired=T)
