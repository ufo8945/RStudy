# Load packages
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm
library('VIM')
library('RColorBrewer') 

#https://www.kaggle.com/redhorse93/r-titanic-data 참고

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

setwd("D:/R/Titanic")
train = read.csv("train.csv")
test = read.csv("test.csv")
gensub = read.csv("gender_submission.csv")

#Check Data
str(train)
head(train)
summary(train)

str(test)
head(test)
summary(test)

str(gensub)
head(gensub)
summary(gensub)


##Dataset 설명
###PassengerID : 승객을 구별하는 고유 ID (integer)
###Survived : 승객의 생존 여부로 생존은 1, 사망은 0 (Factor)
###PClass : 선실의 등급으로 1~3까지 3개 범주 (Ord.Factor)
###Name : 승객의 이름 (Factor)
###Sex : 승객의 성별 (Factor)
###Age : 승객의 나이 (Numeric)
###SibSp : 승객과 동반한 형제 또는 배우자의 수 (0~8) (Integer)
###Parch : 승객과 동반한 부모님 또는 자녀의 수 (0~9) (Integer)
###Ticket : 승객이 탑승한 티켓에 대한 문자열 변수 (Factor)
###Fare : 승객이 지금까지 여행하면서 지불한 금액에 대한 변수 (Numeric)
###Cabin : 승객의 선실 구분 (범주, 결측지 많음) (Factor)
###Embarked : 승선항, 출항짐이며 C, Q, S 범주 (Factor)

#Missing Data Analysis
##Missing values of Age <- mean of Age
VIM::aggr(train, prop = FALSE, combined = TRUE, numbers = TRUE,
          sortVars = TRUE, sortCombs = TRUE)

VIM::aggr(test, prop = FALSE, combined = TRUE, numbers = TRUE,
          sortVars = TRUE, sortCombs = TRUE)

sum(is.na(train))
train$Age[is.na(train$Age)] <- mean(train$Age, na.rm=T)

sum(is.na(test))
test$Age[is.na(test$Age)] <- mean(test$Age, na.rm=T)

sum(is.na(gensub))

#Exploratory Data Analysis
##Total number of survived and not survived
table(gensub$Survived)

##Survived and Not Survived by sex
table(train$Sex, train$Survived)
cat("Proportion of females surviving: ", sum(train$Sex == "female" & train$Survived == 1)/sum(train$Sex == "female"))
cat("\nProportion of males surviving: ", sum(train$Sex == "male" & train$Survived == 1)/sum(train$Sex == "male"))

##Family Size
hist(train$SibSp)
hist(train$Parch)
train$Family <- train$SibSp+train$Parch+1
test$Family <- test$SibSp+test$Parch+1
hist(train$Family)
summary(train$Family)

train$Famils[train$Family==1] <- 'single'
train$Famils[train$Family>1 & train$Family<5] <- 'small'
train$Famils[train$Family>=5] <- 'large'
test$Famils[test$Family==1] <- 'single'
test$Famils[test$Family>1 & test$Family<5] <- 'small'
test$Famils[test$Family>=5] <- 'large'
table(train$Famils)

## Check the Cabin
head(train$Cabin, 10)
sum(train$Cabin=="") #missing value == ""
strsplit(train$Cabin[2], NULL)[[1]][1]
train$Deck <- sapply(train$Cabin, function(x) strsplit(x, NULL)[[1]][1])
test$Deck <- sapply(test$Cabin, function(x) strsplit(x, NULL)[[1]][1])
table(train$Deck)
sum(is.na(train$Deck==""))
train$Deck[is.na(train$Deck=="")]<-'C'
test$Deck[is.na(test$Deck=="")]<-'C'
sum(is.na(train$Deck==""))

## Check the Embarked
sum(train$Embarked=="") #missing value == ""
table(train$Embarked)
train$Embarked[is.na(train$Embarked=="")]<-'S'
test$Embarked[is.na(test$Embarked=="")]<-'S'

## Age(18,54) --> Child and Adult
hist(train$Age)
box<-boxplot(train$Age)
train$AgeG[train$Age < 18] <- 'Child'
train$AgeG[train$Age >=18 & train$Age <55] <- 'Adult'
train$AgeG[train$Age >=55] <- 'Elder'
table(train$AgeG)
test$AgeG[test$Age < 18] <- 'Child'
test$AgeG[test$Age >=18 & test$Age <55] <- 'Adult'
test$AgeG[test$Age >=55] <- 'Elder'

## PClass
table(train$Pclass)

## Fare
box<-boxplot(train$Fare)
box

#Data Visualization
##Age
age.p1 <- train %>% 
  ggplot(aes(Age)) + 
  # 히스토그램 그리기, 설정
  geom_histogram(breaks = seq(0, 80, by = 1), # 간격 설정 
                 col    = "red",              # 막대 경계선 색깔 
                 fill   = "green",            # 막대 내부 색깔 
                 alpha  = .5) +               # 막대 투명도 = 50% 
  # Plot title
  ggtitle("All Titanic passengers age hitogram") +
  theme(plot.title = element_text(face = "bold",    # 글씨체 
                                  hjust = 0.5,      # Horizon(가로비율) = 0.5
                                  size = 15, color = "darkblue"))

age.p2 <- train %>% 
  # test data set의 Survived == NA 인 값들 제외 
  filter(!is.na(Survived)) %>% 
  ggplot(aes(Age, fill = Survived)) + 
  geom_density(alpha = .5) +
  ggtitle("Titanic passengers age density plot") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5,
                                  size = 15, color = "darkblue"))

# multiplot layout 형식 지정
multi.layout = matrix(c(1, 1, 2, 2), 2, 2, byrow = T)

# 위에서 생성한 2개의 그래프 한 화면에 출력 
multiplot(age.p1, age.p2, layout = multi.layout)

##PClass
train %>% 
  # dplyr::group_by(), summarize() 를 이용해서 Pclass 빈도수 구하기
  group_by(Pclass) %>% 
  summarize(N = n()) %>% 
  # Aesthetic setting 
  ggplot(aes(Pclass, N)) +
  geom_col() +
  # Pclass 빈도수 plot에 출력 
  geom_text(aes(label = N),        # Plot의 y에 해당하는 N(빈도수)를 매핑
            size = 5,              # 글씨 크기 
            vjust = 1.2,           # vertical(가로) 위치 설정 
            color = "#FFFFFF") +   # 글씨 색깔 : 흰색
  # Plot title 
  ggtitle("Number of each Pclass's passengers") + 
  # Title setting 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15)) +
  # x, y axis name change  
  labs(x = "Pclass", y = "Count")

##Fare
# Histogram 
Fare.p1 <- train %>%
  ggplot(aes(Fare)) + 
  geom_histogram(col    = "yellow",
                 fill   = "blue", 
                 alpha  = .5) +
  ggtitle("Histogram of passengers Fare") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15))

# Boxplot 
Fare.p2 <- train %>%
  filter(!is.na(Survived)) %>% 
  ggplot(aes(Survived, Fare)) + 
  # 관측치를 회색점으로 찍되, 중복되는 부분은 퍼지게 그려줍니다.
  geom_jitter(col = "gray") + 
  # 상자그림 : 투명도 50% 
  geom_boxplot(alpha = .5) + 
  ggtitle("Boxplot of passengers Fare") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15))

# multiplot layout 형식 지정
multi.layout = matrix(c(1, 1, 2, 2), 2, 2)

# 위에서 생성한 2개의 그래프 한 화면에 출력 
multiplot(Fare.p1, Fare.p2, layout = multi.layout)

##Sex
sex.p1 <- train %>% 
  dplyr::group_by(Sex) %>% 
  summarize(N = n()) %>% 
  ggplot(aes(Sex, N)) +
  geom_col() +
  geom_text(aes(label = N), size = 5, vjust = 1.2, color = "#FFFFFF") + 
  ggtitle("Bar plot of Sex") +
  labs(x = "Sex", y = "Count")

sex.p2 <- train %>% 
  ggplot(aes(Sex, fill = Survived)) +
  geom_bar(position = "fill") + 
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels = percent) +
  ggtitle("Survival Rate by Sex") + 
  labs(x = "Sex", y = "Rate")

multi.layout = matrix(rep(c(1, 2), times = 2), 2, 2, byrow = T)

multiplot(sex.p1, sex.p2, layout = multi.layout)

mosaicplot(Survived ~ Sex,
           data = train, col = TRUE,
           main = "Survival rate by passengers gender")

#ML Model
##Random Forest model generation
set.seed(1901)
train$Survived <- as.factor(train$Survived)
titanic.rf <- randomForest(Survived ~ ., data = train, importance = T, ntree = 2000)
## Feature Importance check
importance(titanic.rf)
varImpPlot(titanic.rf)

#Predict
## Prediction 
pred.rf <- predict(object = titanic.rf, newdata = test, type = "class")
pred.rf <- as.numeric(pred.rf)
pred.rf[pred.rf==1] <- 0
pred.rf[pred.rf==2] <- 1
pred.rf
pred.rf[is.na(pred.rf)]<-0
submit <- data.frame(PassengerID = test$PassengerId, Survived = pred.rf)
submit

#Accuracy
correct = sum(submit$Survived==gensub$Survived)
accuracy = correct/length(gensub$Survived)
accuracy*100

##make table
table(submit$Survived, gensub$Survived)
