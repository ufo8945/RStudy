1. 필요한 Library들
library(knitr) #R markdown
library(ggplot2)#Data Visualization
library(plyr)#Data Pre-Processing by R
library(dplyr)#Data Pre-Processing by C++
library(corrplot)#Correlation Matrix & Confidence Interval
library(caret)#Classification And REgression Training
library(gridExtra)#Chart Distribution
library(scales)#Data Visualization
library(Rmisc)#CI (Confidence:신뢰구간)
library(ggrepel)#ggplot Label
library(randomForest)#RandomForest
library(psych)#Descriptive Statistics Summary
library(xgboost)#xgboost

2. 필요 없는 컬럼은 df$col <- NULL로 없앨 수 있다.
train$Id <- NULL

3. test에 종속변수를 NA값으로 채워 추가하자.
test$SalePrice <- NA

4. train 과 test를 rbind로 합치자. (row를 이어 붙이는 거니까)
all <- rbind(train, test)

5. ggplot2 활용법 (문서 따로 제작)
ggplot(data=all[!is.na(all$SalePrice),], aes(x=SalePrice)) +
        geom_histogram(fill="purple", binwidth=10000) +
        scale_x_continuous(breaks=seq(0,800000,by=100000), labels=comma)

6.sapply 활용법
numericVars <- which(sapply(all, is.numeric))
: sapply( X, FUN, ...) #X:벡터,리스트,DF,표현식 등/FUN:적용할 함수/...:추가인자
: sapply는 결과를 벡터로 반환한다.
: as.data.frame(t(sapply결과)) 하면, DF으로 변환할 수 있다.
: 컬럼별로 데이터 타입을 알아낼 때 유용하게 사용할 수 있다. 


7. Correlation 구하기
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs")
cor_numVar[,'SalePrice']
cor_sorted <- as.matrix(sort(abs(cor_numVar[,'SalePrice']), decreasing=TRUE))
cor_sorted
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]
corrplot.mixed(cor_numVar, tl.col="black", tl.pos="lt")
: cor(data, use=“cor방법”) #pairwise.compelete.obs : 모든 데이터끼리의 cor구함
: 종속변수와의 cor값을 보고 싶으면, cor[,‘종속변수’] 하면 된다.
: as.matrix(sort(abs(결과), decreasing=T)) #cor 큰 순서대로 정렬 후, matrix화 한다.
: apply(DATA, 1, FUN) # DATA를 1(행) 방향으로 FUN 적용한다.
			# 2는 (열)방향 이겠쥬?
			# FUN: function(x) abs(x)>0.5 일반 함수세우 듯이..
			# 왜 as.matrix했냐면, apply는 vector에 적용 못한다.
: 0.5보다 큰 값만 뽑아놓는다.
: corrplot_mixed(DATA, tl.col=“COLOR”, tl.pos=“POSITION”) #tl : text legend


8. column별 NA 개수를 구해보자.
sort(colSums(sapply(all[NAcol], is.na)), decreasing=TRUE)
: 컬럼별 개수 니까 sapply를 쓴다. #sapply(데이터, is.na)
: is.na 의 결과는 T/F 니끼 colSums로 총 개수를 구한다.
: sort함수로 정렬까지 마무리


9. Factor형 변수의 NA 값을 채우는 법 : None으로 대체
all$PoolQC[is.na(all$PoolQC)] <- 'None’

10. 순서가 있는 Factor 변수 : REVALUE 하여 Numeric 변수로
Qualities <- c('None'=0, 'Po'=1, 'Fa'=2, 'TA'=3, 'Gd'=4, 'Ex'=5)
all$PoolQC <- as.integer(revalue(all$PoolQC, Qualities))
table(all$PoolQC)

11. Numeric형 변수의 NA 값을 채우는 법 : 관련 있는 다른 변수를 기준으로 그룹화하여 그룹의 중앙값으로 대체
for (i in 1:nrow(all)){
  if(is.na(all$LotFrontage[i])){
    all$LotFrontage[i] <- as.integer(median(all$LotFrontage[all$Neighborhood==all$Neighborhood[i]], na.rm=TRUE)) 
  }
}

12. Factor형 변수의 NA 값을 채우는 법 : 최빈값으로 채운다.
all$GarageCond[2127] <- names(sort(-table(all$GarageCond)))[1]
all$GarageQual[2127] <- names(sort(-table(all$GarageQual)))[1]
all$GarageFinish[2127] <- names(sort(-table(all$GarageFinish)))[1]
: table로 범주별 개수 구한 뒤, sort해서 가장 앞의 값 [1]의 names를 채택

13. ggplot2 활용법 참고
ys <- ggplot(all[!is.na(all$SalePrice),], aes(x=as.factor(YrSold), y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='purple')+
  scale_y_continuous(breaks= seq(0, 800000, by=25000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..)) +
  coord_cartesian(ylim = c(0, 200000)) +
  geom_hline(yintercept=163000, linetype="dashed", color = "red") #dashed line is median SalePrice

ms <- ggplot(all[!is.na(all$SalePrice),], aes(x=MoSold, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='purple')+
  scale_y_continuous(breaks= seq(0, 800000, by=25000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..)) +
  coord_cartesian(ylim = c(0, 200000)) +
  geom_hline(yintercept=163000, linetype="dashed", color = "red") #dashed line is median SalePrice

grid.arrange(ys, ms, widths=c(1,2))

14. Numeric형과 Factor형을 나누어서 처리하면 편하다.
numericVars <- which(sapply(all, is.numeric))
factorVars <- which(sapply(all, is.factor))

15. RF로 중요한 변수를 예상해보자.
set.seed(2018)
quick_RF <- randomForest(x=all[1:1460,-79], y=all$SalePrice[1:1460],
                         ntree=100, importance=TRUE)
imp_RF <- importance(quick_RF)
imp_DF <- data.frame(Variables=row.names(imp_RF), MSE=imp_RF[,1])
imp_DF <- imp_DF[order(imp_DF$MSE, decreasing=TRUE),]
imp_DF

ggplot(imp_DF[1:20,], aes(x=reorder(Variables, MSE), y=MSE, fill=MSE)) +
        geom_bar(stat="identity") +
        labs(x="Variables", y="% increase MSE if Variable is randomly permuted") 	+coord_flip() + theme(legend.position = "none")
: randomForest(x=설명변수, y=종속변수, ntree=100, importance=TRUE)
  # 대충 100개로 설정하고, 중요도 뽑아내자.
: importance(RFresult) #중요도를 뽑아냈다.
: row.names가 변수명, [,1]의 값이 MSE로 높을수록 중요한 값
: DF[order(컬럼명, decreasing=TRUE) #DF를 한 컬럼을 기준으로 정렬할 수 있다.
: aes(x=reorder(변수명, MSE) + geom_bar(stat=“identity”) + coord_flip() 
  #MSE를 기준으로 정렬하여 큰 값부터 그래프를 그릴 것이다.
16. MULTIPLOT을 그리는 법
layout <- matrix(c(1,2,5,3,4,8,6,7),4,2,byrow=TRUE)
multiplot(s1,s2,s3,s4,s5,s6,s7,s8, layout=layout)
: s1, s2, ... 등 그래프 일단 객체 저장해둔다.
: layout도 설정해둔다. matrix(그래프 순서, 행개수, 열 개수, row부터?)
: multiplot(저장해둔 객체들, 설정한 layout)으로 그리면 끝

17. 어떤 변수들의 합이 다른 변수와 같을 수 도 있다.
cor(all$GrLivArea, (all$X1stFlrSF+all$X2ndFlrSF+all$LowQualFinSF))

18. %in% : SQL의 IN 구문과 같은 기능
all$NeighRich[all$Neighborhood %in% c('StoneBr', 'NridgHt', 'NoRidge')] <- 2
all$NeighRich[!all$Neighborhood %in% c('MeadowV', 'IDOTRR', 'BrDale', 'StoneBr', 'NridgHt', 'NoRidge')] <- 1
all$NeighRich[all$Neighborhood %in% c('MeadowV', 'IDOTRR', 'BrDale')] <- 0
table(all$NeighRich)
: DATA %in% VECTOR #VECTOR 안의 값에 해당하는 값들만 추출
: !DATA 하면 NOT IN 인 것이다.

19. 두 변수를 합쳐서 새로운 TotalSqFeet 변수 생성한다. (파생변수!)
all$TotalSqFeet <- all$GrLivArea + all$TotalBsmtSF

20. ggplot2 활용법 참고
ggplot(data=all[!is.na(all$SalePrice),], aes(x=TotalSqFeet, y=SalePrice))+
  geom_point(col='purple') + #geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  geom_text_repel(aes(label = ifelse(all$GrLivArea[!is.na(all$SalePrice)]>4500, rownames(all), '')))


21. 이상치 두 개 빼고 상관계수 다시 구해보기
cor(all$SalePrice, all$TotalSqFeet, use= "pairwise.complete.obs")
cor(all$SalePrice[-c(524, 1299)], all$TotalSqFeet[-c(524, 1299)], use= "pairwise.complete.obs")
: 이상치 의심되는 것을 빼기 전과 뺀 후의 cor값을 비교해보자.

22. 다중공선성 높은 변수는 제거한다.
##numeric 변수 재정비 후, 새로 추가한 변수 추가
##나머지 변수는 factor형 변수쪽에 추가
numericVarNames <- numericVarNames[!(numericVarNames %in% c('MSSubClass', 'MoSold', 'YrSold', 'SalePrice', 'OverallQual', 'OverallCond'))]

23. LONG TAIL(skewed) 된 값들은 정규화를 해준다.
###정규화를 시켜준다.
###skew값이 0.8이 넘으면, log취한 뒤, 1을 더해준다
###1을 더해주는 이유는, 0으로 나누는 이슈를 없애기 위해..
for(i in 1:ncol(DFnumeric)){
  if (abs(skew(DFnumeric[,i]))>0.8){
    DFnumeric[,i] <- log(DFnumeric[,i] +1)
  }
}

24. CARET패키지의 preProcess 함수
###Normalizing
PreNum <- preProcess(DFnumeric, method=c("center", "scale"))
print(PreNum)
DFnorm <- predict(PreNum, DFnumeric)
dim(DFnorm)
str(DFnorm)
: preProcess 함수를 사용해서 변수변환에 필요한 parameter 설정
: predict 함수를 통해 실제로 값을 변화시킨다.
: preProcess(DATA, METHOD) ==> predict(preProcess, DATA)
: METHOD는 변환 방식 : "range", "BoxCox", c("center", "scale"), "pca“
: c(“center”, “scale”)은 z-score 정규화이다. 정규분포일 때 사용
: range는 [0,1] 범위로 값을 조정하는 것이다. min, max가 0과 1이 된다.
: BoxzCox는 한 쪽에 쏠려있을 때 종모양에 가깝게 조정해준다.

25. FACTOR형 변수들 더미화하기 (model_matrix(COLUMNS, DATA)) : 원 핫 인코딩
###8.3.2 One hot encoding the categorical variables
###factor형 변수들을 더미화 한다.
DFdummies <- as.data.frame(model.matrix(~.-1, DFfactors))
dim(DFdummies)
: COLUMNS에는 더미화하고자 하는 변수들을 모두 써준다.
26. DUMMY화 한 뒤, 빈도수 적은 LEVEL들은 버리자.
###8.3.3 Removing levels with few or no observations in train or test
###더미화 했을 때, train or test 에서 값이 0개인 level은 제거한다.
ZerocolTest <- which(colSums(DFdummies[(nrow(all[!is.na(all$SalePrice),])+1):nrow(all),])==0)
colnames(DFdummies[ZerocolTest])
DFdummies <- DFdummies[,-ZerocolTest] #removing predictors

ZerocolTrain <- which(colSums(DFdummies[1:nrow(all[!is.na(all$SalePrice),]),])==0)
colnames(DFdummies[ZerocolTrain])
DFdummies <- DFdummies[,-ZerocolTrain] #removing predictor

###더미화 했을 떄, 값이 10개 미만인 level들도 없애자.
fewOnes <- which(colSums(DFdummies[1:nrow(all[!is.na(all$SalePrice),]),])<10)
colnames(DFdummies[fewOnes])
DFdummies <- DFdummies[,-fewOnes] #removing predictors
dim(DFdummies)

27. CBIND
### 정규화한 numeric 변수들과 더미화한 factor 변수들을 합친다.
combined <- cbind(DFnorm, DFdummies)

28. SKEW 값과 Q-Q plot >> 너무 크면, 정규화를 해주자.(LOG)
##8.3 Dealing with Skewness of responcse variable
##skew값도 구해보고, Q-Qplot도 그려보자.
skew(all$SalePrice)
qqnorm(all$SalePrice)
qqline(all$SalePrice)
##1.87은 너무 높은 값...log값으로 정규화를 해주자
all$SalePrice <- log(all$SalePrice) #default is the natural logarithm, "+1" is not necessary as there are no 0's
skew(all$SalePrice)
qqnorm(all$SalePrice)
qqline(all$SalePrice)

29. LASSO REGRESSION MODEL
##9.1 Lasso Regression Model
set.seed(27042018)
##5-cross validation 한다는 건가?
my_control <- trainControl(method="cv", number=5)
lassoGrid <- expand.grid(alpha=1, lambda=seq(0.001,0.1, by=0.0005))
##lasso modeling
lasso_mod <- train(x=train1, y=all$SalePrice[!is.na(all$SalePrice)], method="glmnet",
                   trControl=my_control, tuneGrid=lassoGrid)

: set.seed(적절한 값)
: RF를 할 때 더 나은 파라미터를 찾기 위해 교차 검증을 사용한다.
: 다양한 조합에 대해 성능평가 조합은 expand.grid( )로 한다.
: trainControl(데이터 샘플링 기법:cv, N:몇 개)
: expand.grid(alpha설정, lambda값은 seq로 줌) : RF에서도 쓸 수 있다.
: train(DATA, 종속변수, method=“glmnet”, trControl, tuneGrid)로 훈련!!
: glmnet : alpha=0이면 RIDGE회귀, 1이면 LASOSO, 0.5면 ELASTICNET

##모델 결과 보기
lasso_mod$bestTune
min(lasso_mod$results$RMSE)

##Imp로 중요 변수 뽑아보기?
lassoVarImp <- varImp(lasso_mod, scale=F)
lassoImportance <- lassoVarImp$importance
varsSelected <- length(which(lassoImportance$Overall!=0))
varsNotSelected <- length(which(lassoImportance$Overall==0))
cat('Lasso uses', varsSelected, 'variables in its model, and did not select', varsNotSelected, 'variables.')
: bestTune 결과로 varImp를 추출한다.
: varImp$importance로 IMPORTANCE 추출 가능
: 0이 아닌 것과 0인 것이 있다.

30. PREDICT(MODEL, TESTDATA)
##Lasso로 predict 해보기
LassoPred <- predict(lasso_mod, test1)
predictions_lasso <- exp(LassoPred)
head(predictions_lasso)
: predict로 결과 값 구한다. log한 것이니까, exp로 변환해준다.
31. 앙상블 모델 (좀 더 공부 필요)
sub_avg <- data.frame(Id = test_labels, SalePrice = (predictions_XGB+2*predictions_lasso)/3)
head(sub_avg)
: 별거 없다. 적절한 공식으로 각 모델의 결과 값을 만들어준다.

32. write.csv(DATA, FILE_NAME)
write.csv(sub_avg, file = 'average.csv', row.names = F)
