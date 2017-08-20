###特征离散化
#https://www.qcloud.com/community/article/701728
#https://www.zhihu.com/question/34819617


https://www.zhihu.com/question/34819617

train = read.csv("/data/kai.zhang/data0517.csv", sep='\t')
test = read.csv("/data/kai.zhang/data0518.csv", sep='\t')

index <- c("label","impact_factor","cityid","iphone")
train <- train[,index]
test <- test[,index]

library("glmnet")
library("Matrix")
library("pROC")
library('caTools')

str(train)
train[,"iphone"] <- as.numeric(as.character(train[,"iphone"]))

str(test)

train[is.na(train)] <- 0
test[is.na(test)] <- 0

train[,"cityid"] <- as.factor(train[,"cityid"])
train[,"iphone"] <- as.factor(train[,"iphone"])
test[,"cityid"] <- as.factor(test[,"cityid"])
test[,"iphone"] <- as.factor(test[,"iphone"])


train_data <- sparse.model.matrix(~ -1 + cityid + iphone, data=train[,-c(1,2)])
colnames(train_data)

test_data <- sparse.model.matrix(~ -1 + cityid + iphone, data=test[,-c(1,2)])

remove1 <- setdiff(colnames(train_data),colnames(test_data))
remove2 <- setdiff(colnames(test_data),colnames(train_data))

train_data <- train_data[,-which(colnames(train_data) %in% remove1)]
test_data <- test_data[,-which(colnames(test_data) %in% remove2)]


data.sample.weight <- sapply(train[,"impact_factor"], function(x) { ifelse(x == 2, 10, 1) } ) 

fit <- glmnet(train_data,train[,"label"],family=c("binomial"),weights=data.sample.weight, alpha = 1)

trainY <- predict(fit,train_data,s=0.001,type="response")
testY <- predict(fit,test_data,s=0.001,type="response")

colAUC(trainY, as.factor(train[,"label"]))  # 0.7003952
colAUC(testY, as.factor(test[,"label"]))    #  0.692684


coeff <- coef(fit,s=0.001)
coeff[which(coeff!=0),]
df_coef <- data.frame(t(as.matrix(coeff[which(coeff!=0),])))



-------iphone\cityid交叉
train[,"cityid_iphone"] = paste(train[,"cityid"],train[,"iphone"],sep="_")
train[,"cityid_iphone"] <- as.factor(train[,"cityid_iphone"])

test[,"cityid_iphone"] = paste(test[,"cityid"],test[,"iphone"],sep="_")
test[,"cityid_iphone"] <- as.factor(test[,"cityid_iphone"])


train_data <- sparse.model.matrix(~ -1 + cityid_iphone, data=train)

test_data <- sparse.model.matrix(~ -1 + cityid_iphone, data=test)

remove1 <- setdiff(colnames(train_data),colnames(test_data))
remove2 <- setdiff(colnames(test_data),colnames(train_data))



train_data <- train_data[,-which(colnames(train_data) %in% remove1)]
test_data <- test_data[,-which(colnames(test_data) %in% remove2)]


data.sample.weight <- sapply(train[,"impact_factor"], function(x) { ifelse(x == 2, 10, 1) } ) 

fit <- glmnet(train_data,train[,"label"],family=c("binomial"),weights=data.sample.weight, alpha = 1)

trainY <- predict(fit,train_data,s=0.001,type="response")
testY <- predict(fit,test_data,s=0.001,type="response")

colAUC(trainY, as.factor(train[,"label"]))  # 0.7003952
colAUC(testY, as.factor(test[,"label"]))    #  0.692684


coeff <- coef(fit,s=0.001)
coeff[which(coeff!=0),]
df_coef <- data.frame(t(as.matrix(coeff[which(coeff!=0),])))

----gbdt


library("rjson")
library('Matrix')
library("pROC")
library("caTools")
library("xgboost")


data.sample.weight <- sapply(train[,"impact_factor"], function(x) { ifelse(x == 2, 60, 1) } ) 

ctrgbdt <- xgboost(data = train_data, label = train[,"label"], max.depth = 3, eta = 0.3, nround = 300, objective = "binary:logistic", save_name="xgboost.model",eval_metric = "auc", weight=data.sample.weight, verbose = 1)

pred.train <- predict(ctrgbdt, train_data)
pred.test <- predict(ctrgbdt, test_data)

#ctr auc
colAUC(pred.train, train[,"label"])  
colAUC(pred.test, test[,"label"])  

##feature importance 
brenchImp<-xgb.importance(colnames(train_data), model = ctrgbdt)

------

ctrgbdt <- xgboost(data = train_data, label = train[,"label"], max.depth = 3, eta = 0.3, nround = 300, objective = "binary:logistic", save_name="xgboost.model",eval_metric = "auc", weight=data.sample.weight, verbose = 1)

pred.train <- predict(ctrgbdt, train_data)
pred.test <- predict(ctrgbdt, test_data)

#ctr auc
colAUC(pred.train, train[,"label"])  
colAUC(pred.test, test[,"label"])  

##feature importance 
brenchImp<-xgb.importance(colnames(train_data), model = ctrgbdt)



