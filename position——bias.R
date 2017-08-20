#消除pisition bias




#hive -f model_merge.sql -hiveconf cal_dt = '2017-03-08'

library("rjson")
library('Matrix')
library("pROC")
library("caTools")
library("glmnet")


setwd("/data/kai.zhang/model_merge/R_workspace")



train_raw <- read.csv('/data/kai.zhang/data0315.csv', sep='\t', header = TRUE)
test_raw <- read.csv('/data/kai.zhang/data0316.csv', sep='\t', header = TRUE)



train_raw[is.na(train_raw)]<-0
test_raw[is.na(test_raw)]<-0


ctr_selected_feature <- c("label","price_pref","discount","shop_pref_extend","isdealshop","distxctr",
"log_ceil_pictotal","guess_star","istopshop","realclick",
"main_cateadore","spl","topshop","isnewuser","distscore","ctr","pos")

ctr_trainData <- train_raw[,ctr_selected_feature]  #13531947
ctr_testData <- test_raw[,ctr_selected_feature]  #8910975



for (i in 1:ncol(ctr_trainData)){
	if(class(ctr_trainData[,i])=="factor") {
  		ctr_trainData[,i] <- as.numeric(as.character(ctr_trainData[,i]))
  		ctr_testData[,i] <- as.numeric(as.character(ctr_testData[,i]))
     }
}

ctr_trainData[is.na(ctr_trainData)] <- 0
ctr_testData[is.na(ctr_testData)] <- 0

ctr_trainData[,"logpos"] <- log2(ctr_trainData[,"pos"]+1) + 1

str(ctr_trainData)
str(ctr_testData)

summary(ctr_trainData)
summary(ctr_testData)





ctr_trainData[ctr_trainData[,"realclick"] < 0,"realclick"] <- 0 
ctr_testData[ctr_testData[,"realclick"] < 0,"realclick"] <- 0


shoppower.dummies <- dummy(ctr_trainData[,"shoppower"], sep="_", drop=TRUE)
shoppower.dummies.test <- dummy(ctr_testData[,"shoppower"], sep="_", drop=TRUE)

colnames(shoppower.dummies.test) <- c("ctr_trianData_20","ctr_trianData_30","ctr_trianData_35","ctr_trianData_40","ctr_trianData_45","ctr_trianData_50")

#xfactors <- model.matrix( ~ gender + m_edu + p_edu + f_color)[, -1]

ctr_trainData <- cbind(ctr_trainData,shoppower.dummies[,-which("ctr_trainData_0" %in% colnames(shoppower.dummies))])
ctr_testData <- cbind(ctr_testData,shoppower.dummies.test)





sx <- Matrix(data.matrix(ctr_trainData[,-which(colnames(ctr_trainData) %in% c("label","pos"))]),sparse = TRUE)

ctr_testData[,"logpos"] <- 0
stsx <- Matrix(data.matrix(ctr_testData[,-which(colnames(ctr_testData) %in% c("label","pos"))]),sparse = TRUE)


for(i in seq(40,60,by=10)){
  i <- 40
  ctr_lasso <- glmnet(sx, ctr_trainData[,"label"], family=c("binomial"),weights=ifelse(ctr_trainData[,"label"]==1,i,1), alpha = 1)
  trainCtr <- predict(ctr_lasso,sx,s=0.01,type="response")

  dup <- ctr_trainData
  dup[,"pos"] <- NULL
  dup[,"logpos"] <- 0
  withoutpos <- Matrix(data.matrix(dup[,-1]),sparse = TRUE)

  trainCtr_withoutpos <- predict(ctr_lasso,withoutpos,s=0.01,type="response")

  colAUC(trainCtr, ctr_trainData[,"label"])  # 0.6909906
  colAUC(trainCtr_withoutpos, ctr_trainData[,"label"])    

  test_without <- predict(ctr_lasso,stsx,s=0.01,type="response")
  colAUC(test_without, ctr_testData[,"label"])    #  0.6819078



  print(i)
  print(trainauc)
  print(testauc)
  print("-------")
}


coeff <- coef(ctr_lasso,s=0.01)
coeff[which(coeff!=0),]



##########





shoppower.dummies <- dummy(ctr_trainData[,"pos"], sep="_", drop=TRUE)
shoppower.dummies.test <- dummy(ctr_testData[,"pos"], sep="_", drop=TRUE)


#xfactors <- model.matrix( ~ gender + m_edu + p_edu + f_color)[, -1]


colnames(shoppower.dummies.test) <- c("ctr_trianData_20","ctr_trianData_30","ctr_trianData_35","ctr_trianData_40","ctr_trianData_45","ctr_trianData_50")

ctr_trainData <- cbind(ctr_trainData,shoppower.dummies[,-which("ctr_trainData_0" %in% colnames(shoppower.dummies))])
ctr_testData <- cbind(ctr_testData,shoppower.dummies.test)





sx <- Matrix(data.matrix(ctr_trainData[,-which(colnames(ctr_trainData) %in% c("label","pos"))]),sparse = TRUE)

ctr_testData[,"logpos"] <- 0
stsx <- Matrix(data.matrix(ctr_testData[,-which(colnames(ctr_testData) %in% c("label","pos"))]),sparse = TRUE)


for(i in seq(40,60,by=10)){
  i <- 40
  ctr_lasso <- glmnet(sx, ctr_trainData[,"label"], family=c("binomial"),weights=ifelse(ctr_trainData[,"label"]==1,i,1), alpha = 1)
  trainCtr <- predict(ctr_lasso,sx,s=0.01,type="response")

  dup <- ctr_trainData
  dup[,"pos"] <- NULL
  dup[,"logpos"] <- 0
  withoutpos <- Matrix(data.matrix(dup[,-1]),sparse = TRUE)

  trainCtr_withoutpos <- predict(ctr_lasso,withoutpos,s=0.01,type="response")

  colAUC(trainCtr, ctr_trainData[,"label"])  # 0.6909906
  colAUC(trainCtr_withoutpos, ctr_trainData[,"label"])    

  test_without <- predict(ctr_lasso,stsx,s=0.01,type="response")
  colAUC(test_without, ctr_testData[,"label"])    #  0.6819078



  print(i)
  print(trainauc)
  print(testauc)
  print("-------")
}


coeff <- coef(ctr_lasso,s=0.01)




####加上位置信息

library("xgboost")
library("rjson")
library("stringi")
library("caTools")


feature_json <- '/data/qin.zhang/l2r/train_l2r/featuremap.json'
features <- fromJSON(paste(readLines(feature_json), collapse=""))
feature_selected <- names(features$features_selection)[which(features$features_selection == 1)]


train_raw <- read.csv('/data/kai.zhang/data0315.csv', sep='\t', header = TRUE)
test_raw <- read.csv('/data/kai.zhang/data0316.csv', sep='\t', header = TRUE)





train_raw[is.na(train_raw)]<-0
test_raw[is.na(test_raw)]<-0


selected_feature <- c("label",feature_selected,"pos","impact_factor")

ctr_trainData <- train_raw[,selected_feature]  #13531947
ctr_testData <- test_raw[,selected_feature]  



for (i in 1:ncol(ctr_trainData)){
	if(class(ctr_trainData[,i])=="factor") {
  		ctr_trainData[,i] <- as.numeric(as.character(ctr_trainData[,i]))
  		ctr_testData[,i] <- as.numeric(as.character(ctr_testData[,i]))
     }
}

ctr_trainData[is.na(ctr_trainData)] <- 0
ctr_testData[is.na(ctr_testData)] <- 0

str(ctr_trainData)
str(ctr_testData)

summary(ctr_trainData)
summary(ctr_testData)

raw_train1 <- ctr_trainData
raw_test1 <- ctr_testData

####模型gbdt ctr
remove_col <- c("label","impact_factor")


data.train <- data.matrix(raw_train1[,-which(colnames(raw_train1) %in% remove_col)])
data.train.click <- raw_train1[,"label"]
data.train.order <- ifelse(raw_train1[,"impact_factor"] == 2, 1, 0)

data.test <- data.matrix(raw_test1[,-which(colnames(raw_test1) %in% remove_col)])
data.test.click <- raw_test1[,"label"]
data.test.order <- ifelse(raw_test1[,"impact_factor"] == 2, 1, 0)



data.train.clicked <- data.matrix(raw_train1[ raw_train1[,"label"] == 1 ,-which(colnames(raw_train1) %in% remove_col)])
data.train.clicked.order <- ifelse(raw_train1[ raw_train1[,"label"] == 1 ,"impact_factor"] == 2, 1, 0)
data.test.clicked <- data.matrix(raw_test1[ raw_test1[,"label"] == 1 ,-which(colnames(raw_test1) %in% remove_col)])
data.test.clicked.order <- ifelse(raw_test1[ raw_test1[,"label"] == 1 ,"impact_factor"] == 2, 1, 0)


data.sample.weight <- sapply(raw_train1[,"impact_factor"], function(x) { ifelse(x == 2, 60, 1) } ) #click：1 , order: 60

ctrgbdt <- xgboost(data = data.train, label = data.train.click, max.depth = 5, eta = 0.3, nround = 200, objective = "binary:logistic", save_name="xgboost.model",eval_metric = "auc", weight=data.sample.weight, verbose = 1)


pred.train <- predict(ctrgbdt, data.train)
pred.test <- predict(ctrgbdt, data.test)

#ctr auc
colAUC(pred.train, data.train.click)  #0.7242772
colAUC(pred.test, data.test.click)  #0.7217832

#cvr auc
colAUC(pred.train, data.train.order) #0.9454165
colAUC(pred.test, data.test.order)   #0.9318534

#cvr auc clicked
pred.train.clicked <- predict(ctrgbdt, data.train.clicked)
pred.test.clicked <- predict(ctrgbdt, data.test.clicked)
colAUC(pred.train.clicked, data.train.clicked.order) #0.8590254
colAUC(pred.test.clicked, data.test.clicked.order) #0.8403243

data.train[,"pos"] <- 10
data.test[,"pos"] <- 10
data.train.clicked[,"pos"] <- 10
data.test.clicked[,"pos"] <- 10



##feature importance 
xgb.importance(colnames(data.train), model = ctrgbdt)

#Testing whether the results make sense
#chisq.test(train$Age, output_vector)



#save ctr model
ctrmodel <- xgb.dump(ctrgbdt, with_stats = F)
position <- which(!is.na(stri_match_first_regex(ctrmodel, "booster")))
ctrmodel[position] <- "\n"

for(i in 1:length(ctrmodel)){
  if(length(grep("f(\\d+)",ctrmodel[i])!=0)){
    replacex <- colnames(data.train)[as.numeric(unlist(strsplit(regmatches(ctrmodel[i], gregexpr("f(\\d+)", ctrmodel[i]), invert = F)[[1]],"f"))[2]) + 1]
    ctrmodel[i] <- sub("f(\\d+)",replacex, ctrmodel[i])
  }
}

ctr_model.r <-file("ctr_model.r", "w")
writeLines(paste(ctrmodel,collapse="\t") , ctr_model.r)
close(ctr_model.r)

