

/Users/zhangkai/Documents/algorithm_trading/investments-fundamentals/coursera-dl

./coursera-dl -c ../mp4/cookies.txt -u 843889128@qq.com -p 123456  --clear-cache investments-fundamentals




###GBDT调优：
#Gradient Boosting框架提供了Tree Booster和Liner Booster两种基分类器，以及不同的正则化方式，优化指标等。


library("rjson")
library('Matrix')
library("pROC")
library("caTools")
library("xgboost")


setwd("/data/kai.zhang/model_merge/R_workspace")


train_raw <- read.csv('/data/kai.zhang/data0315.csv', sep='\t', header = TRUE,stringsAsFactors=F)
test_raw <- read.csv('/data/kai.zhang/data0316.csv', sep='\t', header = TRUE,stringsAsFactors=F)

train_raw <- read.csv('/data/kai.zhang/data0421.csv', sep='\t', header = TRUE,stringsAsFactors=F)
test_raw <- read.csv('/data/kai.zhang/data0422.csv', sep='\t', header = TRUE,stringsAsFactors=F)

train_raw <- read.csv('/data/kai.zhang/data0510.csv', sep='\t', header = TRUE,stringsAsFactors=F)
test_raw <- read.csv('/data/kai.zhang/data0511.csv', sep='\t', header = TRUE,stringsAsFactors=F)


feature_json <- '/data/qin.zhang/l2r/train_l2r/featuremap.json'
features <- fromJSON(paste(readLines(feature_json), collapse=""))
feature_selected <- names(features$features_selection)[which(features$features_selection == 1)]

selected_feature <- c("label",feature_selected,"impact_factor","confidence_ctr")

raw_train1 <- train_raw[,selected_feature]  
raw_test1 <- test_raw[,selected_feature]


for (i in 2:ncol(raw_train1)){
	if(class(raw_train1[,i])=="character") {
  		raw_train1[,i] <- as.numeric(raw_train1[,i])
  		raw_test1[,i] <- as.numeric(raw_test1[,i])
     }
}


raw_train1[is.na(raw_train1)] <- 0
raw_test1[is.na(raw_test1)] <- 0

str(raw_train1)
str(raw_test1)

summary(raw_train1)
summary(raw_test1)


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

save.image("new_param.RData")

ctrgbdt <- xgboost(data = data.train, label = data.train.click, max.depth = 3, eta = 0.3, nround = 300, objective = "binary:logistic", save_name="xgboost.model",eval_metric = "auc", weight=data.sample.weight, verbose = 1)

ctrgbdt1 <- xgboost(data = data.train[,-27], label = data.train.click, max.depth = 3, eta = 0.3, nround = 300, objective = "binary:logistic", save_name="xgboost.model",eval_metric = "auc", weight=data.sample.weight, verbose = 0)

ctrgbdt <- xgboost(data = data.train, label = data.train.click, max.depth = 5, eta = 0.14635311504826, nround = 143, objective = "binary:logistic", save_name="xgboost.model",eval_metric = "auc", weight=data.sample.weight, verbose = 0, gamma = 0.181607453757897)


##branch mark

pred.train <- predict(ctrgbdt1, data.train)
pred.test <- predict(ctrgbdt1, data.test)

#ctr auc
colAUC(pred.train, data.train.click)  #0.708807
colAUC(pred.test, data.test.click)  #0.707995

#cvr auc
colAUC(pred.train, data.train.order) #0.9492679
colAUC(pred.test, data.test.order)   #0.941635

#cvr auc clicked
pred.train.clicked <- predict(ctrgbdt1, data.train.clicked)
pred.test.clicked <- predict(ctrgbdt1, data.test.clicked)
colAUC(pred.train.clicked, data.train.clicked.order) #0.8735342
colAUC(pred.test.clicked, data.test.clicked.order) #0.8628289




##feature importance 
brenchImp<-xgb.importance(colnames(data.train), model = ctrgbdt)

#Testing whether the results make sense
#chisq.test(train$Age, output_vector)



########tuning#######


set.seed(1000)

#preparing matrix
dtrain <- xgb.DMatrix(data = data.train,label = data.train.click, weight=data.sample.weight)
dtest <- xgb.DMatrix(data = data.test,label=data.test.click)


#default parameters
params <- list(
        booster = "gbtree",
        objective = "binary:logistic",
        eta=0.3,
        gamma=0,
        max_depth=3,
        min_child_weight=1,
        subsample=1,
        colsample_bytree=1
)


xgbcv <- xgb.cv(params = params
                ,data = dtrain
                ,nrounds = 500
                ,nfold = 5
                ,showsd = T
                ,stratified = T
                ,print_every_n = 50
                ,early_stopping_rounds = 20
                ,maximize = T
                ,metrics="auc"
                #,weight=data.sample.weight
)
##best iteration = 


data.sample.weight <- sapply(raw_train1[,"impact_factor"], function(x) { ifelse(x == 2, 60, 1) } ) #click：1 , order: 60

tuning <- xgboost(data = data.train, label = data.train.click, max.depth = 3, eta = 0.3, nround = 300, objective = "binary:logistic", save_name="xgboost.model",eval_metric = "auc", weight=data.sample.weight, verbose = 0)

pred.train <- predict(tuning, data.train)
pred.test <- predict(tuning, data.test)

#ctr auc
colAUC(pred.train, data.train.click) 
colAUC(pred.test, data.test.click)  

#cvr auc
colAUC(pred.train, data.train.order) 
colAUC(pred.test, data.test.order)   

#cvr auc clicked
pred.train.clicked <- predict(tuning, data.train.clicked)
pred.test.clicked <- predict(tuning, data.test.clicked)
colAUC(pred.train.clicked, data.train.clicked.order) 
colAUC(pred.test.clicked, data.test.clicked.order) 

##feature importance 
brenchImp<-xgb.importance(colnames(data.train), model = tuning)



######### grid search

best_param = list()
best_seednumber = 1234
best_auc = -Inf
best_iteration = 0
zz <- file("file.txt","w")

for (iter in 1:100) {
    cvparam <- list(objective = "binary:logistic",
          eval_metric = "auc",
          max_depth = sample(2:6, 1),
          eta = runif(1, 0.1, 0.5),
          gamma = runif(1, 0.0, 0.2),
          subsample = runif(1, .6, 1),
          colsample_bytree = runif(1, .6, 1),
          min_child_weight = sample(1:40, 1),
          max_delta_step = sample(1:10, 1)
      	)
    seed.number = sample.int(10000, 1)[[1]]
    set.seed(seed.number)
    xgbcv <- xgb.cv(data=dtrain, params = cvparam,  
                    nfold=5, nrounds=300,
                    verbose = T, early.stop.round=8, maximize=T)

    max_auc = xgbcv$evaluation_log[xgbcv$best_iteration,test_auc_mean]

    if (max_auc > best_auc) {
        best_auc = max_auc
        best_iteration = xgbcv$best_iteration
        best_seednumber = seed.number
        best_param = cvparam
    }

	writeLines(paste("best_auc",best_auc,sep=": "),con=zz,sep="\n")
	writeLines(paste("best_iteration",best_iteration,sep=": "),con=zz,sep="\n")
	writeLines(paste("best_seednumber",best_seednumber,sep=": "),con=zz,sep="\n")
	writeLines(paste(names(best_param),best_param, collapse="  ",sep=":"),con=zz,sep="\n")
	writeLines("---------------",con=zz,sep="\n")
	flush(zz)
}

close(zz)


--
best_param <- list(objective = "binary:logistic",
          eval_metric = "auc",
          max_depth = 5,
          eta = 0.0479613534803502,
          gamma = 0.00635602558031678
      	)
best_iteration = 459

md <- xgb.train(data=dtrain, params=best_param, nrounds=best_iteration)


pred.train <- predict(md, data.train)
pred.test <- predict(md, data.test)

#ctr auc
colAUC(pred.train, data.train.click)  #0.708807
colAUC(pred.test, data.test.click)  #0.707995

#cvr auc
colAUC(pred.train, data.train.order) #0.9492679
colAUC(pred.test, data.test.order)   #0.941635

#cvr auc clicked
pred.train.clicked <- predict(md, data.train.clicked)
pred.test.clicked <- predict(md, data.test.clicked)
colAUC(pred.train.clicked, data.train.clicked.order) #0.8735342
colAUC(pred.test.clicked, data.test.clicked.order) #0.8628289



########save ctr model
ctrmodel <- xgb.dump(ctrgbdt, with_stats = F)
position <- which(!is.na(stri_match_first_regex(ctrmodel, "booster")))
ctrmodel[position] <- "\n"

for(i in 1:length(ctrmodel)){
  if(length(grep("f(\\d+)",ctrmodel[i])!=0)){
    replacex <- colnames(data.train)[as.numeric(unlist(strsplit(regmatches(ctrmodel[i], gregexpr("f(\\d+)", ctrmodel[i]), invert = F)[[1]],"f"))[2]) + 1]
    ctrmodel[i] <- sub("f(\\d+)",replacex, ctrmodel[i])
  }
}


xgb.importance(colnames(train_tmp), model = ctrgbdt)



ctr_model.r <-file("gbdt_model_5depth.r", "w")
writeLines(paste(ctrmodel,collapse="\t") , ctr_model.r)
close(ctr_model.r)

