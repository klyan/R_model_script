
# Get the feature real names
names <- dimnames(data.matrix(X[,-1]))[[2]]

# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = xgb)
# Nice graph
xgb.plot.importance(importance_matrix[1:10,])

#In case last step does not work for you because of a version issue, you can try following :
barplot(importance_matrix[,1])

#Testing whether the results make sense

test <- chisq.test(train$Age, output_vector)
print(test)

关于NDCG作为优化指标时

library("xgboost")
library("rjson")
library("stringi")
library("caTools")

setwd("/data/kai.zhang/")

csvdata<- read.csv("/data/appjobs/mlplatform/FoodCatGBDTSeq7/20170626142229/data/20170626142229.csv",sep='\t')

csvdata[,"hp_cal_dt"] <- as.Date(csvdata[,"hp_cal_dt"])

for(i in 5:ncol(csvdata)){
  if(class(csvdata[,i]) == "factor"){
    csvdata[,i] <- as.numeric(as.character(csvdata[,i]))
  }
}

summary(csvdata)

csvdata[is.na(csvdata)] <- 0


save.image(file="raw_data_gbdt.RData")

load("raw_data_gbdt.RData")


#feature_json <- '/data/qin.zhang/l2r/train_l2r/featuremap.json'
feature_json <- '/data/kai.zhang/l2r/featuremap.json'
features <- fromJSON(paste(readLines(feature_json), collapse=""))
feature_selected <- names(features$features_selection)[which(features$features_selection == 1)]
remove<-c("haspic","wifi","fourg","confidence_ctr","weathergrade")
feature_selected <- feature_selected[!feature_selected %in% remove]
feature_selected <- c(feature_selected,"label","impact_factor","istakeawayshop")

date_index <- csvdata[,"hp_cal_dt"] == as.Date("2017-06-21")
table(date_index)


raw_train <- csvdata[date_index,feature_selected]
raw_test <- csvdata[!date_index,feature_selected]

#feature_selected %in% colnames(csvdata)

####模型gbdt ctr
remove_col <- c("query_id","shop_id","label","impact_factor")

raw_train[is.na(raw_train)] <- 0
raw_test[is.na(raw_test)] <- 0


data.train <- data.matrix(raw_train[,-which(colnames(raw_train) %in% remove_col)])
data.train.click <- raw_train[,"label"]
data.train.order <- ifelse(raw_train[,"impact_factor"] == 2, 1, 0)

data.test <- data.matrix(raw_test[,-which(colnames(raw_test) %in% remove_col)])
data.test.click <- raw_test[,"label"]
data.test.order <- ifelse(raw_test[,"impact_factor"] == 2, 1, 0)


data.train.clicked <- data.matrix(raw_train[ raw_train[,"label"] == 1 ,-which(colnames(raw_train) %in% remove_col)])
data.train.clicked.order <- ifelse(raw_train[ raw_train[,"label"] == 1 ,"impact_factor"] == 2, 1, 0)
data.test.clicked <- data.matrix(raw_test[ raw_test[,"label"] == 1 ,-which(colnames(raw_test) %in% remove_col)])
data.test.clicked.order <- ifelse(raw_test[ raw_test[,"label"] == 1 ,"impact_factor"] == 2, 1, 0)


#dtrain <- xgb.DMatrix(data = train$data, label = train$label)

data.sample.weight <- sapply(raw_train[,"impact_factor"], function(x) { ifelse(x == 2, 1500, ifelse(x==1,100,1)) } ) #click：60 , order: 1500

data.sample.weight <- sapply( data.train.click, function(x) {ifelse(x==1,100,1)}) 
table(data.sample.weight)
table(data.train.click)

ctrgbdt <- xgboost(data = data.train, label = data.train.click, max.depth = 3, nround = 300, objective = "binary:logistic", save_name="ctr.xgboost.model",eval_metric = "auc", weight=data.sample.weight, verbose = 0, tree_method= 'exact')

#ctrgbdt <- xgboost(data = data.train, label = data.train.click, max.depth = 5, eta = 0.14635311504826, nround = 143, objective = "binary:logistic", save_name="ctr.xgboost.model",eval_metric = "auc", weight=data.sample.weight, verbose = 0, gamma = 0.181607453757897)

pred.train <- predict(ctrgbdt, data.train)
pred.test <- predict(ctrgbdt, data.test)

#ctr auc
colAUC(pred.train, data.train.click)  #0.7231734
colAUC(pred.test, data.test.click)  #0.7209306

#cvr auc
colAUC(pred.train, data.train.order) #0.9454165
colAUC(pred.test, data.test.order)   #0.9318534  0.8938155

#cvr auc clicked
pred.train.clicked <- predict(ctrgbdt, data.train.clicked)
pred.test.clicked <- predict(ctrgbdt, data.test.clicked)

colAUC(pred.train.clicked, data.train.clicked.order) #
colAUC(pred.test.clicked, data.test.clicked.order) #



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



##############cvr模型
#gbdt cvr

data.sample.weight <- sapply(raw_train[,"impact_factor"], function(x) { ifelse(x == 2, 1500, ifelse(x==1, 0.001,0.1)) }) #click：0.01 , order: 100
data.sample.weight <- raw_train[,"impact_factor"] == 2, 1500, ifelse(x==1, 0.001,0.1)) }) #click：0.01 , order: 100

data.sample.weight <- sapply( data.train.order, function(x) {ifelse(x==2,500,1)}) 

cvrgbdt <- xgboost(data = data.train, label = data.train.order, max.depth = 3, nround = 300, objective = "binary:logistic", save_name="cvr.xgboost.model",eval_metric = "auc", weight=data.sample.weight,silent = 1)


#cvrgbdt <- xgboost(data = data.train, label = data.train.order, max.depth = 5, eta = 0.14635311504826, nround = 143,objective = "binary:logistic", save_name="cvr.xgboost.model",eval_metric = "auc", weight=data.sample.weight, gamma = 0.181607453757897, silent = 1)

#####预测
pred.cvr.train <- predict(cvrgbdt, data.train)
pred.cvr.test <- predict(cvrgbdt, data.test)

#ctr auc
colAUC(pred.cvr.train, data.train.click)  #0.6361945
colAUC(pred.cvr.test, data.test.click)    #0.6274463

#cvr auc
colAUC(pred.train, data.train.order)     #0.9454165
colAUC(pred.test, data.test.order)       #0.9318534

#cvr auc clicked
pred.cvr.train.clicked <- predict(cvrgbdt, data.train.clicked)
pred.cvr.test.clicked <- predict(cvrgbdt, data.test.clicked)
colAUC(pred.cvr.train.clicked, data.train.clicked.order)     #0.9166138
colAUC(pred.cvr.test.clicked, data.test.clicked.order)      #0.8929802


##对非合作商户的购买补全

#用户最后一位点击的商户之前若点击的非合作商户

#为什么点击样本加权后，所有指标都会跌

#点击的合作商户，有多少比例购买 -> 要填多少比例的非合作商户的购买标签，同时这部分样本该加权，其他点击样本该降权？

#table(raw_train[((raw_train[,"ishuishop"] == 1) | (raw_train[,"isdealshop"]== 1)) & (raw_train[,"label"] == 1),"impact_factor"])



data.train.order.extend <- data.train.order
extend_index <-(raw_train[,"ishuishop"] == 0) & (raw_train[,"isdealshop"]== 0) & (raw_train[,"impact_factor"] == 1) & (raw_train[,"shoppower"]>=30)
data.train.order.extend[extend_index] <- 1

#购买样本
data.sample.weight.extend <- ifelse(raw_train[,"impact_factor"] == 2, 100, 1) #order: 100

#非合作，点击，扩充的购买样本
data.sample.weight.extend[extend_index] <- 0.1    #0.1

#合作商户的点击，非购买样本
cor_click <- ((raw_train[,"ishuishop"] == 1) | (raw_train[,"isdealshop"]== 1)) & (raw_train[,"impact_factor"] == 1)
data.sample.weight.extend[cor_click] <- 0.01   #0.01



cvrgbdt.extend <- xgboost(data = data.train, label = data.train.order.extend, max.depth = 5, eta = 0.3, nround = 100,
                   objective = "binary:logistic", save_name="cvr.xgboost.model",eval_metric = "auc", weight=data.sample.weight.extend)


#####预测
pred.cvr.train.extend <- predict(cvrgbdt.extend, data.train)
pred.cvr.test.extend <- predict(cvrgbdt.extend, data.test)

#ctr auc
colAUC(pred.cvr.train.extend, data.train.click)  #0.6544305
colAUC(pred.cvr.test.extend, data.test.click)    #0.6463837

#cvr auc
colAUC(pred.cvr.train.extend, data.train.order)     #0.9658179
colAUC(pred.cvr.test.extend, data.test.order)       #0.9482444

#cvr auc clicked
pred.cvr.train.clicked <- predict(cvrgbdt.extend, data.train.clicked)
pred.cvr.test.clicked <- predict(cvrgbdt.extend, data.test.clicked)
colAUC(pred.cvr.train.clicked, data.train.clicked.order)     #0.914572
colAUC(pred.cvr.test.clicked, data.test.clicked.order)      #0.889554



> #ctr auc
> colAUC(pred.cvr.train.extend, data.train.click)  #0.6361945
             [,1]
0 vs. 1 0.6544305
> colAUC(pred.cvr.test.extend, data.test.click)    #0.6274463
             [,1]
0 vs. 1 0.6463837
>
> #cvr auc
> colAUC(pred.cvr.train.extend, data.train.order)     #0.9454165
             [,1]
0 vs. 1 0.9658179
> colAUC(pred.cvr.test.extend, data.test.order)       #0.9318534
             [,1]
0 vs. 1 0.9482444
>
> #cvr auc clicked
> pred.cvr.train.clicked <- predict(cvrgbdt.extend, data.train.clicked)
> pred.cvr.test.clicked <- predict(cvrgbdt.extend, data.test.clicked)
> colAUC(pred.cvr.train.clicked, data.train.clicked.order)     #0.9166138
            [,1]
0 vs. 1 0.914572
> colAUC(pred.cvr.test.clicked, data.test.clicked.order)      #0.8929802
            [,1]
0 vs. 1 0.889554


##feature importance 
xgb.importance(colnames(data.train), model = ctrgbdt)

#Testing whether the results make sense
#chisq.test(train$Age, output_vector)



#save ctr model
cvrmodel <- xgb.dump(cvrgbdt.extend, with_stats = F)
position <- which(!is.na(stri_match_first_regex(cvrmodel, "booster")))
cvrmodel[position] <- "\n"

for(i in 1:length(cvrmodel)){
  if(length(grep("f(\\d+)",cvrmodel[i])!=0)){
    replacex <- colnames(data.train)[as.numeric(unlist(strsplit(regmatches(cvrmodel[i], gregexpr("f(\\d+)", cvrmodel[i]), invert = F)[[1]],"f"))[2]) + 1]
    cvrmodel[i] <- sub("f(\\d+)",replacex, cvrmodel[i])
  }
}

f_cvrmodel <-file("cvr_model.r", "w")
writeLines(paste(cvrmodel,collapse="\t") , f_cvrmodel)
close(f_cvrmodel)



####模型融合###

xgb.plot.tree(model =


#for (i in seq(from=0.1,to=0.9,by=0.1)) {
for (i in c(0.5)) {
  print(paste("ctr weight: ", i, sep=" ")) 
  mergeTr <- i * pred.train + (1-i)*pred.cvr.train 
  mergeTs <- i * pred.test + (1-i)*pred.cvr.test
  #mergeTr <- i * pred.train + (1-i)*pred.cvr.train.extend 
  #mergeTs <- i * pred.test + (1-i)*pred.cvr.test.extend

  print("ctr train & test auc:: ")
  print(colAUC(mergeTr, data.train.click)) 
  print(colAUC(mergeTs, data.test.click))
  print("cvr train & test auc:: ")
  print(colAUC(mergeTr, data.train.order)) 
  print(colAUC(mergeTs, data.test.order))

  print("cvr click train & test auc:: ")
  mergeTr.clicked <- (1-i) * pred.cvr.train.clicked + i * pred.train.clicked
  mergeTs.clicked <- (1-i) * pred.cvr.test.clicked + i * pred.test.clicked
  print(colAUC(mergeTr.clicked, data.train.clicked.order)) 
  print(colAUC(mergeTs.clicked, data.test.clicked.order))  

  print("-------------")
}






[1] "ctr weight:  0.5"
[1] "ctr train & test auc:: "
             [,1]
0 vs. 1 0.7149658
             [,1]
0 vs. 1 0.7116498
[1] "cvr train & test auc:: "
             [,1]
0 vs. 1 0.9579348
             [,1]
0 vs. 1 0.9422038
[1] "cvr click train & test auc:: "
             [,1]
0 vs. 1 0.8982351
             [,1]
0 vs. 1 0.8732883
[1] "-------------"


###60click

1] "ctr weight:  0.5"
[1] "ctr train & test auc:: "
             [,1]
0 vs. 1 0.7148381
           [,1]
0 vs. 1 0.71171
[1] "cvr train & test auc:: "
             [,1]
0 vs. 1 0.9590643
             [,1]
0 vs. 1 0.9422709
[1] "cvr click train & test auc:: "
             [,1]
0 vs. 1 0.8988219
             [,1]
0 vs. 1 0.8725743
[1] "-------------"


###single##
gbdt auc for training datas: 0.703793380927
gbdt auc score: 0.702656050909
auc of train sale 0.947877761225
auc of test sale 0.86076499056
