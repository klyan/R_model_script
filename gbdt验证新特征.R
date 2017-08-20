##gbdt验证新特征
#将这些特征将入原始特征中看auc,输出feature_importance


library("rjson")
library('Matrix')
library("pROC")
library("caTools")
library("xgboost")
library("stringi")

setwd("/data/kai.zhang/gbdt/")



train_raw <- read.csv('/data/qin.zhang/l2r/train_l2r/dataset/samples/2017-04-11_samples.csv_coor', sep='\t', header = TRUE,stringsAsFactors=F)
test_raw <- read.csv('/data/qin.zhang/l2r/train_l2r/dataset/samples/2017-04-12_samples.csv_coor', sep='\t', header = TRUE,stringsAsFactors=F)


feature_json <- '/data/qin.zhang/l2r/train_l2r/featuremap.json'
features <- fromJSON(paste(readLines(feature_json), collapse=""))
feature_selected <- names(features$features_selection)[which(features$features_selection == 1)]
selected_feature <- c("label","shop_id",feature_selected,"impact_factor")

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

setwd("/data/kai.zhang/gbdt/")
save.image(file="Raw0411_0412.RData")

load("Raw0411_0412.RData")

########加入特征，点评率#######

train_reply <- read.csv('/data/kai.zhang/gbdt/review11.csv.tmp', sep='\t', header = TRUE,stringsAsFactors=F)
test_reply <- read.csv('/data/kai.zhang/gbdt/review.csv.tmp', sep='\t', header = TRUE,stringsAsFactors=F)

colnames(train_reply) <- substr(colnames(train_reply),15,1000)
colnames(test_reply) <- substr(colnames(test_reply),12,1000)

nrow(train_reply) == length(unique(train_reply[,"shopid"]))
nrow(test_reply) == length(unique(test_reply[,"shopid"]))

add_features <- c("shopid","avg_review_len","diff_userid_per","replies_per","prefect_reviews_per","badreview_per","goodreview_per")
backup_feature<- c("shopid","badreview","goodreview","replies","dis_userids","userids","total_len","prefect_reviews","total_review")

add_features <- c("shopid","avg_review_len2","diff_userid_per2","replies_per2","prefect_reviews_per2","badreview_per2","goodreview_per2")


case when total_review != 0 then round(total_len/total_review,2) else 0.0 end as avg_review_len,
       case when total_review != 0 then round(dis_userids/userids,2) else 1.0 end as diff_userid_per,
       case when total_review != 0 then round(replies/total_review,2) else 0.0 end as replies_per,
       case when total_review != 0 then round(prefect_reviews/total_review,2) else 0.0 end as prefect_reviews_per,
       case when total_review != 0 then round(badreview/total_review,2) else 0.0 end as badreview_per,
       case when total_review != 0 then round(goodreview/total_review,2) else 0.0 end as goodreview_per


train_reply[,"avg_review_len2"] <-  (train_reply[,"total_len"]/train_reply[,"total_review"] + 1.96^2/2/train_reply[,"total_review"])/(1+1.96^2/train_reply[,"total_review"])
test_reply[,"avg_review_len2"] <-  (test_reply[,"total_len"]/test_reply[,"total_review"] + 1.96^2/2/test_reply[,"total_review"])/(1+1.96^2/test_reply[,"total_review"])

train_reply[,"diff_userid_per2"] <- (train_reply[,"dis_userids"]/train_reply[,"userids"] + 1.96^2/2/train_reply[,"userids"])/(1+1.96^2/train_reply[,"userids"])
test_reply[,"diff_userid_per2"] <- (test_reply[,"dis_userids"]/test_reply[,"userids"] + 1.96^2/2/test_reply[,"userids"])/(1+1.96^2/test_reply[,"userids"])

train_reply[,"replies_per2"] <- (train_reply[,"replies"]/train_reply[,"total_review"] + 1.96^2/2/train_reply[,"total_review"])/(1+1.96^2/train_reply[,"total_review"])
test_reply[,"replies_per2"] <- (test_reply[,"replies"]/test_reply[,"total_review"] + 1.96^2/2/test_reply[,"total_review"])/(1+1.96^2/test_reply[,"total_review"])

train_reply[,"prefect_reviews_per2"] <- (train_reply[,"prefect_reviews"]/train_reply[,"total_review"] + 1.96^2/2/train_reply[,"total_review"])/(1+1.96^2/train_reply[,"total_review"])
test_reply[,"prefect_reviews_per2"] <- (test_reply[,"prefect_reviews"]/test_reply[,"total_review"] + 1.96^2/2/test_reply[,"total_review"])/(1+1.96^2/test_reply[,"total_review"])

train_reply[,"badreview_per2"] <- (train_reply[,"badreview"]/train_reply[,"total_review"] + 1.96^2/2/train_reply[,"total_review"])/(1+1.96^2/train_reply[,"total_review"])
test_reply[,"badreview_per2"] <- (test_reply[,"badreview"]/test_reply[,"total_review"] + 1.96^2/2/test_reply[,"total_review"])/(1+1.96^2/test_reply[,"total_review"])

train_reply[,"goodreview_per2"] <- (train_reply[,"goodreview"]/train_reply[,"total_review"] + 1.96^2/2/train_reply[,"total_review"])/(1+1.96^2/train_reply[,"total_review"])
test_reply[,"goodreview_per2"] <- (test_reply[,"goodreview"]/test_reply[,"total_review"] + 1.96^2/2/test_reply[,"total_review"])/(1+1.96^2/test_reply[,"total_review"])


raw_train1 <- merge(raw_train1, train_reply[,add_features], by.x = "shop_id", by.y = "shopid",sort=F,all.x = TRUE)
raw_test1 <- merge(raw_test1, test_reply[,add_features], by.x = "shop_id", by.y = "shopid",sort=F,all.x = TRUE)

remain_index <- which(!colnames(raw_train1) %in% c("avg_review_len","diff_userid_per","replies_per","prefect_reviews_per","badreview_per","goodreview_per"))
raw_train1 <- merge(raw_train1[,remain_index], train_reply[,backup_feature], by.x = "shop_id", by.y = "shopid",sort=F,all.x = TRUE)
raw_test1 <- merge(raw_test1[,remain_index], test_reply[,backup_feature], by.x = "shop_id", by.y = "shopid",sort=F,all.x = TRUE)

###购买回头率
train_returnback <- read.csv('/data/kai.zhang/new_feature/returnback_customer_0411.csv', sep='\t', header = TRUE,stringsAsFactors=F)
test_returnback <- read.csv('/data/kai.zhang/new_feature/returnback_customer_0412.csv', sep='\t', header = TRUE,stringsAsFactors=F)

returnback_feature <- c("dp_shop_id","return_cm3","return_cm7","return_cm14","return_cm30")

colnames(train_returnback)[1:9] <- substr(colnames(train_returnback)[1:9],4,1000)
colnames(test_returnback)[1:9] <- substr(colnames(test_returnback)[1:9],4,1000)

nrow(train_returnback) == length(unique(train_returnback[,"dp_shop_id"]))
nrow(test_returnback) == length(unique(test_returnback[,"dp_shop_id"]))

raw_train1 <- merge(raw_train1, train_returnback[,returnback_feature], by.x = "shop_id", by.y = "dp_shop_id", sort=F, all.x = TRUE)
raw_test1 <- merge(raw_test1, test_returnback[,returnback_feature], by.x = "shop_id", by.y = "dp_shop_id",sort=F, all.x = TRUE)

----
###新客，商户成立距今的时间
train_newpoi <- read.csv('/data/kai.zhang/new_feature/new_poi0411.csv', sep='\t', header = TRUE,stringsAsFactors=F,colClasses=c("numeric","NULL","numeric","numeric","numeric"))
test_newpoi <- read.csv('/data/kai.zhang/new_feature/new_poi0412.csv', sep='\t', header = TRUE,stringsAsFactors=F,colClasses=c("numeric","NULL","numeric","numeric","numeric"))

dim(raw_train1)

raw_train1 <- merge(raw_train1, train_newpoi, by = "shop_id",sort=F,all.x = TRUE)
raw_test1 <- merge(raw_test1, test_newpoi, by.x = "shop_id", by.y = "shop_id",sort=F,all.x = TRUE)

raw_train1<- na.omit(raw_train1)
raw_test1 <- na.omit(raw_test1)

dim(raw_train1)


######EE
train_ucb <- read.csv('/data/kai.zhang/new_feature/ucb0411.csv', sep='\t', header = F, stringsAsFactors=F)
test_ucb <- read.csv('/data/kai.zhang/new_feature/ucb0412.csv', sep='\t', header = F, stringsAsFactors=F)

colnames(train_ucb) <- c("shop_id", "clicks", "views", "ucbctr", "sdctr")
colnames(test_ucb) <- c("shop_id", "clicks", "views", "ucbctr", "sdctr")

raw_train1 <- merge(raw_train1[,-c(30,31,32)], train_ucb[,-c(2:4)], by.x = "shop_id", by.y="V1",sort=F,all.x = TRUE)
raw_test1 <- merge(raw_test1[,-c(30,31,32)], test_ucb[,-c(2:4)], by.x = "shop_id", by.y="V1", sort=F,all.x = TRUE)

summary(raw_train1)

#raw_train1<- na.omit(raw_train1)
#raw_test1 <- na.omit(raw_test1)

colnames(raw_test1)[30] <- "sd_ctr"
colnames(raw_train1)[30] <- "sd_ctr"

#####add seg ctr

fillNA <- function(featurename, fillvalue){
	if(length(which(colnames(raw_train1) == featurename)) != 0){
		#print("done")
		raw_train1[is.na(raw_train1[,featurename]),featurename] <<- fillvalue
		raw_test1[is.na(raw_test1[,featurename]),featurename] <<- fillvalue
	}
}

fillNA("avg_review_len",0.0)
fillNA("diff_userid_per",1.0)
fillNA("replies_per",0.0)
fillNA("prefect_reviews_per",0.0)
fillNA("badreview_per",0.0)
fillNA("goodreview_per",0.0)


fillNA("avg_review_len2",0.0)
fillNA("diff_userid_per2",1.0)
fillNA("replies_per2",0.0)
fillNA("prefect_reviews_per2",0.0)
fillNA("badreview_per2",0.0)
fillNA("goodreview_per2",0.0)

###

fillNA("badreview",0.0)
fillNA("goodreview",0.0)
fillNA("replies",0.0)
fillNA("dis_userids",0.0)
fillNA("userids",0.0)
fillNA("total_len",0.0)
fillNA("prefect_reviews",0.0)
fillNA("total_review",0.0)

#
fillNA("return_cm3",0.0)
fillNA("return_cm7",0.0)
fillNA("return_cm14",0.0)
fillNA("return_cm30",0.0)


table(is.na(raw_train1))

raw_train1[is.na(raw_train1)] <- 0
raw_test1[is.na(raw_test1)] <- 0


summary(raw_train1)

####模型gbdt ctr
remove_col <- c("label","impact_factor","shop_id")

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

ctrgbdt <- xgboost(data = data.train, label = data.train.click, max.depth = 3, eta = 0.3, nround = 300, objective = "binary:logistic", save_name="xgboost.model",eval_metric = "auc", weight=data.sample.weight, verbose = 0)

remain_index <- which(!colnames(data.train) %in% add_features)

ctrgbdt1 <- xgboost(data = data.train[,remain_index], label = data.train.click, max.depth = 3, eta = 0.3, nround = 300, objective = "binary:logistic", save_name="xgboost.model",eval_metric = "auc", weight=data.sample.weight, verbose = 0)


train_tmp <- data.train[,-27]
ctrgbdt <- xgboost(data = train_tmp, label = data.train.click, max.depth = 3, eta = 0.3, nround = 300, objective = "binary:logistic", save_name="xgboost.model",eval_metric = "auc", weight=data.sample.weight, verbose = 1)

ctrgbdt <- xgboost(data = train_tmp, label = data.train.click, max.depth = 5, eta = 0.14635311504826, nround = 143, objective = "binary:logistic", save_name="xgboost.model",eval_metric = "auc", weight=data.sample.weight, verbose = 1, gamma = 0.181607453757897)


colnames(data.test)[27] <- "sd of ctr" 

##branch mark

pred.train <- predict(ctrgbdt, data.train)
pred.test <- predict(ctrgbdt, data.test)

#ctr auc
colAUC(pred.train, data.train.click) 
colAUC(pred.test, data.test.click)  

#cvr auc
colAUC(pred.train, data.train.order) 
colAUC(pred.test, data.test.order)   

#cvr auc clicked
pred.train.clicked <- predict(ctrgbdt, data.train.clicked)
pred.test.clicked <- predict(ctrgbdt, data.test.clicked)
colAUC(pred.train.clicked, data.train.clicked.order)
colAUC(pred.test.clicked, data.test.clicked.order) 


-------------





#ctr auc
colAUC(pred.train + data.train[,"sd_ctr"], data.train.click) 
colAUC(pred.test + data.test[,"sd_ctr"], data.test.click)  

#cvr auc
colAUC(pred.train + data.train[,"sd_ctr"] , data.train.order) 
colAUC(pred.test + data.test[,"sd_ctr"] , data.test.order)   

#cvr auc clicked
colAUC(pred.train.clicked + data.train.clicked[, "sd_ctr"] , data.train.clicked.order)
colAUC(pred.test.clicked + data.test.clicked[,"sd_ctr"], data.test.clicked.order)




## brench mark
> colAUC(pred.train, data.train.click)
             [,1]
0 vs. 1 0.7204115
> colAUC(pred.test, data.test.click)
             [,1]
0 vs. 1 0.7193222
>
> #cvr auc
> colAUC(pred.train, data.train.order)
            [,1]
0 vs. 1 0.946757
> colAUC(pred.test, data.test.order)
             [,1]
0 vs. 1 0.9410918
>
> #cvr auc clicked
> pred.train.clicked <- predict(ctrgbdt1, data.train.clicked)
> pred.test.clicked <- predict(ctrgbdt1, data.test.clicked)
> colAUC(pred.train.clicked, data.train.clicked.order)
             [,1]
0 vs. 1 0.8662905
> colAUC(pred.test.clicked, data.test.clicked.order)
             [,1]
0 vs. 1 0.8565667

## add ratio
colAUC(pred.train, data.train.click)
             [,1]
0 vs. 1 0.7202915
> colAUC(pred.test, data.test.click)
             [,1]
0 vs. 1 0.7192156
>
> #cvr auc
> colAUC(pred.train, data.train.order)
             [,1]
0 vs. 1 0.9481014
> colAUC(pred.test, data.test.order)
             [,1]
0 vs. 1 0.9410744
>
> #cvr auc clicked
> pred.train.clicked <- predict(ctrgbdt, data.train.clicked)
> pred.test.clicked <- predict(ctrgbdt, data.test.clicked)
> colAUC(pred.train.clicked, data.train.clicked.order)
             [,1]
0 vs. 1 0.8685019
> colAUC(pred.test.clicked, data.test.clicked.order)
             [,1]
0 vs. 1 0.8566032


#####


##feature importance 
brenchImp<-xgb.importance(colnames(data.train), model = ctrgbdt)

                Feature         Gain       Cover   Frequency
 1:           realclick 3.261211e-01 0.078634400 0.071904762
 2:             topshop 2.415311e-01 0.097042918 0.068095238
 3:           distscore 1.024286e-01 0.054380695 0.061428571
 4:    shop_pref_extend 8.914525e-02 0.082064371 0.065238095
 5:                 spl 7.374909e-02 0.067104239 0.054761905
 6:                 ocr 4.866353e-02 0.061119829 0.063333333
 7:      density_thirty 2.248341e-02 0.057986234 0.066190476
 8:                 ctr 1.635946e-02 0.065536508 0.078571429
 9:          isdealshop 1.153185e-02 0.006141303 0.013809524
10:            lastview 1.023771e-02 0.031842080 0.028095238
11:                 seg 9.335039e-03 0.041487157 0.064761905
12:      catprefwithgeo 7.736563e-03 0.036367804 0.032380952
13:          yestdayctr 5.191791e-03 0.025804659 0.038095238
14:            todayctr 4.194049e-03 0.033438404 0.033809524
15:      avg_review_len 3.982196e-03 0.026686222 0.035714286
16:   is_permanent_city 3.645886e-03 0.009491373 0.013333333
17:      is_food_level1 3.464658e-03 0.014050189 0.014285714
18:         replies_per 3.313226e-03 0.034021309 0.030952381
19:   log_ceil_pictotal 2.990628e-03 0.018964792 0.021904762
20:           ishuishop 2.966044e-03 0.005154600 0.010476190
21:     diff_userid_per 2.389856e-03 0.039294824 0.029523810
22:       badreview_per 1.703205e-03 0.035160118 0.027142857
23:           isnewuser 1.401685e-03 0.006471022 0.008571429
24:      goodreview_per 1.095792e-03 0.023010595 0.021904762
25: prefect_reviews_per 9.733275e-04 0.014561700 0.019523810
26:            discount 9.265800e-04 0.007771857 0.009047619
27:    priceprefwithgeo 7.573619e-04 0.012975488 0.007142857
28:             hasstar 7.278054e-04 0.002537992 0.001904762
29:          guess_star 5.612694e-04 0.007584589 0.003809524
30:          price_pref 3.277036e-04 0.003048641 0.003333333
31:              haspic 6.421002e-05 0.000264089 0.000952381


###Lasso 特征选择
library("glmnet")
c("avg_review_len","replies_per","badreview_per","goodreview_per")

remain_index <- which(colnames(data.train) %in% c("replies_per","badreview_per","goodreview_per"))
ctr_lasso <- glmnet(data.train[,remain_index], data.train.click, family=c("binomial"),weights=data.sample.weight, alpha = 1)
trainCtr <- predict(ctr_lasso,data.train[,remain_index],s=0.001,type="response")
testCtr <- predict(ctr_lasso,data.test[,remain_index],s=0.001,type="response")
colAUC(trainCtr, data.train.click)
colAUC(testCtr, data.test.click)    
print(i)
print(trainauc)
print(testauc)
print("-------")
coeff <- coef(ctr_lasso,s=0.0007)
coeff[which(coeff!=0),]


remain_index <- which(colnames(data.train) %in% c("avg_review_len","replies_per","goodreview_per"))
data.train[,"avg_review_len"] <- log(data.train[,"avg_review_len"] + 1)
data.test[,"avg_review_len"] <- log(data.test[,"avg_review_len"] + 1)
ctr_lasso <- glmnet(data.train[,remain_index], data.train.click, family=c("binomial"),weights=data.sample.weight, alpha = 1)
trainCtr <- predict(ctr_lasso,data.train[,remain_index],s=0.001,type="response")
testCtr <- predict(ctr_lasso,data.test[,remain_index],s=0.001,type="response")
colAUC(trainCtr, data.train.click)
colAUC(testCtr, data.test.click)    

####### 添加特征 add feature 

remain_index <- which(colnames(data.train) %in% c("return_cm3","return_cm7","return_cm14","return_cm30"))
ctr_lasso <- glmnet(data.train[,remain_index], data.train.click, family=c("binomial"),weights=data.sample.weight, alpha = 1)
trainCtr <- predict(ctr_lasso,data.train[,remain_index],s=0.001,type="response")
testCtr <- predict(ctr_lasso,data.test[,remain_index],s=0.001,type="response")
colAUC(trainCtr, data.train.click)
colAUC(testCtr, data.test.click)    




remain_index <- which(colnames(data.train) %in% c("startupdays","branch_name","isnewpoi"))
train_tmp <- data.train[,remain_index]
test_tmp <- data.test[,remain_index]

train_tmp[,'startupdays'] <- log(train_tmp[,'startupdays'] + 1) + 1
test_tmp[,'startupdays'] <- log(test_tmp[,'startupdays'] + 1) + 1


ctr_lasso <- glmnet(train_tmp, data.train.click, family=c("binomial"),weights=data.sample.weight, alpha = 1)
trainCtr <- predict(ctr_lasso,train_tmp,s=0.001,type="response")
testCtr <- predict(ctr_lasso,test_tmp,s=0.001,type="response")
colAUC(trainCtr, data.train.click)
colAUC(testCtr, data.test.click)    
