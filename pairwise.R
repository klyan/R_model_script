1。pairwise模型。分三档做标注。nDCG指标提升2.4%(0.585->0.599)	访购率下降15.19%(3.79%->3.21%)，点击率上升0.75%(36.23%->36.50%)。相对于两档标注，点击和访购表现均不佳。	
2.修复模型，重上lambda_mart。相比gbdt，购买未加倍，离线auc稍有下降0.7%(0.717→0.712)。	两天效果，点击率赢1.58%(37.21%->37.80%)，访购率输13.20%(4.96%->4.30%)	
3、lambda_mart模型上线。增加用户搜过筛过的类目特征，含活跃用户，去距离离散化，去毛利特征，购买不加倍。离线auc为0.7126	


https://wiki.sankuai.com/pages/viewpage.action?pageId=727835466
采样方法
Query
         1. 过滤长尾（搜索频率较低）请求样本，长尾Query存在较大的噪音，样本可靠性差，过滤可以减少样本整体的偏差。
         2. 过滤异常活跃与不活跃用户搜索样本，排除极端的搜索行为数据同样有利于减少样本的偏差。
Pair
         1. 根据经验人工设定一些规则，例如通常认为近距离且历史点击率高的商户样本要优于远距离低点击商户。考虑通过样本的历史点击情况，距离等评估形成Pair的可靠性，对于不合理的未点击样本进行欠采样处理。
         2. 依靠统计特征进行采样，例如选取一段时间内（同一区域范围内）用户对样本d1、d2的点击情况，统计到多数用户认为d1优于d2，那么丢弃样本集内的<d2,d1>正样本对和<d1,d2>负样本对。



第一，  根据query的可靠度过滤掉可靠度低的所有query的query-docs集合对(query的可靠度和此query被不同用户搜索次数成正比)，去除低可靠度的query可以减小样本偏差，排除query刷热度的不正常行为。
第二，  根据个体用户的搜索行为集中度过滤掉搜索集中度高的个体的搜索日志。用户的搜索集中度=点击总次数/点击不同的query个数。同样去除集中度高的用户点击行为有利于减小样本偏差，排除行为极端以及点击作弊用户。
经过上述步骤后，需要将初始样本集合划分成训练集、验证集以及测试集。三个集合按照2:1:1的比例随机抽取dpid，某一个dpid归类为某一个集合后，此dpid发生的所有搜索样本均归类为此集合。本项目中，在上海北京等大城市中，训练集中的样本数目控制在1000W左右。


scale_pos_weight
max_delta_step


https://liam0205.me/2016/12/21/XGBoost-idealDCG-and-NDCG/

0无点击

1点击


require(xgboost)
data(agaricus.train, package='xgboost')
train <- agaricus.train
# (data has 6513 rows. 65 queries of length 100 and one with length 13)
groups <- c(rep(100, 65), 13)
weightsData <- 1 + (data$year - max(data$year)) * 5 * 0.01
dtrain <- xgb.DMatrix(data = train$data, label = train$label, group = groups,weight = weightsData)

dtrain1 <- xgb.DMatrix(data = train$data, label = train$label, group = groups)
bst <- xgboost(dtrain1, max.depth = 2, eta = .1, nround = 10, nthread = 2, objective = 'rank:pairwise')




library("xgboost")
library("rjson")
library("stringi")
library("caTools")
library("lightgbm")

setwd("/data/kai.zhang/")

csvdata <- read.csv("/data/kai.zhang/pairwise/train1.csv",sep='\t')
csvdata <- read.csv("/data/kai.zhang/pairwise/train_downsample.csv",sep='\t')

csvdata1 <- read.csv("/data/kai.zhang/pairwise/test.csv",sep='\t')



colnames(csvdata) <- substr(colnames(csvdata),3,800) 
colnames(csvdata1) <- substr(colnames(csvdata1),3,800)

csvdata1[,"hp_cal_dt"] <- NULL
csvdata1[,"remove"] <- NULL
csvdata[,"hp_cal_dt"] <- NULL
csvdata[,"remove"] <- NULL

csvdata[,"query_id"] <- NULL
csvdata[,"pos"] <- NULL
csvdata[,"lastclickpos"] <- NULL

#csvdata[,"shopage"] <- NULL

table(csvdata$label)
table(csvdata$real_order)
table(csvdata$real_order2)

csvdata[,"real_order"] <- csvdata[,"real_order2"]
csvdata1[,"real_order"] <- csvdata1[,"real_order2"]

csvdata[,"real_order1"] <- NULL
csvdata1[,"real_order1"] <- NULL
csvdata[,"real_order2"] <- NULL
csvdata1[,"real_order2"] <- NULL

csvdata[,"waimai_order_shopid"] <- NULL
csvdata1[,"waimai_order_shopid"] <- NULL
csvdata[,"istopshop"] <- NULL
csvdata1[,"istopshop"] <- NULL

csvdata[,"impact_factor"] <- NULL
csvdata1[,"impact_factor"] <- NULL

colnames(csvdata)[34] <- "groupcnt"
colnames(csvdata1)[34] <- "groupcnt"

#csvdata <- csvdata[csvdata[,"groupcnt"] > 1,]
#csvdata1 <- csvdata1[csvdata1[,"groupcnt"] > 1,]

#csvdata <- csvdata[csvdata[,"modelscore"] > 0,]
#csvdata1 <- csvdata1[csvdata1[,"modelscore"] > 0,] 


table(csvdata[,"real_order"])
table(csvdata1[,"real_order"])


for(i in 5:ncol(csvdata)){
  if(class(csvdata[,i]) == "factor"){
    csvdata1[,i] <- as.numeric(as.character(csvdata1[,i]))
    csvdata[,i] <- as.numeric(as.character(csvdata[,i]))
  }
}

csvdata[is.na(csvdata)] <- 0
csvdata1[is.na(csvdata1)] <- 0



removecol <- c("modelscore","dpid","shop_id","label","real_order","groupcnt")

train.data <- csvdata[,which(!colnames(csvdata) %in% removecol)]
test.data <- csvdata1[,which(!colnames(csvdata1) %in% removecol)]

#view：0 click_notorder：0.6 order：1
csvdata[csvdata[,"real_order"] == 0 & csvdata[,"label"] == 1,"real_order"] <- 0.6
csvdata1[csvdata1[,"real_order"] == 0 & csvdata1[,"label"] == 1,"real_order"] <- 0.6

train.label <- csvdata$real_order  
test.label <- csvdata1$

table(train.label)/length(train.label)
table(test.label)/length(test.label)


# view：0 click_notorder：1 order：2
train.label <- rep(0,nrow(csvdata))
test.label <- rep(0,nrow(csvdata1))
train.label[csvdata[,"label"] == 1] <- 1
test.label[csvdata1[,"label"] == 1] <- 1
train.label[csvdata[,"real_order"] == 1] <- 2
test.label[csvdata1[,"real_order"] == 1] <- 2

table(csvdata1[,"real_order"])
table(test.label)
table(csvdata1[,"label"])



train.group <- unique(csvdata[,c("dpid","groupcnt")])
test.group <- unique(csvdata1[,c("dpid","groupcnt")])
train.group <- train.group[,"groupcnt"]
test.group <- test.group[,"groupcnt"]


train.weight <- rep(1,nrow(csvdata))
train.weight[csvdata$real_order == 1] <- 30
test.weight <- rep(1,nrow(csvdata1))
test.weight[csvdata1$real_order == 1] <- 30


train.weight <- rep(1,nrow(csvdata))
train.weight[csvdata$label == 1] <- 30
test.weight <- rep(1,nrow(csvdata1))
test.weight[csvdata1$label == 1] <- 30



#feature_json <- '/data/qin.zhang/l2r/train_l2r/featuremap.json'
feature_json <- '/data/kai.zhang/l2r/featuremap.json'
features <- fromJSON(paste(readLines(feature_json), collapse=""))
feature_selected <- names(features$features_selection)[which(features$features_selection == 1)]
remove<-c("haspic","wifi","fourg","confidence_ctr","weathergrade")
feature_selected <- feature_selected[!feature_selected %in% remove]
feature_selected <- c(feature_selected,"label","impact_factor","istakeawayshop")

date_index <- csvdata[,"hp_cal_dt"] == as.Date("2017-06-21")
table(date_index)


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



dtrain <- xgb.DMatrix(data = as.matrix(train.data), label = train.label, group = train.group, missing = 0), weight= train.weight)
pairwise1 <- xgboost(dtrain, max.depth = 3, nround = 300, objective = "rank:pairwise", save_name="pairwise.model1",eval_metric = "ndcg@10-", verbose = 1, tree_method= 'exact'),weight=train.weight)


dtrain1 <- xgb.DMatrix(data = as.matrix(train.data), label = train.label, group = train.group, missing = 0)
pairwise2 <- xgboost(dtrain1, max.depth = 3, nround = 300, objective = "rank:map", save_name="pairwise.model2",eval_metric = "map@10-", verbose = 1, tree_method= 'exact',weight=train.weight)

dtrain1 <- xgb.DMatrix(data = as.matrix(train.data), label = train.label, group = train.group, missing = 0, weight= train.weight)

train.weight <- rep(1,nrow(csvdata))
train.weight[csvdata$real_order == 1] <- 60
test.weight <- rep(1,nrow(csvdata1))
test.weight[csvdata1$real_order == 1] <- 60
pairwise3 <- xgboost(dtrain1, max.depth = 3, nround = 100, objective = "binary:logistic", save_name="pairwise.model2",eval_metric = "ndcg@10-", verbose = 1, tree_method= 'exact', weight=train.weight)


1、R版本pairwise设置样本weight同样不起效果（试了两种）

2、pairwise设置样本标签：

浏览0，购买1，点击没购买在[0.6,0.9]区间内,
三类样本auc如下：
浏览      vs. 点击未购买   0.6253544
浏览      vs. 购买        0.8798842
点击未购买 vs. 购买        0.8025858
浏览      vs. 点击(包含购买) 0.6321692

关于

install.packages("StatRank")
require("StatRank")

               [,1]
0 vs. 0.6 0.6260354
0 vs. 1   0.8825332
0.6 vs. 1 0.8048957
> colAUC(pred.test, test.label)  #0.7209306
               [,1]
0 vs. 0.6 0.6253544
0 vs. 1   0.8798842
0.6 vs. 1 0.8025858




浏览      vs. 点击未购买     0.6136781
浏览      vs. 购买          0.8911872
点击未购买 vs. 购买          0.8168521
浏览      vs. 点击(包含购买) 0.6202755



pred.train <- predict(pairwise1, as.matrix(train.data))
pred.test <- predict(pairwise1, as.matrix(test.data))

#ctr auc
colAUC(pred.train, train.label) 
colAUC(pred.test, test.label)   

colAUC(pred.train, csvdata$label)  
colAUC(pred.test, csvdata1$label)  

csvdata
#cvr auc
colAUC(pred.train, data.train.order) #0.9454165
colAUC(pred.test, data.test.order)   #0.9318534  0.8938155

#cvr auc clicked
pred.train.clicked <- predict(pairwise, data.train.clicked)
pred.test.clicked <- predict(pairwise, data.test.clicked)

colAUC(pred.train.clicked, data.train.clicked.order) #
colAUC(pred.test.clicked, data.test.clicked.order) #



##feature importance 
xgb.importance(colnames(data.train), model = pairwise)

#Testing whether the results make sense
#chisq.test(train$Age, output_vector)



#save ctr model
ctrmodel <- xgb.dump(pairwise1, with_stats = F)
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





