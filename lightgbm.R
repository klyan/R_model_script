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




#!/usr/bin/python
# -*- coding: utf-8 -*-
#coding=utf-8
import numpy as np
import pandas as pd
import xgboost as xgb
import sys
import json
import re
import copy
import lightgbm as lgb
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import roc_auc_score
reload(sys)
sys.setdefaultencoding('utf8')
import os
from itertools import compress


%cpaste
def get_paths(nodes, nums):
    '''get all path (root to leaf) of one tree'''
    feature_paths = []
    leaf_weight = []
    path = []
    states = np.zeros(nums)
    for i in range(nums):
        if 'leaf' in nodes[i]:
            feature_paths.append('&'.join(path))
            leaf_weight.append(nodes[i].split("=")[1])
            if states[i - 1] == 0:
                states[i - 1] = 1
            else:
                indexs = []
                for j in range(i - 1, 0, -1):
                    if states[j] == 1:
                        indexs.append(j)
                    elif states[j] == -1:
                        continue
                    else:
                        break
                states[indexs] = -1
                states[indexs[-1] - 1] = 1
                for k in range(len(indexs)):
                    node = path.pop()
            if len(path) != 0:
                node = path.pop().replace('<', '>=')
                path.append(node)
                states[i] = -1
        else:
            feature_condition = re.search(
                '\[([a-z_0-9<>=+-\.]*)\]', nodes[i]).group(1)
            if '<' not in feature_condition:
                feature_condition += '<1'
            path.append(feature_condition)
    return feature_paths, leaf_weight
--


%cpaste
def dump_model(weight, bias, file_name, type):
    print('=======dumping model========')
    with open(file_name, 'w') as fw:
        weight_dict = {'name': 'ModelWeight', 'params': weight}
        bias_dict = {'name': 'BiasCoef', 'params': bias}
        fw.write(json.dumps(weight_dict, sort_keys=True, ensure_ascii=False))
        fw.write('\n')
        fw.write(json.dumps(bias_dict, sort_keys=True))
        fw.write('\n')
        pic_param = {'params': {"bins": [50]}, "name": "CEIL_Pictotal"}
        fw.write(json.dumps(pic_param, sort_keys=True, ensure_ascii=False))
        fw.write('\n')
    #distscore_level = {'params': {"bins":[-1.5, 0.01693, 0.02338, 0.03589, 0.06983, 0.13484]},'name': 'LEVEL_DefaultDistScore_CommonDist_Poi_GPoi'}
    #fw.write(json.dumps(distscore_level, sort_keys=True,ensure_ascii=False))
    #fw.write('\n')
        if type == 'Tree':
            fm_dict = {"name": "FeatureMapping", "params": feature_mapping}
            fw.write(json.dumps(fm_dict, sort_keys=True))
            fw.write('\n')
--

%cpaste          
def parse_trees(trees):
    feature_names = []
    feature_weights = []
    for index, tree in enumerate(trees):
        tree = tree.strip('\n')
    #for ele in range(len(featuremap)-1,-1,-1):
    #   tree = tree.replace('f'+str(ele),featuremap['f'+str(ele)])
        nodes = re.split('\n\t+', tree)
        paths, weights = get_paths(nodes, len(nodes))
        feature_paths = [path + "@" + str(index) for path in paths]
        feature_names.extend(feature_paths)
        feature_weights.extend([round(float(ele),8) for ele in weights])
    return feature_names,feature_weights
--

%cpaste    
def dump_tree(trees, file_name):
    features, weights = parse_trees(trees)
    weight = pd.Series(weights, features).to_dict()
    bias = 0.0
    dump_model(weight, bias, file_name, 'Tree')
--

%cpaste    
def training_gbdt(data, data_test, modeltype):
    train_d = data
    train_y = data.label
    print 'just for test'
    print data_test.shape
    test_X =  data_test.ix[:,0:-2]
    test_y = data_test.label

    train_order_label = data.order_label
    test_order_label = data_test.order_label
    train_clicked_order_label = data.order_label.loc[train_y > 0]
    test_clicked_order_label = data_test.order_label.loc[test_y > 0]

    if modeltype == "depth3":
        gbdt = xgb.XGBClassifier(n_estimators=300, max_depth=3)
        print("gdbt depth 3")
    elif modeltype=="depth5":
        gbdt = xgb.XGBClassifier(n_estimators=143, max_depth=5, learning_rate = 0.14635311504826, objective = "binary:logistic", gamma = 0.181607453757897)
        print("gbdt depth 5")
    elif modeltype=="best":
        gbdt = xgb.XGBClassifier(n_estimators=459, max_depth=5, learning_rate = 0.0479613534803502, objective = "binary:logistic", gamma = 0.00635602558031678)
        print("gbdt depth 5 with more trees")

    print('=======start training=======')
    train_X = train_d.ix[:,0:-3]
    sampleWeight = train_d.sample_weight
    gbdt.fit(train_X, train_y, sample_weight=sampleWeight, eval_metric='auc')
    print "features  importance:"
    print train_X.columns
    type(gbdt.feature_importances_)
    print (gbdt.feature_importances_)
    names = [str(ele) for ele in train_X.columns]
    importance = gbdt.feature_importances_
    feat_importance = pd.Series(importance,names).to_dict()
    feat_importance = sorted(feat_importance.iteritems() ,key = lambda asd:asd[1],reverse=True)
    print feat_importance

    pred_for_traindata = gbdt.predict_proba(train_X)
    pred_gbdt = gbdt.predict_proba(test_X)

    #pd.DataFrame(pred_gbdt[:,1],columns=['pred']).to_csv('predict' ,sep='\t',header=True,index=False)
    auc_for_traindata = roc_auc_score(train_y, pred_for_traindata[:, 1])
    auc_gbdt = roc_auc_score(test_y, pred_gbdt[:, 1])
    print('training ctr auc: ' + str(auc_for_traindata))
    print('testing ctr auc: ' + str(auc_gbdt))
    
    print('training cvr auc:'+str(roc_auc_score(train_order_label, pred_for_traindata[:,1])))
    print('testing cvr auc:'+str(roc_auc_score(test_order_label, pred_gbdt[:, 1])))

    print('training clicked cvr auc:'+str(roc_auc_score(train_clicked_order_label, list(compress(pred_for_traindata[:, 1], train_y>0)))))
    print('testing clicked cvr auc:'+str(roc_auc_score(test_clicked_order_label, list(compress(pred_gbdt[:, 1], test_y>0)))))

    return gbdt.booster()
--



%cpaste
def discrete_for_LR(df, bins, colname):
        seg = range(len(bins)-1)
        categorical = pd.cut(df[colname], bins)
        df_num = pd.get_dummies(categorical);
        df_num.columns = [colname + '_' + str(el) for el in seg]
        df = df.merge(df_num, left_index=True, right_index=True)
        df.drop(colname, inplace=True, axis=1)
        return df
--

%cpaste
def featuredata_process(data, features_selected, weights,flag=None):
    data.fillna(value = 0, inplace = True)
    selected = copy.copy(features_selected)
    mapping = copy.copy(feature_mapping)
    #data,selected,mapping = discrete_for_gbdt(data, range(30), 'cityid', selected, feature_mapping)
    selected.append('label')
    df_train = data.loc[:,selected]
    print flag, " data describe:: ", df_train.describe()
    df_train['order_label']=data.impact_factor.map(lambda x:1 if x>=2 else 0)
    if flag=='train':
        sample_w=data.impact_factor.map(lambda x:weights if x>=2  else(1 if x>=1 else 1))
        #sample_w = sample_w * data.cate_searched.map(lambda x:1 if x>=0  else 1)
        #sample_w=sample_w*data.coorview
        df_train['sample_weight'] = sample_w
        #df_train.rename(columns={'is_click':'label'}, inplace=True)
    return df_train,mapping
--



%cpaste
def make_featuremap(featurelist):
    dic={}
    for index,ele in enumerate(featurelist):
        dic['f'+str(index)] = ele
    return dic
--



#评估cityid
%cpaste
def cityid_process(data, weights,flag=None):

    return city_data
--


os.chdir("/data/kai.zhang/l2r")
param_setting = json.load(open('featuremap.json'))
feature_mapping = param_setting['featuremap']
features_selected = [k for k, v in param_setting['features_selection'].items() if v == 1]

features_selected.remove("haspic")
features_selected.remove("wifi")
features_selected.remove("fourg")
features_selected.remove("confidence_ctr")
features_selected.remove("weathergrade")

features_selected.append("istakeawayshop")

features_selected.append("prefect_reviews_pre")

features_selected.append("reply_pre")
features_selected.append("goodreview_per")
features_selected.append("badreview_per")



job_path='dataset/samples'
file_name='2017-05-17_samples.csv_coor'
test_file_name='2017-05-18_samples.csv_coor'
data_filepath = '%s/%s' % (job_path, file_name)
test_data_filepath = '%s/%s' % (job_path, test_file_name)
model_on_depth = 'del_mall'
model_filepath = '../model/gbdt/%s.json' % (model_on_depth)
featuremap_file = '%s/%s.map' % (job_path, file_name)
#arrage data

train_data = pd.read_csv("/data/kai.zhang/train_python.csv", sep=',', header=0)
train_label = pd.read_csv("/data/kai.zhang/train.label_python.csv", sep=',', header=0)
train_weight = pd.read_csv("/data/kai.zhang/train.weight_python.csv", sep=',', header=0)
train_group = pd.read_csv("/data/kai.zhang/train.group_python.csv", sep=',', header=0)
train_queryid = pd.read_csv("/data/kai.zhang/query_id.csv", sep=',', header=0)


test_data = pd.read_csv("/data/kai.zhang/test.data_python.csv", sep=',', header=0)
test_label = pd.read_csv("/data/kai.zhang/test.label_python.csv", sep=',', header=0)


tmp_label = train_label.copy()
tmp_label[tmp_label==0.6] = 1

##
(train_queryid,train_data)

df_c = pd.concat([train_queryid.reset_index(drop=True), train_data], axis=1)
dtrain = lgb.Dataset(df_c, label = list(train_label['x']), query=0)

train_weight1 = list(train_weight['x'])
train_weight1[train_weight1 == 30] = 100

dtrain = lgb.Dataset(train_data, label = list(train_label['x']), group=train_group['x'], weight = train_weight1)
dtest = lgb.Dataset(test_data, label = list(test_label['x']))

dtrain.set_weight(train_weight1)
dtrain.set_group(train_group)

#1e4

params = {"objective":"lambdarank", "metric":"ndcg", "max_depth":3, "verbose":1, "num_threads":10, "min_sum_hessian":100, "min_sum_hessian": 1e-10}

model = lgb.train(params, dtrain, num_boost_round = 300)
ypred = model.predict(train_data)


ypred

array([ 12.38579307,  12.50033335,  12.3834334 , ...,  12.32865658,
        11.8425255 ,  12.43445448])

ypred_t = model.predict(test_data)

np.savetxt("ypred.txt", ypred)
np.savetxt("ypred_t.txt", ypred_t)

roc_auc_score(train_label['x'], ypred)
roc_auc_score(test_label['x'], ypred_t)
    

trainy = read.csv(file="ypred.txt")


colAUC(trainy, train.label) #
colAUC(testy, test.label) #


#data_test,temp = featuredata_process(test_data_ori, features_selected,'test')
#data,feature_mapping = featuredata_process(data_ori, features_selected, 60,'train')


#trainning
model3 = training_gbdt(data, data_test,"depth3")
base = training_gbdt(data, data_test,"depth3")

model5 = training_gbdt(data, data_test,"depth5")
best = training_gbdt(data, data_test,"best")

#output models
model_filepath = "/data/kai.zhang/GBDT_cityid.json"
dump_tree(model3.get_dump(), model_filepath)





require(xgboost)
data(agaricus.train, package='xgboost')
train <- agaricus.train
# (data has 6513 rows. 65 queries of length 100 and one with length 13)
groups <- c(rep(100, 65), 13)
weightsData <- 1 + (data$year - max(data$year)) * 5 * 0.01
dtrain <- xgb.DMatrix(data = train$data, label = train$label, group = groups,weight = weightsData)

dtrain1 <- xgb.DMatrix(data = train$data, label = train$label, group = groups)
bst <- xgboost(dtrain1, max.depth = 2, eta = .1, nround = 10, nthread = 2, objective = 'rank:pairwise')




tmp <- cbind(train.data, 

write.csv(train.data, file = "train_python.csv", row.names = FALSE)
write.csv(train.label, file = "train.label_python.csv", row.names = FALSE)
write.csv(train.weight, file = "train.weight_python.csv", row.names = FALSE)
write.csv(train.group, file = "train.group_python.csv", row.names = FALSE)
write.csv(test.data, file = "test.data_python.csv", row.names = FALSE)
write.csv(test.label, file = "test.label_python.csv", row.names = FALSE)



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
test.label <- csvdata1$real_order

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


write.csv(csvdata[,"dpid"],file="query_id.csv",row.names=F)



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


lgb.unloader(wipe = TRUE)


dtrain <- lgb.Dataset(as.matrix(train.data), label = train.label, weight=train.weight, group = train.group)
dtest <- lgb.Dataset(as.matrix(test.data), label = test.label)

params <- list(objective = "lambdarank", metric = "ndcg", max_depth = 3, verbose = 1, num_threads = 10)
model <- lgb.train(params, data = dtrain, 300, learning_rate = 0.1)




model <- lgb.train(objective = "regression", metric = "ndcg", ndcg_eval_at = c(1,3,5,7,10), max_depth = 3, verbose = 1, num_trees = 300, num_threads = 10, data = dtrain, learning_rate = 0.1, early_stopping_rounds = 10, weight=train.weight, group = train.group)


dtrain <- lgb.Dataset(as.matrix(train.data), label = train.label, weight=train.weight)
params <- list(objective = "regression", metric = "l2", max_depth = 3, verbose = 1, num_threads = 10)
model <- lgb.train(params, data = dtrain, nrounds = 300, learning_rate = 0.1, verbose = 0)

dtrain <- lgb.Dataset(as.matrix(train.data), label = train.label, weight=train.weight,group = train.group)
params <- list(objective = "lambdarank", metric = "ndcg", max_depth = 3, verbose = 1, num_threads = 10)
model <- lgb.train(params, data = dtrain, nrounds = 300, learning_rate = 0.1)



library(lightgbm)
data(agaricus.train, package = "lightgbm")
train <- agaricus.train
dtrain <- lgb.Dataset(train$data, label = train$label, group=6513)
data(agaricus.test, package = "lightgbm")
test <- agaricus.test
dtest <- lgb.Dataset.create.valid(dtrain, test$data, label = test$label)
params <- list(objective = "lambdarank", metric = "ndcg")
valids <- list(test = dtest)
model <- lgb.train(params,
                   dtrain,
                   100,
                   valids,
                   min_data = 1,
                   learning_rate = 1,
                   early_stopping_rounds = 10)


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



pred.train1 <- predict(model, as.matrix(train.data))
pred.test <- predict(model, as.matrix(test.data))

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





