library("glmnet")
library("Matrix")
library("pROC")
library('caTools')
library(dummies)
library("rjson")
library("xgboost")
library(stringr)
library(parallel)
library(stringi)


load("/data/kai.zhang/model_merge/id_model1.RData")

factor_col <- c("shop_id","shoppower","is_permanent_city","gender","pricelevel","cityid","shopcategory1")

sparsedata <- sparse.model.matrix( ~ -1 + is_permanent_city + shoppower + gender + shop_id + cityid + pricelevel + shopcategory1, data=csvdata)

dup_sparsedata <- sparsedata

features_selected <- c("ctr","allcategoryctr","realclick","comm_dist","dist_ctr1","distscore","pics","loccvr","lastview","totalreviews","prefectreviews","reviewslength","badreviews","goodreviews","shopage","price_pref","shop_pref_extend","spl","seg","deal_percent","uvscore","discount","basescore","distsensitivity","topshop","mallpercent","timeseg")

feature_map <- list()
feature_map$ctr <- "FoodShopCtrCat1"
feature_map$allcategoryctr <- "WILSON_allcategoryclicks_allcategoryviews_CONST@key=0.99"
feature_map$realclick <- "FoodRT_CROSSSEEK_RealTimeShopClick_Shopid" 
feature_map$comm_dist <- "CommonDist_Poi_GPoi" 
feature_map$dist_ctr1 <- "PRODUCT_DefaultDistScore_CommonDist_Poi_GPoi_FoodShopCtrCat1" 
feature_map$distscore <- "DefaultDistScore_CommonDist_Poi_GPoi" 
feature_map$pics <- "Pictotal" 
feature_map$loccvr <- "CROSSSEEK_LocCVR_Shopid" 
feature_map$lastview <- "CROSSSEEK_RealTimeShopLastView_Shopid" 
feature_map$totalreviews <- "TotalReviews" 
feature_map$badreviews <- "BadReviews" 
feature_map$prefectreviews <-"PrefectReviews"
feature_map$reviewslength <-"ReviewsLength"
feature_map$goodreviews <-"GoodReviews"
feature_map$price_pref <-"UserCat1PricePref_UserQuInfo_UserCat1PercentileList"
feature_map$deal_percent <-"DealPromotePercent"
feature_map$discount <-"DisCT_DealGroupPrice_DealGroupMarketPrice_MixDealGroupSales"
feature_map$basescore <-"BaseScore"
feature_map$distsensitivity <-"UserDistanceSensitivity_UserQuInfo_CommonDist_Poi_GPoi"
feature_map$shopage <- "shopage" 
feature_map$shop_pref_extend <- "COALESCE_NormalizeUserShopPref_UserQuInfo_Shopid_PrefShopGuess_SimilarityShops_UserQuInfo" 
feature_map$spl <- "IF@3_CROSSSEEK_UserQuInfo_CONST@key=isactive_COALESCE_CROSSSEEK_LocCTR_Shopid_CROSSSEEK_LocCTRSPL_Shopid_CROSSSEEK_LocCTR_Shopid" 
feature_map$uvscore <- "UvScore" 
feature_map$seg <- "TimeRelevanceSeek_TimeRelevance_TimeSeg" 
feature_map$topshop <- "CROSSSEEK_LocationTopShop_locationu_Shopid" 
feature_map$mallpercent <- "LocLandmark_locationu_Mallid" 
feature_map$timeseg <- "TimeSeg" 


set.seed(100)
train_index <- sample(1:nrow(csvdata),nrow(csvdata) * 0.7, replace = FALSE) 
gsample.weight <- sapply(csvdata$real_order[train_index], function(x) {ifelse(x==1,60,1)}) 


sparsedata <- dup_sparsedata


#-------连续型特征分档-----

bin_file <- file("/data/kai.zhang/model_merge/cutoff_model_depth4.json", "w" )

for (fea in features_selected) { 
    gtrain <- csvdata[train_index,fea]
    gtrain.label<- csvdata$label[train_index]
    gtest <- csvdata[-train_index,fea]
    gtest.label<- csvdata$label[-train_index]
    ctrgbdt <- xgboost(data = as.matrix(gtrain), label = gtrain.label, max.depth = 4, nround = 1, objective = "binary:logistic", eval_metric = "auc", weight=gsample.weight, verbose = 0, tree_method= 'exact')
    pred.train <- predict(ctrgbdt, as.matrix(gtrain))
    pred.test <- predict(ctrgbdt, as.matrix(gtest))
    print(colAUC(pred.train, gtrain.label))  
    print(colAUC(pred.test, gtest.label))
    split <- xgb.model.dt.tree(fea, ctrgbdt)
    cutoff <- sort(as.numeric(unique(split$Split[!is.na(split$Split)])))

    print("*********************")
    print(fea)
    print(cutoff)
    print("---------------------")
    
    bins <- paste(c('{"name":"LEVEL_', feature_map[[fea]], '","params": { "bins": [',paste(cutoff, collapse=", "),"]}}"),collapse="")
    writeLines(bins, bin_file)

    interval_OHE <- as.data.frame(findInterval(csvdata[,fea], cutoff) - 1) 
    colnames(interval_OHE) <- paste(c("LEVEL_",fea,"_"),collapse="")
    interval_OHE[,1] <- as.factor(interval_OHE[,1])
    
    f <- as.formula(paste("~ ", paste(c(-1,colnames(interval_OHE)), collapse = "+")))
    tmp <- sparse.model.matrix(f, data=interval_OHE)

    colnames(tmp) <- str_replace_all(string = colnames(tmp),pattern = colnames(interval_OHE),replacement = paste(c("LEVEL_",feature_map[[fea]],"-"),collapse=""))
    sparsedata <- cbind(sparsedata,tmp)
}
close(bin_file)



list <- setdiff(colnames(csvdata),c("label","real_order","dist_poi","pos",factor_col,features_selected))
setdiff(list, colnames(csvdata))


##判断哪些是多类型的离散特征值
for( i in list) {
  if(length(unique(csvdata[,i])) > 2){
    print(colnames(csvdata)[i])
  }
}

others <- Matrix(as.matrix(csvdata[,list]), sparse=TRUE)
sparseMtx <- cbind(sparsedata,others)

save.image(file="/data/kai.zhang/model_merge/id_modelv2.RData") #自动化版本


set.seed(100)
train_index <- sample(1:nrow(csvdata),nrow(csvdata) * 0.1, replace = FALSE) 

train <- sparseMtx[train_index,]
test <- sparseMtx[-train_index,]
train.label<- csvdata$label[train_index]

data_sample_weight <- csvdata[train_index,"real_order"] * 10
data_sample_weight[data_sample_weight == 0] <- 1


plambda <-c(0.0055534634, 0.0050601090, 0.0046105829, 0.0042009914, 0.0038277869, 0.0034877369, 0.0031778959, 0.0028955804, 0.0026383450, 0.0024039617, 0.0021904004, 0.0019958113, 0.0018185089, 0.0016569576, 0.0015097580, 0.0013756353, 0.0012534276, 0.0011420766, 0.0010406176, 0.0009481720, 0.0008639391, 0.0007871891, 0.0007172574, 0.0006535382, 0.0006, 0.0005)

fit <- glmnet(train, as.factor(train.label),family="binomial", alpha = 1,weights = data_sample_weight, lamdba = plambda)


fit <- glmnet(train, as.factor(train.label),family="binomial", alpha = 1,weights = data_sample_weight, lamdba = c(0.003,0.002,0.001,0.0009,0.0008))


save.image(file="/data/kai.zhang/model_merge/id_modelv2.RData") #自动化版本


trainY <- predict(fit,train,s=0.001,type="response")
testY <- predict(fit,test,s=0.001,type="response")

colAUC(trainY, csvdata[train_index,"label"])
colAUC(testY, csvdata[-train_index,"label"])

coeff <- coef(fit,s=0.001)
length(coeff[which(coeff!=0),])

tmp <- tail(colnames(df_coef),100)


nohup R CMD BATCH runIDModel.R > runIDModel.result.file 2>&1 &

