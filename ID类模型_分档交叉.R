library("glmnet")
library("Matrix")
library("pROC")
library('caTools')
library(dummies)
library("rjson")
library("xgboost")
library(stringr)
library(parallel)


csvdata = read.csv("./id_v4.csv", sep='\t')
csvdata = read.csv("/data/kai.zhang/model_merge/id_oneday.csv", sep='\t')
csvdata = read.csv("/data/kai.zhang/model_merge/id_v2.csv", sep='\t')

csvdata = read.csv("/data/kai.zhang/model_merge/id_v3.csv", sep='\t')

str(csvdata)
csvdata[,1] = as.character(csvdata[,1])

csvdata <- csvdata[csvdata[,1] == "FoodCat1GBDTSeq4",]

index = which(csvdata[,1] %in% c("FoodCat1GBDTSeq2","FoodCat1GBDTSeq2|OLFoodCat1GBDTSeq2","FoodCat1GBDTSeq2|OLFoodCat1GBDTSeq2|FoodCat1GBDTSeq4","FoodCat1GBDTSeq4"))

csvdata = csvdata[index,]

#tapply(csvdata$model, summary)
#aggregate(csvdata, by=list(csvdata$model), FUN=summary)
csvdata[,"dpid"] <- NULL
csvdata[,"model"] <- NULL
csvdata[,"pos"] <-NULL
csvdata[,"cityid"] <- NULL

for(i in 3:ncol(csvdata)){
  if(class(csvdata[,i]) == "factor"){
    csvdata[,i] <- as.numeric(as.character(csvdata[,i]))
  }
}

csvdata[is.na(csvdata)] <- 0

remov <-colnames(csvdata)[apply(csvdata,2,max) == 0] #最大值是0的col
remov
summary(csvdata)

for (i in remov){
  csvdata[,i] <- NULL
}



factor_col <- c("shop_id","shoppower","is_permanent_city","gender","pricelevel","cityid","shopcategory1") #,"dpid"

factor_index <- which(colnames(csvdata) %in% factor_col)
for(i in factor_index){
  csvdata[,i] <- as.factor(csvdata[,i])
}


sparsedata <- sparse.model.matrix( ~ -1 + is_permanent_city + shoppower + gender + shop_id + pricelevel + shopcategory1, data=csvdata)

dup_sparsedata <- sparsedata

#-------连续型特征分档-----
#哪些col需要分档

features_selected <- c()
for(i in colnames(csvdata)){
  if(length(unique(csvdata[,i])) > 2){
    features_selected <- c(features_selected,i)
    print(i)
  }
}
setdiff(features_selected, features_selected1)

setdiff(features_selected1, features_selected)


features_selected1 <- c("ctr","allcategoryctr","realclick","comm_dist","dist_ctr1","distscore","pics","loccvr","lastview","totalreviews","prefectreviews","reviewslength","badreviews","goodreviews","shopage","price_pref","shop_pref_extend","spl","seg","deal_percent","uvscore","discount","basescore","distsensitivity","topshop","mallpercent","timeseg","yestdayctr","ocr","todayctr", "shopavgprice", "catprefwithgeo","density_thirty30","priceprefwithgeo")


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

#newadded
feature_map$density_thirty30 <- "LocDensity_locationu_CONST@key=STAR30"
feature_map$density_thirty35 <- "LocDensity_locationu_CONST@key=STAR35"
feature_map$catprefwithgeo <- "PrefWithGeo_CONST@key=catpref_IsLocate_UserQuInfo_UserQuInfo_MainCategoryIds"
feature_map$shopavgprice <- "ShopAvgPrice"
feature_map$todayctr <- "WILSON_ShopRealtimeInfo_ShopRealtimeTraffic_CONST@key=10_CONST@key=1_CONST@key=1_CONST@key=0_ShopRealtimeInfo_ShopRealtimeTraffic_CONST@key=10_CONST@key=1_CONST@key=0_CONST@key=0_CONST@key=0.95"
feature_map$ismallshop<-"ShopInMall_LandmarkInfo_Mallid"
feature_map$ocr<- "CROSSSEEK_MAP@儑@:_FoodShopOvr_CONCAT@3_CONST@key=L_CONST@key=m_QUERY@categoryid"
feature_map$isinMall <- "ShopInMall_LandmarkInfo_Mallid"
feature_map$priceprefwithgeo <- "PrefWithGeo_CONST@key=pricepref_IsLocate_UserQuInfo_UserQuInfo_UserCat1PercentileList"
feature_map$yestdayctr <-"WILSON_ShopRealtimeInfo_ShopRealtimeTraffic_CONST@key=10_CONST@key=1_CONST@key=1_CONST@key=1_ShopRealtimeInfo_ShopRealtimeTraffic_CONST@key=10_CONST@key=1_CONST@key=0_CONST@key=1_CONST@key=0.95" 


setdiff(names(feature_map),features_selected)

setdiff(features_selected,names(feature_map))
#,) #需要分档
#"dist_poi"  #csvdata[,"dist_poi"] <- NULL


set.seed(100)
train_index <- sample(1:nrow(csvdata),nrow(csvdata) * 0.7, replace = FALSE) 

gsample.weight <- sapply(csvdata$real_order[train_index], function(x) {ifelse(x==1,60,1)}) 
sparsedata <- dup_sparsedata

save.image(file="./id_modelv3_addfeature.RData") 

load(file="./id_modelv3_addfeature.RData") 

bin_file <- file("./cutoff_model_depth3.json", "w" )

cross_bin <- NULL

for (fea in features_selected) { 
    gtrain <- csvdata[train_index,fea]
    gtrain.label<- csvdata$label[train_index]
    gtest <- csvdata[-train_index,fea]
    gtest.label<- csvdata$label[-train_index]
    ctrgbdt <- xgboost(data = as.matrix(gtrain), label = gtrain.label, max.depth = 3, nround = 1, objective = "binary:logistic", eval_metric = "auc", weight=gsample.weight, verbose = 0, tree_method= 'exact')
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
    cross_bin <- cbind(cross_bin,interval_OHE[,1])
}
close(bin_file)


list <- setdiff(colnames(csvdata),c("label","real_order","dist_poi","pos",factor_col,features_selected))
setdiff(list, colnames(csvdata))

summary(csvdata[,list])

##判断哪些是多类型的离散特征值
for( i in list) {
  if(length(unique(csvdata[,i])) > 2){
    print(colnames(csvdata)[i])
  }
}

others <- Matrix(as.matrix(csvdata[,list]), sparse=TRUE)
sparseMtx <- cbind(sparsedata,others)

object.size(sparsedata)

#save.image(file="id_raw_v3.RData")
save.image(file="./id_modelv3_addfeature.RData") 

#load("id_raw_v3.RData")


dim(sparseMtx)


set.seed(100)
train_index <- sample(1:nrow(csvdata),nrow(csvdata) * 0.1, replace = FALSE) 

train <- sparseMtx[train_index,]
test <- sparseMtx[-train_index,]
train.label<- csvdata$label[train_index]

for(iweight in c(5,15,20,25,30,45,50)) {
    data_sample_weight <- csvdata[train_index,"real_order"] * iweight
    data_sample_weight[data_sample_weight == 0] <- 1

    ###----
    #data_sample_weight <- csvdata[train_index,"label"] * 10
    #data_sample_weight[data_sample_weight == 0] <- 1

    #tmp <- (csvdata[train_index,"pos"] + 1) * csvdata[train_index,"label"]
    #tmp[tmp == 0] <- 1
    #data_sample_weight<- (10/(1+ 1*exp(-tmp)))

    ####----

    fit <- glmnet(train, as.factor(train.label),family="binomial", alpha = 1,weights = data_sample_weight, lambda=c(0.001))

    #save.image(file="./id_modelv3_addfeature.RData") 

    #load(file="./id_modelv3_addfeature.RData")

    trainY <- predict(fit,train,s=0.001,type="response")  
    testY <- predict(fit,test,s=0.001,type="response") 

    print(colAUC(trainY, csvdata[train_index,"label"]))  #0.714187
    print(colAUC(testY, csvdata[-train_index,"label"]))  #0.7135066

    print(colAUC(trainY, csvdata[train_index,"real_order"]))  #0.9703681
    print(colAUC(testY, csvdata[-train_index,"real_order"]))  #0.9270994

    coeff <- coef(fit,s=0.001)
    print(length(coeff[which(coeff!=0),]))
}

coeff <- coef(fit)
length(coeff[which(coeff!=0),])



coeff[which(coeff!=0),]


df_coef <- data.frame(t(as.matrix(coeff[which(coeff!=0),])))

#save.image(file="/data/kai.zhang/model_merge/id_model1.RData")

#load("/data/kai.zhang/model_merge/id_model1.RData")

feature_map = list()
feature_map$ctr <- "FoodShopCtrCat1"
feature_map$allcategoryctr <- 

tmp <- tail(colnames(df_coef),100)

head(colnames(df_coef),100)

tail(colnames(df_coef),100)

grep("gender",colnames(df_coef))

sub(pattern = "[cityid]",replacement = "OHE_INFO@cityid-",x = "cityid2158") 


colnames(csvdata)
library(stringr)
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "cityid",replacement = "OHE_INFO@cityid-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "shop_id",replacement = "OHE_Shopid-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "shoppower",replacement = "OHE_ShopPower-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "gender",replacement = "OHE_DefaultValue_CROSSSEEK_UserQuInfo_CONST@key=gender_CONST@key=2-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "shopcategory1",replacement = "OHE_category1-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "is_permanent_city",replacement = "OHE_COMP@=_CROSSSEEK_UserQuInfo_CONST@key=permanentcityid_INFO@cityid-")


##LEVEL特征前缀从-1开始
level_index<-grep("LEVEL",colnames(df_coef))
colnames(df_coef)[level_index]
prefix <- substr(colnames(df_coef)[level_index],1,nchar(colnames(df_coef)[level_index])-1)
appends <- as.numeric(substr(colnames(df_coef)[level_index],nchar(colnames(df_coef)[level_index]),nchar(colnames(df_coef)[level_index])))-1
colnames(df_coef)[level_index] <- paste(prefix,appends,sep="")
###LEVEL END

unique(colnames(df_coef)[level_index])


colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_ctr_",replacement = "LEVEL_FoodShopCtrCat1-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_allcategoryctr_",replacement = "LEVEL_WILSON_allcategoryclicks_allcategoryviews_CONST@key=0.99-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_realclick_",replacement = "LEVEL_FoodRT_CROSSSEEK_RealTimeShopClick_Shopid-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_comm_dist_",replacement = "LEVEL_CommonDist_Poi_GPoi-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_dist_ctr1_",replacement = "LEVEL_PRODUCT_DefaultDistScore_CommonDist_Poi_GPoi_FoodShopCtrCat1-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_distscore_",replacement = "LEVEL_DefaultDistScore_CommonDist_Poi_GPoi-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_pics_",replacement = "LEVEL_Pictotal-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_loccvr_",replacement = "LEVEL_CROSSSEEK_LocCVR_Shopid-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_lastview_",replacement = "LEVEL_CROSSSEEK_RealTimeShopLastView_Shopid-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_totalreviews_",replacement = "LEVEL_TotalReviews-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_badreviews_",replacement = "LEVEL_BadReviews-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_shopage_",replacement = "LEVEL_shopage-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_shop_pref_extend_",replacement = "LEVEL_COALESCE_NormalizeUserShopPref_UserQuInfo_Shopid_PrefShopGuess_SimilarityShops_UserQuInfo-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_spl_",replacement = "LEVEL_IF@3_CROSSSEEK_UserQuInfo_CONST@key=isactive_COALESCE_CROSSSEEK_LocCTR_Shopid_CROSSSEEK_LocCTRSPL_Shopid_CROSSSEEK_LocCTR_Shopid-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_uvscore_",replacement = "LEVEL_UvScore-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_seg_",replacement = "LEVEL_TimeRelevanceSeek_TimeRelevance_TimeSeg-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_topshop_",replacement = "LEVEL_CROSSSEEK_LocationTopShop_locationu_Shopid-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_mallpercent_",replacement = "LEVEL_LocLandmark_locationu_Mallid-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_timeseg_",replacement = "LEVEL_TimeSeg-")

colnames(df_coef)[which(colnames(df_coef) == "guess_star")]  = "IsStarGuess_Starsource"
colnames(df_coef)[which(colnames(df_coef) == "smallcity")]  = "Not_Contains@,_CONST@key=1,2,4,7,10,3,5,8,6,16,17,21,9_INFO@cityid"
colnames(df_coef)[which(colnames(df_coef) == "isdealshop")]  = "IsDealShop"
colnames(df_coef)[which(colnames(df_coef) == "ishuishop")]  = "IsHuiShop"
colnames(df_coef)[which(colnames(df_coef) == "hasstar")]  = "AND@1_ShopPower"
colnames(df_coef)[which(colnames(df_coef) == "istakeawayshop")]  = "IsTakeawayShop"
colnames(df_coef)[which(colnames(df_coef) == "holddeal")]  = "PartContains_ReceiptHoldDealGroupId_DealGroupID"
colnames(df_coef)[which(colnames(df_coef) == "istopshop")]  = "IsFoodTopShop"
colnames(df_coef)[which(colnames(df_coef) == "islijian")]  = "OR@2_IsTuanDiscount_IsShanhuiDiscount"
colnames(df_coef)[which(colnames(df_coef) == "isbookingshop")]  = "IsBookingShop"
colnames(df_coef)[which(colnames(df_coef) == "isfushop")]  = "IsFuShop"
colnames(df_coef)[which(colnames(df_coef) == "isnewuser")]  = "isNewUser"
colnames(df_coef)[which(colnames(df_coef) == "isactive")]  = "CROSSSEEK_UserQuInfo_CONST@key=isactive"
colnames(df_coef)[which(colnames(df_coef) == "is_food_level1")]  = "COMP@=_QUERY@categoryid_CONST@key=10"
colnames(df_coef)[which(colnames(df_coef) == "isfake")]  = "IsFakePic"
colnames(df_coef)[which(colnames(df_coef) == "iphone")]  = "PhoneType_CONST@key=iphone"
colnames(df_coef)[which(colnames(df_coef) == "is_food_level1")]  = "COMP@=_QUERY@categoryid_CONST@key=10"
colnames(df_coef)[which(colnames(df_coef) == "smallcity")]  = "Not_Contains@,_CONST@key=1,2,4,7,10,3,5,8,6,16,17,21,9_INFO@cityid"


biasdict <- paste(c('{"name":"BiasCoef", "params":',df_coef[,"X.Intercept."],"}"),collapse=" ")
df_coef[,"X.Intercept."] <- NULL

featuresWeight <- toJSON(unlist(unname(split(df_coef,1:nrow(df_coef)))))
weightdict <- paste(c('{"name":"ModelWeight","params":',featuresWeight,"}"),collapse=" ")

wf <- file("/data/kai.zhang/model_merge/FoodCat1IdLRSeq4.json", "w" )
writeLines( weightdict, wf)
writeLines( biasdict, wf)

writeLines('{"name": "LEVEL_FoodShopCtrCat1", "params": {"bins": [0.033195, 0.044525, 0.058245, 0.086585, 0.105515, 0.137685, 0.186965]}}', wf)
writeLines('{"name": "LEVEL_WILSON_allcategoryclicks_allcategoryviews_CONST@key=0.99", "params": {"bins": [0.0154445, 0.0295115, 0.0355905, 0.0459515, 0.0589055, 0.1072020, 0.1431310]}}', wf)
writeLines('{"name": "LEVEL_FoodRT_CROSSSEEK_RealTimeShopClick_Shopid", "params": {"bins": [-1.776040, -1.776030, 0.468075, 1.615130, 2.379320, 3.264690, 4.251550]}}', wf)
writeLines('{"name": "LEVEL_CommonDist_Poi_GPoi", "params": {"bins": [22.9838, 33.2987, 48.0416, 55.0764, 75.9274, 174.8770, 419.7380]}}', wf)
writeLines('{"name": "LEVEL_PRODUCT_DefaultDistScore_CommonDist_Poi_GPoi_FoodShopCtrCat1", "params": {"bins": [0.0097105, 0.0161365, 0.0247515, 0.0331915, 0.0383945, 0.0451495, 0.0921950]}}', wf)
writeLines('{"name": "LEVEL_DefaultDistScore_CommonDist_Poi_GPoi", "params": {"bins": [0.196173, 0.306280, 0.431181, 0.757025]}}', wf)
writeLines('{"name": "LEVEL_Pictotal", "params": {"bins": [2.5, 17.5, 32.5, 129.5, 391.5, 878.5, 1595.5]}}', wf)
writeLines('{"name": "LEVEL_CROSSSEEK_LocCVR_Shopid", "params": {"bins": [0.0005, 0.0065, 0.0205, 0.0395, 0.0715, 0.1295, 0.2055]}}', wf)
writeLines('{"name": "LEVEL_CROSSSEEK_RealTimeShopLastView_Shopid", "params": {"bins": [0.5, 1.5, 3.5]}}', wf)
writeLines('{"name": "LEVEL_TotalReviews", "params": {"bins": [0.5, 2.5, 7.5, 16.5, 35.5, 73.5, 287.5]}}', wf)
writeLines('{"name": "LEVEL_BadReviews", "params": {"bins": [0.5, 1.5, 2.5, 4.5, 9.5]}}', wf)
writeLines('{"name": "LEVEL_shopage", "params": {"bins": [140.5, 199.5, 448.5, 640.5, 985.5, 1332.5, 1660.5]}}', wf)
writeLines('{"name": "LEVEL_COALESCE_NormalizeUserShopPref_UserQuInfo_Shopid_PrefShopGuess_SimilarityShops_UserQuInfo", "params": {"bins": [-0.2567810, 0.0222130, 0.0444835, 0.0665115, 0.1324750, 0.6107900, 0.8967650]}}', wf)
writeLines('{"name": "LEVEL_IF@3_CROSSSEEK_UserQuInfo_CONST@key=isactive_COALESCE_CROSSSEEK_LocCTR_Shopid_CROSSSEEK_LocCTRSPL_Shopid_CROSSSEEK_LocCTR_Shopid", "params": {"bins": [0.0385, 0.0575, 0.0765, 0.1035, 0.1385, 0.2035, 0.3155]}}', wf)
writeLines('{"name": "LEVEL_UvScore", "params": {"bins": [0.195738, 0.239193, 0.497140, 0.664338, 0.720340, 0.777804, 0.821543]}}', wf)
writeLines('{"name": "LEVEL_TimeRelevanceSeek_TimeRelevance_TimeSeg", "params": {"bins": [0.0000075, 0.0121575, 0.0177525, 0.0320875, 0.0412275, 0.0543575, 0.1082680]}}', wf)
writeLines('{"name": "LEVEL_CROSSSEEK_LocationTopShop_locationu_Shopid", "params": {"bins": [0.278762, 0.581915, 0.648944, 0.700099, 0.731588, 0.764078, 0.795624]}}', wf)
writeLines('{"name": "LEVEL_LocLandmark_locationu_Mallid", "params": {"bins": [0.0029985, 0.0031400, 0.0031600, 0.0406640, 0.0433140, 0.1610610, 0.5909770]}}', wf)
writeLines('{"name": "LEVEL_TimeSeg", "params": {"bins": [23.5, 29.5, 34.5, 36.5, 38.5, 43.5, 44.5]}}', wf)


close(wf)



head(colnames(df_coef))

tail(colnames(df_coef))


{"name": "ModelWeight", "params":{ "LEVEL_WILSON_allcategoryclicks_allcategoryviews_CONST@key=0.99-0":1.0, "LEVEL_WILSON_allcategoryclicks_allcategoryviews_CONST@key=0.99-1":2.0, "LEVEL_WILSON_allcategoryclicks_allcategoryviews_CONST@key=0.99-2":3.0, "LEVEL_WILSON_allcategoryclicks_allcategoryviews_CONST@key=0.99-3":4.0, "LEVEL_WILSON_allcategoryclicks_allcategoryviews_CONST@key=0.99-4":5.0, "LEVEL_WILSON_allcategoryclicks_allcategoryviews_CONST@key=0.99-5":6.0, "LEVEL_WILSON_allcategoryclicks_allcategoryviews_CONST@key=0.99-6":7.0, "LEVEL_WILSON_allcategoryclicks_allcategoryviews_CONST@key=0.99-7":8.0, "LEVEL_WILSON_allcategoryclicks_allcategoryviews_CONST@key=0.99":10.0}}

{"name": "LEVEL_WILSON_allcategoryclicks_allcategoryviews_CONST@key=0.99", "params": {"bins": [0.0154445,0.0295115,0.0355905,0.0459515,0.0589055,0.1072020,0.1431310]}}


{"name": "ModelWeight", "params":{ "WILSON_allcategoryclicks_allcategoryviews_CONST@key=0.99":10.0}}
{"name": "BiasCoef", "params": 5.0}
{"name": "CEIL_Pictotal", "params": {"bins": [50]}}
{"name": "LEVEL_WILSON_allcategoryclicks_allcategoryviews_CONST@key=0.99", "params": {"bins": [0.0154445,0.0295115,0.0355905,0.0459515,0.0589055,0.1072020,0.1431310]}}

#unique(str_sub(colnames(df_coef),1,10))




"ctr", "allcategoryctr", "realclick", "comm_dist", ""

findInterval(0.01, c(0.033195,0.044525,0.058245,0.086585,0.105515,0.137685,0.186965))


[1] "ctr"
0.033195 0.044525 0.058245 0.086585 0.105515 0.137685 0.186965
[1] "allcategoryctr"
0.0154445 0.0295115 0.0355905 0.0459515 0.0589055 0.1072020 0.1431310
[1] "realclick"
[1] -1.776040 -1.776030  0.468075  1.615130  2.379320  3.264690  4.251550
[1] "comm_dist"
[1]  22.9838  33.2987  48.0416  55.0764  75.9274 174.8770 419.7380
[1] "dist_ctr1"
[1] 0.0097105 0.0161365 0.0247515 0.0331915 0.0383945 0.0451495 0.0921950
[1] "distscore"
[1] 0.196173 0.306280 0.431181 0.757025
[1] "pics"
[1]    2.5   17.5   32.5  129.5  391.5  878.5 1595.5
[1] "loccvr"
[1] 0.0005 0.0065 0.0205 0.0395 0.0715 0.1295 0.2055
[1] "lastview"
[1] 0.5 1.5 3.5
[1] "totalreviews"
[1]   0.5   2.5   7.5  16.5  35.5  73.5 287.5
[1] "prefectreviews"
[1]  0.5  1.5  2.5  6.5 10.5 22.5
[1] "reviewslength"
[1]   14.5   16.5  311.5  816.5 1768.5 3165.5 8430.5
[1] "badreviews"
[1] 0.5 1.5 2.5 4.5 9.5
[1] "goodreviews"
[1]   0.5   1.5   5.5  13.5  31.5  84.5 344.5
[1] "shopage"
[1]  140.5  199.5  448.5  640.5  985.5 1332.5 1660.5
[1] "price_pref"
[1] 0.241722 0.241724 0.431922 0.431957 0.589999 0.590011 0.591649

[1] "shop_pref_extend"
[1] -0.2567810  0.0222130  0.0444835  0.0665115  0.1324750  0.6107900  0.8967650

[1] "spl"
[1] 0.0385 0.0575 0.0765 0.1035 0.1385 0.2035 0.3155

[1] "seg"
[1] 0.0000075 0.0121575 0.0177525 0.0320875 0.0412275 0.0543575 0.1082680

[1] "deal_percent"
[1] 0.0488945 0.0714545 0.0723080 0.3875490 0.4888010 0.5000250 0.8008020

[1] "uvscore"
[1] 0.195738 0.239193 0.497140 0.664338 0.720340 0.777804 0.821543

[1] "discount"
[1] 0.468697 0.887856 0.888195 0.990413 0.999335

[1] "basescore"
[1] 0.423571 0.502043 0.613729 0.691731 0.737577 0.789791 0.852432

[1] "distsensitivity"
[1] 0.0000095 0.5698050 0.6165290 0.6582130 0.7181830 0.7505890 0.8017130

[1] "topshop"
[1] 0.278762 0.581915 0.648944 0.700099 0.731588 0.764078 0.795624

[1] "mallpercent"
[1] 0.0029985 0.0031400 0.0031600 0.0406640 0.0433140 0.1610610 0.5909770

"timeseg"
[1] 23.5 29.5 34.5 36.5 38.5 43.5 44.5



{"name": "ModelWeight", "params":{ "LEVEL_WILSON_allcategoryclicks_allcategoryviews_CONST@key=0.99-0":1.0, "LEVEL_WILSON_allcategoryclicks_allcategoryviews_CONST@key=0.99-1":2.0, "LEVEL_WILSON_allcategoryclicks_allcategoryviews_CONST@key=0.99-2":3.0, "LEVEL_WILSON_allcategoryclicks_allcategoryviews_CONST@key=0.99-3":4.0, "LEVEL_WILSON_allcategoryclicks_allcategoryviews_CONST@key=0.99-4":5.0, "LEVEL_WILSON_allcategoryclicks_allcategoryviews_CONST@key=0.99-5":6.0, "LEVEL_WILSON_allcategoryclicks_allcategoryviews_CONST@key=0.99-6":7.0, "LEVEL_WILSON_allcategoryclicks_allcategoryviews_CONST@key=0.99-7":8.0, "LEVEL_WILSON_allcategoryclicks_allcategoryviews_CONST@key=0.99":10.0}}
{"name": "BiasCoef", "params": 6.0}
{"name": "LEVEL_WILSON_allcategoryclicks_allcategoryviews_CONST@key=0.99", "params": {"bins": [0.0154445,0.0295115,0.0355905,0.0459515,0.0589055,0.1072020,0.1431310]}}


curl 'http://localhost:6053/search/mainshop?query=term(categoryids,10),geo(gpoi,116.436805:39.916451,1000),term(shoptype,10)&notquery=term(shopid,3620737),term(power,1),term(weddingcloneshop,1)&filter=range(shoppower,40,51,true,false)&sort=desc(daocannew)&stat=count(rainbowpromoids),count(segmentscene),count(discountstatus)&limit=0,25&fl=shopid,shopname,shoptype,cityid,category1,isqueueable,paytype,hastakeaway,hasbooksetting,booktype,isorderdish,dealgroupid,dealgrouptype,dealgroupprice,dealgrouptitle,tgdefaultpic,contenttitle,marketprice,tgcurrentjoin,hasgroup,hasshortdeals,adshoptype,launchid,hasgroup,mopaystatus,categoryids,regionids,region1,region2,region3,categoryids,gpoi,businesshour,dist(gpoi,116.436805:39.916451)&info=clientversion:9.2.6,queryid:785e87f0-5a6b-4f66-a7b9-0b74daf7ae0a,clientip:10.69.53.143,userid:15640057,unrelatedguidefields:categoryids%3Bregionids,bizname:mainshop,networktype:wifi,geoFieldName:gpoi,poi:116.436805%3A39.916451+1000,shouldblockad:false,rewrite:Keyword-Request-Rewrite%3A%5B%5D%3Bunrelated-pre%3A%5B%5Bcategoryids%2C+regionids%5D%5D%3B,livecity:2,hotarea:true,modelreranksize:300,ganextindex:0,userip:58.132.201.87,needrulefeaturererank:true,platform:MAPI,useragent:MApi+1.1+%28dpscope+9.2.6+appstore%3B+iPhone+10.3.2+iPhone7%2C1%3B+a0d0%29,stattree:categoryids%3Bregionids%3Bmetrostationids%3Bhotregionids%3Bhotspotids,term_categoryids:10,needImFea:FoodCat1GBDTSeq4,algoversion:nearby_l2r_base,posoper%40982:add%4050,mobileplatform:2,shopcategryid:171,ab_itemchain:localdistscore%405.0%7CshopStatus%40100%7CbaseScore%401.0%7CshopBusinessNew%400.4%7Csegscore%400.2,elevateids:,pagecity:2,mappingmodel:FoodCat1IDLRSeq3,modelscoredetail:true,ModelScoreNormKey:10,querymaincategoryid:10,distanceselected:true,admergeenabled:false,pagemodule:nearbyshoplist,querytypeandchannelid:1_10,AdDownGradeRule:true,bu_constrained:1,needrankinfo:true,dpid:-595817224316475918,mobiledeviceid:22f8d1280475e45e832ea97f75419be2fcd08fbb,app:NearbyShopSearch,keys:,sorttype:1,categoryids:10,cityid:2,wifi:,bizClientIp:10.69.56.251,nearby_gpoi:116.436805%3A39.916451%2C1000,requestUuid:,gpoi:116.436805%3A39.916451' | python -m json.tool >/tmp/kai.csv



curl 'http://localhost:5053/search/mainshop?query=term(categoryids,10),geo(poi,121.49004364013672:31.28790283203125,5000)&sort=desc(dpscore)&limit=0,500&fl=poi,shopid,shopname,dealgroupid,dealgrouptype,tgcurrentjoin&info=clientversion:9.0.0,userlng:121.41544,clientip:192.168.224.41,queryid:5f75d97e-6fa4-44f2-839a-f05973cbfd83,userid:,unrelatedguidefields:categoryids%3Bregionids,geoFieldName:poi,poi:121.41544%3A31.21724,pagemodule:mainshoplist,locatecityid:1,dpid:4092218345799710917,mobiledeviceid:868721022533329,userip:180.166.152.90,app:PointShopSearch,platform:MAPI,needdynamicrange:true,useragent:MApi+1.1+%28com.dianping.v1+9.0.0+huawei1+ALE-UL00%3B+Android+5.0.2%29,stattree:categoryids%3Bregionids%3Bmetrostationids%3Bhotregionids%3Bhotspotids,sorttype:0,userlat:31.21724,needexpandrange:true,mobileplatform:1,cityid:1,wifi:,needcpmad:true,pagecity:1,modelscoredetail:true,mappingmodel:FoodCat1IDLRSeq3,modelscoredetail:true,needexplore:false' | python -m json.tool >/tmp/kai.csv






lambda
 [1] 0.0471907459 0.0429984501 0.0391785863 0.0356980687 0.0325267507
 [6] 0.0296371638 0.0270042798 0.0246052940 0.0224194275 0.0204277474
[11] 0.0186130026 0.0169594747 0.0154528418 0.0140800539 0.0128292207
[16] 0.0116895081 0.0106510444 0.0097048350 0.0088426842 0.0080571245
[21] 0.0073413518 0.0066891663 0.0060949192 0.0055534634 0.0050601090
[26] 0.0046105829 0.0042009914 0.0038277869 0.0034877369 0.0031778959
[31] 0.0028955804 0.0026383450 0.0024039617 0.0021904004 0.0019958113
[36] 0.0018185089 0.0016569576 0.0015097580 0.0013756353 0.0012534276
[41] 0.0011420766 0.0010406176 0.0009481720 0.0008639391 0.0007871891
[46] 0.0007172574 0.0006535382


----
购买20倍加权

colAUC(trainY, csvdata[train_index,"label"])  #0.714187
                1
0 vs. 1 0.7095187
> colAUC(testY, csvdata[-train_index,"label"])  #0.7135066
                1
0 vs. 1 0.7103736
>
> colAUC(trainY, csvdata[train_index,"real_order"])  #0.9703681
                1
0 vs. 1 0.9225952
> colAUC(testY, csvdata[-train_index,"real_order"])  #0.9270994
                1
0 vs. 1 0.9264819
--id类特征 LR，用LR离线训练一个纯ID类的模型，包括：shopid，城市id，iphone，星级。和线上GBDT做融合。看效果可行的话，考虑推广更多的分类型变量，接入在线学习
SET hive.cli.print.header=TRUE;
add jar /data/qin.zhang/l2r/train_l2r/fortrus-1.0.jar;
create temporary function SetDefault as 'com.dp.search.entity.SetDefaultValue';

select
  itemid shop_id,
  dpid,
  if(actioncountmap["click"] > 0,1,0) as label,
  case when actioncountmap['order'] > 0 and (actioncountmap['dp_waimai'] != 1 or actioncountmap['dp_waimai'] is null ) then 1 else  0 end as real_order,
  featuredatamap["IsDealShop"] as isdealshop,
  featuredatamap["IsHuiShop"] as ishuishop,
  featuredatamap["AND@1_ShopPower"] as hasstar,
  featuredatamap["IsTakeawayShop"] as istakeawayshop,
  featuredatamap["PartContains_ReceiptHoldDealGroupId_DealGroupID"] as holddeal,
  featuredatamap["ShopPower"] as shoppower,
  featuredatamap["IsFoodTopShop"] as istopshop,
  featuredatamap["OR@2_IsTuanDiscount_IsShanhuiDiscount"] as islijian,
  featuredatamap["IsBookingShop"] as isbookingshop,
  featuredatamap["IsFuShop"] as isfushop,
  featuredatamap["HasDealPromote"] as has_deal_promote,
  featuredatamap['COMP@=_CROSSSEEK_UserQuInfo_CONST@key=permanentcityid_INFO@cityid']  as is_permanent_city,
  featuredatamap['isNewUser']  as isnewuser,
  featuredatamap['CROSSSEEK_UserQuInfo_CONST@key=isactive']  as isactive,
  SetDefault(featuredatamap['COMP@=_QUERY@categoryid_CONST@key=10'], '-1', 1.0)  as is_food_level1,
  featuredatamap['DefaultValue_CROSSSEEK_UserQuInfo_CONST@key=gender_CONST@key=2']  as gender,
  featuredatamap['IsFakePic']  as isfake,
  featuredatamap['PhoneType_CONST@key=iphone']  as iphone,
  featuredatamap['INFO@cityid']  as cityid

  美食二级类目、
  性别_shopid、
  时间、星期、
  广告位和各种ID类特征


  连续特征离散化
from
    bi.dpsearch_online_query_featuredata
    where hp_cal_dt =  "${cal_dt}"
    and searchbusiness = "artsshop"
    and aggregatorbusiness = "search-feature"
    and model rlike 'FoodCat'

;


params <- list(objective="lambdarank", metric="l2")
model <- lgb.cv(params, dtrain, 10, nfold=5, min_data=1, learning_rate=1, early_stopping_rounds=10)



library("glmnet")
library("Matrix")
library("pROC")
library('caTools')
library(dummies)
library("rjson")
library("xgboost")
library(stringr)
library(parallel)


csvdata = read.csv("./id_v4.csv", sep='\t')
csvdata = read.csv("/data/kai.zhang/model_merge/id_oneday.csv", sep='\t')
csvdata = read.csv("/data/kai.zhang/model_merge/id_v2.csv", sep='\t')

csvdata = read.csv("/data/kai.zhang/model_merge/id_v3.csv", sep='\t')

str(csvdata)
csvdata[,1] = as.character(csvdata[,1])

csvdata <- csvdata[csvdata[,1] == "FoodCat1GBDTSeq4",]

index = which(csvdata[,1] %in% c("FoodCat1GBDTSeq2","FoodCat1GBDTSeq2|OLFoodCat1GBDTSeq2","FoodCat1GBDTSeq2|OLFoodCat1GBDTSeq2|FoodCat1GBDTSeq4","FoodCat1GBDTSeq4"))

csvdata = csvdata[index,]

#tapply(csvdata$model, summary)
#aggregate(csvdata, by=list(csvdata$model), FUN=summary)
csvdata[,"dpid"] <- NULL
csvdata[,"model"] <- NULL
csvdata[,"pos"] <-NULL
csvdata[,"cityid"] <- NULL

for(i in 3:ncol(csvdata)){
  if(class(csvdata[,i]) == "factor"){
    csvdata[,i] <- as.numeric(as.character(csvdata[,i]))
  }
}

csvdata[is.na(csvdata)] <- 0

remov <-colnames(csvdata)[apply(csvdata,2,max) == 0] #最大值是0的col
remov
summary(csvdata)

for (i in remov){
  csvdata[,i] <- NULL
}



factor_col <- c("shop_id","shoppower","is_permanent_city","gender","pricelevel","cityid","shopcategory1") #,"dpid"

factor_index <- which(colnames(csvdata) %in% factor_col)
for(i in factor_index){
  csvdata[,i] <- as.factor(csvdata[,i])
}


sparsedata <- sparse.model.matrix( ~ -1 + is_permanent_city + shoppower + gender + shop_id + pricelevel + shopcategory1, data=csvdata)

dup_sparsedata <- sparsedata

#-------连续型特征分档-----
#哪些col需要分档

features_selected <- c()
for(i in colnames(csvdata)){
  if(length(unique(csvdata[,i])) > 2){
    features_selected <- c(features_selected,i)
    print(i)
  }
}
setdiff(features_selected, features_selected1)

setdiff(features_selected1, features_selected)


features_selected1 <- c("ctr","allcategoryctr","realclick","comm_dist","dist_ctr1","distscore","pics","loccvr","lastview","totalreviews","prefectreviews","reviewslength","badreviews","goodreviews","shopage","price_pref","shop_pref_extend","spl","seg","deal_percent","uvscore","discount","basescore","distsensitivity","topshop","mallpercent","timeseg","yestdayctr","ocr","todayctr", "shopavgprice", "catprefwithgeo","density_thirty30","priceprefwithgeo")


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

#newadded
feature_map$density_thirty30 <- "LocDensity_locationu_CONST@key=STAR30"
feature_map$density_thirty35 <- "LocDensity_locationu_CONST@key=STAR35"
feature_map$catprefwithgeo <- "PrefWithGeo_CONST@key=catpref_IsLocate_UserQuInfo_UserQuInfo_MainCategoryIds"
feature_map$shopavgprice <- "ShopAvgPrice"
feature_map$todayctr <- "WILSON_ShopRealtimeInfo_ShopRealtimeTraffic_CONST@key=10_CONST@key=1_CONST@key=1_CONST@key=0_ShopRealtimeInfo_ShopRealtimeTraffic_CONST@key=10_CONST@key=1_CONST@key=0_CONST@key=0_CONST@key=0.95"
feature_map$ismallshop<-"ShopInMall_LandmarkInfo_Mallid"
feature_map$ocr<- "CROSSSEEK_MAP@儑@:_FoodShopOvr_CONCAT@3_CONST@key=L_CONST@key=m_QUERY@categoryid"
feature_map$isinMall <- "ShopInMall_LandmarkInfo_Mallid"
feature_map$priceprefwithgeo <- "PrefWithGeo_CONST@key=pricepref_IsLocate_UserQuInfo_UserQuInfo_UserCat1PercentileList"
feature_map$yestdayctr <-"WILSON_ShopRealtimeInfo_ShopRealtimeTraffic_CONST@key=10_CONST@key=1_CONST@key=1_CONST@key=1_ShopRealtimeInfo_ShopRealtimeTraffic_CONST@key=10_CONST@key=1_CONST@key=0_CONST@key=1_CONST@key=0.95" 


setdiff(names(feature_map),features_selected)

setdiff(features_selected,names(feature_map))
#,) #需要分档
#"dist_poi"  #csvdata[,"dist_poi"] <- NULL


set.seed(100)
train_index <- sample(1:nrow(csvdata),nrow(csvdata) * 0.7, replace = FALSE) 

gsample.weight <- sapply(csvdata$real_order[train_index], function(x) {ifelse(x==1,60,1)}) 
sparsedata <- dup_sparsedata

save.image(file="./id_modelv3_addfeature.RData") 

load(file="./id_modelv3_addfeature.RData") 

bin_file <- file("./cutoff_model_depth3.json", "w" )

cross_bin <- NULL

for (fea in features_selected) { 
    gtrain <- csvdata[train_index,fea]
    gtrain.label<- csvdata$label[train_index]
    gtest <- csvdata[-train_index,fea]
    gtest.label<- csvdata$label[-train_index]
    ctrgbdt <- xgboost(data = as.matrix(gtrain), label = gtrain.label, max.depth = 3, nround = 1, objective = "binary:logistic", eval_metric = "auc", weight=gsample.weight, verbose = 0, tree_method= 'exact')
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
    cross_bin <- cbind(cross_bin,interval_OHE[,1])
}
close(bin_file)


list <- setdiff(colnames(csvdata),c("label","real_order","dist_poi","pos",factor_col,features_selected))
setdiff(list, colnames(csvdata))

summary(csvdata[,list])

##判断哪些是多类型的离散特征值
for( i in list) {
  if(length(unique(csvdata[,i])) > 2){
    print(colnames(csvdata)[i])
  }
}

others <- Matrix(as.matrix(csvdata[,list]), sparse=TRUE)
sparseMtx <- cbind(sparsedata,others)

object.size(sparsedata)

#save.image(file="id_raw_v3.RData")
save.image(file="./id_modelv3_addfeature.RData") 

#load("id_raw_v3.RData")


dim(sparseMtx)


set.seed(100)
train_index <- sample(1:nrow(csvdata),nrow(csvdata) * 0.1, replace = FALSE) 

train <- sparseMtx[train_index,]
test <- sparseMtx[-train_index,]
train.label<- csvdata$label[train_index]

for(iweight in c(5,15,20,25,30,45,50)) {
    data_sample_weight <- csvdata[train_index,"real_order"] * iweight
    data_sample_weight[data_sample_weight == 0] <- 1

    ###----
    #data_sample_weight <- csvdata[train_index,"label"] * 10
    #data_sample_weight[data_sample_weight == 0] <- 1

    #tmp <- (csvdata[train_index,"pos"] + 1) * csvdata[train_index,"label"]
    #tmp[tmp == 0] <- 1
    #data_sample_weight<- (10/(1+ 1*exp(-tmp)))

    ####----

    fit <- glmnet(train, as.factor(train.label),family="binomial", alpha = 1,weights = data_sample_weight, lambda=c(0.001))

    #save.image(file="./id_modelv3_addfeature.RData") 

    #load(file="./id_modelv3_addfeature.RData")

    trainY <- predict(fit,train,s=0.001,type="response")  
    testY <- predict(fit,test,s=0.001,type="response") 

    print(colAUC(trainY, csvdata[train_index,"label"]))  #0.714187
    print(colAUC(testY, csvdata[-train_index,"label"]))  #0.7135066

    print(colAUC(trainY, csvdata[train_index,"real_order"]))  #0.9703681
    print(colAUC(testY, csvdata[-train_index,"real_order"]))  #0.9270994

    coeff <- coef(fit,s=0.001)
    print(length(coeff[which(coeff!=0),]))
}

coeff <- coef(fit)
length(coeff[which(coeff!=0),])



coeff[which(coeff!=0),]


df_coef <- data.frame(t(as.matrix(coeff[which(coeff!=0),])))

#save.image(file="/data/kai.zhang/model_merge/id_model1.RData")

#load("/data/kai.zhang/model_merge/id_model1.RData")

feature_map = list()
feature_map$ctr <- "FoodShopCtrCat1"
feature_map$allcategoryctr <- 

tmp <- tail(colnames(df_coef),100)

head(colnames(df_coef),100)

tail(colnames(df_coef),100)

grep("gender",colnames(df_coef))

sub(pattern = "[cityid]",replacement = "OHE_INFO@cityid-",x = "cityid2158") 


colnames(csvdata)
library(stringr)
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "cityid",replacement = "OHE_INFO@cityid-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "shop_id",replacement = "OHE_Shopid-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "shoppower",replacement = "OHE_ShopPower-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "gender",replacement = "OHE_DefaultValue_CROSSSEEK_UserQuInfo_CONST@key=gender_CONST@key=2-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "shopcategory1",replacement = "OHE_category1-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "is_permanent_city",replacement = "OHE_COMP@=_CROSSSEEK_UserQuInfo_CONST@key=permanentcityid_INFO@cityid-")


##LEVEL特征前缀从-1开始
level_index<-grep("LEVEL",colnames(df_coef))
colnames(df_coef)[level_index]
prefix <- substr(colnames(df_coef)[level_index],1,nchar(colnames(df_coef)[level_index])-1)
appends <- as.numeric(substr(colnames(df_coef)[level_index],nchar(colnames(df_coef)[level_index]),nchar(colnames(df_coef)[level_index])))-1
colnames(df_coef)[level_index] <- paste(prefix,appends,sep="")
###LEVEL END

unique(colnames(df_coef)[level_index])


colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_ctr_",replacement = "LEVEL_FoodShopCtrCat1-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_allcategoryctr_",replacement = "LEVEL_WILSON_allcategoryclicks_allcategoryviews_CONST@key=0.99-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_realclick_",replacement = "LEVEL_FoodRT_CROSSSEEK_RealTimeShopClick_Shopid-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_comm_dist_",replacement = "LEVEL_CommonDist_Poi_GPoi-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_dist_ctr1_",replacement = "LEVEL_PRODUCT_DefaultDistScore_CommonDist_Poi_GPoi_FoodShopCtrCat1-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_distscore_",replacement = "LEVEL_DefaultDistScore_CommonDist_Poi_GPoi-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_pics_",replacement = "LEVEL_Pictotal-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_loccvr_",replacement = "LEVEL_CROSSSEEK_LocCVR_Shopid-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_lastview_",replacement = "LEVEL_CROSSSEEK_RealTimeShopLastView_Shopid-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_totalreviews_",replacement = "LEVEL_TotalReviews-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_badreviews_",replacement = "LEVEL_BadReviews-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_shopage_",replacement = "LEVEL_shopage-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_shop_pref_extend_",replacement = "LEVEL_COALESCE_NormalizeUserShopPref_UserQuInfo_Shopid_PrefShopGuess_SimilarityShops_UserQuInfo-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_spl_",replacement = "LEVEL_IF@3_CROSSSEEK_UserQuInfo_CONST@key=isactive_COALESCE_CROSSSEEK_LocCTR_Shopid_CROSSSEEK_LocCTRSPL_Shopid_CROSSSEEK_LocCTR_Shopid-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_uvscore_",replacement = "LEVEL_UvScore-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_seg_",replacement = "LEVEL_TimeRelevanceSeek_TimeRelevance_TimeSeg-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_topshop_",replacement = "LEVEL_CROSSSEEK_LocationTopShop_locationu_Shopid-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_mallpercent_",replacement = "LEVEL_LocLandmark_locationu_Mallid-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_timeseg_",replacement = "LEVEL_TimeSeg-")

colnames(df_coef)[which(colnames(df_coef) == "guess_star")]  = "IsStarGuess_Starsource"
colnames(df_coef)[which(colnames(df_coef) == "smallcity")]  = "Not_Contains@,_CONST@key=1,2,4,7,10,3,5,8,6,16,17,21,9_INFO@cityid"
colnames(df_coef)[which(colnames(df_coef) == "isdealshop")]  = "IsDealShop"
colnames(df_coef)[which(colnames(df_coef) == "ishuishop")]  = "IsHuiShop"
colnames(df_coef)[which(colnames(df_coef) == "hasstar")]  = "AND@1_ShopPower"
colnames(df_coef)[which(colnames(df_coef) == "istakeawayshop")]  = "IsTakeawayShop"
colnames(df_coef)[which(colnames(df_coef) == "holddeal")]  = "PartContains_ReceiptHoldDealGroupId_DealGroupID"
colnames(df_coef)[which(colnames(df_coef) == "istopshop")]  = "IsFoodTopShop"
colnames(df_coef)[which(colnames(df_coef) == "islijian")]  = "OR@2_IsTuanDiscount_IsShanhuiDiscount"
colnames(df_coef)[which(colnames(df_coef) == "isbookingshop")]  = "IsBookingShop"
colnames(df_coef)[which(colnames(df_coef) == "isfushop")]  = "IsFuShop"
colnames(df_coef)[which(colnames(df_coef) == "isnewuser")]  = "isNewUser"
colnames(df_coef)[which(colnames(df_coef) == "isactive")]  = "CROSSSEEK_UserQuInfo_CONST@key=isactive"
colnames(df_coef)[which(colnames(df_coef) == "is_food_level1")]  = "COMP@=_QUERY@categoryid_CONST@key=10"
colnames(df_coef)[which(colnames(df_coef) == "isfake")]  = "IsFakePic"
colnames(df_coef)[which(colnames(df_coef) == "iphone")]  = "PhoneType_CONST@key=iphone"
colnames(df_coef)[which(colnames(df_coef) == "is_food_level1")]  = "COMP@=_QUERY@categoryid_CONST@key=10"
colnames(df_coef)[which(colnames(df_coef) == "smallcity")]  = "Not_Contains@,_CONST@key=1,2,4,7,10,3,5,8,6,16,17,21,9_INFO@cityid"


biasdict <- paste(c('{"name":"BiasCoef", "params":',df_coef[,"X.Intercept."],"}"),collapse=" ")
df_coef[,"X.Intercept."] <- NULL

featuresWeight <- toJSON(unlist(unname(split(df_coef,1:nrow(df_coef)))))
weightdict <- paste(c('{"name":"ModelWeight","params":',featuresWeight,"}"),collapse=" ")

wf <- file("/data/kai.zhang/model_merge/FoodCat1IdLRSeq4.json", "w" )
writeLines( weightdict, wf)
writeLines( biasdict, wf)

writeLines('{"name": "LEVEL_FoodShopCtrCat1", "params": {"bins": [0.033195, 0.044525, 0.058245, 0.086585, 0.105515, 0.137685, 0.186965]}}', wf)
writeLines('{"name": "LEVEL_WILSON_allcategoryclicks_allcategoryviews_CONST@key=0.99", "params": {"bins": [0.0154445, 0.0295115, 0.0355905, 0.0459515, 0.0589055, 0.1072020, 0.1431310]}}', wf)
writeLines('{"name": "LEVEL_FoodRT_CROSSSEEK_RealTimeShopClick_Shopid", "params": {"bins": [-1.776040, -1.776030, 0.468075, 1.615130, 2.379320, 3.264690, 4.251550]}}', wf)
writeLines('{"name": "LEVEL_CommonDist_Poi_GPoi", "params": {"bins": [22.9838, 33.2987, 48.0416, 55.0764, 75.9274, 174.8770, 419.7380]}}', wf)
writeLines('{"name": "LEVEL_PRODUCT_DefaultDistScore_CommonDist_Poi_GPoi_FoodShopCtrCat1", "params": {"bins": [0.0097105, 0.0161365, 0.0247515, 0.0331915, 0.0383945, 0.0451495, 0.0921950]}}', wf)
writeLines('{"name": "LEVEL_DefaultDistScore_CommonDist_Poi_GPoi", "params": {"bins": [0.196173, 0.306280, 0.431181, 0.757025]}}', wf)
writeLines('{"name": "LEVEL_Pictotal", "params": {"bins": [2.5, 17.5, 32.5, 129.5, 391.5, 878.5, 1595.5]}}', wf)
writeLines('{"name": "LEVEL_CROSSSEEK_LocCVR_Shopid", "params": {"bins": [0.0005, 0.0065, 0.0205, 0.0395, 0.0715, 0.1295, 0.2055]}}', wf)
writeLines('{"name": "LEVEL_CROSSSEEK_RealTimeShopLastView_Shopid", "params": {"bins": [0.5, 1.5, 3.5]}}', wf)
writeLines('{"name": "LEVEL_TotalReviews", "params": {"bins": [0.5, 2.5, 7.5, 16.5, 35.5, 73.5, 287.5]}}', wf)
writeLines('{"name": "LEVEL_BadReviews", "params": {"bins": [0.5, 1.5, 2.5, 4.5, 9.5]}}', wf)
writeLines('{"name": "LEVEL_shopage", "params": {"bins": [140.5, 199.5, 448.5, 640.5, 985.5, 1332.5, 1660.5]}}', wf)
writeLines('{"name": "LEVEL_COALESCE_NormalizeUserShopPref_UserQuInfo_Shopid_PrefShopGuess_SimilarityShops_UserQuInfo", "params": {"bins": [-0.2567810, 0.0222130, 0.0444835, 0.0665115, 0.1324750, 0.6107900, 0.8967650]}}', wf)
writeLines('{"name": "LEVEL_IF@3_CROSSSEEK_UserQuInfo_CONST@key=isactive_COALESCE_CROSSSEEK_LocCTR_Shopid_CROSSSEEK_LocCTRSPL_Shopid_CROSSSEEK_LocCTR_Shopid", "params": {"bins": [0.0385, 0.0575, 0.0765, 0.1035, 0.1385, 0.2035, 0.3155]}}', wf)
writeLines('{"name": "LEVEL_UvScore", "params": {"bins": [0.195738, 0.239193, 0.497140, 0.664338, 0.720340, 0.777804, 0.821543]}}', wf)
writeLines('{"name": "LEVEL_TimeRelevanceSeek_TimeRelevance_TimeSeg", "params": {"bins": [0.0000075, 0.0121575, 0.0177525, 0.0320875, 0.0412275, 0.0543575, 0.1082680]}}', wf)
writeLines('{"name": "LEVEL_CROSSSEEK_LocationTopShop_locationu_Shopid", "params": {"bins": [0.278762, 0.581915, 0.648944, 0.700099, 0.731588, 0.764078, 0.795624]}}', wf)
writeLines('{"name": "LEVEL_LocLandmark_locationu_Mallid", "params": {"bins": [0.0029985, 0.0031400, 0.0031600, 0.0406640, 0.0433140, 0.1610610, 0.5909770]}}', wf)
writeLines('{"name": "LEVEL_TimeSeg", "params": {"bins": [23.5, 29.5, 34.5, 36.5, 38.5, 43.5, 44.5]}}', wf)


close(wf)



head(colnames(df_coef))

tail(colnames(df_coef))


{"name": "ModelWeight", "params":{ "LEVEL_WILSON_allcategoryclicks_allcategoryviews_CONST@key=0.99-0":1.0, "LEVEL_WILSON_allcategoryclicks_allcategoryviews_CONST@key=0.99-1":2.0, "LEVEL_WILSON_allcategoryclicks_allcategoryviews_CONST@key=0.99-2":3.0, "LEVEL_WILSON_allcategoryclicks_allcategoryviews_CONST@key=0.99-3":4.0, "LEVEL_WILSON_allcategoryclicks_allcategoryviews_CONST@key=0.99-4":5.0, "LEVEL_WILSON_allcategoryclicks_allcategoryviews_CONST@key=0.99-5":6.0, "LEVEL_WILSON_allcategoryclicks_allcategoryviews_CONST@key=0.99-6":7.0, "LEVEL_WILSON_allcategoryclicks_allcategoryviews_CONST@key=0.99-7":8.0, "LEVEL_WILSON_allcategoryclicks_allcategoryviews_CONST@key=0.99":10.0}}

{"name": "LEVEL_WILSON_allcategoryclicks_allcategoryviews_CONST@key=0.99", "params": {"bins": [0.0154445,0.0295115,0.0355905,0.0459515,0.0589055,0.1072020,0.1431310]}}


{"name": "ModelWeight", "params":{ "WILSON_allcategoryclicks_allcategoryviews_CONST@key=0.99":10.0}}
{"name": "BiasCoef", "params": 5.0}
{"name": "CEIL_Pictotal", "params": {"bins": [50]}}
{"name": "LEVEL_WILSON_allcategoryclicks_allcategoryviews_CONST@key=0.99", "params": {"bins": [0.0154445,0.0295115,0.0355905,0.0459515,0.0589055,0.1072020,0.1431310]}}

#unique(str_sub(colnames(df_coef),1,10))




"ctr", "allcategoryctr", "realclick", "comm_dist", ""

findInterval(0.01, c(0.033195,0.044525,0.058245,0.086585,0.105515,0.137685,0.186965))






lambda
 [1] 0.0471907459 0.0429984501 0.0391785863 0.0356980687 0.0325267507
 [6] 0.0296371638 0.0270042798 0.0246052940 0.0224194275 0.0204277474
[11] 0.0186130026 0.0169594747 0.0154528418 0.0140800539 0.0128292207
[16] 0.0116895081 0.0106510444 0.0097048350 0.0088426842 0.0080571245
[21] 0.0073413518 0.0066891663 0.0060949192 0.0055534634 0.0050601090
[26] 0.0046105829 0.0042009914 0.0038277869 0.0034877369 0.0031778959
[31] 0.0028955804 0.0026383450 0.0024039617 0.0021904004 0.0019958113
[36] 0.0018185089 0.0016569576 0.0015097580 0.0013756353 0.0012534276
[41] 0.0011420766 0.0010406176 0.0009481720 0.0008639391 0.0007871891
[46] 0.0007172574 0.0006535382


----
购买20倍加权

colAUC(trainY, csvdata[train_index,"label"])  #0.714187
                1
0 vs. 1 0.7095187
> colAUC(testY, csvdata[-train_index,"label"])  #0.7135066
                1
0 vs. 1 0.7103736
>
> colAUC(trainY, csvdata[train_index,"real_order"])  #0.9703681
                1
0 vs. 1 0.9225952
> colAUC(testY, csvdata[-train_index,"real_order"])  #0.9270994
                1
0 vs. 1 0.9264819
