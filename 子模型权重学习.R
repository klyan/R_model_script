

library("glmnet")
library("Matrix")
library("pROC")
library('caTools')
library("rjson")
library(stringr)

csvdata = read.csv(file="modelscore.csv",sep="\t")

csvdata = read.csv(file="OL_GBDT_model_score.csv",sep="\t")

csvdata[,"query_id"] <- NULL
csvdata[,"shop_id"] <- NULL


str(csvdata)

csvdata <- na.omit(csvdata)

summary(csvdata)


gbdtscore           lrscore
Min.   :0.005869   Min.   :0.00536
1st Qu.:0.033355   1st Qu.:0.03361
Median :0.056830   Median :0.04782
Mean   :0.108110   Mean   :0.07393
3rd Qu.:0.115076   3rd Qu.:0.07720
Max.   :0.998804   Max.   :0.99347


modelfeatures = c("gbdtscore","lrscore")

modelfeatures = c("gbdtscore","olscore")

set.seed(100)
train_index <- sample(1:nrow(csvdata),nrow(csvdata) * 0.7, replace = FALSE) 



train <- as.matrix(csvdata[train_index,modelfeatures])
test <- as.matrix(csvdata[-train_index,modelfeatures])
train.label<- csvdata$label[train_index]




data_sample_weight <- csvdata[train_index,"real_order"] * 200
data_sample_weight[data_sample_weight == 0] <- 1

fit <- glmnet(train, csvdata$real_order[train_index],family="gaussian", alpha =  0, weights = data_sample_weight, intercept=FALSE, nlambda=400)

fit <- glmnet(train, train.label,family="gaussian", alpha =  0, weights = data_sample_weight, intercept=FALSE, lower.limits = 0, lambda=c(0.000001))

fit <- glmnet(train, csvdata$real_order[train_index],family="gaussian", alpha =  0, weights = data_sample_weight, intercept=FALSE, lower.limits = 0, nlambda=200)


predTr <- predict(fit,train,s=min(fit$lambda),type="response")
predTs <- predict(fit,test,s=min(fit$lambda),type="response")

colAUC(predTr, csvdata[train_index,"label"])
colAUC(predTs, csvdata[-train_index,"label"])

colAUC(csvdata[train_index,"gbdtscore"], csvdata[train_index,"label"])
colAUC(csvdata[-train_index,"gbdtscore"], csvdata[-train_index,"label"])

#colAUC(csvdata[train_index,"lrscore"], csvdata[train_index,"label"])
#colAUC(csvdata[-train_index,"lrscore"], csvdata[-train_index,"label"])

print("order auc:")
colAUC(predTr, csvdata[train_index,"real_order"])
colAUC(predTs, csvdata[-train_index,"real_order"])

colAUC(csvdata[train_index,"gbdtscore"], csvdata[train_index,"real_order"])
colAUC(csvdata[-train_index,"gbdtscore"], csvdata[-train_index,"real_order"])

#colAUC(csvdata[train_index,"lrscore"], csvdata[train_index,"real_order"])
#colAUC(csvdata[-train_index,"lrscore"], csvdata[-train_index,"real_order"])

coeff <- coef(fit,s=min(fit$lambda))
coeff[which(coeff!=0),]



for ( s_w in seq(50,130,by=10)) {

print(s_w)
data_sample_weight <- csvdata[train_index,"real_order"] * s_w
data_sample_weight[data_sample_weight == 0] <- 1

cvfit <- cv.glmnet(train, csvdata$label[train_index], family="gaussian", alpha=0, weights=data_sample_weight, intercept=FALSE,
 lower.limits = 0, nfolds=5, parallel=TRUE, type.measure="mse", lambda.min.ratio=0.00001, nlambda=100)

predTr <- predict(cvfit,train,s=c(cvfit$lambda.min,cvfit$lambda.1se),type="response")
predTs <- predict(cvfit,test,s=c(cvfit$lambda.min,cvfit$lambda.1se),type="response")

print("click auc")

print(colAUC(predTr, csvdata[train_index,"label"]))
print(colAUC(predTs, csvdata[-train_index,"label"]))

print(colAUC(csvdata[train_index,"gbdtscore"], csvdata[train_index,"label"]))   #0.6669963
print(colAUC(csvdata[-train_index,"gbdtscore"], csvdata[-train_index,"label"])) #0.6661578

print("order auc")

print(colAUC(predTr, csvdata[train_index,"real_order"]))
print(colAUC(predTs, csvdata[-train_index,"real_order"]))

print(colAUC(csvdata[train_index,"gbdtscore"], csvdata[train_index,"real_order"]))   #0.9233628
print(colAUC(csvdata[-train_index,"gbdtscore"], csvdata[-train_index,"real_order"])) #0.9209611


print(coef(cvfit,s=c(cvfit$lambda.min,cvfit$lambda.1se)))

print(coef(cvfit,s=c(cvfit$lambda.min)))

cvfit$lambda.min
cvfit$lambda.1se

print("-------")

}



[1] 130

[1] "click auc"
                1         2
0 vs. 1 0.6679533 0.6693917
               1        2
0 vs. 1 0.667125 0.668607
             [,1]
0 vs. 1 0.6669963
             [,1]
0 vs. 1 0.6661578
[1] "order auc"
                1         2
0 vs. 1 0.9232584 0.9223712
                1         2
0 vs. 1 0.9207393 0.9197289
             [,1]
0 vs. 1 0.9233628
             [,1]
0 vs. 1 0.9209611
3 x 2 sparse Matrix of class "dgCMatrix"
                    1         2
(Intercept) .         .
gbdtscore   0.9648542 0.7944042
lrscore     0.1062276 0.3405131
3 x 1 sparse Matrix of class "dgCMatrix"
                    1
(Intercept) .
gbdtscore   0.9648542
lrscore     0.1062276
[1] "-------"
[1] 135
[1] "click auc"
                1         2
0 vs. 1 0.6678778 0.6688216
                1         2
0 vs. 1 0.6670482 0.6680161
             [,1]
0 vs. 1 0.6669963
             [,1]
0 vs. 1 0.6661578
[1] "order auc"
               1         2
0 vs. 1 0.923278 0.9228512
                1        2
0 vs. 1 0.9207674 0.920249
             [,1]
0 vs. 1 0.9233628
             [,1]
0 vs. 1 0.9209611
3 x 2 sparse Matrix of class "dgCMatrix"
                     1         2
(Intercept) .          .
gbdtscore   0.97718415 0.8751188
lrscore     0.09671434 0.2405236
3 x 1 sparse Matrix of class "dgCMatrix"
                     1
(Intercept) .
gbdtscore   0.97718415
lrscore     0.09671434
[1] "-------"
[1] 140
[1] "click auc"
               1         2
0 vs. 1 0.667805 0.6688953
                1         2
0 vs. 1 0.6669741 0.6680924
             [,1]
0 vs. 1 0.6669963
             [,1]
0 vs. 1 0.6661578
[1] "order auc"
                1         2
0 vs. 1 0.9232947 0.9227996
                1         2
0 vs. 1 0.9207925 0.9201913
             [,1]
0 vs. 1 0.9233628
             [,1]
0 vs. 1 0.9209611
3 x 2 sparse Matrix of class "dgCMatrix"
                    1         2
(Intercept) .         .
gbdtscore   0.9888682 0.8702393
lrscore     0.0876789 0.2544429
3 x 1 sparse Matrix of class "dgCMatrix"
                    1
(Intercept) .
gbdtscore   0.9888682
lrscore     0.0876789
[1] "-------"
[1] 145
[1] "click auc"
                1         2
0 vs. 1 0.6677302 0.6692083
                1         2
0 vs. 1 0.6668981 0.6684167
             [,1]
0 vs. 1 0.6669963
             [,1]
0 vs. 1 0.6661578
[1] "order auc"
                1         2
0 vs. 1 0.9233095 0.9225463
                1         2
0 vs. 1 0.9208166 0.9199163
             [,1]
0 vs. 1 0.9233628
             [,1]
0 vs. 1 0.9209611
3 x 2 sparse Matrix of class "dgCMatrix"
                     1         2
(Intercept) .          .
gbdtscore   1.00032314 0.8324873
lrscore     0.07855213 0.3115972
3 x 1 sparse Matrix of class "dgCMatrix"
                     1
(Intercept) .
gbdtscore   1.00032314
lrscore     0.07855213
