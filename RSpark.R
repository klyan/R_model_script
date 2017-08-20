sparkR

Sys.setenv(SPARK_HOME="/usr/local/hadoop/spark-release")

Sys.setenv(YARN_CONF_DIR=...)
Sys.setenv(SPARK_CONF_DIR=...)
Sys.setenv(LD_LIBRARY_PATH=...)
Sys.setenv(SPARKR_SUBMIT_ARGS=...)


http://spark.rstudio.com/deployment.html

SELECT * FROM bi.dpdim_dp_shop WHERE hp_valid_end_dt = "3000-12-31" limit 100;


library("dplyr")
library("sparklyr")

config <- spark_config()
config$`sparklyr.shell.driver-memory` <- "4G"
config$`sparklyr.shell.executor-memory` <- "4G"
config$`spark.yarn.executor.memoryOverhead` <- "1500"


#Sys.setenv("SPARK_MEM" = "13g")

# Set driver and executor memory allocations

#config$spark.driver.memory <- "4G"
#config$spark.executor.memory <- "1G"

sc <- spark_connect(master = "yarn-client", config = config)#, version="1.6.3", spark_home = '/usr/local/hadoop/spark-1.6.3-bin-cosmos-spark-1.6.3-dp_1/')

spark_disconnect(sc)

sdf_copy_to

library(DBI)
dbListTables(sc)
dbGetQuery(sc, "use database_name")
data_tbl3 <- dbGetQuery(sc, "SELECT * from dpdim_dp_shop WHERE hp_valid_end_dt = \"3000-12-31\"")
dbListFields(sc, data_tbl3)

src_tbls(sc) #列出可用的表


raw_train <- dbGetQuery(sc, sql1)
dbListFields(sc, data_tbl3)

load("lasso_ctr.RData")

spark_version(sc)

tbls <- ctr_trainData[,ctr_selected_feature]

ctr_train_2 <- sdf_copy_to(sc,ctr_trainData[,ctr_selected_feature])


ctr_train_2 <- copy_to(sc,tbls)

ctr_train_2 <- sdf_copy_to(sc,tbls)


daocandatatrain <- spark_read_csv(sc, daocandata0315, path="file://data/kai.zhang/data0315.csv", repartition = 0)


# 将mtcar数据集复制到spark
mtcars_tbl <- copy_to(sc, mtcars)

# 先对数据做变换，然后将数据集分割为训练集和测试集
partitions <- mtcars_tbl %>%
  filter(hp >= 100) %>%
  mutate(cyl8 = cyl == 8) %>%
  sdf_partition(training = 0.5, test = 0.5, seed = 1099)

# 对训练数据集做模型拟合
fit <- partitions$training %>%
  ml_linear_regression(response = "mpg", features = c("wt", "cyl"))



http://codecloud.net/12906.html

https://www.r-bloggers.com/extending-sparklyr-to-compute-cost-for-k-means-on-yarn-cluster-with-spark-ml-library/

http://spark.rstudio.com/images/sparklyr-cheatsheet.pdf


density_thirty
price_pref
haspic
is_food_level1
seg
ocr
spl
realclick
ishuishop
hasstar
loccvr
topshop
discount
lastview
isnewuser
log_ceil_pictotal
ctr
distscore
isdealshop
guess_star
shop_pref_extend

------



