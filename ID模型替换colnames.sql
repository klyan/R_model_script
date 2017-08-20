



LEVEL_WILSON_ShopRealtimeInfo_ShopRealtimeTraffic_CONST@key=10_CONST@key=1_CONST@key=1_CONST@key=1_ShopRealtimeInfo_ShopRealtimeTraffic_CONST@key=10_CONST@key=1_CONST@key=0_CONST@key=1_CONST@key=0.95

LEVEL_WILSON_ShopRealtimeInfo_ShopRealtimeTraffic_CONST@key=10_CONST@key=1_CONST@key=1_CONST@key=0_ShopRealtimeInfo_ShopRealtimeTraffic_CONST@key=10_CONST@key=1_CONST@key=0_CONST@key=0_CONST@key=0.95

LEVEL_PrefWithGeo_CONST@key=catpref_IsLocate_UserQuInfo_UserQuInfo_MainCategoryIds
LEVEL_LocDensity_locationu_CONST@key=STAR30
LEVEL_PrefWithGeo_CONST@key=pricepref_IsLocate_UserQuInfo_UserQuInfo_UserCat1PercentileList




colnames(df_coef)[which(colnames(df_coef) == "LEVEL_LocDensity_locationu_CONST@key=STAR30-.1")] = "LEVEL_LocDensity_locationu_CONST@key=STAR30--1"

colnames(df_coef)[which(colnames(df_coef) == "LEVEL_PrefWithGeo_CONST@key=catpref_IsLocate_UserQuInfo_UserQuInfo_MainCategoryIds-.1")] = "LEVEL_PrefWithGeo_CONST@key=catpref_IsLocate_UserQuInfo_UserQuInfo_MainCategoryIds--1"

colnames(df_coef)[which(colnames(df_coef) == "LEVEL_WILSON_ShopRealtimeInfo_ShopRealtimeTraffic_CONST@key=10_CONST@key=1_CONST@key=1_CONST@key=0_ShopRealtimeInfo_ShopRealtimeTraffic_CONST@key=10_CONST@key=1_CONST@key=0_CONST@key=0_CONST@key=0.95-.1")] = "LEVEL_WILSON_ShopRealtimeInfo_ShopRealtimeTraffic_CONST@key=10_CONST@key=1_CONST@key=1_CONST@key=0_ShopRealtimeInfo_ShopRealtimeTraffic_CONST@key=10_CONST@key=1_CONST@key=0_CONST@key=0_CONST@key=0.95--1"

colnames(df_coef)[which(colnames(df_coef) == "LEVEL_FoodShopCtrCat1-.1")] = "LEVEL_FoodShopCtrCat1--1"

colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_WILSON_allcategoryclicks_allcategoryviews_CONST@key=0.99-.1",replacement = "LEVEL_WILSON_allcategoryclicks_allcategoryviews_CONST@key=0.99--1")

colnames(df_coef)[which(colnames(df_coef) == "LEVEL_FoodRT_CROSSSEEK_RealTimeShopClick_Shopid-.1")] = "LEVEL_FoodRT_CROSSSEEK_RealTimeShopClick_Shopid--1"

colnames(df_coef)[which(colnames(df_coef) == "LEVEL_CommonDist_Poi_GPoi-.1")] = "LEVEL_CommonDist_Poi_GPoi--1"


colnames(df_coef)[which(colnames(df_coef) == "LEVEL_PRODUCT_DefaultDistScore_CommonDist_Poi_GPoi_FoodShopCtrCat1-.1")] = "LEVEL_PRODUCT_DefaultDistScore_CommonDist_Poi_GPoi_FoodShopCtrCat1--1"

colnames(df_coef)[which(colnames(df_coef) == "LEVEL_CROSSSEEK_LocCVR_Shopid-.1")] = "LEVEL_CROSSSEEK_LocCVR_Shopid--1"

colnames(df_coef)[which(colnames(df_coef) == "LEVEL_CROSSSEEK_RealTimeShopLastView_Shopid-.1")] = "LEVEL_CROSSSEEK_RealTimeShopLastView_Shopid--1"

colnames(df_coef)[which(colnames(df_coef) == "LEVEL_IF@3_CROSSSEEK_UserQuInfo_CONST@key=isactive_COALESCE_CROSSSEEK_LocCTR_Shopid_CROSSSEEK_LocCTRSPL_Shopid_CROSSSEEK_LocCTR_Shopid-.1")] = "LEVEL_IF@3_CROSSSEEK_UserQuInfo_CONST@key=isactive_COALESCE_CROSSSEEK_LocCTR_Shopid_CROSSSEEK_LocCTRSPL_Shopid_CROSSSEEK_LocCTR_Shopid--1"

colnames(df_coef)[which(colnames(df_coef) == "LEVEL_CROSSSEEK_LocationTopShop_locationu_Shopid-.1")] = "LEVEL_CROSSSEEK_LocationTopShop_locationu_Shopid--1"

colnames(df_coef)[which(colnames(df_coef) == "LEVEL_WILSON_ShopRealtimeInfo_ShopRealtimeTraffic_CONST@key=10_CONST@key=1_CONST@key=1_CONST@key=1_ShopRealtimeInfo_ShopRealtimeTraffic_CONST@key=10_CONST@key=1_CONST@key=0_CONST@key=1_CONST@key=0.95-.1")] = "LEVEL_WILSON_ShopRealtimeInfo_ShopRealtimeTraffic_CONST@key=10_CONST@key=1_CONST@key=1_CONST@key=1_ShopRealtimeInfo_ShopRealtimeTraffic_CONST@key=10_CONST@key=1_CONST@key=0_CONST@key=1_CONST@key=0.95--1"

colnames(df_coef)[which(colnames(df_coef) == "fourg")] ="Contains_INFO@networktype_CONST@param=4G"





###

colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_FoodShopCtrCat1.",replacement = "LEVEL_FoodShopCtrCat1-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_WILSON_allcategoryclicks_allcategoryviews_CONST.key.0.99.",replacement = "LEVEL_WILSON_allcategoryclicks_allcategoryviews_CONST@key=0.99-")

colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_FoodRT_CROSSSEEK_RealTimeShopClick_Shopid.",replacement = "LEVEL_FoodRT_CROSSSEEK_RealTimeShopClick_Shopid-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_CommonDist_Poi_GPoi.",replacement = "LEVEL_CommonDist_Poi_GPoi-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_PRODUCT_DefaultDistScore_CommonDist_Poi_GPoi_FoodShopCtrCat1.",replacement = "LEVEL_PRODUCT_DefaultDistScore_CommonDist_Poi_GPoi_FoodShopCtrCat1-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_DefaultDistScore_CommonDist_Poi_GPoi.",replacement = "LEVEL_DefaultDistScore_CommonDist_Poi_GPoi-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_Pictotal.",replacement = "LEVEL_Pictotal-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_CROSSSEEK_LocCVR_Shopid.",replacement = "LEVEL_CROSSSEEK_LocCVR_Shopid-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_CROSSSEEK_RealTimeShopLastView_Shopid.",replacement = "LEVEL_CROSSSEEK_RealTimeShopLastView_Shopid-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_TotalReviews.",replacement = "LEVEL_TotalReviews-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_BadReviews.",replacement = "LEVEL_BadReviews-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_shopage..",replacement = "LEVEL_shopage--")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_COALESCE_NormalizeUserShopPref_UserQuInfo_Shopid_PrefShopGuess_SimilarityShops_UserQuInfo.",replacement = "LEVEL_COALESCE_NormalizeUserShopPref_UserQuInfo_Shopid_PrefShopGuess_SimilarityShops_UserQuInfo-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_IF.3_CROSSSEEK_UserQuInfo_CONST.key.isactive_COALESCE_CROSSSEEK_LocCTR_Shopid_CROSSSEEK_LocCTRSPL_Shopid_CROSSSEEK_LocCTR_Shopid.",replacement = "LEVEL_IF@3_CROSSSEEK_UserQuInfo_CONST@key=isactive_COALESCE_CROSSSEEK_LocCTR_Shopid_CROSSSEEK_LocCTRSPL_Shopid_CROSSSEEK_LocCTR_Shopid-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_UvScore..",replacement = "LEVEL_UvScore--")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_TimeRelevanceSeek_TimeRelevance_TimeSeg..",replacement = "LEVEL_TimeRelevanceSeek_TimeRelevance_TimeSeg--")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_CROSSSEEK_LocationTopShop_locationu_Shopid.",replacement = "LEVEL_CROSSSEEK_LocationTopShop_locationu_Shopid-")

colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_LocLandmark_locationu_Mallid.",replacement = "LEVEL_LocLandmark_locationu_Mallid-")
colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_TimeSeg.",replacement = "LEVEL_TimeSeg-")


colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_WILSON_ShopRealtimeInfo_ShopRealtimeTraffic_CONST.key.10_CONST.key.1_CONST.key.1_CONST.key.1_ShopRealtimeInfo_ShopRealtimeTraffic_CONST.key.10_CONST.key.1_CONST.key.0_CONST.key.1_CONST.key.0.95.",replacement = "LEVEL_WILSON_ShopRealtimeInfo_ShopRealtimeTraffic_CONST@key=10_CONST@key=1_CONST@key=1_CONST@key=1_ShopRealtimeInfo_ShopRealtimeTraffic_CONST@key=10_CONST@key=1_CONST@key=0_CONST@key=1_CONST@key=0.95-")

colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_PrefWithGeo_CONST.key.catpref_IsLocate_UserQuInfo_UserQuInfo_MainCategoryIds.",replacement = "LEVEL_PrefWithGeo_CONST@key=catpref_IsLocate_UserQuInfo_UserQuInfo_MainCategoryIds-")



colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_WILSON_ShopRealtimeInfo_ShopRealtimeTraffic_CONST.key.10_CONST.key.1_CONST.key.1_CONST.key.0_ShopRealtimeInfo_ShopRealtimeTraffic_CONST.key.10_CONST.key.1_CONST.key.0_CONST.key.0_CONST.key.0.95.",replacement = "LEVEL_WILSON_ShopRealtimeInfo_ShopRealtimeTraffic_CONST@key=10_CONST@key=1_CONST@key=1_CONST@key=0_ShopRealtimeInfo_ShopRealtimeTraffic_CONST@key=10_CONST@key=1_CONST@key=0_CONST@key=0_CONST@key=0.95-")

colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_LocDensity_locationu_CONST.key.STAR30.",replacement = "LEVEL_LocDensity_locationu_CONST@key=STAR30-")


colnames(df_coef) <- str_replace_all(string = colnames(df_coef),pattern = "LEVEL_CROSSSEEK_MAP.儑.._FoodShopOvr_CONCAT.3_CONST.key.L_CONST.key.m_QUERY.categoryid.", replacement="LEVEL_CROSSSEEK_MAP@儑@:_FoodShopOvr_CONCAT@3_CONST@key=L_CONST@key=m_QUERY@categoryid-")




