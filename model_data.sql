add jar /data/qin.zhang/l2r/train_l2r/fortrus-1.0.jar;
create temporary function SetDefault as 'com.dp.search.entity.SetDefaultValue';
  
SET hive.cli.print.header=TRUE;
select
  query_id,
  shop_id,
  if(actioncountmap["click"] > 0,1,0) as label,
  modelscore,
  if(actioncountmap['order'] > 0,2,if(actioncountmap['click']>0,1,0)) impact_factor,
  if(actioncountmap['clickvalue'] is null or actioncountmap['clickvalue'] == 0,1,ln(actioncountmap['clickvalue'])+1) clickvalue_ln,
  --(81+81)/(q.av+81) as coorview,
  SetDefault(featuredatamap["UserCat1PricePref_UserQuInfo_UserCat1PercentileList"],'', 1.0) as price_pref,
  featuredatamap["DisCT_DealGroupPrice_DealGroupMarketPrice_MixDealGroupSales"] as discount,
  featuredatamap["COALESCE_NormalizeUserShopPref_UserQuInfo_Shopid_PrefShopGuess_SimilarityShops_UserQuInfo"] as shop_pref_extend,
  featuredatamap["PRODUCT_DefaultDistScore_CommonDist_Poi_GPoi_CROSSSEEK_MAP@儑@:_FoodShopOvr_CONCAT@3_CONST@key=L_CONST@key=m_QUERY@categoryid"] as distxocr,
  featuredatamap["CROSSSEEK_LocCVR_Shopid"] as loccvr,
  featuredatamap["IsDealShop"] as isdealshop,
  featuredatamap["TimeRelevanceSeek_TimeRelevance_TimeSeg"] as seg,
  featuredatamap["CROSSSEEK_RealTimeShopLastView_Shopid"] as lastview,
  featuredatamap["LEVEL_CROSSSEEK_RealTimeShopLastView_Shopid"] as level_lastview,
  featuredatamap["IsHuiShop"] as ishuishop,
  featuredatamap["AND@1_CEIL_Pictotal"] as haspic,
  featuredatamap["PRODUCT_DefaultDistScore_CommonDist_Poi_GPoi_FoodShopCtrCat1"] as distxctr,
  featuredatamap["LOG_CEIL_Pictotal"] as log_ceil_pictotal,
  featuredatamap["CROSSSEEK_LocCTR_Shopid"] as locctr,
  featuredatamap["IsStarGuess_Starsource"] as guess_star,
  featuredatamap["AND@1_ShopPower"] as hasstar,
  featuredatamap["FoodRT_CROSSSEEK_RealTimeShopClick_Shopid"] as realclick,
  featuredatamap["UserAdoreRegion_UserQuInfo_MainRegionIds"] as adoreregion,
  featuredatamap["IF@4_COMP@=_CROSSSEEK_UserQuInfo_CONST@key=permanentcityid_INFO@cityid_LocalScore_RemoteScore_CONST@key=1"] as remotelocal,
  featuredatamap["IsTakeawayShop"] as istakeawayshop,
  featuredatamap["PRODUCT_DefaultDistScore_CommonDist_Poi_GPoi_Attribute_FoodShopOvr_CONCAT@2_CONST@key=L_QUERY@categoryid"] as distXcvrwithtime,
  featuredatamap["PartContains_ReceiptHoldDealGroupId_DealGroupID"] as holddeal,
  featuredatamap["ShopPower"] as shoppower,
  featuredatamap["FoodShopCtrReductionWithTimeCat1"] as ctrwithtime,
  featuredatamap["IsFoodTopShop"] as istopshop,
  featuredatamap["OR@2_IsTuanDiscount_IsShanhuiDiscount"] as islijian,
  featuredatamap["IsBookingShop"] as isbookingshop,
  featuredatamap["COALESCE_FoodRT_CROSSSEEK_RealTimeShopClick_Shopid_RTScoreGuess_SimilarityShops_RealTimeShopClick"] as realclick_expand,
  featuredatamap["IsMcardShop"] as ismcardshop,
  featuredatamap["UserAdoreCategory_UserQuInfo_MainCategoryIds"] as main_cateadore,
  featuredatamap["PRODUCT_DefaultDistScore_CommonDist_Poi_GPoi_FoodShopCtrReductionWithTimeCat1"] as distxctrwithtime,
  featuredatamap["WILSON_ShopRealtimeInfo_ShopRealtimeTraffic_CONST@key=10_CONST@key=1_CONST@key=1_CONST@key=1_ShopRealtimeInfo_ShopRealtimeTraffic_CONST@key=10_CONST@key=1_CONST@key=0_CONST@key=1_CONST@key=0.95"] as yestdayctr,
  featuredatamap["UvScore"] as uvscore,
  featuredatamap["LOG_CROSSSEEK_LocSales_Shopid"] as log_locsales,
  featuredatamap["NormalizeUserShopPref_UserQuInfo_Shopid"] as shop_pref,
  featuredatamap["Contains_MainCategorySeq_QUERY@categoryid"] as maincate,
  featuredatamap["IsFuShop"] as isfushop,
  featuredatamap["DisCT_DealGroupPrice_DealGroupMarketPrice_DealGroupSales"] as discount_in_dp,
  featuredatamap["PRODUCT_DefaultDistScore_CommonDist_Poi_GPoi_CROSSSEEK_MAP@儑@:_FoodShopOvr_CONCAT@3_CONST@key=L_IF@3_OnWeekends_CONST@key=1_CONST@key=0_QUERY@categoryid"] as distxweekdaycvr,
  featuredatamap["CROSSSEEK_MAP@儑@:_FoodShopOvr_CONCAT@3_CONST@key=L_IF@3_OnWeekends_CONST@key=1_CONST@key=0_QUERY@categoryid"] as weekdaycvr,
  featuredatamap["CROSSSEEK_LocBBM_Shopid"] as locbbm,
  featuredatamap["CROSSSEEK_WifiStrength_Shopid"] as wifi_strength,
  featuredatamap["Attribute_FoodShopOvr_CONCAT@2_CONST@key=L_QUERY@categoryid"] as cvrwithtime,
  featuredatamap["CROSSSEEK_MAP@儑@:_FoodShopOvr_CONCAT@3_CONST@key=L_CONST@key=m_QUERY@categoryid"] as ocr,
  featuredatamap["WILSON_ShopRealtimeInfo_ShopRealtimeTraffic_CONST@key=10_CONST@key=1_CONST@key=1_CONST@key=0_ShopRealtimeInfo_ShopRealtimeTraffic_CONST@key=10_CONST@key=1_CONST@key=0_CONST@key=0_CONST@key=0.95"] as todayctr,
  featuredatamap["HasDealPromote"] as has_deal_promote,
  featuredatamap["FoodBBMCat1"] as foodbbmcat1,
  featuredatamap["Attribute_FoodShopOvr_CONCAT@2_CONST@key=bnf_QUERY@categoryid"] as benifit_bnf,
  featuredatamap["Attribute_FoodShopOvr_CONCAT@2_CONST@key=dif_QUERY@categoryid"] as benifit_dif,
  featuredatamap["Attribute_FoodShopOvr_CONCAT@2_CONST@key=bpu_QUERY@categoryid"] as benifit_bpu,
  featuredatamap["Attribute_FoodShopOvr_CONCAT@2_CONST@key=br_QUERY@categoryid"] as benifit_br,
  featuredatamap["PrefShopGuess_SimilarityShops_UserQuInfo"]  as guess_shop_pref,
  featuredatamap["PRODUCT_DefaultDistScore_CommonDist_Poi_GPoi_Attribute_FoodShopOvr_CONCAT@2_CONST@key=dif_QUERY@categoryid"] as distxbenifit_dif,
  featuredatamap["PRODUCT_DefaultDistScore_CommonDist_Poi_GPoi_Attribute_FoodShopOvr_CONCAT@2_CONST@key=bpu_QUERY@categoryid"]  as distxbenifit_bpu,
  featuredatamap["PRODUCT_DefaultDistScore_CommonDist_Poi_GPoi_Attribute_FoodShopOvr_CONCAT@2_CONST@key=br_QUERY@categoryid"]  as distxbenifit_br,
  featuredatamap["IF@3_CROSSSEEK_UserQuInfo_CONST@key=isactive_COALESCE_CROSSSEEK_LocCTR_Shopid_CROSSSEEK_LocCTRSPL_Shopid_CROSSSEEK_LocCTR_Shopid"]  as spl,
  coalesce(featuredatamap['IsDealShop']+featuredatamap['IsHuiShop'],featuredatamap['OR@2_IsHuiShop_IsDealShop']) coopshop_two,
  featuredatamap["CROSSSEEK_LocationTopShop_locationu_Shopid"] as topshop,
  featuredatamap["OR@4_IsHuiShop_IsDealShop_IsTakeawayShop_IsFuShop"] as coopshop_four,
  featuredatamap['COMP@=_CROSSSEEK_UserQuInfo_CONST@key=permanentcityid_INFO@cityid']  as is_permanent_city,
  featuredatamap['LocDirection_Poi_GPoi_locationu']  as locationu_dir,
  featuredatamap['DefaultDistScore_CommonDist_Poi_GPoi']  as distscore,
  featuredatamap['FoodShopCtrCat1']  as ctr,
  featuredatamap['LocLandmark_locationu_Airport']  as airport_shopscore,
  featuredatamap['LocLandmark_locationu_Mallid']  as mall_shopscore,
  featuredatamap['LocDensity_locationu_CONST@key=STAR35']  density_thirty_five ,
  featuredatamap['LocDensity_locationu_CONST@key=STAR30']  as density_thirty,
  featuredatamap['isNewUser']  as isnewuser,
  featuredatamap['UserShopCatePref_mtcat2pref_UserQuInfo']  as cat2_pref,
  featuredatamap['UserShopCatePref_mtcat3pref_UserQuInfo']  as cat3_pref,
  featuredatamap['CROSSSEEK_UserQuInfo_CONST@key=isactive']  as isactive,
  featuredatamap['OnWeekends']  as onweekend,
  SetDefault(featuredatamap['COMP@=_QUERY@categoryid_CONST@key=10'], '-1', 1.0)  as is_food_level1,
 featuredatamap['Attribute_FoodShopOvr_CONST@key=bnf']  as bnf,
  featuredatamap['Attribute_FoodShopOvr_CONST@key=dif']  as dif,
  featuredatamap['Attribute_FoodShopOvr_CONST@key=br']  as br,
  featuredatamap['Attribute_FoodShopOvr_CONST@key=bpu']  as bpu,
  featuredatamap['Contains_SearchCateCrossSeek_RealTimeCateSearch_CONST@key=kws_CONST@key=30_MainCategoryIds']  as keyw_cate_searched_30,
  featuredatamap['Contains_SearchCateCrossSeek_RealTimeCateSearch_CONST@key=cats_CONST@key=10_MainCategoryIds']  as cate_searched_10,
  featuredatamap['Contains_SearchCateCrossSeek_RealTimeCateSearch_CONST@key=kws_CONST@key=120_MainCategoryIds']  as keyw_cate_searched_120,
  featuredatamap['Contains_SearchCateCrossSeek_RealTimeCateSearch_CONST@key=cats_CONST@key=30_MainCategoryIds']  as cate_searched_30,
  featuredatamap['OR@2_Contains_SearchCateCrossSeek_RealTimeCateSearch_CONST@key=cats_CONST@key=30_MainCategoryIds_Contains_SearchCateCrossSeek_RealTimeCateSearch_CONST@key=kws_CONST@key=120_MainCategoryIds']  cate_searched,
  featuredatamap['IF@3_COMP@>_UserShopCatePref_mtcat3pref_UserQuInfo_CONST@key=-0.99_UserShopCatePref_mtcat3pref_UserQuInfo_ExtraCatPref_extracat_mtcat3pref']  as cat3pref_new,
  featuredatamap['IF@3_COMP@>_UserShopCatePref_mtcat2pref_UserQuInfo_CONST@key=-0.99_UserShopCatePref_mtcat2pref_UserQuInfo_ExtraCatPref_extracat_mtcat2pref']  as cat2pref_new,
  featuredatamap['PrefWithGeo_CONST@key=catpref_IsLocate_UserQuInfo_UserQuInfo_MainCategoryIds']  as catprefwithgeo,
  SetDefault(featuredatamap['PrefWithGeo_CONST@key=pricepref_IsLocate_UserQuInfo_UserQuInfo_UserCat1PercentileList'],'', 1.0)  as priceprefwithgeo
from
(
select feat.*
from
(
  select
  firstqueryid  query_id,
  itemid shop_id,
  actioncountmap,
  featuredatamap,
  featuredatamap["modelscore"] modelscore,
  if(actioncountmap["order"] > 0,10,1) sample_weight
  from bi.dpsearch_online_query_featuredata
  where hp_cal_dt =  "${cal_dt}"
  and searchbusiness = "artsshop"
  and aggregatorbusiness = "search-feature"
  and model rlike "FoodCat1GBDTSeq3"
)feat
join
(
    select queryid,
    cast(if(requestinfomap["geoFieldName"] = "poi", split(requestinfomap["poi"], "%3A")[0] , split(requestinfomap["gpoi"], "%3A")[0])*200 as int) as lng200,
    cast(if(requestinfomap["geoFieldName"] = "poi", split(requestinfomap["poi"], "%3A")[1] ,  split(requestinfomap["gpoi"], "%3A")[1])*200 as int) as lat200
    from bi.dpsearch_base_log_analysis
    WHERE hp_cal_dt = "${cal_dt}"
    and platformid = 2
    and isnearby = 1
    and keyword = ""
    and originsort in ("desc(dpscore)", "desc(mobile)")
    and (viewedshopidlist is not null and viewedshopidlist <> "")
    and locatecityid <> 0
    and locatecityid = coalesce(pagecityid, searchcityids)
    and requestinfomap["geoFieldName"] is not null
    and if(requestinfomap["geoFieldName"] = "poi", split(requestinfomap["poi"], "%3A")[0] , split(requestinfomap["gpoi"], "%3A")[0]) is not null
    and if(requestinfomap["geoFieldName"] = "poi", split(requestinfomap["poi"], "%3A")[1] , split(requestinfomap["gpoi"], "%3A")[1]) is not null
    and business = "artsshop"
    and (shopclicknum >0 or (shopclicknum <= 0 and viewedlastshoppos>=10))
)p
on feat.query_id = p.queryid
)f

limit 100000000;