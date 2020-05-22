# SSF_japan draft


# library
library(tidyverse)
library(readxl)
library(sp)
library(sf)
library(estatapi)
library(patchwork)


# Load prefcecture shape file
# https://gadm.org/download_country_v3.html

##jp_sh = readRDS("gadm36_JPN_0_sf.rds") # 国 (country)
jp_sh = readRDS("gadm36_JPN_1_sf.rds") # 都道府県 (prefecture)
##jp_sh = readRDS("gadm36_JPN_2_sf.rds") # 市町区村 (cities/towns)

jp_sh2 = st_simplify(jp_sh, preserveTopology = TRUE, dTolerance = 0.01) 
## pryr::object_size(jp_sh)
## pryr::object_size(jp_sh2)
head(jp_sh)

# ggplot

ggplot(jp_sh2) + 
  geom_sf()
  

# find Fisheries Census


appID <- "11217753824fd6ecf1d5cc2880b613e3ee724fdb"

# search Fisheries Census data using estataapi package. 
# Maybe easier to search on this web page
# https://www.e-stat.go.jp/stat-search/files?page=1&layout=datalist&toukei=00500210&tstat=000001033844&cycle=0&tclass1=000001036520&tclass2=000001036521&tclass3=000001036858
# Fisheries Census consists of 10 (9 + English version) sections (巻). 
# 1~4 are about Marine fisheries, and each section corresponds to the level of geographical segmentation.
# 1 is national, 2 is prefectural
# 3 is at city/town level. 4 is even finer, fisheries district level. 
# 7 is about processing/distributing sector at national, prefectural, and city level.
# 8 is about processing/distributing sector at fisheries district level. 

cat_census2 <- estat_getStatsList(appId = appID, searchWord = "漁業センサス第2巻 AND 就業者数") #都道府県別
cat_census3 <- estat_getStatsList(appId = appID, searchWord = "漁業センサス第3巻") #市町区村別
cat_census4 <- estat_getStatsList(appId = appID, searchWord = "漁業センサス第4巻") #市町区村別
cat_census8 <- estat_getStatsList(appId = appID, searchWord = "漁業センサス第8巻") # 加工流通業
##cat_kowan <- estat_getStatsList(appId = appID, searchWord = "漁港港勢調査")

# meta info of the data of fishers
estat_getMetaInfo(appId = appID, statsDataId = "0003117408") 


# obtain the data # 都道府県別漁業者数2008
df_fishers_pref_2008 = estat_getStatsData(appId = appID,
                                statsDataId = "0003117227")

df_fishers_pref_age_2003 = estat_getStatsData(appId = appID,
                                              statsDataId = "0003261221") %>%
  mutate(year = 2003)
  
df_fishers_pref_age_2008 = estat_getStatsData(appId = appID,
                                     statsDataId = "0003123532")%>%
  mutate(year = 2008)

df_fishers_pref_age_2013 = estat_getStatsData(appId = appID,
                                              statsDataId = "0003122869") %>%
  mutate(year = 2013)

# api is not available yet
# obtain the excel file from the e-stat website
download.file("https://www.e-stat.go.jp/stat-search/file-download?statInfId=000031918078&fileKind=0",
                     "fishers_pref_2018.xls")
df_fishers_pref_age_2018 = read_excel("fishers_pref_2018.xls")

df_fishers_pref = bind_rows(df_fishers_pref_age_2003,df_fishers_pref_age_2008,df_fishers_pref_age_2013) %>%
  mutate(area_code_num = as.numeric(area_code)/1000) %>% # make numeric to filter
  filter(area_code_num %in% c(1:47)) %>%
  mutate(cat03_code = ifelse(is.na(cat03_code),11,cat03_code),
         `自営漁業・漁業雇われ`=ifelse(is.na(`自営漁業・漁業雇われ`),"計",`自営漁業・漁業雇われ`))

df_fishers_pref_agg = df_fishers_pref %>%
  filter(cat01_code == 11 & cat02_code == 11 & cat03_code == 11)

# combine with map data
map_fishers_pref_agg = jp_sh2 %>%
  left_join(df_fishers_pref_agg, by = c("NL_NAME_1" = "地域事項(全国・都道府県・大海区)")) %>%
  st_as_sf(.)


# With Hokkaido (too large relative to others)
map_fun0 = function(){
  ggplot(map_fishers_pref_agg %>% filter(!is.na(year)), aes(fill = value)) +
    geom_sf(size = 0.1) + 
    coord_sf(xlim = c(126,148), ylim = c(26,47), # set range, include Okinawa, but not other islands
             datum = NA) + # not show the coordinates and axis labels 
    scale_fill_viridis_c(option = "plasma") +
    labs(title = "Number of fishers by prefecture") +
    theme_bw() +
    facet_wrap(~year, nrow = 2)
}
map_fun0()

# Remove Hokkaido (too large relative to others)
map_fun1 = function(){
ggplot(map_fishers_pref_agg %>% filter(area_code != "01000") %>% filter(!is.na(year)), aes(fill = value)) +
  geom_sf(size = 0.1) + 
  coord_sf(xlim = c(126,143), ylim = c(26,42), # set range, include Okinawa, but not other islands
           datum = NA) + # not show the coordinates and axis labels 
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Number of fishers by prefecture") +
  theme_bw() +
  facet_wrap(~year, nrow = 2)
}
map_fun1()

# 2013 only
map_fun2013 = function(){
  ggplot(map_fishers_pref_agg %>% filter(area_code != "01000") %>% filter(year == 2013), aes(fill = value)) +
    geom_sf(size = 0.1) + 
    coord_sf(xlim = c(126,143), ylim = c(26,42), # set range, include Okinawa, but not other islands
             datum = NA) + # not show the coordinates and axis labels 
    scale_fill_viridis_c(option = "plasma") +
    labs(title = "Number of fishers by prefecture, 2013") +
    theme_bw() 
}
map_fun2013()



#### By ages #####

df_fishers_pref_age = df_fishers_pref %>%
  filter(cat01_code != 11 & cat02_code == 11 & cat03_code == 11) %>%
  dplyr::select(-cat01_code, -cat02_code,-cat03_code) %>% # only 2008 or 2013
  mutate(`年齢階層_15歳-75歳以上` = parse_number(`年齢階層_15歳-75歳以上`)) %>%
  mutate(`年齢階層_15歳-75歳以上` = paste0("age_",`年齢階層_15歳-75歳以上`)) %>%
  pivot_wider(names_from = "年齢階層_15歳-75歳以上", values_from = "value") %>%
  mutate(total = rowSums(select(., starts_with("age"))),
         older = age_65 + age_70 + age_75,
         aging_rate = older/total)

# combine with map data
map_fishers_pref_age = jp_sh2 %>%
  left_join(df_fishers_pref_age, by = c("NL_NAME_1" = "地域事項(全国・都道府県・大海区)")) %>%
  st_as_sf(.)


# With Hokkaido (too large relative to others)
map_age_fun0 = function(){
  ggplot(map_fishers_pref_age %>% filter(!is.na(year)), aes(fill = aging_rate)) +
    geom_sf(size = 0.1) + 
    coord_sf(xlim = c(126,148), ylim = c(26,47), # set range, include Okinawa, but not other islands
             datum = NA) + # not show the coordinates and axis labels 
    scale_fill_viridis_c(option = "plasma") +
    labs(title = "Aging rate of fishers by prefecture") +
    theme_bw() +
    facet_wrap(~year)
}
map_age_fun0()


#======= Not used below ===============


# obtain the data # 市町区村別漁業者数2008
df_fishers = estat_getStatsData(appId = appID,
                   statsDataId = "0003117408")
# obtain the data # 漁業地区別漁業者数2008
df_fishers_region = estat_getStatsData(appId = appID,
                                statsDataId = "0003117445")

# obtain the data # 市町区村別水産加工場従業員数2008
df_process = estat_getStatsData(appId = appID,
                                statsDataId = "0003119220")
# obtain the data # 市町区村別冷凍冷蔵工場従業員数2008
df_freeze = estat_getStatsData(appId = appID,
                                statsDataId = "0003119219")
