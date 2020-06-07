# SSF_japan draft

# compiling the data of small scale fisheries (=coastal fisheries) in Japan. 

# library
pacman::p_load(
  tidyverse,
  readxl,
  sp,
  sf,
  estataapi,
  patchwork
)

library(estatapi)

# Load prefcecture shape file
# https://gadm.org/download_country_v3.html

##jp_sh = readRDS("gadm36_JPN_0_sf.rds") # 国 (country)
jp_sh = readRDS("gadm36_JPN_1_sf.rds") # 都道府県 (prefecture)
##jp_sh = readRDS("gadm36_JPN_2_sf.rds") # 市町区村 (cities/towns)
##jp_sh = st_transform(jp_sh, 54032) # azimuthal equidistant. Transform this to avoid error
# https://stackoverflow.com/questions/60008135/st-simplify-dtolerence-with-decimal-degree

jp_sh2 = st_simplify(jp_sh, preserveTopology = TRUE, dTolerance = 0.1) 
##jp_sh2 = st_transform(jp_sh2, 4326) # go back to WGS84
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


# town level
df_fishers_town_age_2008 = estat_getStatsData(appId = appID,
                                              statsDataId = "0003124203") %>%
  mutate(year = 2008)

df_fishers_town_age_2013 = estat_getStatsData(appId = appID,
                                              statsDataId = "0003124202") %>%
  mutate(year = 2013)

df_fishers_town = bind_rows(df_fishers_town_age_2008,df_fishers_town_age_2013) %>%
  mutate(area_code_num = as.numeric(area_code)/1000) # make numeric to filter


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

### prefecture level ###
df_fishers_pref_age = df_fishers_pref %>%
  filter(cat01_code != 11 & cat02_code == 11 & cat03_code == 11) %>%
  dplyr::select(-cat01_code, -cat02_code,-cat03_code) %>% # only 2008 or 2013
  mutate(`年齢階層_15歳-75歳以上` = parse_number(`年齢階層_15歳-75歳以上`)) %>%
  mutate(`年齢階層_15歳-75歳以上` = paste0("age_",`年齢階層_15歳-75歳以上`)) %>%
  pivot_wider(names_from = "年齢階層_15歳-75歳以上", values_from = "value") %>%
  mutate(total = rowSums(select(., starts_with("age"))),
         older = age_65 + age_70 + age_75,
         fisher_aging_rate = older/total)

# combine with map data
map_fishers_pref_age = jp_sh2 %>%
  left_join(df_fishers_pref_age, by = c("NL_NAME_1" = "地域事項(全国・都道府県・大海区)")) %>%
  st_as_sf(.)


# With Hokkaido (too large relative to others)
map_age_fun0 = function(){
  ggplot(map_fishers_pref_age %>% filter(!is.na(year)), aes(fill = fisher_aging_rate)) +
    geom_sf(size = 0.1) + 
    coord_sf(xlim = c(126,148), ylim = c(26,47), # set range, include Okinawa, but not other islands
             datum = NA) + # not show the coordinates and axis labels 
    scale_fill_viridis_c(option = "plasma") +
    labs(title = "Aging rate of fishers by prefecture") +
    theme_bw() +
    facet_wrap(~year)
}
map_age_fun0()


### town level ###

df_fishers_town_age = df_fishers_town %>%
  mutate(town = ifelse(year == 2008, `地域事項(市区町村（漁業地区))_2008`,`地域事項(市区町村(漁業地区)）_2013`)) %>%
  filter(cat01_code != 11 & cat02_code == 11) %>%
  dplyr::select(-cat01_code, -cat02_code) %>% # only 2008 or 2013
  mutate(`年齢階層_15歳-75歳以上` = parse_number(`年齢階層_15歳-75歳以上`)) %>%
  mutate(`年齢階層_15歳-75歳以上` = paste0("age_",`年齢階層_15歳-75歳以上`)) %>%
  pivot_wider(names_from = "年齢階層_15歳-75歳以上", values_from = "value") %>%
  mutate_at(vars(starts_with("age_")), list(~ifelse(is.na(.),0,.))) %>%
  mutate(total = rowSums(select(., starts_with("age"))),
         older = age_65 + age_70 + age_75,
         fisher_aging_rate = older/total)

# ====== general population ==============

# obtain the excel file from the e-stat website
download.file("https://www.e-stat.go.jp/stat-search/file-download?statInfId=000031594311&fileKind=0",
              "pop_town_2015.xls")
df_pop_town_age_2015 = read_excel("pop_town_2015.xls")

# extract column names
colname_pop_town_age_2015 = df_pop_town_age_2015 %>% slice(6)

# remove headers
df_pop_town_age_2015 <- df_pop_town_age_2015 %>%
  slice(-c(1:10))

# replace column names
colnames(df_pop_town_age_2015) <- colname_pop_town_age_2015
class(df_pop_town_age_2015)
df_pop_town_age_2015[,9:54] <- as.numeric(unlist(df_pop_town_age_2015[,9:54]))

# remove prefecture and district (ward) within large cities
df_pop_town_age_2015 <- df_pop_town_age_2015 %>%
  filter(`市などの別` != "a" & `市などの別` != "0")

# data explore
summary(df_pop_town_age_2015)

# merge fisheries census data

df_pop_town_age_2015_fish <- df_pop_town_age_2015 %>%
  left_join(df_fishers_town_age %>% filter(year == 2013), by = c("都道府県・市区町村名" = "town")) %>%
  mutate(fish_town = ifelse(!is.na(total), "Yes", "No"))

# scatetr plot, 高齢化率
colnames(df_pop_town_age_2015_fish)

ggplot(df_pop_town_age_2015_fish, aes(x = `市などの別`, fill=as.factor(fish_town), y=`【年齢別割合（総数）】65歳以上人口(％)`)) + 
         geom_boxplot() +
  labs(x = "City/town category (1 is large cities)",fill = "Fishing Community", y = "Aging rate (% of pop.  >=65 years old")

#======= Fishery type by town ============


# obtain the excel file from the e-stat website
download.file("https://www.e-stat.go.jp/stat-search/file-download?statInfId=000029149689&fileKind=0",
              "entity_town_2013.xls")
df_fish_entity_town_2013 = read_excel("entity_town_2013.xls") 

# extract header
header_fish_entity_town_2013 <- df_fish_entity_town_2013 %>% slice(c(1:10)) %>%
  tidyr::fill(.,.direction = "down")

# make new header 
colname_fish_entity_town_2013 = c("pref","district","town","nothing","num",
                                  "total_entity","no_ves","ves_no_eng","ves_out_eng","ves_1t",
                                  "ves_1_3","ves_3_5","ves_5_10","ves_10_20","ves_20_30",
                                  "ves_30_50","ves_50_100","ves_100_200","ves_200_500","ves_500_1000",
                                  "ves_1000_3000","ves_3000","setnet_big","setnet_salm","setnet_sma",
                                  "aqa_salm","aqa_yt","aqa_snap","aqa_flou","aqa_tuna","aqa_oth_fish",
                                  "aqa_scal","aqa_oys","aqa_oth_shell","aqa_shri","aqa_hoya",
                                  "aqa_oth_anim","aqa_konbu","aqa_wakame","aqa_nori","aqa_oth_weed",
                                  "aqa_pearl","aqa_peashell","coast_ent_total","coast_ent_aqa",
                                  "coast_ent_oth","middle_ent","large_ent")

# remove header
df_fish_entity_town_2013 <- df_fish_entity_town_2013 %>% slice(-c(1:10))

# new colnames 
colnames(df_fish_entity_town_2013) <- colname_fish_entity_town_2013

# convert - to 0
df_fish_entity_town_2013 <- df_fish_entity_town_2013 %>%
  mutate_all(list(~ifelse(. == "-",0,.))) %>%
  mutate_at(vars(6:48), list(~as.numeric(.))) # NA is generated, because some towns do not have specific breakdown of the entities, which was "x" originally.


# --- merge to df_pop_town_age_2015_fish ----

df_pop_town_age_2015_fish_entity <- df_pop_town_age_2015_fish %>%
  left_join(df_fish_entity_town_2013, by = c("都道府県・市区町村名" = "town")) %>%
  mutate(coast_ent_per_capita =coast_ent_total/`【年齢（総数）】総数人口(人)`)

# --- plot aging vs small scale entity

ggplot(df_pop_town_age_2015_fish_entity %>% filter(fish_town == "Yes"), 
       aes(x = coast_ent_total/total_entity, y = fisher_aging_rate)) + 
  geom_point() + 
  geom_smooth(method = c("loess")) +
  labs(x = "SSF ratio (No. of coastal entities/total entities)", y= "Aging rate of workers in fisheries") +
  theme_bw()

ggplot(df_pop_town_age_2015_fish_entity %>% filter(fish_town == "Yes"), 
       aes(x = coast_ent_oth/total_entity, y = fisher_aging_rate)) + 
  geom_point() + 
  geom_smooth(method = c("loess")) +
  labs(x = "SSF ratio (No. of coastal entities (excl. aquaculture)/total entities)",y = "Aging rate of workers in fisheries") +
  theme_bw()

ggplot(df_pop_town_age_2015_fish_entity %>% filter(fish_town == "Yes"), 
       aes(x = coast_ent_total/total_entity, y = `【年齢別割合（総数）】65歳以上人口(％)`/100)) + 
  geom_point() + 
  geom_smooth(method = c("loess")) +
  labs(x = "SSF ratio (No. of coastal entities/total entities)", y = "Aging rate of town (general pop.)") +
  theme_bw()

ggplot(df_pop_town_age_2015_fish_entity %>% filter(fish_town == "Yes"), 
       aes(x = coast_ent_oth/total_entity, y = `【年齢別割合（総数）】65歳以上人口(％)`/100)) + 
  geom_point() + 
  geom_smooth(method = c("loess")) +
  labs(x = "SSF ratio (No. of coastal entities (excl. aquaculture)/total entities)", y = "Aging rate of town (general pop.)") +
  theme_bw()


#======= Accidents statistics ===============

# the statistics about the accidents are downloaded from the webpage of Coast Guard
# https://www.kaiho.mlit.go.jp/doc/hakkou/toukei/toukei.html
# manually downloaded and copied to compile "accidents_by_boat_size_2015_2019.xlsx"

dat_acci = read_xlsx("accidents_by_boat_size_2015_2019.xlsx") %>%
  pivot_longer(cols = -tons, names_to = "year",values_to = "number") %>%
  mutate(tons = factor(tons, levels = c("<5t","5-20t","20-100t","100-500t","500-1000t","1000-3000t",">3000t")))

plot_acci = ggplot(dat_acci, aes(x = year, y = number, fill = tons, order = tons)) + 
  geom_bar(position = "stack",stat = "identity") + 
  ylim(0,600) + 
  scale_fill_discrete("Boat size", 
                    breaks= c("<5t","5-20t","20-100t","100-500t","500-1000t","1000-3000t",">3000t"),
                    labels= c("<5t","5-20t","20-100t","100-500t","500-1000t","1000-3000t",">3000t")) + 
  labs(x = "Year", y = "Number of Accidents") + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

ggsave("accident_fishery_2015-2019.png", plot_acci)

