pacman::p_load(DT,DBI,duckdb,plotly,tidyverse,shiny)

source('myfunc.R')

################################################################################
## global variables
set_fy <- '2023'
set_fys <- c('2018','2019','2020','2021','2022','2023')

################################################################################
## global data
con <- connect_duckdb()

# mst_pref_tihoをglobalに読み込み
mst_pref_tiho <- dbGetQuery(
  con,statement = "SELECT pref,tiho FROM mst_pref_tiho") %>% tibble()

# areamstを読み込み
mst_area <- dbGetQuery(con,statement = "SELECT * FROM mst_area") %>% tibble()

# mst_hpを読み込み
mst_hp <- dbGetQuery(con,statement = "SELECT * FROM mst_hp") %>% tibble()

# dpcmstを読み込み
dpcmst <- dbGetQuery(con,statement = "SELECT * FROM dpcmst") %>% tibble()
# dpcmst

mdc6cd_icd_with_time <- dbGetQuery(con,statement = 'SELECT * FROM mdc6cd_icd_with_time') %>% tibble()
# mdc6cd_icd_with_time
# mdc6cd_icd_with_time <- mdc6cd_icd_with_time %>% 
#   mutate(`2018`=if_else(`2018`=='○',1,0)) %>% 
#   mutate(`2020`=if_else(`2020`=='○',1,0)) %>% 
#   mutate(`2022`=if_else(`2022`=='○',1,0))
opecd_kcode_with_time <- dbGetQuery(con,statement = 'SELECT * FROM opecd_kcode_with_time') %>% tibble()
# opecd_kcode_with_time

dbDisconnect(con,shutdown = T)

list_tiho <- unique(mst_area$地方) %>% c('全体',.)
list_pref <- unique(mst_area$都道府県) %>% c('全体',.)
list_iryo <- unique(mst_area$医療圏) %>% c('全体',.)
list_city <- unique(mst_area$市町村) %>% c('全体',.)
list_hp <- unique(mst_hp$病院) %>% c('',.)
level_mdc2name <- unique(dpcmst$mdc2name) 
list_mdc2name <-  level_mdc2name %>% c('全体',.)
list_mdc6name <- unique(dpcmst$mdc6name) %>% c('全体',.)
list_opename <- unique(dpcmst$opename) %>% c('全体',.)

# rainbowで18色のパレットを作成
# colors <- rainbow(18)

# rainbowで18色のパレットを作成 透明度を50%に設定
colors <- str_c(rainbow(18),'50')

# color_paletteの作成
color_palette <- setNames(colors,level_mdc2name)

#############################################

# test

#############################################

# input_hp <- '社会医療法人財団石心会　埼玉石心会病院'
# 
# mst_hp
# 
# select_area <- mst_area

################################################################################

# select_dpcmst <- dpcmst %>% 
#   filter(mdc6cd=='010030')
# 
# select_mdc6cd_icd_with_time <- mdc6cd_icd_with_time %>% 
#   inner_join(distinct(select_dpcmst,mdc6cd),by='mdc6cd') %>% 
#   mutate(DPC疾患分類=str_glue('{mdc6cd}:{mdc6}')) %>%
#   select(-mdc6cd,-mdc6) %>% 
#   select(DPC疾患分類,ICD10=icd,ICD10病名=icdname,everything()) %>% 
#   print()
# 
# select_mdc6cd_icd_with_time 
# 
# DT::datatable(
#   data = select_mdc6cd_icd_with_time
#   ,filter='top'
#   ,selection='single'
#   ,rownames=T
#   ,options=list(
#     pageLength= 15
#   )
# )
# 
# mydatatable(select_mdc6cd_icd_with_time)
# 
# select_opecd_kcode_with_time <- opecd_kcode_with_time %>% 
#   inner_join(distinct(select_dpcmst,mdc6cd,opecd),by=c('mdc6cd','opecd')) %>% 
#   mutate(DPC疾患分類=str_glue('{mdc6cd}:{mdc6}')) %>%
#   select(-mdc6cd,-mdc6) %>% 
#   select(DPC疾患分類,DPC手術分類=opecd,会計コード=kcode,会計名称=kname,everything()) %>% 
#   print()
# 
# DT::datatable(
#   data = select_opecd_kcode_with_time
#   ,filter='top'
#   ,selection='single'
#   ,rownames=T
#   ,options=list(
#     pageLength= 15
#   )
# )

################################################################################

# select_hp <- get_select_hp(mst_hp,select_area)
# select_hp
# 
# select_mstno <- get_select_mstno(select_hp)
# 
# hp_table <- get_hp_table(select_mstno,select_hp)
# hp_table
# 
# mdc2_table <- get_mdc2_table(select_mstno, select_hp, select_dpcmst)
# mdc2_table
# 
# mdc2_table_focus <- mdc2_table %>%
#   filter(病院==input_hp)
# 
# mdc2_table_focus
# 
# # mdc2_table_focusからswat分析グラフを作成
# get_plotly_mdc2_focus(mdc2_table_focus, level_mdc2name, corlor_palette)
# 
# mdc6_table <- get_mdc6_table(select_mstno, select_hp, select_dpcmst)
# mdc6_table
# 
# mdc6_table_focus <- mdc6_table %>%
#   filter(病院==input_hp)
# 
# # mdc6_table_focusからswat分析グラフを作成
# get_plotly_mdc6_focus(mdc6_table_focus, select_dpcmst,level_mdc2name,corlor_palette)
# 
# mdc10_table <- get_mdc10_table(select_mstno, select_hp, select_dpcmst)
# mdc10_table

