pacman::p_load(DT,DBI,duckdb,plotly,tidyverse,shiny)

source('myfunc.R')

################################################################################
## global variables
set_fy <- '2022'
set_fys <- c('2018','2019','2020','2021','2022')

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
# input_hp <- '社会医療法人財団石心会　埼玉石心会病院'
# 
# mst_hp
# 
# select_area <- mst_area
# 
# select_dpcmst <- dpcmst
# 
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
# get_graph_mdc2_focus(mdc2_table_focus)
# 
# 
# mdc6_table <- get_mdc6_table(select_mstno, select_hp, select_dpcmst)
# mdc6_table
# 
# mdc6_table_focus <- mdc6_table %>% 
#   filter(病院==input_hp) 
# 
# # mdc6_table_focusからswat分析グラフを作成
# get_graph_mdc6_focus(mdc6_table_focus, select_dpcmst)
# 
# mdc10_table <- get_mdc10_table(select_mstno, select_hp, select_dpcmst)
# mdc10_table
