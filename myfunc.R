# DB 接続設定
connect_duckdb <- function(){
  con = dbConnect(
    drv=duckdb::duckdb(),
    dbdir='dpc_survey.duckdb',
    read_only=T
  )
  return(con)
}


################################################################

# カスタマイズDT関数
mydatatable <- function(df,pctcol,hidecol,select_hpname,row=15){
  
  if(missing(hidecol)){
    dt <- DT::datatable(
      data=df
      # ,filter='top'
      ,selection='single'
      ,rownames=T
      # ,extensions = c('Buttons')
      # ,extensions = c('Buttons','KeyTable') # 拡張機能1:出力用のボタン,# 拡張機能:カーソル機能
      ,options=list(
        # dom='Bfrtip' # Buttonsのボタンの設置?
        # ,buttons=c('print') # Buttonsのボタンの種類を指定
        pageLength=row # 表示行数をセット
        ,keys=TRUE # KerTableに必要なoption
      )
    )  
  }else{
    dt <- DT::datatable(
      data=df
      # ,filter='top'
      ,rownames=T
      ,selection='single'
      # ,extensions = c('Buttons')
      # ,extensions = c('Buttons','KeyTable') # 拡張機能1:出力用のボタン,# 拡張機能:カーソル機能
      ,options=list(
        # dom='Bfrtip' # Buttonsのボタンの設置?
        # ,buttons=c('print') # Buttonsのボタンの種類を指定
        pageLength=row # 表示行数をセット
        ,keys=TRUE # KerTableに必要なoption
        ,columnDefs = list(list(targets=hidecol, visible=FALSE)) #列を非表示にする 
      )
    )
  }
  
  # パーセンテージ表示
  if (!missing(pctcol)){
    dt <- formatPercentage(dt,columns=pctcol,digits=1)
  }
  
  # dtの病院列がselect_hpと一致するとき,太字にする
  if (!missing(select_hpname)){
    dt <- formatStyle(
      dt,
      columns='病院',
      target = 'row',
      fontWeight = styleEqual(select_hpname, 'bold')
    )
  }
  
  return(dt)
}

################################################################################

# サイドバーの地方から絞り込み
get_select_tiho <- function(mst_area,side_tiho){
  if(side_tiho == '全体'){
    mst_area
  }else{
    mst_area %>% 
      filter(地方==side_tiho)
  }
}

# サイドバーの都道府県から絞り込み
get_select_pref <- function(select_tiho,side_pref){
  if(side_pref == '全体'){
    select_tiho
  }else{
    select_tiho %>% 
      filter(都道府県==side_pref)
  }
}

# サイドバーの医療圏から絞り込み
get_select_iryo <- function(select_pref,side_iryo){
  if(side_iryo == '全体'){
    select_pref
  }else{
  select_pref %>% 
    filter(医療圏==side_iryo)
  }
}

# サイドバーの市町村から絞り込み
get_select_city <- function(select_iryo,side_city){
  if(side_city == '全体'){
    select_iryo
  }else{
    select_iryo %>% 
      filter(市町村==side_city)
  }
}

################################################################################

# 選択した地域の病院のマスタを取得
get_select_hp <- function(mst_hp,select_area){
  select_hp <- mst_hp %>% 
    inner_join(
      select_area,
      by = c('都道府県','医療圏','市町村'))
}

################################################################################

# 選択した地域のmstnoを取得
get_select_mstno <- function(select_hp){
  select_mstno <- select_hp %>% pull(mstno)
}

################################################################################

# サイドバーのmdc2nameから絞り込み
get_select_mdc2name <- function(dpcmst,side_mdc2name){
  if(side_mdc2name == '全体'){
    dpcmst
  }else{
    dpcmst %>% 
      filter(mdc2name==side_mdc2name)
    
  }
}

# サイドバーのmdc6nameから絞り込み
get_select_mdc6name <- function(select_mdc2name,side_mdc6name){
  if(side_mdc6name == '全体'){
    select_mdc2name
  }else{
    select_mdc2name %>% 
      filter(mdc6name==side_mdc6name)
  }
}

# サイドバーのopenameから絞り込み
get_select_opename <- function(select_mdc6name,side_opename){
  if(side_opename == '全体'){
    select_mdc6name
  }else{
    select_mdc6name %>% 
      filter(opename==side_opename)
  }
}

################################################################################

# agg_wide_hpの取得
get_agg_wide_hp <- function(select_mstno){
  
  con <- connect_duckdb()
  
  str_mstnos <- str_glue("'{select_mstno}'") %>% 
    str_c(collapse = ",")

    sql <- str_glue(
    "SELECT *
    FROM agg_wide_hp
    WHERE mstno in ({str_mstnos})
    ")
  
  agg_wide_hp <- dbGetQuery(con,statement = sql) %>% tibble()
  
  dbDisconnect(con,shutdown = T)
  
  return(agg_wide_hp)
}

################################################################################

# 病院別のtableを整形
create_hp_table <- function(agg_wide_hp, select_hp,set_fys,set_fy){
  hp_table <- agg_wide_hp %>% 
    inner_join(select_hp,by = c('mstno' = 'mstno')) %>% 
    select(
      都道府県,医療圏,市町村,病院,
      any_of(set_fys),
      県内ランク,医療圏内シェア,救急医療,予定外,救急車率,平均日数,
      DPC病床
    ) %>% 
    arrange(desc(!!sym(set_fy))) 
  return(hp_table)
}

################################################################################

# 病院別のtableを取得
get_hp_table <- function(select_mstno,select_hp){
  

  # hp_table
  agg_wide_hp <- get_agg_wide_hp(select_mstno)
  hp_table <- create_hp_table(agg_wide_hp, select_hp,set_fys,set_fy)
  
  return(hp_table)
  
}
################################################################################

# agg_wide_mdc2の取得
get_agg_wide_mdc2 <- function(select_mstno, select_dpcmst){
  
  con <- connect_duckdb()
  
  select_mdc2cd <- unique(select_dpcmst$mdc2cd)
  
  
  str_mstnos <- str_glue("'{select_mstno}'") %>% 
    str_c(collapse = ",")
  
  str_mdc2cd <- str_glue("'{select_mdc2cd}'") %>% 
    str_c(collapse = ",")
  
  
  sql <- str_glue(
    "SELECT *
    FROM agg_wide_mdc2
    WHERE mstno in ({str_mstnos})
    and mdc2cd in ({str_mdc2cd})
    ")
  
  agg_wide_mdc2 <- dbGetQuery(con,statement = sql) %>% tibble()
  
  dbDisconnect(con,shutdown = T)
  
  return(agg_wide_mdc2)
}

################################################################################

# mdc2別のtableを整形
create_mdc2_table <- function(agg_wide_mdc2, select_hp,select_dpcmst,set_fys,set_fy){
  mdc2_table <- agg_wide_mdc2 %>% 
    inner_join(select_hp,by = c('mstno' = 'mstno')) %>% 
    inner_join(
      distinct(select_dpcmst,mdc2cd,mdc2name),
      by = c('mdc2cd' = 'mdc2cd')) %>% 
    select(
      都道府県,医療圏,市町村,病院,
      DPC大分類 = mdc2name,
      any_of(set_fys),
      県内ランク,医療圏内シェア,救急医療,予定外,救急車率,
      DPC病床
    ) %>% 
    arrange(desc(!!sym(set_fy))) 
  return(mdc2_table)
}

################################################################################

# mdc2別のtableを取得
get_mdc2_table <- function(select_mstno, select_hp,select_dpcmst){
  
  # mdc2_table
  agg_wide_mdc2 <- get_agg_wide_mdc2(select_mstno, select_dpcmst)
  mdc2_table <- create_mdc2_table(agg_wide_mdc2, select_hp,select_dpcmst,set_fys,set_fy)
  
  return(mdc2_table)
  
}

###############################################################################

# mdc2_table_focusからswat分析グラフを作成
# get_graph_mdc2_focus <- function(mdc2_table_focus){
#   mdc2_table_focus %>% 
#     mutate(DPC大分類 = factor(DPC大分類,levels = level_mdc2name)) %>%
#     select(DPC大分類,患者数:=contains(set_fy),医療圏内シェア) %>% 
#     ggplot(aes(y=患者数,x=医療圏内シェア))+
#     geom_point(aes(size=患者数,color=DPC大分類),alpha=.5,)+
#     geom_text_repel(aes(label=DPC大分類),size=6)+
#     scale_size_area(max_size=24)+
#     scale_x_continuous(
#       labels = scales::percent_format(accuracy = 1),
#       limits = c(0,NA),
#       breaks=seq(0,1,0.05)
#     )+
#     scale_y_continuous(
#       labels = scales::comma,
#       limits = c(0,NA)
#     )+
#     scale_color_manual(values = color_palette)+
#     theme_bw()+
#     theme(legend.position = 'none')+
#     # テキストサイズを20に
#     theme(text = element_text(size=20))
# }


# mdc2_table_focusからswat分析グラフを作成
get_plotly_mdc2_focus <- function(mdc2_table_focus, level_mdc2name, corlor_palette){
  p <- mdc2_table_focus %>% 
    mutate(DPC大分類 = factor(DPC大分類,levels = level_mdc2name)) %>%
    select(DPC大分類,患者数:=contains(set_fy),医療圏内シェア) %>% 
    ggplot(aes(y=患者数,x=医療圏内シェア),alpha=.8)+
    geom_point(aes(size=患者数,color=DPC大分類))+
    geom_text(aes(label=DPC大分類),size=4)+
    scale_size_area(max_size=20)+
    scale_x_continuous(
      labels = scales::percent_format(accuracy = 1),
      limits = c(0,NA),
      breaks=seq(0,1,0.05)
    )+
    scale_y_continuous(
      labels = scales::comma,
      limits = c(0,NA)
    )+
    scale_color_manual(values = color_palette)+
    theme_bw()+
    theme(legend.position = 'none')+
    # テキストサイズを20に
    theme(text = element_text(size=16))
  
  plotly::ggplotly(p,tooltip=c('color','y','x'))

}

###############################################################################

# agg_wide_mdc6の取得
get_agg_wide_mdc6 <- function(select_mstno, select_dpcmst){
  
  con <- connect_duckdb()
  
  select_mdc6cd <- unique(select_dpcmst$mdc6cd)
  
  str_mstnos <- str_glue("'{select_mstno}'") %>% 
    str_c(collapse = ",")
  
  str_mdc6cd <- str_glue("'{select_mdc6cd}'") %>% 
    str_c(collapse = ",")
  
  
  sql <- str_glue(
    "SELECT *
    FROM agg_wide_mdc6
    WHERE mstno in ({str_mstnos})
    and mdc6cd in ({str_mdc6cd})
    ")
  
  agg_wide_mdc6 <- dbGetQuery(con,statement = sql) %>% tibble()
  
  dbDisconnect(con,shutdown = T)
  
  return(agg_wide_mdc6)
}

################################################################################

# mdc6別のtableを作成
create_mdc6_table <- function(agg_wide_mdc6, select_hp,select_dpcmst,set_fys,set_fy){
  mdc6_table <- agg_wide_mdc6 %>% 
    inner_join(select_hp,by = c('mstno' = 'mstno')) %>% 
    inner_join(
      distinct(select_dpcmst,mdc6cd,mdc6name),
      by = c('mdc6cd' = 'mdc6cd')) %>% 
    select(
      都道府県,医療圏,市町村,病院,
      DPC疾患分類 = mdc6name,
      any_of(set_fys),
      県内ランク,医療圏内シェア,
      平均日数=平均在院日数,
      DPC病床
    ) %>% 
    arrange(desc(!!sym(set_fy))) 
  return(mdc6_table)
}

###############################################################################

# mdc6別のtableを取得
get_mdc6_table <- function(select_mstno, select_hp, select_dpcmst){
  
  agg_wide_mdc6 <- get_agg_wide_mdc6(select_mstno, select_dpcmst)
  mdc6_table <- create_mdc6_table(
    agg_wide_mdc6, select_hp,select_dpcmst,set_fys,set_fy)
  
  return(mdc6_table)
}


###############################################################################

# mdc6_table_focusからswat分析グラフを作成
# get_graph_mdc6_focus <- function(mdc6_table_focus,select_dpcmst){
#   
#   mdc6_table_focus <- mdc6_table_focus %>% 
#     inner_join(
#       select(select_dpcmst,mdc6name,mdc2name),
#       by=c('DPC疾患分類' = 'mdc6name')
#     ) %>% 
#     mutate(mdc2name = factor(mdc2name,levels = level_mdc2name))
#   
#   mdc6_table_focus %>% 
#     select(mdc2name,DPC疾患分類,患者数:=contains(set_fy),医療圏内シェア) %>% 
#     ggplot(aes(y=患者数,x=医療圏内シェア))+
#     geom_point(aes(size=患者数,color=mdc2name),alpha=.5,)+
#     geom_text_repel(aes(label=DPC疾患分類),size=6)+
#     scale_size_area(max_size=24)+
#     scale_x_continuous(
#       labels = scales::percent_format(accuracy = 1),
#       limits = c(0,NA),
#       breaks=seq(0,1,0.05)
#     )+
#     scale_y_continuous(
#       labels = scales::comma,
#       limits = c(0,NA)
#     )+
#     scale_color_manual(values = color_palette)+
#     theme_bw()+
#     theme(legend.position = 'none')+
#     # テキストサイズを20に
#     theme(text = element_text(size=20))
# }


# mdc2_table_focusからswat分析グラフを作成
get_plotly_mdc6_focus <- function(mdc6_table_focus, select_dpcmst, level_mdc2name, corlor_palette){
  
  mdc6_table_focus <- mdc6_table_focus %>% 
    inner_join(
      distinct(select_dpcmst,mdc6name,mdc2name),
      by=c('DPC疾患分類' = 'mdc6name')
    ) %>% 
    mutate(DPC大分類 = factor(mdc2name,levels = level_mdc2name)) %>% 
    select(DPC大分類,DPC疾患分類,患者数:=contains(set_fy),医療圏内シェア) 
  
  p <- mdc6_table_focus %>%
    ggplot(aes(y=患者数,x=医療圏内シェア,label=DPC疾患分類))+
    geom_point(aes(size=患者数,color=DPC大分類),alpha=.8)+
    # top10だけgeom_textする
    geom_text(
      data=slice_max(mdc6_table_focus,患者数,n=10),
      aes(label=DPC疾患分類),size=4)+
    scale_size_area(max_size=20)+
    scale_x_continuous(
      labels = scales::percent_format(accuracy = 1),
      limits = c(0,NA),
      breaks=seq(0,1,0.05)
    )+
    scale_y_continuous(
      labels = scales::comma,
      limits = c(0,NA)
    )+
    scale_color_manual(values = color_palette)+
    theme_bw()+
    theme(legend.position = 'none')+
    # テキストサイズを20に
    theme(text = element_text(size=16))
  
  pl <- plotly::ggplotly(p,tooltip=c('x','y','label'))
  pl <- plotly::layout(pl, font = list(family="IPAexゴシック",color='gray50'))
    
  return(pl)
}


###############################################################################

# agg_wide_mdc10の取得
get_agg_mdc10 <- function(select_mstno, select_dpcmst){
  
  con <- connect_duckdb()
  
  select_mdc6cd <- unique(select_dpcmst$mdc6cd)
  select_opecd <- unique(select_dpcmst$opecd)
  
  
  str_mstnos <- str_glue("'{select_mstno}'") %>% 
    str_c(collapse = ",")
  
  str_mdc6cd <- str_glue("'{select_mdc6cd}'") %>% 
    str_c(collapse = ",")
  
  str_opecd <- str_glue("'{select_opecd}'") %>% 
    str_c(collapse = ",")
  
  sql <- str_glue(
    "SELECT *
    FROM agg_wide_mdc10
    WHERE mstno in ({str_mstnos})
    and mdc6cd in ({str_mdc6cd})
    and opecd in ({str_opecd})
    ")
  
  agg_mdc10 <- dbGetQuery(con,statement = sql) %>% tibble()
  
  dbDisconnect(con,shutdown = T)
  return(agg_mdc10)
}

###############################################################################

# mdc10別のtableを作成
create_mdc10_table <- function(agg_wide_mdc10, select_hp,select_dpcmst,set_fys,set_fy){
  mdc10_table <- agg_wide_mdc10 %>% 
    inner_join(select_hp,by = c('mstno' = 'mstno')) %>% 
    inner_join(
      select(select_dpcmst,mdc6cd,mdc6name,opecd,opename),
      by = c('mdc6cd','opecd')) %>% 
    select(
      都道府県,医療圏,市町村,病院,
      DPC疾患分類 = mdc6name,
      DPC手術分類 = opename,
      any_of(set_fys),
      県内ランク,医療圏内シェア,
      平均日数=平均在院日数,
      DPC病床
    ) %>% 
    arrange(desc(!!sym(set_fy))) 
  return(mdc10_table)
}

###############################################################################

# mdc10別のtableを取得
get_mdc10_table <- function(select_mstno, select_hp, select_dpcmst){
  
  agg_wide_mdc10 <- get_agg_mdc10(select_mstno, select_dpcmst)
  mdc10_table <- create_mdc10_table(
    agg_wide_mdc10, select_hp,select_dpcmst,set_fys,set_fy)
  
  return(mdc10_table)
}

###############################################################################

# mdc10_table_focusからswat分析グラフを作成
get_plotly_mdc10_focus <- function(mdc10_table_focus, select_dpcmst, level_mdc2name, corlor_palette){
  
  mdc10_table_focus <- mdc10_table_focus %>% 
    inner_join(
      distinct(select_dpcmst,mdc6name,mdc2name),
      by=c('DPC疾患分類' = 'mdc6name')
    ) %>% 
    mutate(DPC大分類 = factor(mdc2name,levels = level_mdc2name)) %>% 
    mutate(DPC疾患手術分類 = str_c(DPC疾患分類,"\n",DPC手術分類)) %>%
    select(DPC大分類,DPC疾患手術分類,患者数:=contains(set_fy),医療圏内シェア) 
  
  p <- mdc10_table_focus %>%
    ggplot(aes(y=患者数,x=医療圏内シェア,label=DPC疾患手術分類))+
    geom_point(aes(size=患者数,color=DPC大分類),alpha=.8)+
    # top10だけgeom_textする
    geom_text(
      data=slice_max(mdc10_table_focus,患者数,n=10),
      aes(label=DPC疾患手術分類),size=4)+
    scale_size_area(max_size=20)+
    scale_x_continuous(
      labels = scales::percent_format(accuracy = 1),
      limits = c(0,NA),
      breaks=seq(0,1,0.05)
    )+
    scale_y_continuous(
      labels = scales::comma,
      limits = c(0,NA)
    )+
    scale_color_manual(values = color_palette)+
    theme_bw()+
    theme(legend.position = 'none')+
    # テキストサイズを20に
    theme(text = element_text(size=16))
  
  
  plotly::ggplotly(p,tooltip=c('x','y','label'))

}
