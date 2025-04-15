server <- function(input, output, session) {

  # # # 検証用
  # output$dt_mst_pref_tiho <- renderDT(mydatatable(mst_pref_tiho))
  # output$dt_mst_area <- renderDT(mydatatable(mst_area))
  # output$dt_mst_hp <- renderDT(mydatatable(mst_hp))
  # output$dt_dpcmst <- renderDT(mydatatable(dpcmst))
  
  # 地域選択クリアボタン
  observeEvent(input$clear_area, {
    updateSelectInput(session, 'input_hp', selected = '',choices=list_hp)
    updateSelectInput(session, 'input_tiho', selected = '全体',choices=list_tiho)
    updateSelectInput(session, 'input_pref', selected = '全体',choices=list_pref)
    updateSelectInput(session, 'input_iryo', selected = '全体',choices=list_iryo)
    updateSelectInput(session, 'input_city', selected = '全体',choices=list_city)
  })
  
  # 地域選択クリアボタン
  observeEvent(input$clear_dpc, {
    updateSelectInput(session, 'input_mdc2name', selected = '全体',choices=list_mdc2name)
    updateSelectInput(session, 'input_mdc6name', selected = '全体',choices=list_mdc6name)
    updateSelectInput(session, 'input_opename', selected = '全体',choices=list_opename)
  })
  
  ##############################################################################
  # areaのsidebar処理
  ##############################################################################
  
  # input_hpが更新された時,input_tihoとinput_prefとinput_iryoを更新
  # observeEvent(input$input_hp,{
  observeEvent(input$input_hp,{
    
    if (input$input_hp != '') {
      
      select_hp_focus <- mst_hp %>% 
        filter(病院 == input$input_hp) 
      
      focus_hp_tiho <- mst_area %>%
        filter(都道府県 == select_hp_focus$都道府県) %>%
        pull(地方) %>% 
        unique()
      

      focus_hp_pref <- select_hp_focus$都道府県
      focus_hp_iryo <- select_hp_focus$医療圏
      focus_hp_city <- select_hp_focus$市町村

      updateSelectInput(session, 'input_tiho',selected=focus_hp_tiho,choices=c('全体',focus_hp_tiho))
      updateSelectInput(session, 'input_pref',selected=focus_hp_pref,choices=c('全体',focus_hp_pref))
      updateSelectInput(session, 'input_iryo',selected=focus_hp_iryo,choices=c('全体',focus_hp_iryo))
      # updateSelectInput(session, 'input_city',selected=focus_hp_city,choices=c('全体',focus_hp_city))
      updateSelectInput(session, 'input_city',choices=c('全体',focus_hp_city))
    } else{
      updateSelectInput(session, 'input_tiho',choices=list_tiho,selected='全体')
      updateSelectInput(session, 'input_pref',choices=list_pref,selected='全体')
      updateSelectInput(session, 'input_iryo',choices=list_iryo,selected='全体')
      updateSelectInput(session, 'input_city',choices=list_city,selected='全体')
    }
  })
  
  ##############################################################################
  
  # サイドバーの地方から絞り込み
  select_tiho <- reactive({
    get_select_tiho(
      mst_area,
      side_tiho = input$input_tiho
    )
  })
  
  # select_tihoが更新された時,list_prefsを更新
  observeEvent(input$input_tiho,{
    
    # prefのselectedの制御
    if(input$input_tiho == '全体'){
      updateSelectInput(
        session, 'input_pref',
        selected='全体'
      )
    }
    
    # prefのchoicesの制御
    if(input$input_tiho != '全体' & input$input_hp==''){
      updateSelectInput(
        session, 'input_pref',
        choices=unique(select_tiho()$都道府県) %>% c('全体',.)
      )
    }
  })
  
  ##############################################################################
  
  # サイドバーの都道府県から絞り込み
  select_pref <- reactive({
    get_select_pref(
      select_tiho(),
      side_pref = input$input_pref
    )
  })
  
  # select_tihoが更新された時,list_prefsを更新
  observeEvent(input$input_pref,{
    
    # prefのselectedの制御
    if(input$input_pref == '全体'){
      updateSelectInput(
        session, 'input_iryo',
        selected='全体'
      )
    }
    
    # prefのchoicesの制御
    if(input$input_pref != '全体' & input$input_hp==''){
      updateSelectInput(
        session, 'input_iryo',
        choices=unique(select_pref()$医療圏) %>% c('全体',.)
      )
    }
  })
  
  ##############################################################################
  
  # サイドバーの医療圏から絞り込み
  select_iryo <- reactive({
    get_select_iryo(
      select_pref(),
      side_iryo = input$input_iryo
    )
  })
  
  # select_tihoが更新された時,list_prefsを更新
  observeEvent(input$input_iryo,{
    
    # prefのselectedの制御
    if(input$input_iryo == '全体'){
      updateSelectInput(
        session, 'input_city',
        selected='全体'
      )
    }
    
    # prefのchoicesの制御
    if(input$input_iryo != '全体' & input$input_hp==''){
      updateSelectInput(
        session, 'input_city',
        choices=unique(select_iryo()$市町村) %>% c('全体',.)
      )
    }
  })
  
################################################################################
  
  # サイドバーの市町村から絞り込み
  select_area <- reactive({
    get_select_city(
      select_iryo(),
      side_city = input$input_city
    )
  })
  
  # # # 検証用
  # output$dt_select_area<- renderDT(mydatatable(select_area()))

  ##############################################################################
  # select_hpとselect_mstnoを取得
  ##############################################################################
  
  select_hp <- reactive({
    get_select_hp(mst_hp, select_area())
  })
  
  select_mstno <- reactive({
    get_select_mstno(select_hp())
  })
  
  # select_hpが更新された時List_hpを更新
  observe({
    list_hp <- unique(select_hp()$病院) %>% c('',input$input_hp,.)
    updateSelectInput(
      session,
      'input_hp',
      choices=list_hp,
      selected=input$input_hp
    )
  })

  ##############################################################################
  # dpcmstのsidebar処理
  ##############################################################################

  # # input_mdc2nameが更新された時,mdc6name,openameを全体に戻す
  # observeEvent(input$input_mdc2name,{
  #   updateSelectInput(session, 'input_mdc6name',selected='全体')
  #   updateSelectInput(session, 'input_opename',selected='全体')
  # })
  # 
  # # input_mdc6nameが更新された時,openameを全体に戻す
  # observeEvent(input$input_mdc6name,{
  #   updateSelectInput(session, 'input_opename',selected='全体')
  # })

  #########################################################################
  
  # サイドバーのmdc2nameから絞り込み
  select_mdc2name <- reactive({
    get_select_mdc2name(
      dpcmst,
      side_mdc2name = input$input_mdc2name
    )
  })
  
  # select_mdc2nameが更新された時,list_mdc6nameを更新
  observe({
    list_mdc6name <- unique(select_mdc2name()$mdc6name) %>% c('全体',.)
    
    selected_mdc6name <- if(input$input_mdc6name %in% list_mdc6name){
      input$input_mdc6name
    }else{
      '全体'
    }
    
    updateSelectInput(
      session, 'input_mdc6name',
      choices=list_mdc6name,
      selected=selected_mdc6name
    )
  })
  
  # サイドバーのmdc6nameから絞り込み
  select_mdc6name <- reactive({
    get_select_mdc6name(
      select_mdc2name(),
      side_mdc6name = input$input_mdc6name
    )
  })
  
  # select_mdc6nameが更新された時,list_openameを更新
  observe({
    list_opename <- unique(select_mdc6name()$opename) %>% c('全体',.)
    
    selected_opename <- if(input$input_opename %in% list_opename){
      input$input_opename
    }else{
      '全体'
    }
    
    updateSelectInput(
      session,
      'input_opename',
      choices=list_opename,
      selected=selected_opename
    )
  })
  
  # サイドバーのopenameから絞り込み
  select_dpcmst <- reactive({
    get_select_opename(
      select_mdc6name(),
      side_opename = input$input_opename
    )
  })
  
  # # # 検証用
  # output$dt_select_dpcmst<- renderDT(mydatatable(select_dpcmst()))
  
  ##############################################################################
  # 集計表の取得
  ##############################################################################
  
  hp_table <- reactive({
    get_hp_table(
      select_mstno(),
      select_hp()
    )
  })
  output$dt_hp_table <- renderDT(
    mydatatable(
      hp_table(),
      pctcol=c('医療圏内シェア','救急医療','予定外','救急車率'),
      select_hpname = input$input_hp
    )
  )
  
  # DTで表をクリックしたときに施設の絞り込みをする
  observe({
    
    select_row <- input$dt_hp_table_rows_selected
    
    if(!is.null(select_row)){
      
      selected_hp <- hp_table()$病院[select_row]
      
      if(!is.na(selected_hp)){
        updateSelectInput(
          session,
          inputId='input_hp',
          selected = selected_hp,
          choices = list_hp
        )
      }
    }
  })
  
  
  mdc2_table <- reactive({
    get_mdc2_table(
      select_mstno(),
      select_hp(),
      select_dpcmst()
    )
  })
  
  # mdc2を注目病院にしぼりこみ
  mdc2_table_focus <- reactive({
    mdc2_table() %>% 
      filter(病院 == input$input_hp)
  })
  
  output$dt_mdc2_table_focus <- renderDT(
    mydatatable(
      mdc2_table_focus(),
      pctcol=c('医療圏内シェア','救急医療','予定外','救急車率'),
      select_hpname = input$input_hp,
    )
  )
  
  # DTで表をクリックしたときに,DPCの絞り込みする(DPC大分類別(注目病院))
  observe({
    
    select_row <- input$dt_mdc2_table_focus_rows_selected
    
    if(!is.null(select_row)){
      
      selected_mdc2name <- mdc2_table_focus()$DPC大分類[select_row]
      
      if(!is.na(selected_mdc2name)){
        updateSelectInput(
          session,
          inputId='input_mdc2name',
          selected = selected_mdc2name,
          # choices = c('全体',unique(mdc2_table_focus()$DPC大分類)) 
        )
      }
    }
  })
  ##############################################################################
  
  # mdc2_table_focusからswat分析グラフを作成
  # output$graph_mdc2_focus <- renderPlot({
  #   get_graph_mdc2_focus(mdc2_table_focus())
  # })
  
  output$plotly_mdc2_focus <- renderPlotly({
    get_plotly_mdc2_focus(mdc2_table_focus(), level_mdc2name, corlor_palette)
  })
  
  output$dt_mdc2_table <- renderDT(
    mydatatable(
      mdc2_table(),
      pctcol=c('医療圏内シェア','救急医療','予定外','救急車率'),
      select_hpname = input$input_hp
    )
  )
  
  # 検証用: 未選択状態の時はNULL
  # output$Rows<- renderPrint({input$dt_mdc2_table_rows_selected})
  # output$MDC2<- renderPrint({mdc2_table()$DPC大分類[input$dt_mdc2_table_rows_selected]})
  
  # DTで表をクリックしたときに,DPCの絞り込みする(DPC大分類別(注目病院))
  observe({
    
    select_row <- input$dt_mdc2_table_rows_selected
    
    if(!is.null(select_row)){
      
      selected_mdc2name <- mdc2_table()$DPC大分類[select_row]
      
      if(!is.na(selected_mdc2name)){
        updateSelectInput(
          session,
          inputId='input_mdc2name',
          selected = selected_mdc2name,
          # choices = c('全体',unique(mdc2_table()$DPC大分類)) 
        )
      }
    }
  })
  
  ##############################################################################

  mdc6_table <- reactive({
    get_mdc6_table(
      select_mstno(),
      select_hp(),
      select_dpcmst()
    )
  })
  
  # mdc6を注目病院にしぼりこみ
  mdc6_table_focus <- reactive({
    mdc6_table() %>% 
      filter(病院 == input$input_hp)
  })
  
  output$dt_mdc6_table_focus <- renderDT(
    mydatatable(
      mdc6_table_focus(),
      pctcol=c('医療圏内シェア'),
      select_hpname = input$input_hp,
    )
  )
  
  # DTで表をクリックしたときに,DPCの絞り込みする(DPC大分類別(注目病院))
  observe({
    
    select_row <- input$dt_mdc6_table_focus_rows_selected
    
    if(!is.null(select_row)){
      
      selected_mdc6name <- mdc6_table_focus()$DPC疾患分類[select_row]
      
      if(!is.na(selected_mdc6name)){
        updateSelectInput(
          session,
          inputId='input_mdc6name',
          selected = selected_mdc6name,
          # choices = c('全体',unique(mdc6_table_focus()$DPC疾患分類)) 
        )
      }
    }
  })
  
  # mdc2_table_focusからswat分析グラフを作成
  # output$graph_mdc6_focus <- renderPlot({
  #   get_graph_mdc6_focus(
  #     mdc6_table_focus(),
  #     select_dpcmst()
  #   )
  # })
  
  output$plotly_mdc6_focus <- renderPlotly({
    get_plotly_mdc6_focus(mdc6_table_focus(), select_dpcmst(), level_mdc2name, corlor_palette)
  })
  
  
  output$dt_mdc6_table <- renderDT(
    mydatatable(
      mdc6_table(),
      pctcol=c('医療圏内シェア'),
      select_hpname = input$input_hp
    )
  )
  
  # DTで表をクリックしたときに,DPCの絞り込みする(DPC大分類別(注目病院))
  observe({
    
    select_row <- input$dt_mdc6_table_rows_selected
    
    if(!is.null(select_row)){
      
      selected_mdc6name <- mdc6_table()$DPC疾患分類[select_row]
      
      if(!is.na(selected_mdc6name)){
        updateSelectInput(
          session,
          inputId='input_mdc6name',
          selected = selected_mdc6name,
          # choices = c('全体',unique(mdc6_table()$DPC疾患分類)) 
        )
      }
    }
  })
  

  ##############################################################################
  
  
  mdc10_table <- reactive({
    get_mdc10_table(
      select_mstno(),
      select_hp(),
      select_dpcmst()
    )
  })
  # mdc10を注目病院にしぼりこみ
  mdc10_table_focus <- reactive({
    mdc10_table() %>% 
      filter(病院 == input$input_hp)
  })

  output$dt_mdc10_table_focus <- renderDT(
    mydatatable(
      mdc10_table_focus(),
      pctcol=c('医療圏内シェア'),
      select_hpname = input$input_hp,
    )
  )
  
  # DTで表をクリックしたときに,DPCの絞り込みする(DPC大分類別(注目病院))
  observe({
    
    select_row <- input$dt_mdc10_table_focus_rows_selected
    
    if(!is.null(select_row)){
      
      selected_mdc6name <- mdc10_table_focus()$DPC疾患分類[select_row]
      selected_opename <- mdc10_table_focus()$DPC手術分類[select_row]
      
      if(!is.na(selected_mdc6name)){
        updateSelectInput(
          session,
          inputId='input_mdc6name',
          selected = selected_mdc6name,
          # choices = c('全体',unique(mdc10_table_focus()$DPC疾患分類)) 
        )
      }
      
      if(!is.na(selected_opename)){
        updateSelectInput(
          session,
          inputId='input_opename',
          selected = selected_opename,
          # choices = c('全体',unique(mdc10_table_focus()$DPC手術分類)) 
        )
      }
    }
  })
  
  
  
  output$dt_mdc10_table <- renderDT(
    mydatatable(
      mdc10_table(),
      pctcol=c('医療圏内シェア'),
      select_hpname = input$input_hp
    )
  )
  
  # DTで表をクリックしたときに,DPCの絞り込みする(DPC大分類別)
  observe({
    
    select_row <- input$dt_mdc10_table_rows_selected
    
    if(!is.null(select_row)){
      
      selected_mdc6name <- mdc10_table()$DPC疾患分類[select_row]
      selected_opename <- mdc10_table()$DPC手術分類[select_row]
      
      if(!is.na(selected_mdc6name)){
        updateSelectInput(
          session,
          inputId='input_mdc6name',
          selected = selected_mdc6name,
          # choices = c('全体',unique(mdc10_table()$DPC疾患分類)) 
        )
        
      }
      if(!is.na(selected_opename)){
        updateSelectInput(
          session,
          inputId='input_opename',
          selected = selected_opename,
          # choices=unique(select_mdc6name()$opename) %>% c('全体',.)
          # choices = c('全体',unique(mdc10_table()$DPC手術分類)) 
        )
      }
    }
  })
  
  output$plotly_mdc10_focus <- renderPlotly({
    get_plotly_mdc10_focus(mdc10_table_focus(), select_dpcmst(), level_mdc2name, corlor_palette)
  })

  ##############################################################################
  
  # DPC病名-ICD10マスタ
  select_mdc6cd_icd_with_time <- reactive({
    mdc6cd_icd_with_time %>% 
      inner_join(distinct(select_dpcmst(),mdc6cd),by='mdc6cd') %>% 
      mutate(DPC疾患分類=str_glue('{mdc6cd}:{mdc6}')) %>%
      select(-mdc6cd,-mdc6) %>% 
      select(DPC疾患分類,ICD10=icd,ICD10病名=icdname,starts_with('2'),desc(変更)) 
      
  })
  
  output$dt_select_mdc6cd_icd_with_time <- renderDT(
    DT::datatable(
      data = select_mdc6cd_icd_with_time()
      ,filter='top'
      ,selection='single'
      # ,rownames=T
      ,options=list(pageLength= 15)
    ) %>% 
    formatStyle(
      columns='2018',valueColumns='2018'
      ,color=styleEqual(levels=c(0,1),values=c('black','red'))
    ) %>% 
    formatStyle(
      columns='2020',valueColumns='2020'
      ,color=styleEqual(levels=c(0,1),values=c('black','red'))
    ) %>% 
    formatStyle(
      columns='2022',valueColumns='2022'
      ,color=styleEqual(levels=c(0,1),values=c('black','red'))
    ) %>% 
    formatStyle(
      columns='変更',valueColumns='変更'
      ,color=styleEqual(levels=c('変更なし','変更あり'),values=c('black','red'))
    )
  )
  
  ##############################################################################
  
  # DPC手術-会計マスタ
  select_opecd_kcode_with_time <- reactive({
    
    select_mdc6cd <- unique(select_dpcmst()$mdc6cd)
    select_opecd<- unique(select_dpcmst()$opecd)
    
    side_opecd <- if(input$input_opename=='全体'){
      'xx'
    }else{
      str_sub(input$input_opename,1,2)
    }
     
    
    opecd_kcode_with_time %>% 
      filter(mdc6cd %in% select_mdc6cd) %>% 
      filter(
        `2018` %in% select_opecd
        | `2020` %in% select_opecd
        | `2022` %in% select_opecd
      ) %>% 
      mutate(DPC疾患分類=str_glue('{mdc6cd}:{mdc6}')) %>%
      select(-mdc6cd,-mdc6) %>% 
      mutate(F2022 = if_else(`2022` == side_opecd,1,0)) %>% 
      mutate(F2020 = if_else(`2020` == side_opecd,1,0)) %>% 
      mutate(F2018 = if_else(`2018` == side_opecd,1,0)) %>% 
      arrange(DPC疾患分類,desc(F2022),desc(F2020),desc(F2018),`2022`,`2020`,`2018`,desc(変更),kcode) %>% 
      select(DPC疾患分類,会計コード=kcode,会計名称=kname,starts_with('2'),変更,starts_with('F'))  
  })
  
  output$dt_select_opecd_kcode_with_time <- renderDT(
    DT::datatable(
      data = select_opecd_kcode_with_time()
      ,filter='top'
      ,selection='single'
      ,options=list(
        pageLength= 15
        ,columnDefs=list(list(targets=c(8,9,10),visible=FALSE))
      )
    ) %>% 
    formatStyle(
      columns='2022',valueColumns = 'F2022'
      ,color=styleInterval(cuts=0,values=c('black','red')) # 0以下は黒,0より大きいものは赤
    ) %>% 
    formatStyle(
      columns='2020',valueColumns = 'F2020'
      ,color=styleInterval(cuts=0,values=c('black','red')) # 0以下は黒,0より大きいものは赤
    ) %>%
    formatStyle(
      columns='2018',valueColumns = 'F2018'
      ,color=styleInterval(cuts=0,values=c('black','red')) # 0以下は黒,0より大きいものは赤
    ) %>% 
    formatStyle(
      columns='変更',valueColumns='変更'
      ,color=styleEqual(levels=c('変更なし','変更あり'),values=c('black','red'))
    )
  )
  
  ##############################################################################
  
  # 絞り込み条件をまとめたdfを作成する
  rt_sidebar <- reactive({
    tibble(
      絞込条件 = c(
        "注目病院",
        "比較対象地方",
        "比較対象都道府県",
        "比較対象医療圏",
        "比較対象市区町村",
        "DPC絞込_大分類",
        "DPC絞込_疾患分類",
        "DPC絞込_手術分類"
      ),
      入力 = c(
        input$input_hp,
        input$input_tiho,
        input$input_pref,
        input$input_iryo,
        input$input_city,
        input$input_mdc2name,
        input$input_mdc6name,
        input$input_opename
      )
    )
  })
  
  ##############################################################################

  # データダウンロードのダウンロード機能
  output$download_data <- downloadHandler(
    filename = function(){
      str_glue('DPC退院患者調査分析_{format(Sys.time(), "%Y%m%d_%H%M%S")}.xlsx')
    },
    content = function(file) {
      list(
        "絞込条件" = rt_sidebar(),
        "病院別" = hp_table(),
        "DPC大分類別(注目病院)" = mdc2_table_focus(),
        "DPC大分類別" = mdc2_table(),
        "DPC疾患分類別(注目病院)" = mdc6_table_focus(),
        "DPC疾患分類別" = mdc6_table(),
        "DPC手術分類別(注目病院)" = mdc10_table_focus(),
        "DPC手術分類別" = mdc10_table()
      ) %>%
        writexl::write_xlsx(file)
    }
  )
  ##############################################################################
}

