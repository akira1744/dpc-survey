server <- function(input, output, session) {

  # # 検証用
  # output$dt_mst_pref_tiho <- renderDT(mydatatable(mst_pref_tiho))
  # output$dt_mst_area <- renderDT(mydatatable(mst_area))
  # output$dt_mst_hp <- renderDT(mydatatable(mst_hp))
  # output$dt_dpcmst <- renderDT(mydatatable(dpcmst))
  
  ##############################################################################
  # areaのsidebar処理
  ##############################################################################
  
  # input_hpが更新された時,input_tihoとinput_prefとinput_iryoを更新
  observeEvent(input$input_hp,{
    if (input$input_hp != '') {
      select_hp_focus  <- select_hp() %>% 
        filter(病院 == input$input_hp) 
      updateSelectInput(session, 'input_tiho',selected=select_hp_focus$地方)
      updateSelectInput(session, 'input_pref',selected=select_hp_focus$都道府県)
    }
  })
  
  
  # input_tihoが更新された時,都道府県,医療圏,市町村を全体に戻す
  observeEvent(input$input_tiho,{
    updateSelectInput(session, 'input_pref',selected='全体')
    updateSelectInput(session, 'input_iryo',selected='全体')
    updateSelectInput(session, 'input_city',selected='全体')
  })
  
  # input_prefが更新された時,医療圏,市町村を全体に戻す
  observeEvent(input$input_pref,{
    updateSelectInput(session, 'input_iryo',selected='全体')
    updateSelectInput(session, 'input_city',selected='全体')
  })
  
  # input_iryoが更新された時,市町村を全体に戻す
  observeEvent(input$input_iryo,{
    updateSelectInput(session, 'input_city',selected='全体')
  })
  
  #########################################################################
  
  
  # サイドバーの地方から絞り込み
  select_tiho <- reactive({
    get_select_tiho(
      mst_area,
      side_tiho = input$input_tiho
    )
  })
  
  # select_tihoが更新された時,list_prefsを更新
  observe({
    list_pref <- unique(select_tiho()$都道府県) %>% c('全体',.)
    updateSelectInput(
      session, 'input_pref',
      choices=list_pref,
      selected=input$input_pref
    )
  })
  
  # サイドバーの都道府県から絞り込み
  select_pref <- reactive({
    get_select_pref(
      select_tiho(),
      side_pref = input$input_pref
    )
  })
  
  # select_prefが更新された時,list_iryoを更新
  observe({
    list_iryo <- unique(select_pref()$医療圏) %>% c('全体',.)
    updateSelectInput(
      session,
      'input_iryo',
      choices=list_iryo,
      selected=input$input_iryo
    )
  })
  
  # サイドバーの医療圏から絞り込み
  select_iryo <- reactive({
    get_select_iryo(
      select_pref(),
      side_iryo = input$input_iryo
    )
  })
  
  # select_iryoが更新された時,list_cityを更新
  observe({
    list_city <- unique(select_iryo()$市町村) %>% c('全体',.)
    updateSelectInput(
      session,
      'input_city',
      choices=list_city,
      selected=input$input_city
    )
  })
  
  # サイドバーの市町村から絞り込み
  select_area <- reactive({
    get_select_city(
      select_iryo(),
      side_city = input$input_city
    )
  })
  
  # # 検証用
  # output$dt_select_area<- renderDT(mydatatable(select_city()))

    
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

  # input_mdc2nameが更新された時,mdc6name,openameを全体に戻す
  observeEvent(input$input_mdc2name,{
    updateSelectInput(session, 'input_mdc6name',selected='全体')
    updateSelectInput(session, 'input_opename',selected='全体')
  })
  
  # input_mdc6nameが更新された時,openameを全体に戻す
  observeEvent(input$input_mdc6name,{
    updateSelectInput(session, 'input_opename',selected='全体')
  })

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
    updateSelectInput(
      session, 'input_mdc6name',
      choices=list_mdc6name,
      selected=input$input_mdc6name
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
    updateSelectInput(
      session,
      'input_opename',
      choices=list_opename,
      selected=input$input_opename
    )
  })
  
  # サイドバーのopenameから絞り込み
  select_dpcmst <- reactive({
    get_select_opename(
      select_mdc6name(),
      side_opename = input$input_opename
    )
  })
  
  # # 検証用
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
  
  output$dt_mdc10_table <- renderDT(
    mydatatable(
      mdc10_table(),
      pctcol=c('医療圏内シェア'),
      select_hpname = input$input_hp
    )
  )
  
  output$plotly_mdc10_focus <- renderPlotly({
    get_plotly_mdc10_focus(mdc10_table_focus(), select_dpcmst(), level_mdc2name, corlor_palette)
  })
  
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

