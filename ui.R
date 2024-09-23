ui <- 
  navbarPage(
    title='DPC退院患者調査',
    tabPanel(
      # Tab1
      title='実績比較',
      sidebarLayout(
        # Tab1-sidebar
        sidebarPanel(
          width=2,
          selectInput(
            inputId='input_hp',
            label=h4('注目病院'),
            choices=list_hp,
            selected='全体'
          ),
          hr(),
          h4('地域で絞り込み'),
          selectInput(
            inputId='input_tiho',
            label='地方',
            choices=list_tiho,
            selected='全体'
          ),
          selectInput(
            inputId='input_pref',
            label='都道府県',
            choices=list_pref,
            selected='全体'
          ),
          selectInput(
            inputId='input_iryo',
            label='医療圏',
            choices=list_iryo,
            selected='全体'
          ),
          selectInput(
            inputId='input_city',
            label='市区町村',
            choices=list_city,
            selected='全体'
          ),
          hr(),
          h4('DPCで絞り込み'),
          selectInput(
            inputId='input_mdc2name',
            label='DPC大分類',
            choices=list_mdc2name,
            selected='全体'
          ),
          selectInput(
            inputId='input_mdc6name',
            label='DPC疾患分類',
            choices=list_mdc6name,
            selected='全体'
          ),
          selectInput(
            inputId='input_opename',
            label='DPC手術分類',
            choices=list_opename,
            selected='全体'
          ),
          
        ),
        # Tab1-mainpanel
        mainPanel(
          width=10,
          tabsetPanel(
            type='tabs',
            # # 検証用
            # tabPanel(
            #   title='mst_pref_tiho',
            #   DTOutput('dt_mst_pref_tiho')
            # ),
            # # 検証用
            # tabPanel(
            #   title='mst_area',
            #   DTOutput('dt_mst_area')
            # ),
            # # 検証用
            # tabPanel(
            #   title='mst_hp',
            #   DTOutput('dt_mst_hp')
            # ),
            # # 検証用
            # tabPanel(
            #   title='dpcmst',
            #   DTOutput('dt_dpcmst')
            # ),
            # # 検証用
            # tabPanel(
            #   title='select_area',
            #   DTOutput('dt_select_area')
            # ),
            # # 検証用
            # tabPanel(
            #   title='select_dpcmst',
            #   DTOutput('dt_select_dpcmst')
            # ),
            tabPanel(
              title='病院別',
              DTOutput('dt_hp_table')
            ),
            tabPanel(
              title='DPC大分類別(注目病院)',
              DTOutput('dt_mdc2_table_focus'),
              plotlyOutput('plotly_mdc2_focus',height='800px')
            ),
            tabPanel(
              title='DPC大分類別',
              DTOutput('dt_mdc2_table')
            ),
            tabPanel(
              title='DPC疾患分類別(注目病院)',
              DTOutput('dt_mdc6_table_focus'),
              plotlyOutput('plotly_mdc6_focus',height='800px')
            ),
            tabPanel(
              title='DPC疾患分類別',
              DTOutput('dt_mdc6_table')
            ),
            tabPanel(
              title='DPC手術分類別(注目病院)',
              DTOutput('dt_mdc10_table_focus'),
              plotlyOutput('plotly_mdc10_focus',height='800px')
            ),
            tabPanel(
              title='DPC手術分類別',
              DTOutput('dt_mdc10_table')
            ),

          )
        )
      )
    ),
    # tabPanel(
    #   # Tab1
    #   title='TabTitle2',
    #   sidebarLayout(
    #     # Tab1-sidebar
    #     sidebarPanel(
    #       width=2,
    #       selectInput(
    #         inputId='input2',
    #         label='input2',
    #         choices=c('近畿','関東'),
    #         selected='近畿'
    #       ),
    #     ),
    #     # Tab1-mainpanel
    #     mainPanel(
    #       width=10,
    #       tabsetPanel(
    #         type='tabs',
    #         # Tab1-mainpanel-tabset1
    #         tabPanel(
    #           title='tabpanel2',
    #         )
    #       )
    #     )
    #   )
    # ),
  )
