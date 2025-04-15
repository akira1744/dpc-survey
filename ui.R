ui <- 
  navbarPage(
    title='DPC退院患者調査分析',
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
          h4('比較対象地域'),
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
          actionButton("clear_area", "注目病院・地域選択をクリア", icon = icon("eraser"), class = "btn-light btn-sm"),
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
          actionButton("clear_dpc", "DPC選択をクリア", icon = icon("eraser"), class = "btn-light btn-sm"),
          hr(style = "border: 1px solid gray;"),
          h4('データダウンロード'),
          downloadButton('download_data',label='Download'),
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
              title='TOP',
              h3('概要'),
              p('本サイトは、DPC退院患者調査の結果を分析するためのサイトです。'),
              p('病院ごとDPCごとの年間退院患者数を比較することができます。'),
              p('現在,2018年度~2023年度の6年間分のデータを格納しています。'),
              
              h3('使用方法'),
              h4('Step1: サイドバーで注目病院を設定してください'),
              p('・注目病院の実績が太字で表示されるようになります。'),
              p('・DPC大分類別(注目病院),DPC疾患分類別(注目病院),DPC手術分類類別(注目病院)の実績が表示されるようになります。'),
              
              h4('Step2: サイドバーで比較対象地域を設定してください'),
              p('・比較対象設定のタブで、選択された施設を確認できます。'),
              
              h4('Step3: 実績確認'),
              p('・タブを切り替えることで、深掘りして実績を確認することができます。'),
              p('・病院別: 病院全体の実績'),
              p('・DPC大分類別: DPC上2桁ごとの実績'),
              p('・DPC疾患分類別: DPC上6桁ごとの実績'),
              p('・DPC手術分類別: DPC上6桁 + DPC手術分類ごとの実績'),
              
              h3('データソース'),
              a('令和５年度DPC導入の影響評価に係る調査「退院患者調査」の結果報告について',href='https://www.mhlw.go.jp/stf/shingi2/newpage_00137.html'),
              br(),
              a('令和４年度DPC導入の影響評価に係る調査「退院患者調査」の結果報告について',href='https://www.mhlw.go.jp/stf/shingi2/newpage_39119.html'),
              br(),
              a('令和３年度DPC導入の影響評価に係る調査「退院患者調査」の結果報告について',href='https://www.mhlw.go.jp/stf/shingi2/0000196043_00006.html'),
              br(),
              a('令和２年度DPC導入の影響評価に係る調査「退院患者調査」の結果報告について',href='https://www.mhlw.go.jp/stf/shingi2/0000196043_00005.html'),
              br(),
              a('令和元年度DPC導入の影響評価に係る調査「退院患者調査」の結果報告について',href='https://www.mhlw.go.jp/stf/shingi2/0000196043_00004.html'),
              br(),
              a('平成30年度DPC導入の影響評価に係る調査「退院患者調査」の結果報告について',href='https://www.mhlw.go.jp/stf/shingi2/0000196043_00003.html'),

              h3('DPC退院患者調査データの注意点'),
              h4('DPC退院患者調査の退院患者数は、実際の退院患者よりも少ないです。'),
              p('・詳細は、「参考資料1:分析対象データ」、「参考資料2:集計条件について」をご参照ください。'),
              a('・参考資料1:分析対象データ',href='https://www.mhlw.go.jp/content/12404000/001469141.pdf'),
              br(),
              a('・参考資料2:集計条件について',href='https://www.mhlw.go.jp/content/12404000/001468864.pdf'),
              
              h4('DPC退院患者調査に参加していない病院のデータは含まれていません。'),
              # （１）分析対象データについてから施設数を算出
              p('DPC退院患者調査集計対象施設数は6304施設(2023年度)です。'),
              p('DPCに該当しないケースは集計対象外です。'),
              p('主に急性期病院の実績データとして解釈してください。'),
              p('医療圏内シェア率は、あくまでDPC退院患者調査の範囲内でのシェア率を算出しておりますのでご注意ください。'),
              
              h4('経年比較する際はDPC定義の変更にご注意ください。'),
              p('DPC定義の変更は「DPC病名マスタ」、「DPC手術分類マスタ」のタブで確認できます。'),
              
              h3('問い合わせ先'),
              HTML("
                <ul>
                  <li>Twitter: <a href='https://x.com/akira1744' target='_blank'>@akira1744</a></li>
                  <li>GitHub: <a href='https://github.com/akira1744' target='_blank'>https://github.com/akira1744</a></li>
                </ul>
              "),
              
            ),
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
            tabPanel(
              title='DPC病名マスタ',
              DTOutput('dt_select_mdc6cd_icd_with_time')
            ),
            tabPanel(
              title='DPC手術分類マスタ',
              DTOutput('dt_select_opecd_kcode_with_time')
            )
          )
        )
      )
    ),
  )
