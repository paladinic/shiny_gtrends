# libs ####
library(shiny)
library(gtrendsR)
library(plotly)
library(shinycssloaders)

# ui   ####
ui <- fluidPage(
  tags$title("GTrends"),
# Head --------------------------------------------------------------------


  tags$head(tags$script(
    HTML(
      '$(document).keyup(function(event){if(event.keyCode == 13){$("#get_gt_btn").click();}});'
    )
  )),
  tags$head(tags$style(
    HTML("*{text-align:center !important;margin:5px auto !important;}")
  )),

# Body --------------------------------------------------------------------
  fluidRow(column(4, h3(
    "Google Trends (lite) in Shiny"
  )),
  column(
    4,
    textInput(
      "keywords",
      label = NULL,
      value = "",
      placeholder = "Enter keywords separated by ';'"
    )),
  column(
    4,
    actionButton("get_gt_btn",
                 width = "300px",
                     "Get Google Trends")
  )),
  hr(),
  downloadButton("downloadData", "Download Data"),
  withSpinner(plotlyOutput("plot"))
)

# serv ####
server <- function(input, output, session) {
  
  get_keywords = reactive({
    k = input$keywords
    if (k == "") {
      k = "Example"
    }
    k = strsplit(x = k, split = ";")[[1]]
    return(k)
  })
  
  chart_title = eventReactive(input$get_gt_btn,{get_keywords()})
  
  search_f = function(){  
    output = gtrends(keyword = keyword)  
  }
  
  get_trend = eventReactive(input$get_gt_btn, {
    len = length(get_keywords())
    
    if (len < 6) {
      df = gtrends(get_keywords())$interest_over_time
      
      if (is.character(df$hits)) {
        df$hits[df$hits == "<1"] = 0
        df$hits = as.numeric(df$hits)
      }
    }
    if (len >= 6 & len < 10) {
      kw_1 = get_keywords()[1:5]
      kw_2 = get_keywords()[5:len]
      
      kw_s = get_keywords()[5]
      
      df_1 = gtrends(kw_1)$interest_over_time
      df_2 = gtrends(kw_2)$interest_over_time
      
      if (is.character(df_1$hits)) {
        df_1$hits[df_1$hits == "<1"] = 0
        df_1$hits = as.numeric(df_1$hits)
      }
      if (is.character(df_2$hits)) {
        df_2$hits[df_2$hits == "<1"] = 0
        df_2$hits = as.numeric(df_2$hits)
      }
      
      avg_1 = mean(df_1[df_1$keyword == kw_s, "hits"])
      avg_2 = mean(df_2[df_2$keyword == kw_s, "hits"])
      
      scalar = avg_1 / avg_2
      
      df_1$hits = df_1$hits / scalar
      
      df = rbind(df_1,df_2[df_2$keyword != kw_s,])
    }
    if (len >= 10 & len < 13) {
      kw_1 = get_keywords()[1:5]
      kw_2 = get_keywords()[5:9]
      kw_3 = get_keywords()[c(5,10:len)]
      
      kw_s = get_keywords()[5]
      
      df_1 = gtrends(kw_1)$interest_over_time
      df_2 = gtrends(kw_2)$interest_over_time
      df_3 = gtrends(kw_3)$interest_over_time
      
      if (is.character(df_1$hits)) {
        df_1$hits[df_1$hits == "<1"] = 0
        df_1$hits = as.numeric(df_1$hits)
      }
      if (is.character(df_2$hits)) {
        df_2$hits[df_2$hits == "<1"] = 0
        df_2$hits = as.numeric(df_2$hits)
      }
      if (is.character(df_3$hits)) {
        df_3$hits[df_3$hits == "<1"] = 0
        df_3$hits = as.numeric(df_3$hits)
      }
      
      avg_1 = mean(df_1[df_1$keyword == kw_s, "hits"])
      avg_2 = mean(df_2[df_2$keyword == kw_s, "hits"])
      avg_3 = mean(df_3[df_3$keyword == kw_s, "hits"])
      
      scalar_1 = avg_1 / avg_2
      scalar_2 = avg_3 / avg_2
      
      df_1$hits = df_1$hits / scalar_1
      df_3$hits = df_3$hits / scalar_2
      
      df = rbind(df_1,df_2[df_2$keyword != kw_s,],df_3[df_3$keyword != kw_s,])
    }
    return(df)
  })
  
  output$plot = renderPlotly({
    plot_ly(
      data = get_trend(),
      color = ~ keyword,
      x = ~ date,
      y = ~ hits,
      type = 'scatter',
      mode = 'lines'
    ) %>% layout(title = paste0(chart_title(), collapse = ", "))
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("Google_Trend_", Sys.Date(), "_Download.csv", sep = "")
    },
    content = function(file) {
      write.csv(get_trend(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
