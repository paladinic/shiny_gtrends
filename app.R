# libs ####
library(shiny)
library(gtrendsR)
library(plotly)
library(shinycssloaders)
library(shinythemes)
library(countrycode)

TRY = function(x) {
  tryCatch(
    x,
    error = function(e)
      NULL
  )
}


# ui   ####
ui <- fluidPage(
  theme = shinytheme("yeti"),
  tags$title("GTrends"),
  # Head --------------------------------------------------------------------
  tags$head(tags$script(
    HTML(
      '$(document).keyup(function(event){if(event.keyCode == 13){$("#get_gt_btn").click();}});'
    )
  )),
  
  
  # Body --------------------------------------------------------------------
  h3("Google Trends (lite) in Shiny"),
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      textInput(
        "keywords",
        label = NULL,
        value = "",
        placeholder = "Enter keywords separated by ';'"
      ),
      selectizeInput(
        inputId = "time",
        label = "Time",
        selected = "all",
        choices = c(
          "now 1-H",
          "now 4-H",
          "now 1-d",
          "now 7-d",
          "today 1-m",
          "today 4-m",
          "today+5-y",
          "all"
        )
      ),
      uiOutput("geo_ui"),
      actionButton("get_gt_btn","Get Google Trends"),
      downloadButton("downloadData", "Download Data")
    ),
    mainPanel = mainPanel(withSpinner(plotlyOutput("plot")))
  )
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
  output$geo_ui = renderUI({
    v = unique(codelist$ecb)
    v = v[!is.na(v)]
    v = c("all", v)
    
    selectizeInput(
      inputId = "geo",
      label = "Location",
      selected = "all",
      choices = v
    )
  })
  get_time = reactive({
    time = TRY(input$time)
    if (is.null(time)) {
      time = "all"
    }
    time
  })
  get_geo = reactive({
    geo = TRY(input$geo)
    if (is.null(geo)) {
      geo = "all"
    }
    geo
  })
  
  chart_title = eventReactive(input$get_gt_btn, {
    paste0(paste0(get_keywords(), collapse = ", "),
           " - ",
           get_geo(),
           " - ",
           get_time())
  })
  
  search_f = function(keyword, time, geo) {
    if (geo == "all") {
      o = gtrends(keyword = keyword, time = time)
    }
    else{
      o = gtrends(keyword = keyword,
                  time = time,
                  geo = geo)
    }
    o
  }
  
  get_trend = eventReactive(input$get_gt_btn, {
    len = length(get_keywords())
    
    if (len < 6) {
      df = search_f(get_keywords(), get_time(), get_geo())$interest_over_time
      
      if (is.character(df$hits)) {
        df$hits[df$hits == "<1"] = 0
        df$hits = as.numeric(df$hits)
      }
    }
    if (len >= 6 & len < 10) {
      kw_1 = get_keywords()[1:5]
      kw_2 = get_keywords()[5:len]
      
      kw_s = get_keywords()[5]
      
      df_1 = search_f(kw_1, get_time(), get_geo())$interest_over_time
      df_2 = search_f(kw_2, get_time(), get_geo())$interest_over_time
      
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
      
      df = rbind(df_1, df_2[df_2$keyword != kw_s, ])
    }
    if (len >= 10 & len < 13) {
      kw_1 = get_keywords()[1:5]
      kw_2 = get_keywords()[5:9]
      kw_3 = get_keywords()[c(5, 10:len)]
      
      kw_s = get_keywords()[5]
      
      df_1 = search_f(kw_1, get_time(), get_geo())$interest_over_time
      df_2 = search_f(kw_2, get_time(), get_geo())$interest_over_time
      df_3 = search_f(kw_3, get_time(), get_geo())$interest_over_time
      
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
      
      df = rbind(df_1, df_2[df_2$keyword != kw_s, ], df_3[df_3$keyword != kw_s, ])
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
    ) %>% layout(title = chart_title())
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
