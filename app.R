library(shiny)
library(ggplot2)
library(plotly)
library(DT)
library(dplyr)
library(readr)

is_cat <- function(x) is.character(x) || is.factor(x)

df <- read_csv("weather.csv", show_col_types = FALSE)

# convert karakter menjadi faktor jika kategorikal
df <- df %>% mutate_if(~is.character(.x) && n_distinct(.x) < 30, as.factor)

ui <- fluidPage(
  titlePanel("Visualisasi Interaktif - weather.csv"),
  sidebarLayout(
    sidebarPanel(
      helpText("Pilih jenis plot dan variabel."),
      
      selectInput("plotType", "Jenis plot:",
                  choices = c("Scatter (interaktif)" = "scatter",
                              "Line (interaktif)"    = "line",
                              "Bar (interaktif)"     = "bar",
                              "Tabel data"           = "table")),
      
      uiOutput("var_select_ui"),
      
      conditionalPanel(
        condition = "input.plotType == 'bar'",
        sliderInput("bins", "Bins (untuk numerik):", min = 2, max = 50, value = 10)
      )
    ),
    
    mainPanel(
      conditionalPanel(condition = "input.plotType != 'table'",
                       plotlyOutput("plot", height = "600px")),
      conditionalPanel(condition = "input.plotType == 'table'",
                       DTOutput("table_out"))
    )
  )
)

server <- function(input, output, session) {
  
  # dynamic UI for variable selection
  output$var_select_ui <- renderUI({
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    cat_cols     <- names(df)[sapply(df, function(x) is_cat(x) || is.factor(x))]
    all_cols     <- names(df)
    
    if (input$plotType %in% c("scatter", "line")) {
      tagList(
        selectInput("xvar", "X variable", choices = all_cols, selected = numeric_cols[1]),
        selectInput("yvar", "Y variable", choices = all_cols, selected = numeric_cols[2])
      )
    } else if (input$plotType == "bar") {
      selectInput("barvar", "Variabel untuk bar plot", choices = all_cols,
                  selected = cat_cols[1] %||% all_cols[1])
    } else if (input$plotType == "table") {
      selectizeInput("cols_show", "Kolom (Opsional)", choices = all_cols, multiple = TRUE)
    }
  })
  
  # PLOT
  output$plot <- renderPlotly({
    req(input$plotType)
    
    d <- df
    
    if (input$plotType == "scatter") {
      req(input$xvar, input$yvar)
      p <- ggplot(d, aes_string(x = input$xvar, y = input$yvar)) +
        geom_point(alpha = 0.7) +
        theme_minimal()
      ggplotly(p)
      
    } else if (input$plotType == "line") {
      req(input$xvar, input$yvar)
      p <- ggplot(d, aes_string(x = input$xvar, y = input$yvar)) +
        geom_line() + geom_point() + theme_minimal()
      ggplotly(p)
      
    } else if (input$plotType == "bar") {
      req(input$barvar)
      var <- input$barvar
      
      if (is_cat(d[[var]]) || is.factor(d[[var]])) {
        summary_df <- d %>% group_by(.data[[var]]) %>% summarize(count = n())
        p <- ggplot(summary_df, aes_string(x = var, y = "count")) +
          geom_col() + theme_minimal()
        ggplotly(p)
      } else {
        bins <- input$bins
        brks <- pretty(range(d[[var]], na.rm = TRUE), n = bins)
        d2 <- d %>% mutate(.bin = cut(.data[[var]], breaks = brks, include.lowest = TRUE))
        summary_df <- d2 %>% group_by(.bin) %>% summarize(count = n())
        p <- ggplot(summary_df, aes(x = .bin, y = count)) +
          geom_col() + theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        ggplotly(p)
      }
    }
  })
  
  # TABEL DATA
  output$table_out <- renderDT({
    d <- df
    if (!is.null(input$cols_show) && length(input$cols_show) > 0) {
      d <- d[, input$cols_show, drop = FALSE]
    }
    datatable(d, options = list(pageLength = 20, scrollX = TRUE))
  })
}

shinyApp(ui, server)
