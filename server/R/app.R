library(shiny)
library(ggplot2)
library(tidyr)
library(dplyr)

# パラメータ名に応じた逆変換関数
inverse_transform <- function(param, value) {
  if (param %in% c("etaLcl", "etaLvc", "CcPropSd")) {
    return(exp(value))
  } else if (param %in% c("X.etaLcl.etaLvc.")) {
    return(tanh(value))
  } else {
    return(value)
  }
}

ui <- fluidPage(
  titlePanel("リアルタイム最適化モニタリング"),
  plotOutput("combinedPlot", height = "700px")
)

server <- function(input, output, session) {
  log_file_path <- "/project/logs/optimization_log.csv"

  # ログファイルの読み込み
  logData <- reactiveFileReader(
    intervalMillis = 1000,
    session = session,
    filePath = log_file_path,
    readFunc = function(path) {
      if (file.exists(path)) {
        read.csv(path)
      } else {
        data.frame(iter = numeric(0), objf = numeric(0))
      }
    }
  )

  output$combinedPlot <- renderPlot({
    df <- logData()
    if (nrow(df) == 0 || !"iter" %in% names(df)) return(NULL)

    # objf も含めてロング形式に変換
    df_long <- pivot_longer(
      df,
      cols = -iter,
      names_to = "parameter",
      values_to = "value"
    )

    # objf 以外のパラメータを逆変換
    df_long <- df_long %>%
      mutate(
        value_transformed = ifelse(
          parameter == "objf",
          value,
          mapply(inverse_transform, parameter, value)
        )
      )

    ggplot(df_long, aes(x = iter, y = value_transformed)) +
      geom_line(color = "blue") +
      geom_point(color = "blue") +
      facet_wrap(~ parameter, scales = "free_y") +
      labs(
        title = paste("目的関数とパラメータ推移（元スケール）| 最新イテレーション:", max(df$iter)),
        x = "Iteration",
        y = "値"
      ) +
      theme_bw(base_size = 14)
  })
}

shinyApp(ui = ui, server = server)
