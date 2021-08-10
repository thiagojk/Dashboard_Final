library(shiny)
library(flexdashboard)
library(shinydashboard)
library(rio, warn.conflicts = F)
library(tidyverse, warn.conflicts = F)
library(magrittr, warn.conflicts = F)
library(plotly, warn.conflicts = F)
library(shinythemes, warn.conflicts = F)
library(R0, warn.conflicts = F)
library(brazilmaps, warn.conflicts = F)
library(forecast, warn.conflicts = F)
library(ggfortify, warn.conflicts = F)
library(lubridate, warn.conflicts = F)
library(rsconnect, warn.conflicts = F)
library(readxl)





theme_set(theme_minimal())

ui <- dashboardPage(skin = "purple",
                    dashboardHeader(title = "COVID-19 Belém-PA"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem(text = "Casos COVID-19", tabName = "linhas", icon = icon("virus")),
                        menuItem(text = "Óbitos COVID-19", tabName = "mortes", icon = icon("skull")),
                        menuItem(text = "Taxa de propagação", tabName = "rt", 
                                 icon = icon("head-side-cough")),
                        fileInput(inputId = "df1", label = "Escolha o arquivo CSV",
                                  multiple = F, accept = c("csv", "xlsx"))
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        # Casos COVID-19
                        tabItem(tabName = "linhas",
                                fluidRow(
                                  box(width = 3,
                                      dateRangeInput(inputId = "dat", label = "Insira o período", 
                                                     start = "2020-02-08", end = "2021-07-31")),
                                  valueBoxOutput(width = 3, outputId = "total_casos"),
                                  valueBoxOutput(width = 2, outputId = "media_casos")
                                ),
                                fluidPage(
                                  tabsetPanel(
                                    tabPanel(title = "Casos COVID-19", 
                                             wellPanel(
                                               plotlyOutput(outputId = "plots", width = "100%"))),
                                    tabPanel(title = "Casos/Agosto",
                                             plotOutput(outputId = "barra_semana", width = "100%")),
                                    tabPanel(title = "Previsões",
                                             plotlyOutput(outputId = "previs", width = "100%")),
                                    tabPanel("Mapa",
                                             plotOutput(outputId = "map", width = "100%")),
                                    tabPanel("Dados",
                                             plotOutput(outputId = "grafico1", width = "100%"))
                                  )
                                )),
                        
                        tabItem(tabName = "rt",
                                fluidPage(
                                  fluidRow(
                                    plotlyOutput(outputId = "plot1")))),
                        tabItem(
                          tabName = "mortes",
                          fluidRow(
                            box(width = 3,
                                dateRangeInput(inputId = "datm", label = "Insira o período", 
                                               start = "2020-02-08", end = "2021-07-31")),
                            valueBoxOutput(width = 2, outputId = "total_obito"),
                            valueBoxOutput(width = 2, outputId = "media_obito")
                          ),
                          fluidPage(
                            tabsetPanel(
                              tabPanel(title = "Óbitos COVID-19",
                                       plotlyOutput(outputId = "plot_obito", width = "100%")),
                              tabPanel(title = "Mês/Agosto",
                                       plotOutput(outputId = "barra_semana_obito", width = "100%")
                              ))))
                      )))

###############################################################
###############################################################
###############################################################
###############################################################

options(shiny.maxRequestSize = 150*1024^2)

server <- function(input, output, session){
  
  df <- reactive({
    
    if (is.null(input$df1))
      return(NULL)
    
    
    df2 <- read_xlsx(input$df1$datapath, skip = 6) %>% 
      filter(`Municipio Residência` == "Belém") %>% 
      mutate(datanot = as.Date(`Data Notificação`, format = "%d/%m/%Y"),
             dataobito = as.Date(`Data Obito`, format = "%d/%m/%Y"))
    
    df2
  })
  
  
  # 
  # plotgrafico1 <- reactive({
  #   df %>% 
  #     filter(datanot >= "2021-01-01") %>% 
  #     ggplot(aes(x = as.numeric(Idade))) +
  #     geom_histogram(bins = 30, color  = "black", fill = "salmon", alpha = .7) +
  #     theme_bw()
  # })
  # 
  # output$grafico1 <- renderPlot({
  #   plotgrafico1()
  # })  
  # 
  # 
  
  
  # --------------------------
  
  output$map <- renderPlot({
    get_brmap("City", geo.filter = list(State = 15)) %>% 
      left_join(pop2017, c("City" = "mun")) %>% 
      ggplot() +
      geom_sf(aes(fill = pop2017/1e6))})
  
  
  
  #--------------------------
  
  # Plot do numero de casos
  
  output$plots = renderPlotly({
    
    req(df())
    
    
    df() %>%
      filter(datanot >=  min(input$dat) & datanot <= max(input$dat)) %>% 
      group_by(datanot) %>%
      summarise(count = n()) %>%
      ggplot(aes(x = datanot, y = count)) +
      geom_line(color = "black") +
      geom_point(color = "black") +
      geom_smooth(method = "loess", color = "red") +
      geom_hline(yintercept = 0) +
      labs(x = "Data", y = "Número de casos") +
      scale_x_date(date_labels = "%b %y", date_breaks = "1 month") +
      ggtitle("Número de casos de COVID-19 em Belém") +
      theme_bw()
    
  })
  
  
  # Caixas de valores
  
  # Casos totais de covid
  output$total_casos <- renderValueBox({
    
    req(df())
    
    valueBox(
      df() %>% 
        filter(datanot >= min(input$dat) & datanot <= max(input$dat)) %>%
        group_by(datanot) %>% 
        summarise(count = n()) %>%
        summarise(sum(count)), subtitle = "Casos Totais de COVID", 
      icon = icon("calculator"),
      color = "navy"
    )
  }
  )
  #################################
  
  output$media_casos <- renderValueBox({
    req(df())
    
    valueBox(
      df() %>% 
        filter(datanot > max(input$dat)-7 & datanot <= max(input$dat)) %>%
        group_by(datanot) %>% 
        summarise(count = n()) %>%
        summarise(round(sum(count))),
      subtitle = "Total de casos da Última semana",
      icon = icon("calculator"),
      color = "navy")
  })
  ##################################################################################
  # Barra semana
  # 
  
  
  output$barra_semana <- renderPlot({
    req(df())
    
    df() %>% 
      filter(datanot >= max(input$dat)-30 & datanot <= max(input$dat)) %>%
      group_by(datanot) %>% 
      summarise(count = n()) %>%
      ggplot(aes(x = datanot, y = count)) +
      geom_col(color = "black", fill = "royalblue") +
      geom_point(color = "red") +
      geom_smooth(method = "loess", color = "red") +
      labs(x = "Data", y = "Número de casos") +
      geom_label(aes(label = count)) +
      ylim(c(0,120)) +
      scale_x_date(date_breaks = "1 day", date_labels = "%d %b") +
      theme_bw()
    
  })
  
  # ------------------------------------------------------------------------------
  
  # PrevisÃ£o #
  
  
  output$previs <- renderPlotly({
    
    req(df())
    
    acumulados_julho <- df() %>%  
      filter(datanot >= "2021-06-01") %>% 
      group_by(datanot) %>% 
      summarise(count = n())
    
    i <- 3
    acumulados_julho$acumulado[1] = acumulados_julho$count[1]
    acumulados_julho$acumulado[2] = acumulados_julho$count[2] +
      acumulados_julho$count[1]
    
    
    while(i <= length(acumulados_julho$count)) {
      acumulados_julho$acumulado[i] = acumulados_julho$acumulado[i-1] +
        acumulados_julho$count[i]
      i = i+1
    }
    
    # ------------------------------------------------------------------------------
    # ---------- Criando a sÃ©rie temporal
    # ------------------------------------------------------------------------------
    ts_covid <- ts(acumulados_julho$acumulado, frequency = 24,
                   start = c(2021/06/01,1))
    
    covid_ts <- as.data.frame(ts_covid)
    
    acumulados_julho %>%
      ggplot(aes(x = datanot, y = acumulado)) +
      geom_line() +
      scale_x_date(date_labels = "%d/%b", date_breaks = "1 week") +
      theme_bw()
    
    
    
    
    
    
    previsao <- auto.arima(ts_covid)
    
    prev <- forecast(previsao, h = 30)
    fort <- fortify(prev, ts.connect = FALSE, data = NULL, is.date = NULL)
    fort$Index <- seq(ymd("2021-06-01"), ymd("2021-08-29"), by = "1 day")
    
    
    fort <- fort %>% 
      mutate(casos = coalesce(Data, `Point Forecast`)) %>% 
      rename('date' = "Index",
             "LI" = "Lo 95",
             "LS" = "Hi 95")
    
    
    
    fort %>% 
      ggplot(aes(x = date, y = casos)) +
      geom_line(lwd = 1) +
      scale_x_date(date_labels = "%d/%B", date_breaks = "15 days") +
      geom_hline(yintercept = 3500, color = "red") +
      geom_vline(xintercept = as.Date("2021-07-31"), linetype = "dashed") +
      ylim(c(0, 5000)) +
      geom_ribbon(aes(ymin = LI, ymax = LS), fill = "salmon", alpha = .4) +
      annotate("text", x = as.Date('2021-06-07'), y = 3650, 
               label = "3500 casos", fontface = "bold") +
      labs(x = "Data", y = "Casos acumulados") +
      theme_bw()
  })
  
  
  
  ##################################################################################
  ##################################################################################
  ##################################################################################
  ##################################################################################
  
  #Obitos
  
  
  
  # Óbitos totais de covid
  output$total_obito <- renderValueBox({
    req(df())
    
    valueBox(
      df() %>% 
        filter(Obito == "Sim") %>% 
        group_by(dataobito) %>% 
        summarise(count = n()) %>%
        summarise(sum(count)), subtitle = "Óbitos totais de COVID", 
      icon = icon("calculator"),
      color = "navy"
    )
  }
  )
  #################################
  
  output$media_obito <- renderValueBox({
    req(df())
    
    valueBox(
      df() %>% 
        filter(dataobito > max(input$datm)-7 & dataobito <= max(input$datm),
               Obito == "Sim") %>%
        group_by(dataobito) %>% 
        summarise(count = n()) %>%
        summarise(sum(count)),
      subtitle = "Total de Óbitos da Última semana",
      icon = icon("calculator"),
      color = "navy")
  })
  
  ##################################################################################
  
  output$barra_semana_obito <- renderPlot({
    
    req(df())
    
    df() %>% 
      filter(dataobito >=  max(input$datm)-30 & dataobito <= max(input$datm),
             Obito == "Sim") %>% 
      group_by(dataobito) %>% 
      summarise(count = n()) %>%
      ggplot(aes(x = dataobito, y = count)) +
      geom_col(color = "black") +
      geom_point(color = "red") +
      geom_smooth(method = "loess", color = "red") +
      labs(x = "Data", y = "Número de Óbitos") +
      geom_label(aes(label = count)) +
      scale_x_date(date_breaks = "1 day", date_labels = "%d %b") +
      theme_bw()
  })
  
  #######################################################################
  # Plot obito
  
  output$plot_obito = renderPlotly({
    
    req(df())
    
    df() %>%
      filter(dataobito >=  min(input$datm) & dataobito <= max(input$datm), 
             Obito == "Sim") %>% 
      group_by(dataobito) %>%
      summarise(count = n()) %>%
      ggplot(aes(x = dataobito, y = count)) +
      geom_line(color = "black") +
      geom_point(color = "black") +
      geom_smooth(method = "loess", color = "red") +
      labs(x = "Data", y = "Número de casos") +
      scale_x_date(date_labels = "%b %y", date_breaks = "1 month") +
      ggtitle("Óbitos por COVID-19 em Belém") +
      theme_bw()})
  
  
  
  ##################################################################################
  ##################################################################################
  
  # Calculo taxa de propagaÃ§Ã£o Rt
  
  output$plot1 <- renderPlotly({
    
    req(df())
    
    # Dados para calculo do RT
    df1 <- df() %>%
      dplyr::select(datanot) %>%
      tidyr::drop_na() %>% 
      dplyr::group_by(datanot) %>% 
      summarise(count = n()) %>% 
      rename(date = datanot,
             confirmed = count)
    
    
    gt <-generation.time("gamma", c(3, 1.5))
    
    curva <- abs(df1$confirmed)
    
    rt=est.R0.TD(epi=curva,gt,begin=1,end=(as.numeric(length(curva))-1),
                 correct=T,nsim=1000)
    
    re=estimate.R(epi=curva,gt,method="EG")
    
    
    names(curva)=df1$date
    
    df2=as.data.frame(cbind(rt$R,rt$conf.int))
    df2$date=seq.Date(from=as.Date(rt$begin,
                                   origin = "2020-02-08"),
                      length.out = rt$end.nb,by="days",)
    names(df2)=c("rt","lower","upper","date")
    
    hline <-  function(y = 0, color = "black") {
      list(
        type = "line", 
        x0 = 0, 
        x1 = 1, 
        xref = "paper",
        y0 = y, 
        y1 = y, 
        line = list(color = color)
      )
    }
    
    plot.rt <- function(){
      plot_ly(df2,x=~date) %>%
        add_lines(y=~rt,color=I("red")) %>%
        add_ribbons(ymin=~lower,ymax=~upper,color=I("grey"),opacity=50) %>%
        layout(shapes = list(hline(1))) %>%
        layout(title="Taxa de Transmissão de COVID-19 em Belém-PA",xaxis=list(title="Date"),
               yaxis=list(title="R(t)"),
               showlegend = FALSE)}
    
    plot <- plot.rt()
    
  })}

shinyApp(ui = ui, server = server)



