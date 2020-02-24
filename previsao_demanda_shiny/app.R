library("shiny")
library("shinydashboard")
library("ggplot2")
library("plotly")
library("tidyverse")
library("forecast")
library("moments")
library("tseries")
library("Quandl")

# UI ----------------------------------------------------------------------
ui <- fluidPage(
    fluidRow(
        sidebarPanel(width = 3,
                     h4(strong("Painel de controle")),
                     br(),
                     uiOutput(outputId = "serie_ui"),
                     br(),
                     uiOutput(outputId = "slider_ui"),
                     br(),
                     tableOutput("pif"),
                     hr(),
                     uiOutput(outputId = "select_crit"),
                     br(),
                     uiOutput(outputId = "horizon"),
                     br(),
                     verbatimTextOutput(outputId = "auto_arima_print"),
                     br(),
                     h5(strong(textOutput(outputId = "model_par")))
        ),
        mainPanel(width = 9,
                  # uiOutput("arima_param"),
                  br(),
                  br(),
                  br(),
                  plotlyOutput("plot"),
                  
                  
        )
    )
)





# SERVER ------------------------------------------------------------------
server <- function(input, output) {
    
    # API ---------------------------------------------------------------------
    
    serie_quandl <- reactive({
        sq1 <- Quandl::Quandl(paste0("BCB/", input$serie_serv), type = "raw", collapse = "monthly", api_key = "gGdvN9gXsx9hxHMTWPNL")
        sq2 <- sq1[seq(dim(sq1)[1],1),]
        # sq3 <- sq2[seq(dim(sq2)[1],1),]
    })
    
    serie_quandl_ts <- reactive({
        sqts <- ts(serie_quandl()[,2], start = c(as.numeric(substring(serie_quandl()[1,1],1,4)), as.numeric(substring(serie_quandl()[1,1],6,7))), frequency = 12)
    })
    
    arima_data <- reactive({
        tsa <- auto.arima(serie_quandl_ts()[input$slider_serv[1]:input$slider_serv[2]], seasonal = T, trace = F, ic = input$crit_eval)
    })
    
    arima_forecast <- reactive({
        ff <- forecast(arima_data(), h=8)
    })
    

# Ui inputs in server ------------------------------------------------------------

    output$serie_ui <- renderUI({
        shinyWidgets::pickerInput(
            inputId = "serie_serv",
            label = "Série sob análise",
            choices = c("Empregos formais gerados - 12625" = 12625,
                        "Nível do emprego formal - 12622" = 12622,
                        "Índice de volume de vendas no varejo - Total - 1477" = 1477,
                        "Índice de volume de vendas no varejo - Combustíveis e lubrificantes - 1492" = 1492,
                        "Índice de volume de vendas no varejo - Hiper/supermercados, Prod. alimentícios, bebidas, fumo - 1505" = 1505,
                        "Índice de volume de vendas no varejo - Tecidos, vestuário e calçados - 1518" = 1518,
                        "Índice de volume de vendas no varejo - Móveis e eletrodomésticos - 1531" = 1531,
                        "Índice de volume de vendas no varejo - Automóveis, motocicletas, partes e peças - 1557" = 1557,
                        "Índice de volume de vendas no varejo - Hipermercados e supermercados - 1570" = 1570,
                        "Índice de volume de vendas no varejo - Ampliado - 20199" = 20199,
                        "Índice de volume de vendas no varejo - Artigos farmacêuticos, médicos, ortopédicos, de perfumaria e cosméticos - 20200" = 20200,
                        "Índice de volume de vendas no varejo - Livros, jornais, revistas e papelaria - 20201" = 20201,
                        "Índice de volume de vendas no varejo - Equipamentos e materiais para escritório, informática e comunicação - 20202" = 20202,
                        "Índice de volume de vendas no varejo - Outros artigos de uso pessoal e doméstico - 20203" = 20203,
                        "Índice de volume de vendas no varejo - Material de construção - 20204" = 20204,
                        "Exportação de bens - 13081" = 13081,
                        "Importação de bens - 13082" = 13082,
                        "Saldo da balança comercial - 13083" = 13083,
                        "Receita dos estados e municípios (Fluxos) - Arrecadação de ICMS - 4348" = 4348,
                        "Receita dos estados e municípios (Fluxos) - Transferências da União - 4375" = 4375,
                        "Arrecadação de ICMS (Fluxos) - Setor primário - 7646" = 7646,
                        "Arrecadação de ICMS (Fluxos) - Setor terciário - 7668" = 7668,
                        "Arrecadação de ICMS (Fluxos) - Energia - 7679" = 7679,
                        "Arrecadação de ICMS (Fluxos) - Petróleo - 7690" = 7690,
                        "Arrecadação de ICMS (Fluxos) - Outras fontes - 7701" = 7701,
                        "Saldo das operações de crédito do Sistema Financeiro Nacional - Pessoas físicas - 14025" = 14025,
                        "Saldo das operações de crédito do Sistema Financeiro Nacional - Pessoas jurídicas - 14052" = 14052,
                        "Saldo das operações de crédito do Sistema Financeiro Nacional - Total - 14079" = 14079,
                        "Taxa de inadimplência das operações de crédito do Sistema Financeiro Nacional - Pessoas físicas - 15884" = 15884,
                        "Taxa de inadimplência das operações de crédito do Sistema Financeiro Nacional - Pessoas jurídicas - 15916" = 15916,
                        "Taxa de inadimplência das operações de crédito do Sistema Financeiro Nacional - Total - 15948" = 15948
            ),
            selected = "Empregos formais gerados - 12625"
        )
    })
    
    output$slider_ui <- renderUI({
        sliderInput(
            inputId = "slider_serv",
            label = "Período sob análise",
            value = c(1, 120), min = 1 , max = 500, step = 1)
    })
    
    output$select_crit <- renderUI({
        selectInput(inputId = "crit_eval", "Critério seletivo:",
                    multiple = F,
                    selectize = T,
                    c("Akaike Information Criterion - AIC" = "aic",
                      "Akaike Information Criterion Corrected - AICc" = "aicc",
                      "Bayesian Information Criterion - BIC" = "bic"),
                    selected = "aic")
    })
    
    output$horizon <- renderUI({
        numericInput(inputId = "hor", label = "Horizonte de previsão", value = 6, min = 0, max = 12, step = 1)
    })

# UI outputs in server ----------------------------------------------------

    output$pif <- renderTable({
        pif1 <- tibble("Período Inicial" = paste0(substring(serie_quandl()[input$slider_serv[1], 1], 6, 7), " / ", substring(serie_quandl()[input$slider_serv[1], 1], 1, 4)),
                         "-" = paste("-"),
                         "Período Final" = paste0(substring(serie_quandl()[input$slider_serv[2], 1], 6, 7), " / ", substring(serie_quandl()[input$slider_serv[2], 1], 1, 4)))
    })
    
    

# Plots -------------------------------------------------------------------

    output$plot <- renderPlotly({
        nn <- arima_data()[["arma"]]
        ff <- forecast(arima_data(), h = input$hor)
        autoplot(ff)+
            autolayer(ff$mean, color = "blue")+
            labs(title = "", 
                     # paste0("Previsão automatizada através da minimação do parâmetro  ", input$crit_eval),
                     # paste0("Previsoes de um Arima(", nn[1], ",", nn[6], ",", nn[2], ")(", nn[3], ",", nn[7], ",", nn[6], ")[", nn[5], "]"),
                 y = "Y", x = "Observações")+
            theme(legend.position = "none")+
            theme_minimal()
    })


# Prints ------------------------------------------------------------------

    output$model_par <- renderText({
        # nn <- arima_data()[["arma"]]
        # paste0("Parâmetros selecionados: Arima(", nn[1], ",", nn[6], ",", nn[2], ")(", nn[3], ",", nn[7], ",", nn[6], ")[", nn[5], "]")
    })
    
    
    # output$auto_arima_print <- renderPrint({
    #     (auto.arima(serie_quandl_ts()[input$slider_serv[1]:input$slider_serv[2]], seasonal = T, trace = F, ic = "aic"))
    # })    
    
}

shinyApp(ui = ui, server = server)
