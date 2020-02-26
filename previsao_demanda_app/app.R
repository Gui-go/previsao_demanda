library("shiny")
library("ggplot2")
library("dplyr")
library("forecast")
library("moments")
library("tseries")
library("Quandl")

# UI ----------------------------------------------------------------------
ui <- fluidPage(
    fluidRow(
        sidebarPanel(width = 3,
                     uiOutput(outputId = "serie_ui"),
                     br(),
                     uiOutput(outputId = "slider_ui"),
                     br(),
                     column(12, align="center", tableOutput("pif")),
                     br(),
                     br(),
                     br(),
                     br(),
                     br()
        ),
        mainPanel(width = 9,
                  plotOutput("plot"),
        )
    )
)





# SERVER ------------------------------------------------------------------
server <- function(input, output) {
    
    serie_quandl <- reactive({
        sq1 <- Quandl::Quandl(paste0("BCB/", input$serie_serv), type = "raw", collapse = "monthly", api_key = "gGdvN9gXsx9hxHMTWPNL")
        sq2 <- sq1[seq(dim(sq1)[1],1),]
    })
    
    output$serie_ui <- renderUI({
        shinyWidgets::pickerInput(
            inputId = "serie_serv",
            label = "Série em análise",
            choices = c("Índice de volume de vendas no varejo - Total" = 1477,
                        "Índice de volume de vendas no varejo - Combustíveis e lubrificantes" = 1492,
                        "Índice de volume de vendas no varejo - Hiper/supermercados, Prod. alimentícios, bebidas, fumo" = 1505,
                        "Índice de volume de vendas no varejo - Tecidos, vestuário e calçados" = 1518,
                        "Índice de volume de vendas no varejo - Móveis e eletrodomésticos" = 1531,
                        "Índice de volume de vendas no varejo - Automóveis, motocicletas, partes e peças" = 1557,
                        "Índice de volume de vendas no varejo - Hipermercados e supermercados" = 1570,
                        "Índice de volume de vendas no varejo - Artigos farmacêuticos, médicos, ortopédicos, de perfumaria e cosméticos" = 20200,
                        "Índice de volume de vendas no varejo - Livros, jornais, revistas e papelaria" = 20201,
                        "Índice de volume de vendas no varejo - Equipamentos e materiais para escritório, informática e comunicação" = 20202,
                        "Índice de volume de vendas no varejo - Outros artigos de uso pessoal e doméstico" = 20203,
                        "Índice de volume de vendas no varejo - Material de construção" = 20204
            ),
            selected = "Índice de volume de vendas no varejo - Tecidos, vestuário e calçados"
        )
    })
    
    output$slider_ui <- renderUI({
        ls <- length(serie_quandl()[,1])
        sliderInput(
            inputId = "slider_serv",
            label = "Período em análise",
            value = c(round(ls/2, 0), ls), min = 1 , max = ls, step = 1)
    })
    
    output$pif <- renderTable({
        pif1 <- tibble("Período Inicial" = paste0(substring(serie_quandl()[input$slider_serv[1], 1], 6, 7), " / ", substring(serie_quandl()[input$slider_serv[1], 1], 1, 4)),
                       "-" = paste("-"),
                       "Período Final" = paste0(substring(serie_quandl()[input$slider_serv[2], 1], 6, 7), " / ", substring(serie_quandl()[input$slider_serv[2], 1], 1, 4)))
    })
    
    output$plot <- renderPlot({
        sq2 <- serie_quandl()
        sqts <- ts(sq2[,2], start = c(as.numeric(substring(sq2[1,1],1,4)), as.numeric(substring(sq2[1,1],6,7))), frequency = 12)
        tsa <- auto.arima(sqts[input$slider_serv[1]:input$slider_serv[2]], seasonal = T, trace = F, ic = input$crit_eval)
        ff <- forecast(tsa, h=6)
        autoplot(ff, ts.colour = 'firebrick1', predict.colour = 'red',
                 predict.linetype = 'dashed', conf.int = T)+
            labs(title="", y="",x="")+
            theme(legend.position = "none")+
            theme_minimal()
    })
    
}

shinyApp(ui = ui, server = server)
