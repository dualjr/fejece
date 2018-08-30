library("shiny")
library("lpSolveAPI")
library("ucminf")
library("Benchmarking")
library("readxl")
#library("plotly")
library("plyr")

U <- read_excel("fejece.xlsx", sheet=1, col_names=TRUE)
#U <- matrix(c(1,2,3,4,5,6,7,8,8,7,6,5,4,3,2,1,2,4,6,8,10,12,14,16), nrow = 3, ncol = 8)

entradas <- data.frame(U[c(6,7,8)])
saidas <- data.frame(U[c(3,4,5)])

e_crs <- dea(entradas, saidas, RTS = "CRS")
e_vrs <- dea(entradas, saidas,  RTS = "VRS")
A <- eff(e_crs)
B <- eff(e_vrs)

dados <- data.frame(c(U[,1]), c(U[,2]), c(U[,3]), c(U[,4]), c(U[,5]), c(U[,6]), c(U[,7]), c(U[,8]))
efficient <- data.frame(c(U[,1]), "Eficiência CCR" = paste(round(A, digits = 4)*100, "%"), "Eficiência BBC" = paste(round(B, digits = 4)*100, "%"))
library("DT")
library("rsconnect")

server <- function(input, output){
  output$tabela <- DT::renderDataTable({
    datatable(dados, extensions = "Buttons", options = list(dom = "Bfrtip", buttons = c("excel", "pdf", "print"),
              autoWidth = TRUE), editable = TRUE, class = 'cell-border stripe',
              callback = JS("return table;"), filter = "top", style = 'bootstrap')
    })
  output$tabela1 <- DT::renderDataTable({
    datatable(efficient, extensions = "Buttons", options = list(dom = "Bfrtip", buttons = c("excel", "pdf", "print"),
              autoWidth = TRUE), class = 'cell-border stripe',
              callback = JS("return table;"), filter = "top", style = 'bootstrap')
    })
  observeEvent(input$saveBnt,
               write.csv(hot_to_r(input$tabela), file = "base_de_dados.csv", row.names = FALSE)
               )
}

ui <- fluidPage(
  titlePanel("Análise de Eficiência Empresas Juniores - FEJECE"),
  mainPanel(
    tabsetPanel(type = "tab",
                tabPanel("Base de dados", DTOutput("tabela"), 
                         br(),
                         actionButton("saveBnt", "Salvar")),
                tabPanel("Eficiência", DTOutput("tabela1"))
                )
    )
    
  )

shinyApp(ui, server)