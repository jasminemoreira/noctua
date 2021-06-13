library(shiny)
library(shinythemes)

shinyUI(fluidPage(

  theme = shinytheme("cosmo"), 
  titlePanel(h1('NOCTUA - Analisador Exploratório de Corpora Textuais',align="center"),"NOCTUA"),
  headerPanel(hr()), 
  
  sidebarLayout(
    sidebarPanel(
       strong(p("ETAPA 1 - Subir o arquivo ZIP")),
       fileInput("file", "", accept = ".zip"),br(),
       strong(p("ETAPA 2 - Explorar os dados!")),br(),
       selectInput("category", "Selecione a Categoria:",c("todas")),
       #selectInput("ntopics", "Selecione o Número de Tópicos",c(1:16),selected = "5"),
       actionButton("wcButton", "Nuvem", width = "100%"), br(),
       actionButton("woButton", "Ocorrências", width = "100%"), br(),
       actionButton("bgButton", "Bigramas", width = "100%"), br(),
       actionButton("tgButton", "Trigramas", width = "100%"), br(),
       actionButton("qgButton", "Quadrigramas", width = "100%"), br(),
       actionButton("tpButton", "Tópicos", width = "100%"), br(),
       actionButton("ddButton", "Distribuição", width = "100%"),br(),
       sliderInput("ntopics", "Selecione o Número de Tópicos:",min = 2, max = 16, value = 5),
       width = 3
    ),
    mainPanel(
      br(),br(),
      plotOutput('plot',width = "100%",height = "780px"),
        hr(),
        HTML('<footer>por Jasmine Moreira</footer>'),
        align="center"
    )
  )
))
