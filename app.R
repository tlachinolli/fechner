library(shiny)
library(shinyWidgets)
source("fechner.R") # archivo de funciones
# 1. el objeto ui
ui <- fluidPage(# lo que ve el usuario
  shinyjs::useShinyjs(),
  # Nombre de App
  headerPanel("Fechner Test"),
  sidebarPanel(# Copy the line below to make a number input box into the UI.
    numericInput(
      "etiqueta",
      label = h3("Ingresa etiqueta de rectángulo elegido"),
      value = NA,
      min = 1,
      max = 10
    ),
    actionButton("enviar", "Enviar")
    ),
  mainPanel(
    setBackgroundImage(src = "https://images.unsplash.com/photo-1615799998603-7c6270a45196?ixlib=rb-4.0.3&ixid=MnwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8&auto=format&fit=crop&w=1304&q=80"),
    tabsetPanel(
    tabPanel(
      "Fechner Test",
      plotOutput("fechner_plot"),
      verbatimTextOutput("gracias")
    ),
    tabPanel("Tendencias", plotOutput("barras")),
    tabPanel("Información",
             div(
             div(   
             h2("Acerca del Autor"),
             p("Atila Orlov"),
             p("tlachinolliatl@gmail.com"),
             tags$a(href="https://www.youtube.com/@atltl", icon("youtube"), "Mi Canal", target="_blank"),
             br(),
             tags$a(href="https://www.linkedin.com/in/atila-orlov/" ,icon("linkedin"), "Mi Perfil", target="_blank"),
             br(),
             tags$a(href="https://github.com/tlachinolli", icon("github"), "Código Fuente", target="_blank"),
             HTML('<center><img src="atltl_logo.gif", height = "80px", width = "120px"></center>'),
             #img(src="atltl_logo.gif", align = 'center',height='80px',width='120px'),
             style = "background-color:#FFFFFF;margin-top:20px;margin-bottom:20px;margin-left:20px;margin-right:20px"
             ),
             style = "background-color:#FFFFFF;margin-top:-20px"
             )
             ),
    tabPanel("Referencias",
             div(style = "background-color:#FFFFFF;margin-top:-20px",
                 div(
                 uiOutput("ref"),
                 style = "background-color:#FFFFFF;margin-top:20px;margin-bottom:20px;margin-left:20px;margin-right:20px"
                 )
             )
             )
  )))
# 2. Servidor (logica interna)
server <- function(input, output) {
  v_ref <- readLines("data/ref.txt")
  l_ref <- list(art = v_ref[3], man = v_ref[c(1, 4, 5, 6)], foto = v_ref[2], codigo = v_ref[c(7, 8)])
  
  df_puntos <- 0
  df_res <- 0
  # engranajes
  output$fechner_plot <- renderPlot({
    inicializar_grafica()
    df_puntos <- agregar_rectangulos()
    df_res <<- agregar_etiquetas(df_puntos)
  })
  
observeEvent(
  input$enviar, {
  output$gracias <- renderPrint({
    prop_ele <- recuperar_respuesta(df_res, input$etiqueta)
    if (!is.na(input$etiqueta) &&
        input$etiqueta <= 10 && input$etiqueta >= 1) {
      print("Su respuesta ha sido enviada")
      print(paste("La proporción Ud eligió fué: ", prop_ele))
      write(prop_ele, file = "proporciones_elegidas.txt", append = TRUE)
    }
  })
  shinyjs::disable("etiqueta")
  }
)
  
  output$barras <- renderPlot({
    v_tendencias <- readLines("data/proporciones_elegidas.txt")
    strtoi(v_tendencias)
    tabla <- table(round(as.numeric(v_tendencias), 2))
    v_col <- colors()[seq(1, 100, len = 10)]
    par(cex = 1.2)
    barplot(tabla,
            axes = FALSE,
            main = "Tendencias",
            xlab = "proporción elegida",
            ylab = "frecuencia",
            col = v_col
            )
    axis(2,
         at = 0:max(tabla))
  })
  
  output$ref <- renderUI({
    HTML(paste(
      h2("Referencias"),
      h3("Artículo Inspiración"),
      paste(l_ref$art, collapse =  "<br/><br/>"),
      h3("Manuales de R"),
      paste(l_ref$man, collapse =  "<br/><br/>"),
      h3("Código Fuente"),
      paste(l_ref$codigo, collapse =  "<br/><br/>"),
      h3("Fotografía de Fondo"),
      paste(l_ref$foto, collapse =  "<br/><br/>")
    ))
    
  })
  
  
}
# 3. Llamada a shiny app
shinyApp(ui, server)
