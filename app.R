library(shiny)
library(readxl)
library(shinyjs)
library(dplyr)

ui <- bootstrapPage(theme = "theme.css",
  tags$head(tags$script(src="scripts.js")),
  useShinyjs(),
  div(id = "buttonScreen",
    actionButton("buscar", label = "BUSCA TU RECETA", width = "100%"),
    hr(),
    actionButton("crear", label = "CREA TU RECETA", width = "100%")
  ),
  div(id = "crearScreen", class = "crearScreen",
    uiOutput("select_ingUI"),
    uiOutput("ing_count"),
    uiOutput("priceUI"),
    uiOutput("select_regionUI"),
    actionButton("volver1", label = "Volver", width = "100%"),
    div(id = "recetas", "Recetas"),
    actionButton("orderTiempo", label = "Order por tiempo", width = "50%"),
    uiOutput('results')
  ),
  div(id = "buscarScreen",
    uiOutput("searchNameUI"),
    uiOutput("show_receta"),
    actionButton("volver2", label = "Volver", width = "100%")
  )
)

server <- function(input, output, session) {
  
  recetas <- readRDS("data/recetas.Rda")
  
  data <- reactive({
    d <- recetas 
    
    if (!is.null(input$select_ing)) {
      uids_to_show <- d %>%
        filter(ing %in% input$select_ing) %>%
        count(uid) %>%
        filter(n == length(input$select_ing))
      d <- d %>%
        filter(uid %in% uids_to_show$uid)
    }
    
    if (!is.null(input$price)) {
      d <- d %>%
        filter(price <= input$price)
    }
    
    if (!is.null(input$region) && input$region != "Todos") {
      d <- d %>%
        filter(region == input$region)
    }
    d
  })
  
  rv <- reactiveValues(
    lastClick = "volver"
  )
  
  observeEvent(input$buscar, {
    rv$lastClick <- "buscar"
  })
  
  observeEvent(input$crear, {
    rv$lastClick <- "crear"
  })
  
  observeEvent(input$volver1, {
    rv$lastClick <- "volver"
  })
  
  observeEvent(input$volver2, {
    rv$lastClick <- "volver"
  })
  
  observeEvent(input$orderTiempo, {
    if (rv$lastClick == "desc") {
      rv$lastClick <- "asc"
    } else {
      rv$lastClick <- "desc"
    }
  })
  
  observe({
    hide("buttonScreen")
    hide("crearScreen")
    hide("buscarScreen")
    if (rv$lastClick == "buscar") {
      showElement("buscarScreen")
    } else if (rv$lastClick == "volver") {
      showElement("buttonScreen")
    } else {
      showElement("crearScreen")
    }
  })
  
  observeEvent(session$clientData$url_search, {
    if (session$clientData$url_search != "") {
      url <- parseQueryString(session$clientData$url_search)
      rv$lastClick <- "buscar"
      showRecetaModal(url$id)
    }
  })
  
  observeEvent(input$last_btn, {
    showRecetaModal(input$last_btn)
  })
  
  showRecetaModal <- function(uidInput) {
    receta <- recetas %>%
      filter(uid == uidInput)
    twitterLink <- paste0("http://twitter.com/share?http://127.0.0.1/?id=", 
                          uidInput)
    facebookLink <- paste0("http://www.facebook.com/sharer.php?u=http://127.0.0.1/?id=",
                           uidInput)
    showModal(modalDialog(
      title = receta$name,
      htmlTemplate("templates/receta_detail.html",
                   instructions = receta$instruc,
                   twitter = twitterLink,
                   facebook = facebookLink
                   
      )
    ))
  }

  output$select_ingUI <- renderUI({
    selectizeInput("select_ing", 
                   "Escribe los ingredientes",
                   label = NULL,
                   choices = unique(recetas$ing), 
                   width = "100%",
                   multiple = TRUE, 
                   options = list(plugins = list("remove_button"))
    )
  })
  
  output$select_regionUI <- renderUI({
    regiones <- recetas %>%
      count(region) %>%
      na.omit()
    regiones_list <- append("Todos", regiones$region)
    radioButtons("region", 
                 "Filtre por región:",
                 choices = regiones_list)
  })
  
  output$priceUI <- renderUI({
    sliderInput("price", "¿Cuánto dinero tienes?", min = 0, max = 100, value = 60, width = "100%")
  })
  
  output$searchNameUI <- renderUI({
    textInput("searchName", "BUSCA TU RECETA", label = NULL, width = "100%")
  })
  
  search_table <- function(query, table, props){
    if (is.null(query) || query == "")
      return(table)
    
    found <- table %>% 
      select(props) 
    found <- found %>% 
      filter(rowSums(mutate_all(found, funs(grepl(query,.,ignore.case = TRUE)))) >= 1L)
    table %>%
      filter(name %in% found$name)
  }
  
  output$show_receta <- renderUI({
    d <- search_table(input$searchName, data(), "name") %>%
      group_by(uid) %>%
      top_n(1) %>%
      head(5)
    if (nrow(d) > 0) {
      purrr::map(1:nrow(d), function(i) {
        html <- htmlTemplate("templates/receta_list.html",
                             id = d$uid[i],
                             name = d$name[i],
                             tiempo = d$tiempo_mins[i]
        )
        html
      })
    } else {
      return("Amigo hoy no hay almuerzo")
    }
  })
  
  output$ing_count <- renderUI({
    h <- 0
    if (!is.null(input$select_ing)) {
      h <- length(input$select_ing)
    }
    HTML(paste("Numero de ingredientes:", h))
  })

  output$results <- renderUI({
    if (!is.null(data()) && nrow(data()) > 0) {
      d <- data() %>%
        group_by(uid) %>%
        top_n(1) %>%
        select(uid, name, region, dificultad, tiempo_mins)
      if (rv$lastClick == "desc") {
        d <- d %>%
          arrange(desc(tiempo_mins))
      } else {
        d <- d %>%
          arrange(tiempo_mins)
      }
      purrr::map(1:nrow(d), function(i) {
        recetaId <- d$uid[i]
        receta <- data() %>%
          filter(uid == recetaId)
        ingredientes <- purrr::map(receta$ing, function(ing) {
          paste0(ing, ', ')
        })
        html <- htmlTemplate("templates/receta_list_detailed.html",
          id = recetaId,
          name = d$name[i],
          dificultad = d$dificultad[i],
          tiempo = d$tiempo_mins[i],
          ingredientes = ingredientes
        )
        html
      })
    } else {
      return("Amigo hoy no hay almuerzo")
    }
  })
}

shinyApp(ui = ui, server = server)

