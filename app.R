library(shiny)
library(readxl)
library(shinyjs)
library(dplyr)
source("functions.R")

ui <- bootstrapPage(theme = "theme.css",
  tags$head(tags$script(src="scripts.js")),
  useShinyjs(),
  div(id = "buttonScreen", 
      style = "display: flex; width: 100%; text-align: center; justify-content: center;",
    tags$button(
      id = "buscar",
      class = "btn btn-default action-button shiny-bound-input",
      width = "100%",
      img(src = "img/botones ceular-12.png")
    ),
    hr(),
    tags$button(
      id = "crear", 
      style = "display: flex; width: 100%; text-align: center; justify-content: center;",
      class = "btn btn-default action-button shiny-bound-input",
      width = "100%",
      img(src = "img/botones ceular-13.png")
    )
  ),
  div(id = "crearScreen", class = "crearScreen",
    img(src = "img/botones ceular-13.png",
        style = "display: block; margin-left: auto; margin-right: auto;"),  
    p(id = "ref", '"Tomado de: El libro Cocina"'),
    select_ingUI("mobile"),
    uiOutput("ing_count"),
    uiOutput("selected_ing_list"),
    br(),
    br(),
    priceUI("mobile"),
    br(),
    select_regionUI("mobile"),
    br(),
    actionButton("volver1", label = "Volver", width = "100%"),
    br(),
    div(id = "recetas_title",
      div(id = "recetas", "Recetas"),
      br(),
      tags$button(
        id = "orderTiempo",
        class = "btn btn-default action-button shiny-bound-input",
        img(src = "img/iconos especial cocina 50-04.png")
      ),
      br()
    ),
    uiOutput('results')
  ),
  div(id = "buscarScreen",
    div(id = "search",
      tags$img(src = "img/Iconos especial cocina-01.png"),
      searchNameUI("mobile")
    ),
    br(),
    uiOutput("show_receta"),
    actionButton("volver2", label = "Volver", width = "100%")
  )
)

server <- function(input, output, session) {
  
  recetas_ing <- readRDS("data/recetas.Rda")
  
  rv <- reactiveValues(
    lastClick = "volver",
    lastClickTiempo = "asc" 
  )
  
  data <- callModule(selectData, "mobile", recetas_ing, rv)
  
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
    if (rv$lastClickTiempo == "desc") {
      rv$lastClickTiempo <- "asc"
    } else {
      rv$lastClickTiempo <- "desc"
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
  
  observeEvent(session$clientData$url_search, once = TRUE, {
    if (session$clientData$url_search != "") {
      url <- parseQueryString(session$clientData$url_search)
      rv$lastClick <- "buscar"
      if (url$id != "recetas_prohibidas") {
        showRecetaModal(url$id)
      }
    }
  })
  
  observeEvent(input$last_btn, {
    showRecetaModal(input$last_btn)
  })
  
  showRecetaModal <- function(uidInput) {
    receta <- recetas_ing %>%
      filter(uid == uidInput) %>%
      group_by(uid) %>%
      filter(row_number() == 1)
    showModal(modalDialog(
      title = tags$span(receta$name, id = "modal_title"),
      htmlTemplate("templates/receta_detail.html",
                   instructions = receta$instruc,
                   dificultadImage = getDifcultadImage(receta$dificultad),
                   dificultadText = getDifcultadText(receta$dificultad),
                   twitter = getTwitterLink(uidInput),
                   facebook = getFacebookLink(uidInput),
                   tiempo = ifelse(is.na(receta$tiempo_mins), "", paste(receta$tiempo_mins, " mins")),
                   hiddenTiempo = ifelse(is.na(receta$tiempo_mins), "hidden", ""),
                   hiddenDificultad = ifelse(is.na(receta$dificultad), "hidden", "")
      ),
      footer = modalButton("Cerrar")
    ))
  }
  
  output$selected_ing_list <- callModule(selected_ing_listUI, "mobile")
  
  callModule(observe_checkbox, "mobile")
  
  output$ing_count <- callModule(ing_countUI, "mobile")
  
  output$show_receta <- renderUI({
    d <- data()
    if (nrow(d) > 0) {
      purrr::map(1:nrow(d), function(i) {
        html <- htmlTemplate("templates/receta_list.html",
                             id = d$uid[i],
                             name = d$name[i],
                             tiempo = ifelse(is.na(d$tiempo_mins[i]), "", paste(d$tiempo_mins[i], " mins")),
                             hiddenTiempo = ifelse(is.na(d$tiempo_mins[i]), "hidden", "")
        )
        html
      })
    } else {
      noResults()
    }
  })

  output$results <- renderUI({
    if (!is.null(data()) && nrow(data()) > 0) {
      if (rv$lastClickTiempo == "desc") {
        d <- data() %>%
          arrange(desc(tiempo_mins))
      } else {
        d <- data() %>%
          arrange(tiempo_mins)
      }
      purrr::map(1:nrow(d), function(i) {
        recetaId <- d$uid[i]
        receta <- recetas_ing %>%
          filter(uid == recetaId)
        html <- htmlTemplate("templates/receta_list_detailed.html",
          id = recetaId,
          name = d$name[i],
          dificultadImage = getDifcultadImage(d$dificultad[i]),
          dificultadText = getDifcultadText(d$dificultad[i]),
          tiempo = ifelse(is.na(d$tiempo_mins[i]), "", paste(d$tiempo_mins[i], " mins")),
          ingredientes = createIngredientesText(receta$ing) ,
          twitter = getTwitterLink(recetaId),
          facebook = getFacebookLink(recetaId),
          hiddenTiempo = ifelse(is.na(d$tiempo_mins[i]), "hidden", ""),
          hiddenDificultad = ifelse(is.na(d$dificultad[i]), "hidden", "")
        )
        html
      })
    } else {
      noResults()
    }
  })
}

shinyApp(ui = ui, server = server)
