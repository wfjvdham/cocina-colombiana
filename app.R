library(shiny)
library(readxl)
library(shinyjs)
library(dplyr)
#library(dmaps)

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
    uiOutput("select_ingUI"),
    uiOutput("ing_count"),
    uiOutput("selected_ing_list"),
    br(),
    br(),
    uiOutput("priceUI"),
    br(),
    #dmapsOutput("select_regionUI"),
    uiOutput("select_regionUI"),
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
      uiOutput("searchNameUI")
    ),
    br(),
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
  
  getDifcultadImage <- function(dificultad) {
    if (is.na(dificultad)) {
      dificultadImage <- ""
    } else if (dificultad == 1) {
      dificultadImage <- "img/Iconos especial cocina-06.png"
    } else if (dificultad == 2) {
      dificultadImage <- "img/Iconos especial cocina-07.png"
    } else if (dificultad == 3) {
      dificultadImage <- "img/Iconos especial cocina-08.png"
    }
    dificultadImage
  }
  
  getDifcultadText <- function(dificultad) {
    if (is.na(dificultad)) {
      dificultadText <- ""
    } else if (dificultad == 1) {
      dificultadText <- "Fácil"
    } else if (dificultad == 2) {
      dificultadText <- "Normal"
    } else if (dificultad == 3) {
      dificultadText <- "Difícil"
    }
    dificultadText
  }
  
  firstup <- function(x) {
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
  }
  
  createIngredientesText <- function(ingredientes) {
    result <- ingredientes %>%
      paste(collapse=', ' ) 
    if (result == "NA") {
      ""
    } else {
      result %>%
        stringr::str_sub(end = -1L) %>%
        firstup()
    }
  }
  
  getTwitterLink <- function (id) {
    paste0("http://twitter.com/share?url=http://127.0.0.1/?id=", id)
    #text=
    #hastags=
  }
  
  getFacebookLink <- function (id) {
    paste0("http://www.facebook.com/sharer.php?u=http://127.0.0.1/?id=", id)
    #
  }

  showRecetaModal <- function(uidInput) {
    receta <- recetas %>%
      filter(uid == uidInput)
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

  output$select_ingUI <- renderUI({
    selectizeInput("select_ing", 
                   label = NULL,
                   choices = purrr::map(unique(recetas$ing), firstup), 
                   width = "100%",
                   multiple = TRUE, 
                   options = list(plugins = list("remove_button"),
                                  placeholder = "Escribe los ingredientes")
    )
  })
  
  output$selected_ing_list <- renderUI({
    checkboxGroupInput("selected_ing_checkbox_group", label = NULL,
                       choices = purrr::map(input$select_ing, firstup),
                       selected = purrr::map(input$select_ing, firstup)
    )
  })
  
  observeEvent(input$selected_ing_checkbox_group, {
    selectedOptions <- list()
    if (!is.null(input$selected_ing_checkbox_group))
      selectedOptions <- input$selected_ing_checkbox_group
    updateSelectizeInput(session, "select_ing",
                         selected = selectedOptions)
  }, ignoreNULL = FALSE)
  
  output$ing_count <- renderUI({
    n <- 0
    if (!is.null(input$selected_ing_checkbox_group)) {
      n <- length(input$selected_ing_checkbox_group)
    }
    htmlTemplate("templates/ing_count.html",
                 n = n
    )
  })
  
  # opts <- list(
  #   choroLegend = list(labelFormat = ".0f"),
  #   projectionOpts = list(scale = 3),
  #   palette = 'Oranges'
  # )
  # data2 <- read.csv(system.file("data/carto-2-vars.csv", package = "dmaps"))
  # data2 <- data2[1:20,]
  # 
  # output$select_regionUI <- renderDmaps({
  #   print(data2)
  #   dmaps(data2[c("mupio","depto",var)],
  #         "col_municipalities",
  #         regionCols = c("mupio","depto"),
  #         valueCol = "IDH_1_1")
  #   data <- read.csv(system.file("data/co/ncolegios-departamento.csv",package="dmaps"))
  #   dmaps(data, "col_departments", regionCols = "departamento", valueCol = "count", opts = opts)
  #   dmaps(data = NULL,
  #        "col_departments",
  #        opts = opts
  #   )
  # })
  
  output$select_regionUI <- renderUI({
    regiones <- recetas %>%
      count(region) %>%
      na.omit()
    regiones_list <- append("Todos", regiones$region)
    radioButtons("region",
                 "Filtre por región",
                 choices = regiones_list)
  })
  
  output$priceUI <- renderUI({
    sliderInput("price",  min = 0, max = 100,
                htmlTemplate("templates/price_label.html"),  
                value = 60, width = "100%", pre = "$ ", post = " mil")
  })
  
  output$searchNameUI <- renderUI({
    textInput("searchName", placeholder = "BUSCA TU RECETA", label = NULL, width = "100%")
  })
  
  search_table <- function(query, table, props){
    if (is.null(query) || query == "")
      return(table)
    
    found <- table %>% 
      select(props) %>% 
      filter(rowSums(mutate_all(., funs(grepl(query,.,ignore.case = TRUE)))) >= 1L)
    table %>%
      filter(name %in% found$name)
  }
  
  noResults <- function() {
    htmlTemplate("templates/no_results.html")
  }
  
  output$show_receta <- renderUI({
    d <- search_table(input$searchName, data(), "name") %>%
      group_by(uid) %>%
      filter(row_number() == 1) %>%
      head(5)
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
      d <- data() %>%
        group_by(uid) %>%
        filter(row_number() == 1) %>%
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

