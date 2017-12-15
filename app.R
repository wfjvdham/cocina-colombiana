library(shiny)
library(readxl)
library(shinyjs)
library(dplyr)
source("functions.R")

ui <- bootstrapPage(
  theme = "theme.css",
  tags$head(tags$script(src="scripts.js")),
  useShinyjs(),
  div(id = "mobile",
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
          div(style = "width: 100%;",
              img(src = "img/botones ceular-13.png", style = "display: block; margin-left: auto; margin-right: auto;"),  
              p(id = "ref", '"Tomado de: Gran Libro de la Cocina Colombiana"')
          ),
          div(id = "left",
              uiOutput("select_ingUI"),
              uiOutput("ing_count"),
              uiOutput("selected_ing_list"),
              br(),
              br(),
              uiOutput("priceUI"),
              br(),
              uiOutput("select_regionUI"),
              br(),
              actionButton("volver1", label = "Volver", width = "100%"),
              br()
          ),
          div(id = "right",
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
          )
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
)

recetas <- readRDS("data/recetas.Rda")

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    lastClick = "volver",
    lastClickTiempo = "asc" 
  )
  
  dataBuscar <- reactive({
    d <- recetas %>%
      group_by(uid) %>%
      filter(row_number() == 1) %>%
      ungroup()
    
    tmp <- search_table(input$searchName, d, "name") %>%
      head(5)
    hasSearchTerm <- !is.null(input$searchName) && input$searchName != ""
    if (!hasSearchTerm && session$clientData$url_search != "") {
      url <- parseQueryString(session$clientData$url_search)
      if (url$id == "recetas_prohibidas") {
        tmp <- d %>%
          filter(prohibida == TRUE)
      }
    }
    tmp
  })
  
  dataCrear <- reactive({
    d <- recetas %>%
      group_by(uid) %>%
      filter(row_number() == 1) %>%
      ungroup()
    
    if (!is.null(input$select_ing)) {
      uids_to_show <- recetas %>%
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
  
  output$select_ingUI <- renderUI({
    d <- recetas %>%
      filter(!is.na(ing))
    choices <- setNames(unique(d$ing), purrr::map(unique(d$ing), firstup))
    selectizeInput("select_ing", 
                   label = NULL,
                   choices = choices, 
                   width = "100%",
                   multiple = TRUE, 
                   options = list(plugins = list("remove_button"),
                                  placeholder = "Escribe los ingredientes")
    )
  })
  
  
  output$selected_ing_list <- renderUI({
    choices <- NULL
    if (!is.null(input$select_ing)) {
      choices <- setNames(input$select_ing, purrr::map(input$select_ing, firstup))
    }
    checkboxGroupInput("selected_ing_checkbox_group", label = NULL,
                       choices = choices,
                       selected = input$select_ing
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
    div(id = "price",
        sliderInput("price",  min = 0, max = 100,
                    htmlTemplate("templates/price_label.html"),  
                    value = 60, width = "100%", pre = "$ ", post = " mil")
    )
  })
  
  output$searchNameUI <- renderUI({
    textInput("searchName", placeholder = "BUSCA TU RECETA", 
              label = NULL, width = "100%")
  })
  
  showRecetaModal <- function(uidInput) {
    receta <- recetas %>%
      filter(uid == uidInput) %>%
      group_by(uid) %>%
      filter(row_number() == 1)
    fillDownloadData(uidInput, "Modal")
    showModal(modalDialog(
      title = tags$span(receta$name, id = "modal_title"),
      htmlTemplate("templates/receta_detail.html",
                   instructions = receta$instruc,
                   dificultadImage = getDifcultadImage(receta$dificultad),
                   dificultadText = getDifcultadText(receta$dificultad),
                   twitter = getTwitterLink(uidInput),
                   facebook = getFacebookLink(uidInput),
                   pinterest = getPinterestLink(uidInput),
                   whatsapp = getWhatsAppLink(uidInput),
                   tiempo = ifelse(is.na(receta$tiempo_mins), "", paste(receta$tiempo_mins, " mins")),
                   hiddenTiempo = ifelse(is.na(receta$tiempo_mins), "hidden", ""),
                   hiddenDificultad = ifelse(is.na(receta$dificultad), "hidden", ""),
                   download = uiOutput(paste0("downloadButtonModal", uidInput))
      ),
      footer = modalButton("Cerrar")
    ))
  }
  
  output$show_receta <- renderUI({
    d <- dataBuscar()
    if (nrow(d) > 0 && rv$lastClick == "buscar") {
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
  
  fillDownloadData <- function (id, namespace = "List") {
    receta <- recetas %>%
      filter(uid == id) %>%
      filter(row_number() == 1)
    
    output[[paste0("downloadButton", namespace, id)]] <- renderUI({
      downloadLink(paste0("downloadData", namespace, id), 
                   div(style="display:flex; font-weight: 300; font-size: 8pt;",
                    img(src="img/Iconos especial cocina-05.png", class="image_smaller", style="margin-top: 2px;font-weight: 300;"),
                    p("Descargar", style="margin-top: 3px;")
                   )
      )
    })
    
    output[[paste0("downloadData", namespace, id)]] <- downloadHandler(
      paste0('receta_', id, '.pdf'),
      content = function(file) {
        params <- list(
          name = receta$name,
          instruc = receta$instruc
        )
        rmarkdown::render("download_template.Rmd",
                          params = params,
                          output_file = "built_report.pdf")
        readBin(con = "built_report.pdf",
                what = "raw",
                n = file.info("built_report.pdf")[, "size"]) %>%
          writeBin(con = file)
        contentType = 'built_report.pdf'
      }
    )   
  }
  
  output$results <- renderUI({
    if (!is.null(dataCrear()) && nrow(dataCrear()) > 0 && rv$lastClick == "crear") {
      if (rv$lastClickTiempo == "desc") {
        d <- dataCrear() %>%
          arrange(desc(tiempo_mins))
      } else {
        d <- dataCrear() %>%
          arrange(tiempo_mins)
      }
      withProgress(message = 'Leyendo las recetas', value = 0, {
        purrr::map(1:nrow(d), function(i) {
          incProgress(1/nrow(d), detail = paste("receta ", i))
          recetaId <- d$uid[i]
          receta <- recetas %>%
            filter(uid == recetaId)
          fillDownloadData(recetaId)
          html <- htmlTemplate("templates/receta_list_detailed.html",
                               id = recetaId,
                               name = d$name[i],
                               dificultadImage = getDifcultadImage(d$dificultad[i]),
                               dificultadText = getDifcultadText(d$dificultad[i]),
                               tiempo = ifelse(is.na(d$tiempo_mins[i]), "", paste(d$tiempo_mins[i], " mins")),
                               ingredientes = createIngredientesText(receta$ing) ,
                               twitter = getTwitterLink(recetaId),
                               facebook = getFacebookLink(recetaId),
                               pinterest = getPinterestLink(recetaId),
                               whatsapp = getWhatsAppLink(recetaId),
                               hiddenTiempo = ifelse(is.na(d$tiempo_mins[i]), "hidden", ""),
                               hiddenDificultad = ifelse(is.na(d$dificultad[i]), "hidden", ""),
                               download = uiOutput(paste0("downloadButtonList", recetaId))
          )
          html
        })
      })
    } else {
      noResults()
    }
  })
}

shinyApp(ui = ui, server = server)
