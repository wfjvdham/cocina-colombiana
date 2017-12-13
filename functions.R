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

select_ingUI <- function(id) {
  ns <- NS(id)
  
  d <- recetas_ing %>%
    filter(!is.na(ing))
  choices <- setNames(unique(d$ing), purrr::map(unique(d$ing), firstup))
  selectizeInput(ns("select_ing"), 
                 label = NULL,
                 choices = choices, 
                 width = "100%",
                 multiple = TRUE, 
                 options = list(plugins = list("remove_button"),
                                placeholder = "Escribe los ingredientes")
  )
}

selected_ing_listUI <- function(input, output, session) {
  renderUI({
    choices <- NULL
    if (!is.null(input$select_ing)) {
      choices <- setNames(input$select_ing, purrr::map(input$select_ing, firstup))
    }
    checkboxGroupInput(session$ns("selected_ing_checkbox_group"), label = NULL,
                       choices = choices,
                       selected = input$select_ing
    )
  })
}

observe_checkbox <- function(input, output, session) {
  observeEvent(input$selected_ing_checkbox_group, {
    selectedOptions <- list()
    if (!is.null(input$selected_ing_checkbox_group))
      selectedOptions <- input$selected_ing_checkbox_group
    updateSelectizeInput(session, "select_ing",
                         selected = selectedOptions)
  }, ignoreNULL = FALSE)
}

ing_countUI <- function(input, output, session) {
  renderUI({
    n <- 0
    if (!is.null(input$selected_ing_checkbox_group)) {
      n <- length(input$selected_ing_checkbox_group)
    }
    htmlTemplate("templates/ing_count.html",
                 n = n
    )
  })
}

select_regionUI <- function(id) {
  ns <- NS(id)
  regiones <- recetas_ing %>%
    count(region) %>%
    na.omit()
  regiones_list <- append("Todos", regiones$region)
  radioButtons(ns("region"),
               "Filtre por región",
               choices = regiones_list)
}

priceUI <- function(id) {
  ns <- NS(id)
  
  div(id = ns("price"),
    sliderInput(ns("price"),  min = 0, max = 100,
                htmlTemplate("templates/price_label.html"),  
                value = 60, width = "100%", pre = "$ ", post = " mil")
  )
}



searchNameUI <- function(id) {
  ns <- NS(id)
  
  textInput(ns("searchName"), placeholder = "BUSCA TU RECETA", 
            label = NULL, width = "100%")
}
  
selectData <- function(input, output, session, recetas_ing, rv) {
  reactive({
    print("calculating d")
    d <- recetas_ing %>%
      group_by(uid) %>%
      filter(row_number() == 1) %>%
      ungroup()
    
    if (rv$lastClick == 'buscar') {
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
      d <- tmp
    } else {
      if (!is.null(input$select_ing)) {
        uids_to_show <- recetas_ing %>%
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
    }
    print("finished calculating d")
    d
  })
}
  
