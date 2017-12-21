getDifcultadImage <- function(dificultad) {
  dificultadImage <- ""
  if (!is.na(dificultad)) {
    if (dificultad == 1) {
      dificultadImage <- "img/Iconos especial cocina-06.png"
    } else if (dificultad == 2) {
      dificultadImage <- "img/Iconos especial cocina-07.png"
    } else if (dificultad == 3) {
      dificultadImage <- "img/Iconos especial cocina-08.png"
    }
  }
  dificultadImage
}

getDifcultadText <- function(dificultad) {
  dificultadText <- ""
  if (!is.na(dificultad)) {
    if (dificultad == 1) {
      dificultadText <- "Fácil"
    } else if (dificultad == 2) {
      dificultadText <- "Normal"
    } else if (dificultad == 3) {
      dificultadText <- "Difícil"
    }
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
  paste0("http://twitter.com/share?url=https://randommonkey.shinyapps.io/cocina-colombia/?id=", id)
  #text=
  #hastags=
}

getFacebookLink <- function (id) {
  paste0("http://www.facebook.com/sharer.php?u=https://randommonkey.shinyapps.io/cocina-colombia/?id=", id)
  #
}

getPinterestLink <- function (id) {
  paste0("http://pinterest.com/pin/create/button/?url=", id)
}

getWhatsAppLink <- function (id) {
  paste0("https://api.whatsapp.com/send?text=https://randommonkey.shinyapps.io/cocina-colombia/?id=", id)
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
