sliderbuttomUI <- function(id,choice,label,inline=T) {
  ns<- NS(id)
  
  tagList(
    radioButtons(ns("cat_slid"), 
                 label = paste0(label," :"), 
                 choices = choice,
                 selected = choice[1],
                 inline = inline)
  )
}

sliderbuttom <- function(input,output,session){
  
  categ <- reactiveValues()
  
  observe({ categ$cat_slid <- input$cat_slid })
  
  return(categ)
  
  
}
