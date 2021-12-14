sliderUI <- function(id,choice) {
  ns<- NS(id)
  
  tagList(
    checkboxGroupInput(ns("cat_slid"), 
                       label = "Category: ", 
                       choices = choice,
                       selected = choice,
                       inline = T)
  )
}

slider <- function(input,output,session){
  
  categ <- reactiveValues()
  
  observe({ categ$cat_slid <- input$cat_slid })
  
  return(categ)
  
  
}
