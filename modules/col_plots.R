colplotUI <- function(id) {
  ns<- NS(id)
  
  plotOutput(ns("plot1"))
  
}


colplot <- function(input, output, session, data, var_data, breaks, slid, slid_2,slid_buttom) {

  
  output$plot1 <- renderPlot({
    
    df_infra<-data()
    
    if(var_data == "high_cat"){
      df_infra$category_filter<- df_infra$high_cat
    }else {
      df_infra$category_filter<- df_infra$category
      
    }
    
   
    df_infra<-df_infra%>%
      ungroup()%>%
      filter(high_cat %in% slid$cat_slid,
             category %in% slid_2$cat_slid)%>%
      group_by(category_filter,year)%>%
      summarise(total= sum(total,na.rm = T))
    

    if(slid_buttom$cat_slid == "fill (%)"){
      ggplot(data = df_infra) +
      geom_col(aes(year,total/1000,
                   fill=category_filter),
               position = "fill")+
      theme_classic()+
      scale_fill_manual(values = c("skyblue4","wheat2","deepskyblue2"),
                        breaks = breaks)+
      theme(text = element_text(size=13))+
      ylab("Share (%)")+
      xlab("")
    }else{
      ggplot(data = df_infra) +
      geom_col(aes(year,total/1000,
                   fill=category_filter),
               position = "stack")+
      theme_classic()+
      scale_fill_manual(values = c("skyblue4","wheat2","deepskyblue2"),
                        breaks = breaks)+
      theme(text = element_text(size=13))+
      ylab("USD Bn")+
      xlab("")        
      }
  })
  
}

