shinyServer(function(input, output, session) {


# Reactive objects --------------------------------------------------------


    infra_react <- reactive({

      infrastucture_data %>%
            filter(year >= input$year[1],
                   year <= input$year[2]) %>%
            mutate(total=if(input$unit=="Gross investment"){gross_inv}
                   else if(input$unit=="Gross investment at 2012"){gross_inv_chain}
                   else{gross_inv_ipd})
    })
    
    map_react <- reactive({
      
      df1<-geo_data %>%
        filter(year >= input$year[1],
               year <= input$year[2]) %>%
        mutate(total=if(input$unit_geo=="Gross investment"){gross_inv}
               else if(input$unit_geo=="Gross investment at 2012"){gross_inv_chain}
               else if(input$unit_geo=="Population"){population}
               else if(input$unit_geo=="Investment at 2012 per capita"){inv_ch_x_pop}
               else{ipd})%>%
        select(-gross_inv,-gross_inv_chain,-ipd,-population,-inv_ch_x_pop)%>%
        group_by(state)%>%
        summarise(total=sum(total,na.rm = T))
      
      df2<-states%>%
        left_join(df1,by = c("region"="state"))
      
      df2
    })
    
    tab_states<- reactive({
      
      map_react()%>%
        group_by(region)%>%
        summarise(total=mean(total))%>%
        rename("Total"="total")%>%
        arrange(desc(Total))%>%
        mutate(Total= prettyNum(round(Total,0),
                                big.mark = ","))
    })


# Tab1: Dashboard ---------------------------------------------------------

  
    # Sub_tab1: Global ----------------------------------------------------

        
    output$year_plot <- renderPlot({
      
      infrastucture_data<-infra_react()%>%
        ungroup()%>%
        group_by(year)%>%
        summarise(total= sum(total,na.rm = T))
      
      if(nrow(infrastucture_data)==0){}else{
        ggplot(data = infrastucture_data) +
        geom_col(aes(year,total/1000),
                 fill="steelblue4")+
        theme_classic()+
        theme(text = element_text(size=13))+
        ylab(paste0(as.character(input$unit)," (USD Bn)"))+
        xlab("")
          
      }
      
    })
     
    
    output$cat_plot <- renderPlot({
      
      infrastucture_data<-infra_react()%>%
        ungroup()%>%
        group_by(meta_cat)%>%
        summarise(total= sum(total,na.rm = T))
      
      if(nrow(infrastucture_data)==0){}else{
        ggplot(data = infrastucture_data) +
        geom_col(aes(x=reorder(meta_cat,-total,FUN = sum),total/1000),
                 fill="steelblue4")+
        theme_classic()+ 
        ylab(paste0(as.character(input$unit)," (USD Bn)"))+
        theme(text = element_text(size=15),
              axis.text.x = element_text(angle=90, hjust=1,vjust = 1))+
        xlab("")
        
      }
    })
    
    # Sub_tab2: Category  -------------------------------------------------
    
    slid<- callModule(module = slider,id="high_cat")
    slid_2<- callModule(module = slider,id="sub_cat")
    slid_buttom<- callModule(module = sliderbuttom,id="posit")
    
    
    plot1<-callModule(module = colplot,id="colplot1",
               data= infra_react, 
               var_data="high_cat", 
               breaks= high_cat, 
               slid, 
               slid_2,
               slid_buttom)
    
    plot2<-callModule(module = colplot,id="colplot2",
                      data= infra_react, 
                      var_data="sub_cat", 
                      breaks= sub_cat, 
                      slid, 
                      slid_2,
                      slid_buttom)

    

# TAB 2: States -----------------------------------------------------------


    output$us_map <- renderPlot({
  
      geo_data<-map_react()
      
      if(nrow(geo_data)==0){}else{
        
        ggplot(data = geo_data) +
          geom_polygon(aes(x = long, y = lat, fill = total, group = group), 
                       color = "grey") + 
          guides(fill=guide_legend(title=input$unit_geo))+
          theme_classic()+
          theme(axis.title=element_blank(),
                axis.text=element_blank(),
                axis.ticks=element_blank(),
                axis.line = element_blank())+ 
          scale_fill_gradientn(colours = c("red3",rgb(.886, .812, .016),"springgreen4"))+
          coord_fixed(1.3)
      }
      
    })  
  
    output$table_map <- renderDataTable({
      
      tab_states()
      
    },options = list(pageLength = 10))
    
    output$download_df_state<-downloadHandler(
      
      filename = "States_detail.csv",
      content = function(filename){
        write.csv(tab_states(), filename,fileEncoding = "UTF-8",row.names = F,na = "" )
        }
      )
    

# Cleaning Data  ----------------------------------------------------------


    observeEvent(input$procesar, {
      showModal(modalDialog("Cleaning Data", footer = NULL))
      source('Cleaning/Cleaning_data.R',encoding = "UTF-8")
      removeModal()
    }
    )

})

