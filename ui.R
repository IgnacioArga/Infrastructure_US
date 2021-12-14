dashboardPage(

# Title -------------------------------------------------------------------
  dashboardHeader(title = "Infrastructure US"),



# Slide Bar ---------------------------------------------------------------

  
  dashboardSidebar(
    sidebarMenu(menuItem("Dashboard",id = "princ",icon = icon("tachometer-alt"),
                         menuSubItem("Global",icon = icon("city"),tabName = "global"),
                         menuSubItem("Category",icon = icon("building"),tabName = "category")),
                menuItem("States",tabName = "states",icon = icon("globe-americas"))
                ),
    
    sliderInput("year", label = "Years:",
                min = 1947, max = 2017, 
                value = c(1995,2010)),
    
    radioButtons("unit", label = "Units:", choices = unit,
                 selected = "Gross investment at 2012",inline = F),hr(),
    
    actionButton("procesar", "Refresh Data Source")
    
    ),
    
    

# Dashboard Body ----------------------------------------------------------

  
  
  dashboardBody(
        
        tabItems(
        # Tab1: Dashboard-------------------------------------------------
          # Sub_tab1: Global
         tabItem(tabName = "global",

                 h1("Global per Year"),
                 plotOutput("year_plot"),hr(),
                 
                 h1("Global per Category"),
                 plotOutput("cat_plot")
                 
                 ),
          # Sub_tab2: Category
        tabItem(tabName = "category",
                sliderUI("high_cat",high_cat),
                sliderUI("sub_cat",sub_cat),
                sliderbuttomUI("posit",c("fill (%)","stack (USD)"),"Position"),
                
                h1("Category & Year"),
                
                colplotUI("colplot1"),
                colplotUI("colplot2")
                
        ),
        
        # Tab2: States ---------------------------------------------------
        tabItem(tabName = "states",
                
                radioButtons("unit_geo", label = "Units map:", choices = unit_geo,
                             selected = "Investment at 2012 per capita",inline = T),
                h1("Transportation - Highways and streets - S&L"),
                
                plotOutput("us_map"),
                dataTableOutput("table_map"),
                downloadButton("download_df_state", "Download CSV")
                
                )
        )
        )
  )


