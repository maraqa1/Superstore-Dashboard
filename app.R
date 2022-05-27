library(shiny)
library(shinydashboard)
library(fontawesome)
library(readxl)
source("R/map.R")
source("R/totals.R")
#source("R/graphs.R")






ui <- 
  shinydashboard::dashboardPage( 
    

    dashboardHeader(title = "Superstore Dashboards",
                    #long title
                    titleWidth = 450),

    #side bar - setup the filters
    dashboardSidebar(
      #setup regional filters
      checkboxGroupInput(inputId ="regional_select","Choose Regions", 
                         choices=regions,
                         selected=regions)
    ),
    
  dashboardBody(
    # Row for value boxes
    fluidRow(
      valueBoxOutput("value1",width=3),
      valueBoxOutput("value2",width=3),
      valueBoxOutput("value3",width=3),
      valueBoxOutput("value4",width=3),
      ),
    fluidRow(
    column(12,

            box(title="US Regions Map",leafletOutput("bid_no_bid_map", width="100%",height="400px"),width = NULL ,solidHeader = TRUE,status = "primary")
#           box(title="UK Regions Map",leafletOutput("bid_no_bid_map", width="100%",height="400px"),width = NULL ,solidHeader = TRUE,status = "primary")
           
    )
#    column(4,
#           box(title="Balance",plotOutput("Plot.balance",width="100%",height="200px"),width = NULL ,solidHeader = TRUE,status = "primary"),
#           box(title="Age",plotOutput("Plot.age",width="100%",height="200px"),width = NULL ,solidHeader = TRUE,status = "primary")
#           #box(title="UK regions",plotOutput("bid_no_bid_map"),width = NULL ,solidHeader = TRUE,status = "primary"),
#           
#    ),
#    column(4,
#           box(title="Gender",plotOutput("Plot.gender",width="100%",height="200px"),width = NULL ,solidHeader = TRUE,status = "primary"),
#           box(title="Job",plotOutput("Plot.job",width="100%",height="200px"),width = NULL ,solidHeader = TRUE,status = "primary")

           #box(title="UK regions",plotOutput("bid_no_bid_map"),width = NULL ,solidHeader = TRUE,status = "primary"),
#    )
  ), #fluidRow

    fluidRow(
      column(6,
             
             box(title = "Top 20 Customers",DT::DTOutput("table1",width="100%",height="400px"),width = NULL ,solidHeader = TRUE,status = "primary")
             ),
      column(6,
             box(title = "Top 20 Sold Quantity", DT::DTOutput("table2",width="100%",height="400px"),width = NULL ,solidHeader = TRUE,status = "primary")
      
    )
    )

  ) #fluidBody
)

server <- function(input, output,session) {


  # Part A. Filters  
  # A.1 Get regional Filters
  #regional value filter "input$regional_select"
  #output$regional_value <- renderText({ 
  #  print(input$regional_select)
  #  paste("You have selected", output$number) 
  # })
  
########################################################################  
  
  #Part B. The data
  
  # B.1.0 Set the path
  data.file="data/Sample - Superstore.xls"
  
  # B.2.0 Read the data & apply create fields
  df.read <<-readxl::read_excel(data.file, sheet = 1)
  
  # B.3.0 Apply sidebar filters 
  df.filter <- reactive({ 
    return(
      df.read %>%
        dplyr::filter(Region %in% input$regional_select ) 
    )
  })
  
###########################################################################
  # Part C. Set the value boxes
  # value box output 
  #https://fontawesome.com

  
  output$value1 <- renderValueBox({infoBox("Total Sales",formatC(find_totals(dplyr::quo(Sales), df.filter()),format="d", big.mark=','),icon=icon("dollar-sign"),color = "purple")})
  output$value2 <- renderInfoBox({infoBox("Total Profit",formatC(find_totals(dplyr::quo(Profit), df.filter()),format="d", big.mark=','),icon=icon("money-bill"),color = "green")})
  output$value3 <- renderInfoBox({infoBox("Total Quantity",formatC(find_totals(dplyr::quo(Quantity), df.filter()),format="d", big.mark=','),icon=icon("calculator"),color = "blue")})
  output$value4 <- renderInfoBox({infoBox("Total Discount",formatC(find_totals(dplyr::quo(Discount), df.filter()),format="d", big.mark=','),icon=icon("percent"),color = "orange")})

####################################################################################
  #part D. Find value box values
  #part D. Set the map
  #column 1 output - Map data
  output$bid_no_bid_map <- renderLeaflet({plot_uk_map(regional.filter=input$regional_select ) })
###################################################################################
  #part E. Top 20 customers and sold products
  
  output$table1<-DT::renderDT({
  
    DT::datatable( top_n_var(dplyr::quo(`Customer Name`),df.filter()) ) %>% 
      
      DT::formatStyle('Customer Name',backgroundColor = DT::styleInterval(3.4, c('gray', 'yellow')))
  })
  
  output$table2<-DT::renderDT({
    
    top_n_var(dplyr::quo(`Quantity`),df.filter()) #%>% 
    
    #DT::formatStyle('Customer Name',
    #backgroundColor = DT::styleInterval(3.4, c('gray', 'yellow'))
    # )
  })
  

}

shinyApp(ui = ui, server = server)

