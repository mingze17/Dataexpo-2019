library(shinydashboard)
library(leaflet)
library(tigris)
library(dplyr)
library(sp)
library(maptools)
library(broom)
library(httr)
library(rgdal)
library(png)
library(grid)
library(shinyWidgets)
library(readr)
library(geojsonio)
library(ggplot2)

## Only run this example in interactive R sessions
if (interactive()) 
  library(shiny)
####Initialization 1
states <- geojsonio::geojson_read("Community Districts.geojson",what = "sp")
ans <- read.csv("state_data - state_data.csv")
states$id = 1:71
states$Name = as.character(ans$Name)
states$Random = 100*runif(71)

my_new_choices <- split(1:71, states$Name)
bins <- c(0,0.2,0.4,0.6,0.8,Inf)
pal <- colorBin("YlOrRd", domain= states$Random, bins=bins)
labels <- sprintf("<strong>%s</strong><br/>%g " , states$Name , states$Random) %>% lapply(htmltools::HTML)


##Initialization 2
df <- read_csv("data720.csv")   #change to new_data

### You can make it interactive add conditions in the following []
df_im1=df[df$is_im1==1,]
df_im2=df[df$is_im2==1,]
df_im0 <- df[(df$is_im1+df$is_im2)==0,]

t11=t.test(df_im1$Rent,df_im2$Rent,alternative = 'less')$p.value
t12=t.test(df_im2$Rent,df_im0$Rent,alternative = 'less')$p.value
t13=t.test(c(df_im2$Rent,df_im1$Rent),df_im0$Rent,alternative = 'less')$p.value

t31=t.test(df_im1$index,df_im2$index,alternative = 'less')$p.value
t32=t.test(df_im2$index,df_im0$index,alternative = 'less')$p.value
t33=t.test(c(df_im2$index,df_im1$index),df_im0$index,alternative = 'less')$p.value

t21=t.test(df_im1$Household_Income,df_im2$Household_Income,alternative = 'less')$p.value
t22=t.test(df_im2$Household_Income,df_im0$Household_Income,alternative = 'less')$p.value
t23=t.test(c(df_im2$Household_Income,df_im1$Household_Income),df_im0$Household_Income,alternative = 'less')$p.value

t=matrix(c(t11,t12,t13,t21,t22,t23,t31,t32,t33),3,3)
colnames(t) <- c('Rent','Household Income','House Index')
t=as.data.frame(t)
t$Group <- c('1st Generation vs 2nd Generation','2nd Generation vs Native','Immigrants vs Native')
t=t[,c(4,1,2,3)]



########Initialization 3
mapId <- read.csv("Mapping.csv") 
load("rent12020.RData") # Give you predrent vector
load("re.RData")
Variable_Nm <- c("Sex","Age","Married","is_im2","is_im1","Number_of_Persons","Household_Income","Length_of_Lease","index","rent1")
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

############################################
############################################
titles <- tags$a(href = 'https://www.google.com',tags$img(src = "GW.jpg",height='50',width = '50'),span("An Analysis of Immigrants and House Condition in New York 
City ",span("________",style="color:orange",span(" By Xiang Shen, Mingze Zhang, Shunyan Luo", style = "color: black; font-size: 15px"))))
ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(title = titles, titleWidth = 1500),
  dashboardSidebar(
    sidebarMenu(id = "sidebarmenu",
                menuItem("Index Maps", tabName = "a", icon = icon("globe")),
                menuItem("Model Description", tabName = "b", icon = icon("cubes", lib = "font-awesome")),
                menuItem("Find your home", tabName = "c", icon = icon("search", lib = "font-awesome")),
                conditionalPanel("input.sidebarmenu == 'a'",
                                 sliderTextInput("a_year", h3("Calendar Year"),choices=c(1991,1993,1996,1999,2002,2005,2008,2011,2014,2017), grid=TRUE,selected  = 2005),
                                 selectInput("a_bourough", h3("sub-bourough"),choices =my_new_choices, selected = 71),
                                 prettyRadioButtons(
                                   inputId = "a_status",
                                   label = "Status", 
                                   choices = c("First Gen" = '1', "Second-Gen" = '2', "Native" = '0'),
                                   icon = icon("check"), 
                                   bigger = TRUE,
                                   status = "info",
                                   animation = "jelly"
                                 )
                                 
                )
    )
  ),
  dashboardBody(
    
    #The custom font for title
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
    #Tabs Content
    tabItems(
      #First Page
      tabItem(
        tabName = "a",
        fluidRow(
          column(8,
          box(width = 12,
              title = "NYC Map",status = "primary", solidHeader = TRUE,
              collapsible = FALSE,
              leafletOutput("Index_map",width="100%",height = 700)
              )
          ),
          column(4,
                 column(12,infoBoxOutput(width=6,"a_Index")),
                 
                 hr(),
                 column(12,infoBoxOutput(width=6,"a_AverageRent"),
                        infoBoxOutput(width=6,"a_Population")),
                 
            box(width = 12,
              title = "Highlights", solidHeader = TRUE, status = "warning",
              includeMarkdown("Highlights.md")
            ),
            box(width = 12,
                plotOutput("piechart")
            )
          )
        )
      ),
      #############Tab b
      tabItem(tabName = "b",
              
       fluidRow(align = "center",box(width=12,h1("House Index Construction"))),
        
       fluidRow(align = "center",
         column(align = "left",width = 6,
            box(width = 12,
                title = "Composite Index Construction", solidHeader = TRUE, status = "primary",
                includeMarkdown("Construction.Rmd")),
                box(width =12, plotOutput("VariableDetail"))
                ),
         column(width = 6,
               box(width = 12,plotOutput("correlation")),
               box(width = 12,
                 tableOutput( "mytable")
               )
          )
       ),
       fluidRow(align = "left",box(width = 12,
           title = "Conclusion", solidHeader = TRUE, status = "primary",
           includeMarkdown("Conclusion.md"))
       )
      )
      ,
      tabItem(tabName = "c",
              fluidRow(align = "center",box(width=12,h1("Rent Prediction"))),
              fluidRow(
                column(8,
                       column(6,
                              box(
                                height = 700,
                                title = "Tell us More about You", width = NULL, solidHeader = TRUE, status = "primary",
                                radioGroupButtons(
                                  inputId = "c_Gender",
                                  label = "What's your gender",
                                  choices = c("Male" = 1, 
                                              "Female" = 0),
                                  selected = 1,
                                  checkIcon = list(
                                    yes = tags$i(class = "fa fa-check-square", 
                                                 style = "color: steelblue"),
                                    no = tags$i(class = "fa fa-square-o", 
                                                style = "color: steelblue"))
                                ),
                                awesomeRadio(
                                  inputId = "c_marry",
                                  label = "Your Marriage Status:", 
                                  choices = c("Single" = 0, "Married" = 1),
                                  selected = 0,
                                  inline = TRUE, 
                                  status = "success"
                                ),
                                sliderTextInput(
                                  inputId = "c_income",
                                  label = "Please enter your approximate yearly income:($)", 
                                  choices = seq(from = 0,
                                                to = 300000,
                                                by = 5000),
                                  grid = TRUE,
                                  selected  = 100000
                                ),
                                sliderTextInput(
                                  inputId = "c_people",
                                  label = "What is the size of your household:", 
                                  choices = seq(from = 1,
                                                to = 20,
                                                by = 1),
                                  grid = TRUE
                                ),
                                awesomeRadio(
                                  inputId = "c_status",
                                  label = "Your Immigrant Status:", 
                                  choices = c("Native" = 0, "1st-gen Immigrant" = 1, "2nd-gen Immigrant" =2),
                                  selected = 0,
                                  inline = TRUE, 
                                  checkbox = TRUE
                                )
                        
                              )),
                       
                       column(6, 
                              box(
                                height = 700,
                                title = "Tell us What You Like", width = NULL, solidHeader = TRUE, status = "primary",
                                
                                knobInput(
                                  inputId = "c_Index",
                                  label = "Please choose your preferred Index:",
                                  value = 2,
                                  min = 1,
                                  max = 5,
                                  displayPrevious = TRUE, 
                                  lineCap = "round",
                                  fgColor = "#428BCA",
                                  inputColor = "#428BCA"
                                ),
                                pickerInput(
                                  inputId = "c_bourough",
                                  label = "Please enter the area you preferred:", 
                                  choices = my_new_choices,selected = 1),
                                sliderTextInput(
                                  inputId = "c_lease",
                                  label = "Your Preferred Lease Term", 
                                  grid = TRUE,
                                  force_edges = TRUE,
                                  choices = c("0 Month" , 
                                              "6 Months" , "1 Year", "18 Months", "2 Years" , "30 Months"),
                                  selected = "1 Year"
                                ),
                                valueBoxOutput("vbox"),
                                actionBttn(inputId = "ActionFinal",label = "Calculate", style = "gradient",color = "danger", icon = icon("usd"))
                                ,
                                hr(),
                                verbatimTextOutput("predictedRent", placeholder = TRUE)
                                
                              )
                            )),        
                column(4, 
                       box(height = 500,title = "Spatial Temporal Regression for Selected Sub-Bourough", width = NULL, solidHeader = TRUE, status = "primary",
                           plotOutput("PredRent1") 
                           )
                )
              )
      )
    )
  )
)
    

server <- function(input, output) {
  #First Page
  output$Index_map <- renderLeaflet({
    leaflet(states) %>% 
      setView(lng = -74, lat = 40.71, zoom = 11) %>% 
      addTiles() %>%  
      addPolygons(data = states,fillColor = ~pal(Random), group = "chosen_year", weight=2 , opacity= 1, color = "white", dashArray = "3", fillOpacity = "0.2",highlight = highlightOptions(weight =5, color = "#666",dashArray = "",fillOpacity = 0.7, bringToFront = TRUE),label=labels,labelOptions = labelOptions(style = list("font-weight"= "normal", padding="3px 8px"), textsize = "15px",direction = "auto")) %>% 
      addLegend(layerId ="Legend",pal = pal, values = states$Random, opacity = 0.7, title = NULL,position = "bottomright") %>%
      addPolygons(data = subset(states,states$Name == "Mid-Island0"),group = "chosen_shape",fillColor = ~pal(Random), weight=3 , opacity= 1, color = "white", dashArray = "3", fillOpacity = "0.2",label=labels,labelOptions = labelOptions(style = list("font-weight"= "normal", padding="3px 8px"), textsize = "15px",direction = "auto")) %>% 
      addProviderTiles("CartoDB.Positron")})
  
  
  observe({
    leafletProxy("Index_map") %>% 
      clearGroup("chosen_shape")%>%
      addPolygons(data = subset(states,states$id == input$a_bourough),group = "chosen_shape",fillColor = ~pal(Random), weight=3 , opacity= 1, color = "white", dashArray = "3", fillOpacity = "0.2",label=labels,labelOptions = labelOptions(style = list("font-weight"= "normal", padding="3px 8px"), textsize = "15px",direction = "auto"))
    
  })
  
  observe({
    dap = read.csv(paste0(as.character(input$a_year),"_Index.csv"))
    for(j in 1:71){
      tempa<- subset(dap,dap$Borough== mapId$Borough[mapId$id == j] )
      tempa<- subset(tempa,tempa$Sub_Borough== mapId$Sub.Borough[mapId$id == j])
      states$Random[j] = tempa$averaged
    }
    pal <- colorBin("YlOrRd", domain= states$Random, bins=5)
    labels <- sprintf("<strong>%s</strong><br/>%g " , states$Name , states$Random) %>% lapply(htmltools::HTML)
    
    leafletProxy("Index_map") %>% 
      
      clearGroup("chosen_year")%>%
      removeControl("Legend") %>%
      addPolygons(data = states,fillColor = ~pal(Random), group = "chosen_year", weight=2 , opacity= 1, color = "white", dashArray = "3", fillOpacity = "0.5",highlight = highlightOptions(weight =5, color = "#666",dashArray = "",fillOpacity = 0.7, bringToFront = TRUE),label=labels,labelOptions = labelOptions(style = list("font-weight"= "normal", padding="3px 8px"), textsize = "15px",direction = "auto")) %>% 
      addLegend(layerId ="Legend",pal = pal, values = states$Random, opacity = 0.7, title = NULL,position = "bottomright") 
      
    
  })
  
  
  output$a_Index <- renderInfoBox({
    filename <- paste0(as.character(input$a_year),"_Index.csv")
    b <- read.csv(filename)
    b<- subset(b,b$Borough== mapId$Borough[mapId$id == input$a_bourough] )
    b<- subset(b,b$Sub_Borough== mapId$Sub.Borough[mapId$id == input$a_bourough])
    Index1 = round(b$averaged*10,2)
    infoBox("Index", Index1 , icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow", fill = TRUE
    )
  })
  
  
  
  output$a_AverageRent <- renderInfoBox({
    filename <- paste0(as.character(input$a_year),input$a_status,"_Index.csv")
    b <- read.csv(filename)
    b<- subset(b,b$Borough== mapId$Borough[mapId$id == input$a_bourough] )
    b<- subset(b,b$Sub_Borough== mapId$Sub.Borough[mapId$id == input$a_bourough])
    AveRent = round(b$averageRent,2)
    infoBox( "Avg. Rent", AveRent , icon = icon("list", lib = "glyphicon"),
      color = "blue", fill = TRUE
    )
  })
  
  output$a_Population <- renderInfoBox({
    filename <- paste0(as.character(input$a_year),input$a_status,"_Index.csv")
    b <- read.csv(filename)
    b<- subset(b,b$Borough== mapId$Borough[mapId$id == input$a_bourough] )
    b<- subset(b,b$Sub_Borough== mapId$Sub.Borough[mapId$id == input$a_bourough])
    popl= round(b$population,2)
    infoBox( "Count", popl, icon = icon("list", lib = "glyphicon"),
      color = "blue", fill = TRUE
    )
  })
  
  CountD <- reactive({
    filename0 <- paste0(as.character(input$a_year),'0',"_Index.csv")
    b0 <- read.csv(filename0)
    b0<- subset(b0,b0$Borough== mapId$Borough[mapId$id == input$a_bourough] )
    b0<- subset(b0,b0$Sub_Borough== mapId$Sub.Borough[mapId$id == input$a_bourough])
    pop0= b0$population
    filename1 <- paste0(as.character(input$a_year),'1',"_Index.csv")
    b1 <- read.csv(filename1)
    b1<- subset(b1,b1$Borough== mapId$Borough[mapId$id == input$a_bourough] )
    b1<- subset(b1,b1$Sub_Borough== mapId$Sub.Borough[mapId$id == input$a_bourough])
    pop1= b1$population
    filename2 <- paste0(as.character(input$a_year),'2',"_Index.csv")
    b2 <- read.csv(filename2)
    b2<- subset(b2,b2$Borough== mapId$Borough[mapId$id == input$a_bourough] )
    b2<- subset(b2,b2$Sub_Borough== mapId$Sub.Borough[mapId$id == input$a_bourough])
    pop2= b2$population
    count.data <- data.frame(
      class = c('Native','1st Gen','2nd Gen'),
      prop = c(pop0,pop1,pop2)
    )
    print(count.data)
    # Add label position
    count.data <- count.data %>% arrange(desc(class)) %>% mutate(lab.ypos = cumsum(prop) - 0.5*prop)
    print(count.data)
    count.data
  })

  output$piechart = renderPlot({
    
   
    mycols <- c("#0073C2FF", "#868686FF", "#CD534CFF")
    
    ggplot(CountD(), aes(x = 2, y = prop, fill = class)) +
      geom_bar(stat = "identity", color = "white") +
      coord_polar(theta = "y", start = 0)+ 
      scale_fill_brewer(palette = "Pastel1","Status") +
      geom_text(aes(y = lab.ypos,label = prop),size=5,color = "black") +
      theme_void() +
      theme(legend.position="top")
     
  })
  
  #Second Page
  output$correlation <- renderPlot(
    {img <- readPNG("www/correlation.png")
    grid::grid.raster(img)}
  )
  output$VariableDetail <- renderPlot(
    {img <- readPNG("www/table.png")
    grid::grid.raster(img)}
  )
  output$mytable <- renderTable(t,digits = 4)
  
  #Third Page
  observeEvent(input$ActionFinal, {
    im1 <- 0;im2 <- 0;
    if (input$c_status == 1) {im1 <- 1} 
    else if (input$c_status == 2) {im2 <- 1}
    if (input$c_lease == "0 Month") {lease_term = 0}
    else if (input$c_lease == "6 Months") {lease_term = 0.5}
    else if (input$c_lease == "1 Year") {lease_term = 1}
    else if (input$c_lease == "18 Months") {lease_term = 1.5}
    else if (input$c_lease == "2 Years") {lease_term = 2}
    else {lease_term = 2.5}
    rent12 <- predrent[11,mapId$rent12[mapId$id== input$c_bourough]   ]
    newdata <- data.frame(as.integer(input$c_Gender),30,as.integer(input$c_marry),im1,im2,as.integer(input$c_people),as.integer(input$c_income),lease_term,as.double(input$c_Index/10.0),rent12)
    names(newdata) = Variable_Nm
    outcome <- predict(re,newdata = newdata, interval = 'confidence', level=0.99)
    output$predictedRent <-  renderText({ paste0("Your rent ranges from $",as.character(round(outcome[2],2))," to $",as.character(round(outcome[3],2)) )})#
  })
  
  
  output$PredRent1 <- renderPlot(
    {img <- readPNG("www/rent.png")
    grid::grid.raster(img)}
  )

}

shinyApp(ui, server)