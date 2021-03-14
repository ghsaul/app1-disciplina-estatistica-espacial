library(shinydashboard)
library(tidyverse)
library(lubridate)
library(RSocrata)
library(rgdal)
library(sf)
library(mgcv)
library(leaflet)
library(leaflet.extras)

# setwd("F:/Gabriel/Documentos/ufrgs_estatistica/8_semestre/Estatística Espacial/tarefas/lista2")

#==================== ui ====================
ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(
    title = "Lista 2 - Estatistica Espacial"
  ),
  dashboardSidebar(   
    sidebarMenu(
      menuItem("Mapa", tabName = "dashboard", icon = icon("map-pin")),
      radioButtons("acidentes", "Acidentes?",choices = list("Feridos" = 1, "Nao feridos" = 2, "Ambos" = 3) ,selected = 3)),
      sliderInput("meses","Pelo menos quantos meses faz que o acidente ocorreu?",min = 1,max = 36,value = 1),  
      sliderInput("hora","Horario",min = 1,max = 24,value = c(1,24)),
      checkboxGroupInput(
        "report",
        "Tipo de laudo:",
        c("Na cena" = "ON SCENE",
          "Nao na cena" = "NOT ON SCENE (DESK REPORT)",
          "Desconhecido" = "UNKNOWN"),
        selected = c("ON SCENE","NOT ON SCENE (DESK REPORT)","UNKNOWN")
      ),
      sliderInput("num_envolv","Numero de veiculos envolvidos no acidente",min = 1,max = 18,value = c(1,18)),
      sliderInput("lim_vel","Limite de velocidade publicado",min = 0,max = 70,value = c(0,70)),
      checkboxGroupInput(
        "iluminacao",
        "Condicoes de iluminacao:",
        c("Luz do dia" = "DAYLIGHT",
          "Amanhecer" = "DAWN",
          "Escuro" = "DARKNESS",
          "Escuro com iluminacao na pista" = "DARKNESS, LIGHTED ROAD",
          "Anoitecer" = "DUSK",
          "Desconhecido" = "UNKNOWN"),
        selected = c("DAYLIGHT","DAWN","DARKNESS","DARKNESS, LIGHTED ROAD","DUSK","UNKNOWN")
      )
  ),
  #Corpo
   dashboardBody(
       # Boxes need to be put in a row (or column)
       fluidRow(
         box(plotOutput("static_inj", height = 512)),
         box(plotOutput("static_none", height = 512))
       ),
       fluidRow(
         box(tags$h1("Heatmap dos feridos mais nao feridos"),leafletOutput("leaf_plot", height = 712))
       )
   )
)
                     


#==================== server ====================
server <- function(input, output) {
  
  read <- reactive({
    crash = read.csv("crash.csv")
    
    return(crash)
  })
  
  meses <- reactive({
    crash = read()
    meses <- input$meses
    tempo <- today() - months(meses)
    #data = crash[crash$crash_date>tempo,]
    data = crash[as.Date(crash$crash_date)>tempo,] # aconteceu de x meses ate hoje
    return(data)
  })
  
  resto <- reactive({
    data <- meses()
    
    
    # iluminacao
    nomes <- input$iluminacao
    data = data[which(data$lighting_condition %in% nomes),]
    
    # report
    nomes <- input$report
    data = data[which(data$report_type %in% nomes),]
    
    # se houve feridos
    acidentes <- input$acidentes
    
    if(acidentes==1){
      data = data[data$injuries=="injuries",]
    }
    
    if(acidentes == 2){
      data = data[data$injuries=="none",]
    }
    
    ## gambiarra
    data = data[data$latitude>0, ]
    data = data[data$longitude<0,]
    data$crash_hour[data$crash_hour==0] = 24
    data = data[data$crash_hour>=input$hora[1],]
    data = data[data$crash_hour<=input$hora[2],]
    data = data[data$crash_hour>=input$num_envolv[1],]
    data = data[data$crash_hour<=input$num_envolv[2],]
    data = data[data$crash_hour>=input$lim_vel[1],]
    data = data[data$crash_hour<=input$lim_vel[2],]
    
    return(data)
  })
  
  
  output$static_inj <- renderPlot({
    data = resto() %>%
      filter(injuries == "injuries")
    
    map = readOGR("shapes/ward1998.shp", verbose = F)
    map = st_as_sf(map)
    
    
    p = ggplot(map)+
      geom_sf()+
      geom_point(data = data,mapping = aes(longitude, latitude), color = "red", size = .5, alpha = 0.8)+
      labs(title = "Acidentes de trânsito na cidade de Chicago com feridos", color = NULL) +
      theme_bw()
    p
    
  })
  
  output$static_none <- renderPlot({
    data = resto() %>%
      filter(injuries == "none")
    
    map = readOGR("shapes/ward1998.shp", verbose = F)
    map = st_as_sf(map)
    
    
    p = ggplot(map)+
      geom_sf()+
      geom_point(data = data,mapping = aes(longitude, latitude), color = "blue", size = .5, alpha = 0.8)+
      labs(title = "Acidentes de trânsito na cidade de Chicago sem feridos", color = NULL) +
      theme_bw()
    p
    
  })
  
  output$leaf_plot <- renderLeaflet({
    dados = resto()
    
    dados = dados %>%
      filter(latitude != 0) %>%
      filter(longitude != 0)
    
    leaflet(dados) %>%
      addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google')  %>%
      addMarkers(lng = ~longitude,
                 lat = ~latitude,
                 clusterOptions = markerClusterOptions) %>% 
      addHeatmap(
        lng = ~longitude, lat = ~latitude,
        blur = 17, radius = 8
      )
    
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)