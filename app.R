library(shiny)
library(leaflet)
library(leaflegend)
library(sf)
library(shinydashboard)
library(shinydashboardPlus)

local=DBI::dbConnect(RSQLite::SQLite(), "outputs/grafos/networks.sqlite")

paleta_nodos=colorNumeric(palette ='turbo',domain = seq(100,3000,0.01) )
paleta_aristas=colorNumeric(palette ='turbo',domain = seq(0.01,14.91,0.01) )
#demograficos_scince proviene de definicion_cartografia_demografia

lista_csvs=list.files('outputs/municipal_simple/',pattern = ".csv",full.names = T)
ui <-dashboardPage(
  options = list(sidebarExpandOnHover = TRUE),
  header = dashboardHeader(title = "Patrones de movilidad por hora",disable = F),
  sidebar = dashboardSidebar(
    selectInput("mes",
                label = "Mes", 
                choices = c("junio" = "06",
                            "octubre" = "10",
                            "diciembre" = "12",
                            "Todos"=NA),
                selectize = TRUE,selected ="junio" ),
    selectInput("trend",
                label = "Rutina", 
                choices = c("weekday","weekend"),
                selectize = TRUE,selected ="weekday" ),
    sliderInput("hora",label = "Hora del día",min = 0,max = 23,value = 0,animate = animationOptions(loop = T,1500),step = 1),
    collapsed = F,minified = F),
  body = dashboardBody(
    box(id='mapa_principal_container',width = 12, class = "map-box",
        leafletOutput("mapa_principal", width = "100%", height = "75vh")
    )
  ),
  title = "DashboardPage"
)

shinyApp(ui, function(input, output,session) {
  output$mapa_principal=renderLeaflet({
    #Mapa con tiles por defecto y barra de herramientas para dibujar polígonos
    leaflet() |>   addProviderTiles("CartoDB.DarkMatter")#addTiles()
  })
  #Agregamos el select (nivel de atencion) con debounce
  input_mes=reactive({
    input$mes
  })
  input_mes_d=input_mes |> debounce(1000)
  ##Pendiente. Lista de 24 .csv por cada eleccion(mes,rutina)
  observeEvent(input$hora,
               {
                 archivo=lista_csvs[grepl(paste0("OD_",input$mes,"_",input$trend,sprintf("%02d",input$hora)),lista_csvs)]
                 grafo=escalar_proxy_uso(archivo |> read.csv())
                 grafo_individual=grafo[['aristas']]
                 nodos_individual=grafo[['nodos']]
                 nodos_sub_grafo=nodos_municipios |> 
                   merge(nodos_individual,by.x='CVEGEO',by.y='CVEGEO_origin')
                 sub_grafo=grafo_individual|> 
                   dplyr::mutate(CVEGEO_origin=as.character(CVEGEO_origin),
                                 CVEGEO_destination=as.character(CVEGEO_destination)) |> 
                   dplyr::left_join(edges_municipios_sf,by = dplyr::join_by(CVEGEO_origin==CVEGEO_origin,
                                                                            CVEGEO_destination==CVEGEO_destination))
  
                    leafletProxy("mapa_principal") |> 
                    clearShapes() |> 
                      addCircles(data=nodos_sub_grafo,radius =~(proxy_uso),group = as.character(i),fillColor = ~paleta_nodos((proxy_uso)),
                                 color = ~paleta_nodos((proxy_uso)) ) |> 
                    addPolylines(data=sub_grafo |> st_as_sf(),weight = ~(proxy_uso),group = as.character(i),
                                  ,fillColor = ~paleta_aristas((proxy_uso)),
                                  color = ~paleta_aristas((proxy_uso)) ) 
               })
})

#shiny::runApp("app.R",host = "0.0.0.0", port = 80)