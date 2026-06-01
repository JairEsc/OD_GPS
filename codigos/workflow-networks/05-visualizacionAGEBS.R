##Consulta al grafo completo
con <- DBI::dbConnect(RSQLite::SQLite(), "outputs/grafos/networks.sqlite")
edges_agebs_sf=dplyr::tbl(con,'aristas_agebs')
nodos_agebs=dplyr::tbl(con,'nodos_agebs')

#CVEGEO_origin/ destination
grafos_agebs_weekday=list.files("outputs/agebs/",pattern = "12_weekday",full.names = T) |> 
  sort() |> tail(24) |> 
  lapply(read.csv)

grafos_agebs_weekday |> 
  lapply(\(x){
    x |> 
      dplyr::group_by(CVEGEO_origin)|>
      dplyr::summarise(proxy_uso=sum(proxy_uso,na.rm=T)) |>
      dplyr::arrange(CVEGEO_origin)|> dplyr::pull(proxy_uso) |>log() |>  max()
  }) |> unlist() |> max()->max_log_proxy_uso_auto_ciclos
grafos_agebs_weekday |> 
  lapply(\(x){
    x |> dplyr::pull(proxy_uso) |>log() |>  max()
  }) |> unlist() |> max()->max_log_proxy_uso
grafos_agebs_weekday |> 
  lapply(\(x){
    x |> dplyr::pull(proxy_uso) |>log() |>  quantile(0.8)
  }) |> unlist() |> unname() ->umbrales_log_minimos
aristas_agebs_weekday=1:(grafos_agebs_weekday |> length()) |> 
  lapply(\(i){
    grafos_agebs_weekday[[i]] |> 
      dplyr::filter(log(proxy_uso)>=umbrales_log_minimos[i]) |> 
      dplyr::ungroup() |>
      dplyr::group_by(CVEGEO_origin) |> 
      dplyr::arrange(dplyr::desc(proxy_uso)) |> 
      dplyr::slice_head(n=5) |> 
      dplyr::ungroup() |> 
      dplyr::mutate(proxy_uso=log(proxy_uso)) |> 
      dplyr::mutate(proxy_uso=.01+14.9*(proxy_uso-min(umbrales_log_minimos)  )/(max_log_proxy_uso - min(umbrales_log_minimos))) 
  })
nodos_agebs_weekday=1:(grafos_agebs_weekday |> length()) |> 
  lapply(\(i){
    grafos_agebs_weekday[[i]] |> 
      dplyr::filter(log(proxy_uso)>=umbrales_log_minimos[i]) |> 
      dplyr::group_by(CVEGEO_origin)|>
      dplyr::summarise(proxy_uso=sum(proxy_uso,na.rm=T)) |>
      dplyr::mutate(proxy_uso=log(proxy_uso)) |> 
      dplyr::mutate(proxy_uso=10+290*(proxy_uso-min(umbrales_log_minimos)  )/(max_log_proxy_uso_auto_ciclos - min(umbrales_log_minimos)))
  })
leafletProxy=leaflet() |> 
  addProviderTiles("CartoDB.DarkMatter")#addTiles()

paleta_nodos=colorNumeric(palette ='turbo',domain = seq(10,300,0.1) )
paleta_aristas=colorNumeric(palette ='turbo',domain = seq(0.01,14.91,0.01) )
for(i in 1:24){
  #i=1
  grafo_individual=aristas_agebs_weekday[[i]]
  nodos_individual=nodos_agebs_weekday[[i]]
  #grafo_individual=dplyr::tbl(con,"temp")
  sub_grafo=grafo_individual|> 
    dplyr::filter(CVEGEO_origin!=CVEGEO_destination) |> 
    dplyr::mutate(CVEGEO_origin=as.character(CVEGEO_origin),
                  CVEGEO_destination=as.character(CVEGEO_destination)) |> 
    dplyr::left_join(edges_agebs_sf,by = dplyr::join_by(CVEGEO_origin==CVEGEO_origin,
                                                             CVEGEO_destination==CVEGEO_destination),copy = T) |> 
    dplyr::collect() |> 
    dplyr::mutate(geometry= sf::st_as_sfc(structure(geometry,class = "WKB" ),EWKB=T)) |> st_as_sf()
  
  nodos_sub_grafo= nodos_individual|> 
    dplyr::left_join(nodos_agebs,by = dplyr::join_by(CVEGEO_origin==CVEGEO),copy = T) |> 
    dplyr::collect() |> 
    dplyr::mutate(geometry= sf::st_as_sfc(structure(geometry,class = "WKB" ),EWKB=T)) |> st_as_sf()
  #print(nodos_individual$proxy_uso |> quantile(c(1:10/10)))
  nodos_individual |> nrow() |> print()
  leafletProxy=leafletProxy |> 
    addCircles(data=nodos_sub_grafo,radius =~proxy_uso,group = as.character(i-1),fillColor = ~paleta_nodos((proxy_uso)),
               color = ~paleta_nodos((proxy_uso)) ) |> 
    addPolylines(data=sub_grafo |> st_as_sf(),weight = ~proxy_uso,group = as.character(i-1),
                 ,fillColor = ~paleta_aristas((proxy_uso)),
                 color = ~paleta_aristas((proxy_uso)) ) 
}

leafletProxy=leafletProxy |> 
  addLayersControl(baseGroups = as.character(0:23))
leafletProxy=leafletProxy |> 
  addPolygons(data=agebs,color = "white",fillColor = "white",stroke = F,weight = .01,opacity = 0.1)
leafletProxy
