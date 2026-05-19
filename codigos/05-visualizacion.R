##Consulta al grafo completo
edges_municipios_sf=st_read(con,'aristas_municipios')
nodos_municipios=st_read(con,'nodos_municipios')

#CVEGEO_origin/ destination
grafos_municipal_weekday=list.files("outputs/municipal/",pattern = "12_weekday",full.names = T) |> 
  sort() |> tail(24) |> 
  lapply(read.csv)

grafos_municipal_weekday=grafos_municipal_weekday |> 
  lapply(\(z){
    z |>   
      dplyr::ungroup() |>
      dplyr::group_by(CVEGEO_origin) |> 
      dplyr::arrange(dplyr::desc(proxy_uso)) |> 
      dplyr::slice_head(n=5)
  })

grafos_municipal_weekday |> 
  lapply(\(x){
    x |> 
    dplyr::group_by(CVEGEO_origin)|>
    dplyr::summarise(proxy_uso=sum(proxy_uso,na.rm=T)) |>
    dplyr::arrange(CVEGEO_origin)|> dplyr::pull(proxy_uso) |>log() |>  max()
  }) |> unlist() |> max()->max_log_proxy_uso_auto_ciclos
grafos_municipal_weekday |> 
  lapply(\(x){
    x |> dplyr::pull(proxy_uso) |>log() |>  max()
  }) |> unlist() |> max()->max_log_proxy_uso
grafos_municipal_weekday |> 
  lapply(\(x){
    x |> dplyr::pull(proxy_uso) |>log() |>  quantile(0.7)
  }) |> unlist() |> unname() ->umbrales_log_minimos
aristas_municipal_weekday=1:(grafos_municipal_weekday |> length()) |> 
  lapply(\(i){
    grafos_municipal_weekday[[i]] |> 
      dplyr::filter(log(proxy_uso)>=umbrales_log_minimos[i]) |> 
      dplyr::mutate(proxy_uso=log(proxy_uso)) |> 
      dplyr::mutate(proxy_uso=.01+14.9*(proxy_uso-min(umbrales_log_minimos)  )/(max_log_proxy_uso - min(umbrales_log_minimos))) 
  })
nodos_municipal_weekday=1:(grafos_municipal_weekday |> length()) |> 
  lapply(\(i){
    grafos_municipal_weekday[[i]] |> 
      dplyr::filter(log(proxy_uso)>=umbrales_log_minimos[i]) |> 
      dplyr::group_by(CVEGEO_origin)|>
      dplyr::summarise(proxy_uso=sum(proxy_uso,na.rm=T)) |>
      dplyr::mutate(proxy_uso=log(proxy_uso)) |> 
      dplyr::mutate(proxy_uso=100+2900*(proxy_uso-min(umbrales_log_minimos)  )/(max_log_proxy_uso - min(umbrales_log_minimos)))
  })
leafletProxy=leaflet() |> 
  addProviderTiles("CartoDB.DarkMatter")#addTiles()

paleta_nodos=colorNumeric(palette ='turbo',domain = seq(100,3000,0.1) )
paleta_aristas=colorNumeric(palette ='turbo',domain = seq(0.01,14.9,0.01) )
for(i in 1:24){
  grafo_individual=aristas_municipal_weekday[[i]]
  nodos_individual=nodos_municipal_weekday[[i]]
  
  sub_grafo=grafo_individual|> 
    dplyr::mutate(CVEGEO_origin=as.character(CVEGEO_origin),
                  CVEGEO_destination=as.character(CVEGEO_destination)) |> 
    dplyr::left_join(edges_municipios_sf,by = dplyr::join_by(CVEGEO_origin==CVEGEO_origin,
                                                             CVEGEO_destination==CVEGEO_destination))
  nodos_sub_grafo=nodos_municipios |> 
    merge(nodos_individual,by.x='CVEGEO',by.y='CVEGEO_origin')
  print(nodos_individual$proxy_uso |> quantile(c(1:10/10)))
  leafletProxy=leafletProxy |> 
    addCircles(data=nodos_sub_grafo,radius =~proxy_uso,group = as.character(i),fillColor = ~paleta_nodos((proxy_uso)),
               color = ~paleta_nodos((proxy_uso)) ) |> 
    addPolylines(data=sub_grafo |> st_as_sf(),weight = ~proxy_uso,group = as.character(i),
                 ,fillColor = ~paleta_aristas((proxy_uso)),
                 color = ~paleta_aristas((proxy_uso)) ) 
}

leafletProxy=leafletProxy |> 
  addLayersControl(baseGroups = as.character(1:24))
leafletProxy
