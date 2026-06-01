##Insumos de mapa web 
edges_municipios_sf=st_read(con,'aristas_municipios')
nodos_municipios=st_read(con,'nodos_municipios')


##Para cada mes

patrones=c(
  "06_weekday","06_weekend",
  "10_weekday","10_weekend",
  "12_weekday","12_weekend"
           )
#1 patron de mobilidad son 24 archivos (cada hora)
#CVEGEO_origin/ destination
grafos_municipal_patron=list.files("outputs/municipal/",pattern = patrones[1],full.names = T) |> 
  sort() |> tail(24) |> 
  lapply(read.csv)
corte_significancia=function(patron,qth_quantile=0.7,slice_max=5){
  lista_archivos=list.files("outputs/municipal/",pattern =patron ,full.names = T) |> 
    sort() |> tail(24)
  
  grafos_municipal_patron=lista_archivos |> 
    lapply(read.csv)
  grafos_municipal_patron |> 
    lapply(\(x){
      x |> 
        dplyr::group_by(CVEGEO_origin)|>
        dplyr::summarise(proxy_uso=sum(proxy_uso,na.rm=T)) |>
        dplyr::arrange(CVEGEO_origin)|> dplyr::pull(proxy_uso) |>log() |>  max()
    }) |> unlist() |> max()->max_log_proxy_uso_auto_ciclos
  grafos_municipal_patron |> 
    lapply(\(x){
      x |> dplyr::pull(proxy_uso) |>log() |>  max()
    }) |> unlist() |> max()->max_log_proxy_uso
  grafos_municipal_patron |> 
    lapply(\(x){
      x |> dplyr::pull(proxy_uso) |>log() |>  quantile(qth_quantile)
    }) |> unlist() |> unname() ->umbrales_log_minimos
  aristas_municipal_patron=1:(grafos_municipal_patron |> length()) |> 
    lapply(\(i){
      grafos_municipal_patron[[i]] |> 
        dplyr::filter(log(proxy_uso)>=umbrales_log_minimos[i]) |> 
        dplyr::ungroup() |>
        dplyr::group_by(CVEGEO_origin) |> 
        dplyr::arrange(dplyr::desc(proxy_uso)) |> 
        dplyr::slice_head(n=slice_max) |> 
        dplyr::ungroup() |> 
        write.csv(gsub(pattern = "municipal",replacement = "municipal_simple",lista_archivos[i]) ,row.names = F )
      })
  catalogo=list(
  )
  catalogo[['max_log_proxy_uso_auto_ciclos']]=max_log_proxy_uso_auto_ciclos
  catalogo[['max_log_proxy_uso']]=max_log_proxy_uso
  catalogo[['umbral_log_minimo']]=min(umbrales_log_minimos)
  return(catalogo)
}

patrones |> lapply(\(p){corte_significancia(patron =p) |> unlist()})
z=do.call(rbind,z)
z=cbind(patron=patrones,z) |>
  as.data.frame()
z |> write.csv("outputs/metricas_patrones_movilidad.csv",row.names = F)
escalar_proxy_uso=function(aristas_municipal_patron_hora,
                           grosor_arista_min=.01,
                           grosor_arista_max=14.9,
                           diametro_nodo_min=100,
                           diametro_nodo_max=2900){
  lista_grafo=list()
  lista_grafo[['aristas']]=aristas_municipal_patron_hora |> 
    dplyr::mutate(proxy_uso=log(proxy_uso)) |> 
    dplyr::mutate(proxy_uso=grosor_arista_min+grosor_arista_max*(proxy_uso-min(umbrales_log_minimos)  )/(max_log_proxy_uso - min(umbrales_log_minimos))) 
  
  lista_grafo[['nodos']]=aristas_municipal_patron_hora |> 
    dplyr::group_by(CVEGEO_origin)|>
    dplyr::summarise(proxy_uso=sum(proxy_uso,na.rm=T)) |>
    dplyr::ungroup() |> 
    dplyr::mutate(proxy_uso=log(proxy_uso)) |> 
    dplyr::mutate(proxy_uso=diametro_nodo_min+diametro_nodo_max*(proxy_uso-min(umbrales_log_minimos)  )/(max_log_proxy_uso_auto_ciclos - min(umbrales_log_minimos)))
  return(lista_grafo)
}
