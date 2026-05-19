#Definicion de grafo completo
grafo_completo=function(nodos){
  
}
nodos_municipios=limite_municipal |> 
  dplyr::select(CVEGEO) |> 
  st_centroid()

edges_municipios_start=nodos_municipios |> 
  st_drop_geometry() |> 
  dplyr::rename(CVEGEO_origin=CVEGEO) |> 
  dplyr::mutate(CVEGEO_destination=CVEGEO_origin)
edges_municipios=edges_municipios_start
for(i in 1:nrow(nodos_municipios)
){
  destino=nodos_municipios$CVEGEO[i]
  edges_municipios=edges_municipios |> 
    rbind(edges_municipios_start |> 
            dplyr::mutate(CVEGEO_destination=destino)) 
}
edges_municipios=edges_municipios |> 
  dplyr::filter(CVEGEO_origin!=CVEGEO_destination)
edges_municipios_geom=apply(edges_municipios, 1, function(row) {
    p1=nodos_municipios |> 
      dplyr::filter(CVEGEO == row[1]) |> 
      st_centroid() |>  
      st_coordinates()
    
    p2=nodos_municipios |> 
      dplyr::filter(CVEGEO == row[2]) |> 
      st_centroid() |>  
      st_coordinates()
    
    return(linea_curvada(p1, p2,ecc = 0.1 ))
    
  }, simplify = FALSE)

edges_municipios_geom=st_sfc(edges_municipios_geom, crs = st_crs(limite_municipal))

edges_municipios_sf=st_sf(edges_municipios##Top destinos
                   , 
                   geometry = edges_municipios_geom
)
leaflet() |> 
  addTiles() |> 
  addPolylines(data=edges_municipios_sf |> 
                 head(1000))


##################
nodos_agebs=agebs |> 
  dplyr::select(CVEGEO) |> 
  st_centroid()

##No tiene caso usar todos las aristas. Tomemos el grafo más denso y ese será el maximal

read.csv("outputs/agebs/OD__weekday.csv")->grafo_maximal
grafo_maximal=grafo_maximal |> 
  dplyr::filter(CVEGEO_origin!=CVEGEO_destination) |> dplyr::select(-proxy_uso)
edges_agebs_geom=apply(grafo_maximal, 1, function(row) {
  p1=nodos_agebs |> 
    dplyr::filter(CVEGEO == row[1]) |> 
    st_centroid() |>  
    st_coordinates()
  
  p2=nodos_agebs |> 
    dplyr::filter(CVEGEO == row[2]) |> 
    st_centroid() |>  
    st_coordinates()
  
  return(linea_curvada(p1, p2,ecc = 0.1 ))
  
}, simplify = FALSE)

edges_agebs_geom=st_sfc(edges_agebs_geom, crs = st_crs(agebs))

edges_agebs_sf=st_sf(grafo_maximal##Top destinos
                          , 
                          geometry = edges_agebs_geom
)
