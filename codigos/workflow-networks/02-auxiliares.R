
identificar_AGEB=function(sf_df,particion=agebs){
  sf_df |> 
    st_as_sf(coords=c('overlap_origin_long','overlap_origin_lat'),crs=4326) |> 
    st_join(
      y=particion |> 
        dplyr::select(CVEGEO) |> 
        dplyr::rename(origin_geoid=CVEGEO),join = st_intersects
    ) |> st_drop_geometry() |> 
    st_as_sf(coords=c('overlap_destination_long','overlap_destination_lat'),crs=4326) |> 
    st_join(
      y=particion |> 
        dplyr::select(CVEGEO) |> 
        dplyr::rename(destination_geoid=CVEGEO),join = st_intersects
    ) |> st_drop_geometry()
}

rutina_deviceID=function(ID_device='a212dDZuNWlnNG46Y2k3OWUzcTYwam5uMg=='){
  viajes=OD_crudo |> 
    dplyr::filter(device_id==ID_device) |> 
    dplyr::collect()
  lista_rutina=list()
  viajes |> 
    dplyr::group_by(origin_geoid) |> 
    dplyr::summarise(conteo=dplyr::n(),.groups = "keep") |> 
    dplyr::arrange(dplyr::desc(conteo))->lista_rutina[['origenes-comunes']]
  viajes |> 
    dplyr::group_by(destination_geoid) |> 
    dplyr::summarise(conteo=dplyr::n(),.groups = "keep") |> 
    dplyr::arrange(dplyr::desc(conteo))->lista_rutina[['destinos-comunes']]
  viajes |> 
    dplyr::group_by(origin_geoid,destination_geoid) |> 
    dplyr::summarise(conteo=dplyr::n(),.groups = "keep") |> 
    dplyr::arrange(dplyr::desc(conteo))->lista_rutina[['origenes-destinos-comunes']]
  lista_rutina[['datos']]=viajes
  return(lista_rutina)
}

identificarHogares=function(ID_device,use_partition=F){
  ## Para cada dispositivo, se filtran orígenes y destino a nivel punto. 
  ## Se identifica cercanía. 
  ## En Automatic Identification of Relevant Places from Cellular Network Data (Colonna, Galassi)
  ### se propone definir un límite inferior para la proporción de lugares relevantes. 
  ### Se asigna un peso a cada cluster y si el peso sobrepasa el umbral, se asocia al hogar.
  if(use_partition){
    rutina=ID_device |> 
      dplyr::select(-destination_geoid) |> 
      dplyr::rename(punto_relevante=origin_geoid) |> 
      dplyr::bind_rows(ID_device |> 
                         dplyr::select(-origin_geoid) |> 
                         dplyr::rename(punto_relevante=destination_geoid) 
                       
      ) |> 
      dplyr::mutate(conteo=1)
  }
  else{
    pre_rutina=rutina_deviceID(ID_device)
    rutina=pre_rutina[['origenes-comunes']]|> 
      dplyr::rename(punto_relevante=origin_geoid) |> 
      dplyr::bind_rows(
        pre_rutina[['destinos-comunes']] |> 
          dplyr::rename(punto_relevante=destination_geoid) 
      )
  }
  rutina  |> 
    dplyr::group_by(punto_relevante) |> 
    dplyr::summarise(conteo=sum(conteo,na.rm=T)) |> 
    dplyr::ungroup() |> 
    dplyr::arrange(dplyr::desc(conteo)) |> 
    dplyr::mutate(prop=round(conteo/sum(conteo),3) ) 
}
verRutina=function(ID_device){
  rutina_usuario=rutina_deviceID(ID_device)
  leaflet() |> 
  addTiles() |> 
  addCircleMarkers(data=rutina_usuario[['datos']] |> 
                     st_as_sf(coords=c('overlap_origin_long','overlap_origin_lat'),crs=4326),color='green',clusterOptions = markerClusterOptions(),group = "origenes") |> 
  addCircleMarkers(data=rutina_usuario[['datos']]|> 
                     st_as_sf(coords=c('overlap_destination_long','overlap_destination_lat'),crs=4326),color='red',clusterOptions = markerClusterOptions(),group = "destinos") |> 
  addLayersControl(overlayGroups = c("origenes","destinos"))
}

##Funciones auxiliares
##Fechas disponibles: 
  #Entre semana de un mes/3 meses. 
  #Fines de semana de un mes/3 meses
  #Por hora de cada día
##Ejemplo: 2023-06-06 07-08
filtro_por_fecha=function(con, mes=NA,trend_week=NA,horas=NA,travel_modes=NA,collect=T){
  sub_query=OD_crudo |> 
    dplyr::select(device_id,origin_geoid,destination_geoid,overlap_origin_lat,overlap_origin_long,
                  overlap_destination_lat,overlap_destination_long,start_timestamp,
                  trip_duration_sec,trip_distance_m,trip_speed_mps,travel_mode,trend_wknd_week,trip_scaled_ratio,)
  if(!is.na(mes)){
    sub_query=sub_query |> 
      dplyr::filter( lubridate::month(start_timestamp)==mes)
  }
  if(!is.na(trend_week)){
    sub_query=sub_query |> 
      dplyr::filter(trend_wknd_week==trend_week)  
  }
  if(length(horas)>1){
      sub_query=sub_query |> 
        dplyr::filter(lubridate::hour(start_timestamp)%in%horas) 
  }else{
   if(!is.na(horas)){
     sub_query=sub_query |> 
       dplyr::filter(lubridate::hour(start_timestamp)==horas) 
   } 
  }
  if(length(travel_modes)>1){
    sub_query=sub_query |> 
      dplyr::filter(travel_mode %in% travel_modes) 
  }else{
    if(!is.na(travel_modes)){
      sub_query=sub_query |> 
        dplyr::filter(travel_mode == travel_modes) 
    }
  }
  if(!collect){
    return(sub_query)
  }
  print(dplyr::show_query(sub_query))
  return(sub_query |> dplyr::collect())
}
filtro_por_fecha(collect = F)
filtro_por_fecha(mes=6,collect = F)
filtro_por_fecha(mes=6,trend_week = "weekend",collect = F)
filtro_por_fecha(mes=6,trend_week = "weekend",horas = c(5,9),collect = F)
filtro_por_fecha(mes=c(10),trend_week = "weekend",horas = c(5,9),travel_mode = c("cycling","walking"),collect = F)

OD_generador=function(OD_crudo=NULL,particion){
  OD_join=
    OD_crudo |> 
    st_as_sf(coords=c('overlap_origin_long','overlap_origin_lat'),crs=4326) |> 
    st_join(
      y=particion |> 
        dplyr::select(CVEGEO) |> 
        dplyr::rename(CVEGEO_origin=CVEGEO),join = st_intersects
    ) |> st_drop_geometry() |> 
    st_as_sf(coords=c('overlap_destination_long','overlap_destination_lat'),crs=4326) |> 
    st_join(
      y=particion |> 
        dplyr::select(CVEGEO) |> 
        dplyr::rename(CVEGEO_destination=CVEGEO),join = st_intersects
    ) |> st_drop_geometry() |> 
    dplyr::filter(!is.na(CVEGEO_destination) & 
                    !is.na(CVEGEO_origin)) |> 
    dplyr::group_by(CVEGEO_origin,CVEGEO_destination) |> 
    dplyr::summarise(
      proxy_uso=sum(trip_scaled_ratio,na.rm = T),
      ) |> 
    dplyr::ungroup()
  
  return(OD_join)
}


OD_precargado=function(OD_crudo=NULL){
  OD_resumen=OD_crudo |> 
    dplyr::group_by(device_id,travel_mode,trend_wknd_week,origin_geoid,destination_geoid) |> 
    dplyr::summarise(
      proxy_uso=sum(trip_scaled_ratio,na.rm = T),
      trip_duration_sec=sum(trip_duration_sec,na.rm = T),
      trip_distance_m=sum(trip_distance_m,na.rm = T),
      trip_speed_mps=mean(trip_speed_mps,na.rm=T)
    ) |> 
    dplyr::ungroup()
  return(OD_resumen)
}


particionar_por_voronoi=function(polygon,k=10){
  #polygon=agebs[1582,]
  puntos=st_sample(polygon,size = k)
  voronoi=dismo::voronoi(puntos |> as_Spatial()) |> st_as_sf()
  return(st_intersection(polygon,voronoi))
}
