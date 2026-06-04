
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
  agebs_intersection=agebs |> merge(identificar_AGEB(rutina_deviceID(ID_device)[['datos']] |>
                                                       dplyr::select(-origin_geoid,-destination_geoid)) |>
                                      identificarHogares(use_partition = T),by.x='CVEGEO',by.y='punto_relevante',all.y=T)
  ##Juntar origenes y destinos en una sola con un campo distintivo para colorear
  leaflet() |> 
    addTiles() |> 
    addCircleMarkers(data=rutina_usuario[['datos']] |> 
                       st_as_sf(coords=c('overlap_origin_long','overlap_origin_lat'),crs=4326),color='green',clusterOptions = markerClusterOptions(),group = "origenes") |> 
    addCircleMarkers(data=rutina_usuario[['datos']]|> 
                       st_as_sf(coords=c('overlap_destination_long','overlap_destination_lat'),crs=4326),color='red',clusterOptions = markerClusterOptions(),group = "destinos") |> 
    addLayersControl(overlayGroups = c("origenes","destinos")) |> 
    addPolygons(data=agebs_intersection,fillOpacity = 0.2)
}

calcularNivelUso=function(OD_crudo){
  device_nivel_de_uso=OD_crudo |>
    dplyr::mutate(day=lubridate::day(start_timestamp)) |>
    dplyr::mutate(mes=lubridate::month(start_timestamp)) |>
    dplyr::mutate(day=dplyr::if_else(day<10,stringr::str_c("0", day),stringr::str_c(day) )) |>
    dplyr::mutate(mes=dplyr::if_else(mes==6,stringr::str_c("0",  "6"),stringr::str_c(mes)) ) |>
    dplyr::mutate(day=stringr::str_c(mes,"-",day)) |>
    dplyr::group_by(device_id) |>
    dplyr::summarise(
      trip_duration_sec_cum=sum(trip_duration_sec,na.rm = T),
      trip_duration_sec_mean=mean(trip_duration_sec,na.rm = T),
      trip_distance_m_cum=sum(trip_distance_m,na.rm = T),
      trip_distance_m_mean=mean(trip_distance_m,na.rm = T),
      trip_speed_mps=mean(trip_speed_mps,na.rm = T),
      usos=dplyr::n(),
      proxy_uso_mean=mean(trip_scaled_ratio,na.rm=T),
      numero_usos=as.integer(dplyr::n()),
      dias_registrados=as.integer(dplyr::n_distinct(day)),.groups = "keep"
                     ) |> 
    dplyr::collect()
}

