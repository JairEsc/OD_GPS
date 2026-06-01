##Estadísticas posiblemente útiles

#Estadísticas por usuario
filtro_por_fecha(##Usa OD_crudo por default
  mes=NA,
                    trend_week = NA,travel_modes = 'driving',
                    horas = NA,collect = F)|> 
  dplyr::group_by(device_id,trend_wknd_week) |> 
  dplyr::summarise(
    trip_duration_sec_cum=sum(trip_duration_sec,na.rm = T),
    trip_duration_sec_mean=mean(trip_duration_sec,na.rm = T),
    trip_distance_m_cum=sum(trip_distance_m,na.rm = T),
    trip_distance_m_mean=mean(trip_distance_m,na.rm = T),
    trip_speed_mps=mean(trip_speed_mps,na.rm = T),
    usos=dplyr::n()) |>
  dplyr::ungroup() |> 
  dplyr::arrange(device_id) |> 
  dplyr::collect()->usuarios_por_rutina
usuarios_por_rutina_sample=usuarios_por_rutina |> head(1000) |> dplyr::collect()
con <- DBI::dbConnect(RSQLite::SQLite(), "outputs/estadisticas/estadisticas.sqlite")
#st_write(usuarios_por_rutina, con, "usuarios_por_rutina", delete_layer = FALSE)
usuarios_por_rutina=dplyr::tbl(con,"usuarios_por_rutina")

#Promedio pesado de distancia recorrida
weighted.mean(usuarios_por_rutina |> dplyr::pull(trip_distance_m_mean),
              w = usuarios_por_rutina |> dplyr::pull(usos))

#Orígenes más comunes por usuario.
filtro_por_fecha(mes=NA,
                    trend_week = "weekday",travel_modes = NA,
                    horas = c(4:12),collect = F) |> 
  head(100000) |> 
  dplyr::collect() |> 
  dplyr::mutate(dia=lubridate::day(start_timestamp),
                hora=lubridate::hour(start_timestamp)) |> 
  dplyr::group_by(device_id,origin_geoid) |> 
  dplyr::summarise(inicios_viaje=dplyr::n())->z




##Rutina por usuario :
#a212dDZuNWlnNG46Y2k3OWUzcTYwam5uMg==
rutina_deviceID=function(ID_device='a212dDZuNWlnNG46Y2k3OWUzcTYwam5uMg=='){
  viajes=OD_crudo |> 
    dplyr::filter(device_id==ID_device) |> 
    dplyr::collect()
  lista_rutina=list()
  viajes |> 
    dplyr::group_by(origin_geoid) |> 
    dplyr::summarise(conteo_origen=dplyr::n()) |> 
    dplyr::arrange(dplyr::desc(conteo_origen))->lista_rutina[['origenes-comunes']]
  viajes |> 
    dplyr::group_by(destination_geoid) |> 
    dplyr::summarise(conteo_destino=dplyr::n()) |> 
    dplyr::arrange(dplyr::desc(conteo_destino))->lista_rutina[['destinos-comunes']]
  viajes |> 
    dplyr::group_by(origin_geoid,destination_geoid) |> 
    dplyr::summarise(conteo_origen_destino=dplyr::n()) |> 
    dplyr::arrange(dplyr::desc(conteo_origen_destino))->lista_rutina[['origenes-destinos-comunes']]
  lista_rutina[['datos']]=viajes
  return(lista_rutina)
}
rutina_usuario=rutina_deviceID(ID_device = usuarios_por_rutina_sample$device_id[792])
rutina_usuario[['datos']] ->zzz
leaflet() |> 
  addTiles() |> 
  addCircleMarkers(data=rutina_usuario[['datos']] |> 
              st_as_sf(coords=c('overlap_origin_long','overlap_origin_lat'),crs=4326),color='green') |> 
  addCircleMarkers(data=rutina_usuario[['datos']]|> 
              st_as_sf(coords=c('overlap_destination_long','overlap_destination_lat'),crs=4326),color='red')
