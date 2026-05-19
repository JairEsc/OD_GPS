##Funciones auxiliares

linea_curvada = function(p1, p2, ecc = 0.5,signo=1,lenght=20) {
  p1=as.numeric(p1)
  p2=as.numeric(p2)
  centro=(p1 + p2) / 2
  v=p2 - p1 #Vector perpendicular
  v_perp=c(-v[2], v[1])
  punto_control=centro + signo*(v_perp * ecc)
  t=seq(0, 1, length.out = lenght)
  curva_matriz=matrix(0, nrow = length(t), ncol = 2)
  for (i in 1:length(t)) {
    curva_matriz[i, ]=(1-t[i])**2*p1+
      2*(1-t[i])*t[i]*punto_control+
      t[i]**2*p2
  }
  return(st_linestring(curva_matriz))
}
##Fechas disponibles: 
  #Entre semana de un mes/3 meses. 
  #Fines de semana de un mes/3 meses
  #Por hora de cada día
##Ejemplo: 2023-06-06 07-08
filtro_por_fecha=function(con, mes=NA,trend_week=NA,horas=NA,travel_modes=NA,collect=T){
  sub_query=OD_crudo |> 
    dplyr::select(device_id,overlap_origin_lat,overlap_origin_long,
                  overlap_destination_lat,overlap_destination_long,start_timestamp,
                  trip_duration_sec:trip_scaled_ratio)
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
    return(dplyr::show_query(sub_query))
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
    dplyr::summarise(proxy_uso=sum(trip_scaled_ratio,na.rm = T)) |> 
    dplyr::ungroup()
  
  return(OD_join)
}




particionar_por_voronoi=function(polygon,k=10){
  #polygon=agebs[1582,]
  puntos=st_sample(polygon,size = k)
  voronoi=dismo::voronoi(puntos |> as_Spatial()) |> st_as_sf()
  return(st_intersection(polygon,voronoi))
}




# st_intersects_or_nearest_m=function(df,m){
#   ##Por renglón
#   
#   particion_1_n=1:25000 |> 
#     split(gl(25,1000))
#   particion_1_n[[26]]=25001:nrow(llamadas_localidades_sin_col)
#   #colonias=colonias |> st_make_valid()
#   
#   (particion_1_n) |> 
#     sapply(\(x){
#       subset_llamadas=llamadas_localidades_sin_col[x,]
#       subset_colonias=st_crop(colonias,y = st_bbox(subset_llamadas) )
#       #=localidades_poligonos_exterior[1:300,]
#       subset_colonias |> nrow() |> print()
#       w=st_distance(subset_llamadas,subset_colonias)
#       #print(w)
#       indexes=numeric(0)
#       for(i in 1:nrow(w)){
#         x=w[i,]
#         min_index=which.min(x)
#         if(x[min_index]<=units::set_units(200, "m")){
#           indexes[length(indexes)+1]<-subset_colonias$cvegeo[min_index]
#         }
#         else{
#           indexes[length(indexes)+1]=NA
#         }
#       }
#       return(indexes)
#     },simplify = T) |> unlist()->colonias_mas_cercanas200
#   colonias_mas_cercanas200_unlist=colonias_mas_cercanas200 |> unlist()
#   
#   llamadas_localidades_sin_col$cve_col=colonias_mas_cercanas200_unlist
#   llamadas_localidades_sin_col=llamadas_localidades_sin_col |> 
#     merge(colonias |> 
#             dplyr::select(cvegeo,nom_asen,tipo) |> 
#             dplyr::rename(cve_col=cvegeo,
#                           colonia=nom_asen,
#                           tipo_colonia=tipo) |> st_drop_geometry(),
#           by='cve_col',all.x=T)
#   
#   llamadas_limpia=llamadas_localidades_sin_col |> 
#     dplyr::select(-c(cvegeo_colonia,nom_asen,tipo)) |> 
#     dplyr::relocate(cve_col,.before = colonia) |> 
#     st_drop_geometry() |> 
#     rbind(llamadas_localidades_con_col |> 
#             dplyr::rename(cve_col=cvegeo_colonia,
#                           colonia=nom_asen,
#                           tipo_colonia=tipo) |> st_drop_geometry()) |> 
#     dplyr::rename(colonia_original=Colonia)
# }
# nngeo::st_nn(OD_crudo |> 
#                st_as_sf(coords=c('overlap_origin_long','overlap_origin_lat'),crs=4326)|> st_transform(32614)
#              ,y = colonias|> st_transform(32614),maxdist = 100)


