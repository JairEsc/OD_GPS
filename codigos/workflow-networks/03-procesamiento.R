# source("codigos/00-conexiones.R")
# source("codigos/01-definiciones.R")
# source("codigos/02-auxiliares.R")
meses=c(6,10,12,NA)
trend_weeks=c("weekday","weekend")
horas=c(0:23,NA)
particion=list(limite_municipal,agebs)
travel_modes=c("driving","cycling")
#OD_{particion}_{mes}{trend}{hora}
for(mes in meses){
  for(trend in trend_weeks){
    for(hora in horas){
      OD=filtro_por_fecha(mes=mes,
                            trend_week = trend,travel_modes = travel_modes,
                            horas = hora,collect = T)
      for(particion_i in particion){
        OD_matrix=OD_generador(OD_crudo = OD,particion = particion_i)
        OD_matrix |> write.csv(paste0("outputs/",
                                      ifelse( identical(particion_i,limite_municipal),
                                              'municipal','agebs')
                                      ,"/OD_",
                                      ifelse(is.na(mes),"",sprintf("%02d",mes)),
                                      '_',
                                      trend,ifelse(is.na(hora),"",sprintf("%02d",hora)),
                                      ".csv"),row.names = F)
      }
    }
  }
}


######
OD=filtro_por_fecha(mes=06,
                    trend_week = trend_weeks[1],travel_modes = 'driving',
                    horas = NA,collect = F)
#OD=OD |> dplyr::filter(origin_geoid==destination_geoid & trip_duration_sec>300 & trip_distance_m>500)
OD=OD |> 
  dplyr::group_by(device_id) |> 
  dplyr::summarise(
    trip_duration_sec=sum(trip_duration_sec,na.rm = T),
                   trip_distance_m=sum(trip_distance_m,na.rm = T),
    usos=dplyr::n())

OD=OD_precargado(OD)
OD |> dplyr::collect()->z
#3 horas