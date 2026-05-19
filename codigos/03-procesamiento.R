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

#3 horas