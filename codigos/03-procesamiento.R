# source("codigos/00-conexiones.R")
# source("codigos/01-definiciones.R")
# source("codigos/02-auxiliares.R")
meses=c(6,10,12,NULL)
trend_weeks=c("weekday","weekend")
horas=c(0:23,NULL)
particion=list(limite_municipal,agebs)
#OD_{particion}_{mes}{trend}{hora}
for(mes in meses){
  for(trend in trend_weeks){
    for(hora in horas){
      for(particion_i in particion){
        OD=filtro_por_fecha(mes=mes,trend_week = "weekend",horas = hora,collect = T)#Junio en fines de semana
        OD_matrix=OD_generador(OD_crudo = OD,particion = particion_i)
        OD_matrix |> write.csv(paste0("outputs/",
                                      ifelse( identical(particion_i,limite_municipal),
                                              'municipal','agebs')
                                      ,"/OD_"
                                      ,sprintf("%02d",mes),'_',trend,sprintf("%02d",hora) ,".csv"),row.names = F)
      }
    }
  }
}

#Definicion de grafo completo
nodos_municipios=limite_municipal |> 
  dplyr::select(CVEGEO,NOM_MUN) |> 
  st_centroid()
  
edges_municipios=nodos_municipios |> 
  dplyr::select(CVEGEO) |> 
  dplyr::mutate()
  
