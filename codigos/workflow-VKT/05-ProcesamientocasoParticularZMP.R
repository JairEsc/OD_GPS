source("codigos/workflow-VKT/04-casoParticularZMP.R")
source("codigos/workflow-networks/01-definiciones.R")

library(snow)
cl <- makeCluster(10, type = "SOCK")
clusterEvalQ(cl, {
  library(dplyr)
  library(sf)
  source("codigos/workflow-VKT/04-casoParticularZMP.R")
})

results=clusterApply(cl, 1:52826, function(i) {#2 horitas +-
  prop_minima=0.25
  ID <- device_IDs[i]
  datos_raw <- rutina_deviceID(ID)[['datos']] |> 
    dplyr::select(-origin_geoid, -destination_geoid)
  lugares_de_actividad <- identificar_AGEB(datos_raw) |> 
    identificarHogares(use_partition = TRUE)
  lugares_de_actividad_umbral <- lugares_de_actividad |> 
    dplyr::filter(prop > prop_minima)
  if (nrow(lugares_de_actividad_umbral) > 0) {
    return(list(status = "hogar", value = lugares_de_actividad_umbral$punto_relevante[1], id = ID))
  } else {
    return(list(status = "indeciso", value = ID, id = ID))
  }
})

hogares <- sapply(results, function(res) if(res$status == "hogar") res$value else NA)
indecisos <- sapply(results, function(res) if(res$status == "indeciso") res$id else NA)

identificacion_hogares=cbind(device_IDs,hogares) |> as.data.frame()
identificacion_hogares |> write.csv("outputs/VKT/identificacion_hogares_NV_2.csv",fileEncoding = "utf-8",row.names = F)

conteo_hogares=identificacion_hogares |> 
  dplyr::group_by(hogares) |> 
  dplyr::summarise(conteo=dplyr::n()) |> 
  dplyr::filter(!is.na(conteo)) |> 
  dplyr::filter(!is.na(hogares)) |> 
  dplyr::ungroup() |> 
  merge(agebs |> dplyr::select(CVEGEO,POB1) |> st_drop_geometry(),by.x='hogares',by.y='CVEGEO',all.x=T)
conteo_hogares|> write.csv("outputs/VKT/conteo_hogares_NV_2.csv",row.names = F,fileEncoding = "utf-8")

OD_crudo |> 
  dplyr::group_by(device_id) |> 
  dplyr::summarise(
    trip_duration_sec_cum=sum(trip_duration_sec,na.rm = T),
    trip_duration_sec_mean=mean(trip_duration_sec,na.rm = T),
    trip_distance_m_cum=sum(trip_distance_m,na.rm = T),
    trip_distance_m_mean=mean(trip_distance_m,na.rm = T),
    trip_speed_mps=mean(trip_speed_mps,na.rm = T),
    usos=dplyr::n(),
    proxy_uso_mean=mean(trip_scaled_ratio,na.rm=T)) |>
  dplyr::ungroup() |> dplyr::collect()->device_nivel_de_uso
device_nivel_de_uso |> write.csv("outputs/VKT/nivel_uso_metricas_device_id_NV_2.csv",fileEncoding = "UTF-8",row.names = F)



clusterEvalQ(cl, {
  library(dplyr)
  library(sf)
  DBI::dbDisconnect(local)
})
stopCluster(cl)
#identificacion_hogares_metricas=identificacion_hogares |> merge(device_nivel_de_uso,by.x ='device_IDs',by.y='device_id',all.x=T )
#leafsync::sync()
#cor(conteo_hogares$Freq,conteo_hogares$POB1,use = "pairwise.complete.obs",method = "k")cor(conteo_hogares$Freq,conteo_hogares$POB1,use = "pairwise.complete.obs",method = "k")