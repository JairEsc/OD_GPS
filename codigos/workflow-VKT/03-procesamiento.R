### Parámetro de número de viajes mínimo para representatividad. 
# source("codigos/workflow-VKT/00-conexiones.R")
# source("codigos/workflow-VKT/01-definiciones.R")
# source("codigos/workflow-VKT/02-auxiliares.R")
NVM=1 ##Sin filtro. Usando todos

# device_nivel_de_uso=OD_crudo |> 
#   dplyr::mutate(day=lubridate::day(start_timestamp)) |> 
#   dplyr::mutate(mes=lubridate::month(start_timestamp)) |> 
#   dplyr::mutate(day=dplyr::if_else(day<10,stringr::str_c("0", day),stringr::str_c(day) )) |> 
#   dplyr::mutate(mes=dplyr::if_else(mes==6,stringr::str_c("0",  "6"),stringr::str_c(mes)) ) |> 
#   dplyr::mutate(day=stringr::str_c(mes,"-",day)) |> 
#   dplyr::group_by(device_id) |> 
#   dplyr::summarise(numero_usos=as.integer(dplyr::n()),
#                    dias_registrados=as.integer(dplyr::n_distinct(day)),.groups = "keep"
#                    ) |> dplyr::collect()
device_nivel_de_uso=dplyr::tbl(local,"device_nivel_de_uso")
#DBI::dbWriteTable(local,name = "device_nivel_de_uso",value = device_nivel_de_uso)
device_IDs=device_nivel_de_uso |> dplyr::filter(numero_usos>NVM) |> dplyr::pull(device_id)
# seed_fija=100000
# set.seed(seed_fija)
# identificarHogares(sample(device_IDs,1) )
# set.seed(seed_fija)
# identificar_AGEB(rutina_deviceID(sample(device_IDs,1))[['datos']] |> dplyr::select(-origin_geoid,-destination_geoid)) |> 
#   identificarHogares(use_partition = T)
# ##La identificación es más fácil usando AGEBs
# prop_minima=0.25
# indecisos=character(0)
# hogares=character(0)
# 1:100 |>  
#   lapply(\(i){
#     ID=device_IDs[i]
#     lugares_de_actividad=identificar_AGEB(rutina_deviceID(ID)[['datos']] |> dplyr::select(-origin_geoid,-destination_geoid)) |> 
#       identificarHogares(use_partition = T)
#     lugares_de_actividad_umbral=lugares_de_actividad |> 
#       dplyr::filter(prop>prop_minima)
#     if(nrow(lugares_de_actividad_umbral)>0){
#       hogares[i]<<-lugares_de_actividad_umbral$punto_relevante[1]
#     }else{
#       indecisos[i]<<-ID
#     }
#   })
# indecisos=indecisos[!is.na(indecisos)]
# identificar_AGEB(rutina_deviceID(sample(indecisos,1))[['datos']] |> dplyr::select(-origin_geoid,-destination_geoid)) |> 
#   identificarHogares(use_partition = T)
# verRutina(sample(indecisos,1))
# 
# library(snow)
# z=vector('list',4)
# z=1:4
# system.time(lapply(z,function(x) Sys.sleep(1)))
# cl<-makeCluster(4,type = "SOCK")
# system.time(clusterApply(cl, z,function(i){
#   
#   ID=device_IDs[i]
#   lugares_de_actividad=identificar_AGEB(rutina_deviceID(ID)[['datos']] |> dplyr::select(-origin_geoid,-destination_geoid)) |> 
#     identificarHogares(use_partition = T)
#   lugares_de_actividad_umbral=lugares_de_actividad |> 
#     dplyr::filter(prop>prop_minima)
#   if(nrow(lugares_de_actividad_umbral)>0){
#     hogares[i]<<-lugares_de_actividad_umbral$punto_relevante[1]
#   }else{
#     indecisos[i]<<-ID
#   }
# }))
# stopCluster(cl)
