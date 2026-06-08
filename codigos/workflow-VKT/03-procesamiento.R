### Parámetro de número de viajes mínimo para representatividad. 
# source("codigos/workflow-VKT/00-conexiones.R")
# source("codigos/workflow-VKT/01-definiciones.R")
# source("codigos/workflow-VKT/02-auxiliares.R")


device_nivel_de_uso=dplyr::tbl(local,"device_nivel_de_uso")
#DBI::dbWriteTable(local,name = "device_nivel_de_uso",value = device_nivel_de_uso)
device_IDs=device_nivel_de_uso |> dplyr::filter(numero_usos>NVM) |> dplyr::pull(device_id)

OD_crudo=dplyr::tbl(local,"cityflow") |> 
  dplyr::filter(lubridate::month(start_timestamp)%in%c(6,10,12))

#Para cada usuario, se filtran sus viajes
prop_minima=0.05
i=sample(1:length(device_IDs),1)
ID <- device_IDs[i]
datos_raw <- rutina_deviceID(ID)[['datos']] |> 
  dplyr::select(-origin_geoid, -destination_geoid)
lugares_de_actividad <- identificar_AGEB(datos_raw)  
lugares_de_actividad=lugares_de_actividad |> 
  identificarHogares(use_partition = T)
lugares_de_actividad |> nrow() |> paste("distintos AGEBS visitados")
lugares_de_actividad_umbral=lugares_de_actividad |> 
  dplyr::filter(prop>prop_minima)
verRutina(ID)
rutina_detallada=identificarHogaresDetallado(ID)
##Ejemplos: 
#"ZHZ0aWFibjkxNGphazo2djU1YzloaWFtODRs"
#"MWNydDRzYnN1cjMxNjo5aTFycW5jbm5qbWpk"
#"YmRvY2VmbnZqNWpkbTpkamk0cDRiaHR0djhv"
if (nrow(lugares_de_actividad_umbral) > 0) {
  return(list(status = "hogar", value = lugares_de_actividad_umbral$punto_relevante[1], id = ID))
} else {
  return(list(status = "indeciso", value = ID, id = ID))
}
