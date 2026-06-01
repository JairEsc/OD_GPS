
library(snow)
cl <- makeCluster(10, type = "SOCK")
clusterEvalQ(cl, {
  library(dplyr)
  library(sf)
  source("codigos/workflow-VKT/04-casoParticularZMP.R")
})

results=clusterApply(cl, 1:112073, function(i) {
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

stopCluster(cl)

hogares <- sapply(results, function(res) if(res$status == "hogar") res$value else NA)
indecisos <- sapply(results, function(res) if(res$status == "indeciso") res$id else NA)

