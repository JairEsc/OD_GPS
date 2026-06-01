###Caso particular: Zona metropolitana de Pachuca + Tizayuca + Zempoala
source("codigos/workflow-VKT/00-conexiones.R")
source("codigos/workflow-VKT/01-definiciones.R")
source("codigos/workflow-VKT/02-auxiliares.R")
source("codigos/workflow-networks/02-auxiliares.R")

##Filtramos los dispositivos a un mes
# OD_crudo=filtro_por_fecha(mes = 6)
# OD_generador(OD_crudo,particion = limite_municipal |> dplyr::filter(
#   CVE_MUN%in%c("048","051","052","039","022","082","083","069")
# ))
# 
# OD_crudo=filtro_por_fecha(mes = 6,collect = F) 
# OD_crudo=OD_crudo |> 
#   dplyr::filter(stringr::str_sub(origin_geoid,1,5)%in%
#                          c("13048","13051","13052","13039","13022","13082","13083","13069"))
# device_nivel_de_uso=calcularNivelUso(OD_crudo)
# device_nivel_de_uso |> 
#   dplyr::filter(numero_usos>1)|>
#   dplyr::pull(device_id) |> unique()->device_IDs
# 
# 
# DBI::dbWriteTable(local,"cityflow_ZMP",value = OD_crudo)
OD_crudo=dplyr::tbl(local,"cityflow_ZMP")
device_nivel_de_uso=calcularNivelUso(OD_crudo)
device_nivel_de_uso |> 
  dplyr::filter(numero_usos>100)|>
  dplyr::pull(device_id) |> unique()->device_IDs

prop_minima=0.25

# 
# identificarHogares(sample(device_IDs,1) )
sample(device_IDs,1)->sample_ID
identificar_AGEB(rutina_deviceID(sample_ID)[['datos']] |>
                   dplyr::select(-origin_geoid,-destination_geoid)) |>
  identificarHogares(use_partition = T)
verRutina(sample_ID)
