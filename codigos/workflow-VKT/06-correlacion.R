##Correlación. 
### Nos basamos fuértemente en el artículo de nature, sobre CDR en México y su validación a través de Censo a nivel de AGEB
#source("codigos/workflow-VKT/04-casoParticularZMP.R")
#Insumos: 
# -AGEBs 
  #Con CVEGEO y población Censal. Se estima con población rural de localidades
CVE_MUN_ZMP=c("048","051","052","039","022","082","083","069")
agebs=read.csv("outputs/VKT/poblacion_por_ageb.csv")
agebs=agebs |> dplyr::mutate(CVE_MUN=substr(CVEGEO,3,5)) |> dplyr::filter(
  CVE_MUN%in%CVE_MUN_ZMP
) 
# -Metricas por usuarios
  #Con device_id, número de usos, número de días registrados, y distancias recorridas
#calcularNivelUso(OD_crudo = OD_crudo)->device_nivel_de_uso  #Ejecutar con precaución

device_nivel_de_uso=read.csv("outputs/VKT/device_nivel_de_uso.csv")
##Lo guardamos en un RDS para fácil acceso

# -Identificación de Hogares
list.files("outputs/VKT","identificacion",full.names=T) |> 
  lapply(read.csv)->hogares_devices
hogares_devices=do.call(rbind,hogares_devices) |> 
  dplyr::filter(!is.na(hogares)) |> 
  dplyr::rename(device_id=device_IDs)
hogares_devices |> nrow() |> paste("dispositivos únicos en la ZMP")


#Proceso
#Filtramos usuarios con poco uso
device_nivel_de_uso |> 
  dplyr::filter(numero_usos>1 & dias_registrados>3) |> 
  dplyr::right_join(hogares_devices,by=dplyr::join_by(device_id)) |> 
  dplyr::filter(!is.na(dias_registrados))->seleccion_usuarios
seleccion_usuarios |> nrow() |> paste("dispositivos únicos seleccionados en la ZMP")

#Contamos número de usuario por AGEB
seleccion_usuarios |> 
  dplyr::group_by(hogares) |> 
  dplyr::summarise(conteo=dplyr::n()) |> 
  dplyr::left_join(agebs,by=dplyr::join_by(hogares==CVEGEO)) |> 
  dplyr::ungroup() |> 
  dplyr::filter(substr(hogares,3,5)%in%CVE_MUN_ZMP)->conteo_usuarios_ageb

conteo_usuarios_ageb$conteo |> sum() |> paste("usuarios distribuidos en")
conteo_usuarios_ageb$hogares |> unique() |> length() |> paste("AGEBS")
#Aún así, hay AGEBS donde solamente hay un usuario identificado.
#Como referencia, 
#En la ZMP+Tiza hay 
agebs|> nrow() |> paste(" AGEBs")

#Ahora sí podemos calcular la correlación entre usuarios y población censal
conteo_usuarios_ageb |> dplyr::select(conteo,POB1) |> cor(method='s',use='pairwise.complete.obs')


#
library(tmap)
#tmap::qtm(limite_municipal)
