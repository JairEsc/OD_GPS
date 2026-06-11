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
  dplyr::filter(usos>10 & dias_registrados>3) |> 
  dplyr::right_join(hogares_devices,by=dplyr::join_by(device_id)) |> 
  dplyr::filter(!is.na(dias_registrados))->seleccion_usuarios
seleccion_usuarios |> nrow() |> paste("dispositivos únicos seleccionados en la ZMP")

#Contamos número de usuario por AGEB
seleccion_usuarios |> 
  dplyr::group_by(hogares) |> 
  dplyr::summarise(conteo=dplyr::n()) |> 
  merge(agebs,by.x='hogares',by.y='CVEGEO', all.y = T) |> 
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

conteo_usuarios_ageb |> dplyr::filter(substr(hogares,1,5)=='13051' ) |> 
  dplyr::select(conteo,POB1) |> cor(method='s',use='pairwise.complete.obs')

#

##Nota para Roberto
##Leer un vectorial con geometría
limite_municipal=sf::st_read("inputs/cartografia/municipiosjair.shp") #es importante seleccionar el de extensión .shp

library(tmap)
tmap::qtm(limite_municipal)




#==========================================
#Dividir la informacion en grupos percentiles para analizar su correlación
#de acuerdo al grupo percentil al que pertenecen


conteo_hogares=read.csv("conteo_hogares_NV_1_10_.csv")
# Calcular cuantiles (eliminamos el primer valor: 0%)
per=c(10:200/200)[120:180]

qconteo <- quantile(conteo_hogares$conteo, na.rm = TRUE,probs=per)[-1]
qPB     <- quantile(conteo_hogares$POB1, na.rm = TRUE,probs=per)[-1]

# Inicializar matriz de correlaciones
corrs <- matrix(0, nrow = length(qconteo), ncol = length(qPB))

# Bucles con índices numéricos para recorrer los vectores de umbrales
for (i in seq_along(qconteo)) {
  for (j in seq_along(qPB)) {
    # Filtrar datos con ambos umbrales
    datos_filtro <- conteo_hogares |>
      dplyr::filter(conteo >= qconteo[i] & POB1 >= qPB[j])
    
    # Calcular correlación (Spearman)
    corrs[i, j] <- cor(datos_filtro$conteo, datos_filtro$POB1,
                       use = "pairwise.complete.obs",
                       method = "spearman")
  }
}

# Mostrar matriz con nombres de filas/columnas (opcional)
rownames(corrs) <- paste0("conteo ≥", round(qconteo, 2))
colnames(corrs) <- paste0("POB1 ≥ ", round(qPB, 2))
heatmap(corrs)
heatmap(corrs,Rowv = NA,verbose = T)
corrs|>max()
corr_max_global <- which(corrs == max(corrs), arr.ind = TRUE)

#===============================================================
#Realizando el analisis por filtros con percentilas sobre la ZMP
conteo_hogares=conteo_usuarios_ageb
# Calcular cuantiles (eliminamos el primer valor: 0%)
#Utilizamos diferentes intervalor de percentilas para filtrar los datos [1:60]-[60:120]-etc
per=c(10:200/200)[120:180]

qconteo <- quantile(conteo_hogares$conteo, na.rm = TRUE,probs=per)[-1]
qPB     <- quantile(conteo_hogares$POB1, na.rm = TRUE,probs=per)[-1]

# Inicializar matriz de correlaciones
corrs <- matrix(0, nrow = length(qconteo), ncol = length(qPB))

# Bucles con índices numéricos para recorrer los vectores de umbrales
for (i in seq_along(qconteo)) {
  for (j in seq_along(qPB)) {
    # Filtrar datos con ambos umbrales
    datos_filtro <- conteo_hogares |>
      dplyr::filter(conteo >= qconteo[i] & POB1 >= qPB[j])
    
    # Calcular correlación (Spearman)
    corrs[i, j] <- cor(datos_filtro$conteo, datos_filtro$POB1,
                       use = "pairwise.complete.obs",
                       method = "spearman")
  }
}

# Mostrar matriz con nombres de filas/columnas (opcional)
rownames(corrs) <- paste0("conteo ≥", round(qconteo, 2))
colnames(corrs) <- paste0("POB1 ≥ ", round(qPB, 2))
heatmap(corrs)
heatmap(corrs,Rowv = NA,verbose = T)
corrs|>max()
which(corrs == max(corrs), arr.ind = TRUE)

#Correlacion sin filtrado por percentilas
conteo_usuarios_ageb %>% filter( POB1> 0)->conteo_hogares
conteo_hogares |> dplyr::select(conteo,POB1) |> cor(method='s',use='pairwise.complete.obs')

library(dplyr)

# Agrupamos nuestros datos por localidad con substr
conteo_hogares_municipios <- conteo_hogares %>% dplyr::group_by(prefijo = substr(hogares, 1, 9))|>
  dplyr::summarise(POB1=sum(POB1, na.rm = TRUE),conteo=sum(conteo, na.rm = TRUE))

conteo_hogares_municipios |> dplyr::select(conteo,POB1) |> cor(method='s',use='pairwise.complete.obs')

library(ggplot2)
library(treemapify)
#Hacemos un Treemap de acuerdo al tamaño de la poblacion con el conteo de usos
ggplot(conteo_hogares_municipios, aes(area = POB1, fill = conteo)) +
  geom_treemap()
#Realizamos un modelo lineal para visualizar el uso de aplicacion por poblacion
ggplot(conteo_hogares_municipios,aes(x = POB1, y = conteo)) + geom_point() + 
  geom_smooth(method = "lm")

conteo_hogares_municipios= conteo_hogares_municipios %>% mutate(use_ratio = conteo/POB1)
mean(conteo_hogares_municipios$use_ratio)

#Eliminacion de outliers en el filtrado por municipio pues hay valores atipicos

# columnas a filtrar
cols <- c("POB1", "conteo")

conteo_outliers <- conteo_hogares_municipios %>%
  filter(!if_any(all_of(cols), ~ .x < quantile(.x, 0.25, na.rm = TRUE) - 1.5 * IQR(.x, na.rm = TRUE) |
                   .x > quantile(.x, 0.75, na.rm = TRUE) + 1.5 * IQR(.x, na.rm = TRUE)))   
#Realizamos el treemap con datos filtrados
ggplot(conteo_outliers, aes(area = POB1, fill = use_ratio)) +
  geom_treemap()

ggplot(conteo_outliers,aes(x = POB1, y = conteo)) + geom_point() + 
  geom_smooth(method = "lm")

conteo_outliers |> dplyr::select(conteo,POB1) |> cor(method='p',use='pairwise.complete.obs')
mean(conteo_outliers$use_ratio)
