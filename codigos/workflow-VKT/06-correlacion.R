
# Correlación entre usos por población ------------------------------------

#Nos basamos fuértemente en el artículo de nature, sobre CDR en México y su validación a través de Censo a nivel de AGEB
#source("codigos/workflow-VKT/04-casoParticularZMP.R")

# Librerias utilizadas ----------------------------------------------------
library(dplyr)
library(ggplot2)
library(treemapify) #Para treemap
library(tmap) # Para graficar los mapas de la region

# Limpieza de datos -------------------------------------------------------
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
#Filtramos usuarios por numero de usos 
device_nivel_de_uso |> 
  dplyr::filter(usos>5 & dias_registrados>3) |> 
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

conteo_usuarios_ageb$conteo |> sum(na.rm=T) |> paste("usuarios distribuidos en")
conteo_usuarios_ageb$hogares |> unique() |> length() |> paste("AGEBS")

# Correlación global-------------------------------------------------
#Ahora sí podemos calcular la correlación entre usuarios y población censal
conteo_usuarios_ageb |> dplyr::select(conteo,POB1) |> cor(method='s',use='pairwise.complete.obs')

#Correlacion unica asociada al municipio 13051
conteo_usuarios_ageb |> dplyr::filter(substr(hogares,1,5)=='13051' ) |> 
  dplyr::select(conteo,POB1) |> cor(method='s',use='pairwise.complete.obs')

#Correlacion filtrado por poblacion > 0 
conteo_usuarios_ageb %>% filter( POB1> 0) |>
  dplyr::select(conteo,POB1) |> cor(method='s',use='pairwise.complete.obs')

#Agrupamos los datos por localidad (primeros 9 digitos del campo hogares) con substr
conteo_localidad <- conteo_usuarios_ageb %>% dplyr::group_by(prefijo = substr(hogares, 1, 9))|>
  dplyr::summarise(POB1=sum(POB1, na.rm = TRUE),conteo=sum(conteo, na.rm = TRUE))

conteo_localidad |> dplyr::select(conteo,POB1) |> cor(method='s',use='pairwise.complete.obs')

#Agrupamos los datos por municipios (primeros 5 digitos del campo hogares) 
conteo_municipio <- conteo_usuarios_ageb %>% dplyr::group_by(prefijo = substr(hogares, 1, 5))|>
  dplyr::summarise(POB1=sum(POB1, na.rm = TRUE),conteo=sum(conteo, na.rm = TRUE))

conteo_municipio |> dplyr::select(conteo,POB1) |> cor(method='s',use='pairwise.complete.obs')
#Se define una nueva variable use_ratio= conteo/POB1
conteo_municipio<-conteo_municipio %>% mutate(use_ratio = conteo/POB1)

mean(conteo_municipio$use_ratio)
sd(conteo_municipio$use_ratio)

#Hacemos un Treemap de acuerdo al tamaño de la poblacion con el conteo de usos
ggplot(conteo_municipio, aes(area = POB1, fill = use_ratio,label=prefijo)) +
  geom_treemap() +
  geom_treemap_text(colour = "white",
                    place = "centre",
                    size = 15) + labs(title = "Uso de Aplicación por tamaño de la poblacion en el municipio.")

#Realizamos un modelo lineal para visualizar el uso de aplicacion por poblacion
ggplot(conteo_municipio,aes(x = POB1, y = conteo)) + geom_point() + 
  geom_smooth(method = "lm") + theme_classic() + ggtitle("Proyección de Uso por Municipios de la ZMP")

#Eliminacion de outliers por localidad pues hay valores atipicos de población (Pachuca)

# columnas a filtrar
cols <- c("POB1", "conteo")

conteo_outliers <- conteo_localidad %>%
  filter(!if_any(all_of(cols), ~ .x < quantile(.x, 0.25, na.rm = TRUE) - 1.5 * IQR(.x, na.rm = TRUE) |
                   .x > quantile(.x, 0.75, na.rm = TRUE) + 1.5 * IQR(.x, na.rm = TRUE)))   
#Realizamos el treemap con datos filtrados
conteo_outliers<-conteo_outliers %>% mutate(use_ratio = conteo/POB1)
ggplot(conteo_outliers, aes(area = POB1, fill = use_ratio)) +
  geom_treemap()
#Modelo lineal sobre las localidades sin outliers 
ggplot(conteo_outliers,aes(x = POB1, y = conteo)) + geom_point() + 
  geom_smooth(method = "lm")

conteo_outliers |> dplyr::select(conteo,POB1) |> cor(method='p',use='pairwise.complete.obs')
mean(is.finite(conteo_outliers$use_ratio))

# Correlación asociada a cada municipio de acuerdo a sus localidades --------
conteo_localidad <- conteo_usuarios_ageb %>% group_by(CVE_MUN) %>%
  mutate( correlacion = cor(conteo, POB1, method = "s", use = "pairwise.complete.obs"))

corrs_municipio <- conteo_localidad %>%  select(CVE_MUN,correlacion)

#Leer un vectorial con geometría
limite_municipal=sf::st_read("inputs/cartografia/municipiosjair.shp") #es importante seleccionar el de extensión .shp

#Creacion de mapas
mapa_correlaciones <- limite_municipal %>% right_join(corrs_municipio, by="CVE_MUN") %>% 
  filter(!is.na(correlacion))

tmap::tm_shape(mapa_correlaciones) + tm_polygons(col = "correlacion", midpoint = 0, n=3 )  

# Correlación filtrado percentilas ----------------------------------------
#Podemos realizar el analisis de correlación de acuerdo a la informacion de los doc conteo_hogares
conteo_hogares=read.csv("conteo_hogares_NV_1_10_.csv")
#Realizando el analisis por filtros con percentilas sobre la ZMP con el filtrado del conteo_usuarios_ageb
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

# Mostrar matriz con nombres de filas/columnas
rownames(corrs) <- paste0("conteo ≥", round(qconteo, 2))
colnames(corrs) <- paste0("POB1 ≥ ", round(qPB, 2))
heatmap(corrs)
heatmap(corrs,Rowv = NA,verbose = T)
which(corrs == max(corrs), arr.ind = TRUE)
