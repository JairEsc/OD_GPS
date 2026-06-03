##Correlación. 
### Nos basamos fuértemente en el artículo de nature, sobre CDR en México y su validación a través de Censo a nivel de AGEB
list.files("outputs/VKT/",pattern='identificacion',full.names = T)
list.files("outputs/VKT/",pattern='identificacion',full.names = T)[c(2:5)] |> 
  lapply(read.csv)->identificacion_hogares
identificacion_hogares=do.call(rbind,identificacion_hogares)


identificacion_hogares |> nrow()

identificacion_hogares |> 
  dplyr::filter(!is.na(hogares)) |> 
  dplyr::group_by(hogares) |> 
  dplyr::summarise(conteo=dplyr::n()) |> 
  merge(agebs |> dplyr::select(CVEGEO,POB1) |> st_drop_geometry(),by.x='hogares',by.y='CVEGEO',all.x=T)->conteo_hogares
conteo_hogares=conteo_hogares |> 
  dplyr::filter(POB1>1000 & conteo>20)
cor(conteo_hogares$conteo,
    conteo_hogares$POB1,
    use = "pairwise.complete.obs",
    method = "s")

car::scatterplot(conteo_hogares$conteo,
                 conteo_hogares$POB1)


##############
metricas=device_nivel_de_uso
identificacion_hogares=identificacion_hogares|> merge(device_nivel_de_uso,by.x='device_IDs',by.y='device_id',all.x=T)
identificacion_hogares |> 
  dplyr::filter(!is.na(hogares)) |> 
  dplyr::group_by(hogares) |> 
  dplyr::summarise(conteo=dplyr::n(),
                   proxy_uso=sum(proxy_uso_mean,na.rm=T)) |> 
  merge(agebs |> dplyr::select(CVEGEO,POB1) |> st_drop_geometry(),by.x='hogares',by.y='CVEGEO',all.x=T)->conteo_hogares_metricas
conteo_hogares_metricas=conteo_hogares_metricas |> 
  dplyr::filter( conteo>22)

cor(conteo_hogares_metricas$conteo,
    conteo_hogares_metricas$POB1,
    use = "pairwise.complete.obs",
    method = "s") #0.73
