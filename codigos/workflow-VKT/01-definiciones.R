##Definiciones
#sample_citydata="../../CityData/Citflow/2023-06/cityflow/citydata_hidalgo_state_mx_2023-06_deviceTrips_cityflow_00000.csv.gz" |> readr::read_csv()
#dbCreateTable(conn = local,name = "cityflow",fields = sample_citydata)

OD_crudo=dplyr::tbl(local,"cityflow") |> 
  dplyr::filter(lubridate::month(start_timestamp)%in%c(6,10,12))

limite_municipal=dplyr::tbl(local,"limite_municipal")

##Definición de particiones
#Municipios 

limite_municipal=limite_municipal |> 
  dplyr::collect() |> 
  dplyr::mutate(CVEGEO=paste0(CVE_ENT,CVE_MUN)) |> 
  dplyr::mutate(geometry= sf::st_as_sfc(structure(geometry,class = "WKB" ),EWKB=T)) |> st_as_sf()
limite_municipal_pob=st_read("../../Reutilizables/Demograficos/scince/municipal.dbf")
limite_municipal=limite_municipal |> 
  merge(limite_municipal_pob |> dplyr::select( CVEGEO,POB1) |>st_drop_geometry(),by='CVEGEO',all.x=T )
#Localidades

# #Colonias
# colonias=st_read("../../Reutilizables/Cartografia/colonias_Hidalgo_2025/colonias_Hidalgo_2025.shp")
# colonias=colonias |> 
#   dplyr::rename(CVEGEO=cvegeo) |> 
#   st_transform(4326) |> 
#   st_make_valid()
# #En el proyecto de llamadas a 911 está la función para asignar al polígono más cercano con límite de distancia
#AGEBS
## También queremos demografia porque se hará correlación
agebs=st_read("../../Reutilizables/Cartografia/Cartografia_2025/13_hidalgo/conjunto_de_datos/13a.shp") |> 
  st_transform(4326)
agebs_pob=st_read("../../Reutilizables/Demograficos/scince/ageb_urb.shp")
agebs=agebs |> 
  merge(agebs_pob |> dplyr::select(CVEGEO,POB1) |> st_drop_geometry(),by='CVEGEO',all.x=T)

##Nos interesa asignarle población a los AGEB rurales. 
##Método 1. Diferencia de Población municipal y Población en AGEB Urbano distribuida proporcional al AREA

##Si calculamos la población de las agebs urbanas
pob_municipal_en_agebs=agebs |> 
  st_drop_geometry() |> 
  dplyr::group_by(CVE_MUN) |> 
  dplyr::summarise(POB1=sum(POB1,na.rm = T))
pob_municipal_en_agebs=pob_municipal_en_agebs |> 
  merge(limite_municipal |> dplyr::select(CVE_MUN,POB1) |> st_drop_geometry() |> dplyr::rename(POB1_=POB1),by='CVE_MUN')
pob_municipal_en_agebs$diff_pob=pob_municipal_en_agebs$POB1_-pob_municipal_en_agebs$POB1
pob_municipal_en_agebs$POB1 |> sum()
##Método 2. Intersección entre Localidad Rural y AGEB Rural (suma)
pob_localidad_rural="../../Reutilizables/Demograficos/scince/loc_rur.shp" |> st_read()
pob_localidad_rural=pob_localidad_rural |> 
  dplyr::select(CVEGEO,POB1,geometry)
pob_localidad_rural$POB1 |> sum()
set.seed(1000)
agebs_rural=
  st_read("../../Reutilizables/Cartografia/Cartografia_2025/13_hidalgo/conjunto_de_datos/13ar.shp") |> 
  st_transform(4326)
agebs_rural=agebs_rural |>
  st_cast("POLYGON")
agebs_rural=agebs_rural |> 
  merge(agebs_rural |> dplyr::group_by(CVEGEO) |> dplyr::summarise(count=dplyr::n()) |> st_drop_geometry(),by='CVEGEO')


agebs_rural=agebs_rural |> 
  dplyr::rowwise() |> 
  dplyr::mutate(CVEGEO=
                  ifelse(count==1,CVEGEO,paste0(CVEGEO,"_",sample(LETTERS,size = 1))))

agebs_rural$CVEGEO |> unique() |> length()
agebs_rural_repartir=agebs_rural |> 
  dplyr::mutate(area=as.numeric(st_area(geometry))) |> 
  merge(
    y=agebs_rural |> 
      dplyr::mutate(area=as.numeric(st_area(geometry))) |> 
      st_drop_geometry() |> 
      dplyr::group_by(CVE_MUN) |> 
      dplyr::summarise(area_tot=sum(area,na.rm=T)) |> 
      dplyr::ungroup(),by='CVE_MUN',all.x=T
  ) |> 
  dplyr::mutate(area_prop=area/area_tot) |> 
  merge(pob_municipal_en_agebs |> dplyr::select(CVE_MUN, diff_pob),by='CVE_MUN')

agebs_rural_repartir=agebs_rural_repartir |> 
  dplyr::mutate(POB1=diff_pob*area_prop) |> 
  dplyr::select(-area,-count,-area_tot,-area_prop,-diff_pob)

agebs_rural_repartir_locs=agebs_rural_repartir |> 
  st_join(y = pob_localidad_rural |> st_transform(st_crs(agebs_rural_repartir)) |> 
            dplyr::rename(POB1_locs=POB1) |> 
            dplyr::select(POB1_locs),join = st_intersects,left = T)
agebs_rural_repartir_locs=agebs_rural_repartir_locs |> 
  st_drop_geometry() |> 
  dplyr::group_by(CVEGEO) |> 
  dplyr::summarise(POB1_locs=sum(POB1_locs,na.rm=T))
agebs_rural_repartir_locs$POB1_locs |> sum()
agebs_rural_repartir=agebs_rural_repartir |> 
  merge(agebs_rural_repartir_locs,by='CVEGEO')
# agebs=agebs|>
#   plyr::rbind.fill(agebs_rural_repartir)
agebs=agebs|>
  plyr::rbind.fill(agebs_rural_repartir |>
                     dplyr::select(-POB1) |>
                     dplyr::rename(POB1=POB1_locs)
  ) |> st_as_sf()
#agebs |> st_is_valid() |> all()
#agebs$CVEGEO |> unique() |> length()##1779
#agebs|> nrow()##1779
remove(agebs_rural)
