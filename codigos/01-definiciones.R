##Definiciones
OD_crudo=dplyr::tbl(local,"trips_chida")
limite_municipal=dplyr::tbl(local,"limite_municipal")


##Definición de particiones
#Municipios 

limite_municipal=limite_municipal |> 
  dplyr::collect() |> 
  dplyr::mutate(CVEGEO=paste0(CVE_ENT,CVE_MUN)) |> 
  dplyr::mutate(geometry= sf::st_as_sfc(structure(geometry,class = "WKB" ),EWKB=T)) |> st_as_sf()

#Localidades

#Colonias
colonias=st_read("../../Reutilizables/Cartografia/colonias_Hidalgo_2025/colonias_Hidalgo_2025.shp")
colonias=colonias |> 
  dplyr::rename(CVEGEO=cvegeo) |> 
  st_transform(4326) |> 
  st_make_valid()
#En el proyecto de llamadas a 911 está la función para asignar al polígono más cercano con límite de distancia
#AGEBS

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
agebs=st_read("../../Reutilizables/Cartografia/Cartografia_2025/13_hidalgo/conjunto_de_datos/13a.shp") |> 
  st_transform(4326) |> 
  plyr::rbind.fill(agebs_rural
  ) |> st_as_sf() 
agebs=agebs |> 
  dplyr::select(-count)
#agebs |> st_is_valid() |> all()
#agebs$CVEGEO |> unique() |> length()##1779
#agebs|> nrow()##1779
remove(agebs_rural)
