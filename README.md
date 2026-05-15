##Fuente de los datos
22,858,456 registros limpios en el siguiente formato
device_id | origin_long | origin_lat | overlap_destination_lat | overlap_destination_long | start_timestamp | trip_duration_sec | travel_mode | trip_scaled_ratio 
Se asegura precisión menor a 20 metros.
##Procesamiento de los datos

#### OD matrix
Para una partición S={S_1,\ldots, S_n} dada por polígonos (e.g. Municipios, Localidades, Agebs y Colonias.)
Se construye la matriz de origen-destino dada por 
    OD_{ij}=#Viajes estimados del polígono S_i a S_j. 
##Representación
    