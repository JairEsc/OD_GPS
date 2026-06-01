##Guardamos |> |> |> |> |> |> |>  grafos
con <- DBI::dbConnect(RSQLite::SQLite(), "outputs/grafos/networks.sqlite")
##Falta agregar datos calculados a los geojsons

st_write(nodos_municipios, con, "nodos_municipios", delete_layer = FALSE)
st_write(edges_municipios_sf, con, "aristas_municipios", delete_layer = FALSE)
st_write(nodos_agebs, con, "nodos_agebs", delete_layer = FALSE)
st_write(edges_agebs_sf, con, "aristas_agebs", delete_layer = FALSE)

nodos_municipios |> 
  st_write("outputs/grafos/nodos_municipios.geojson",driver='GeoJSON')
edges_municipios_sf |> 
  st_write("outputs/grafos/aristas_municipios.geojson",driver='GeoJSON')


nodos_agebs |> 
  st_write("outputs/grafos/nodos_agebs.geojson",driver='GeoJSON')
edges_agebs_sf |> 
  st_write("outputs/grafos/aristas_agebs.geojson",driver='GeoJSON')
