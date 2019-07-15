
####################################################################################################################################
#         __  __                              _____   _____            __  __ _    _ _   _ _____ _____ _____ _____ _____ ____      #
#        |  \/  |                            |  __ \ / ____|          |  \/  | |  | | \ | |_   _/ ____|_   _|  __ \_   _/ __ \     #
#       | \  / | __ _ _ __   __ _           | |__) | (___            | \  / | |  | |  \| | | || |      | | | |__) || || |  | |     #
#      | |\/| |/ _` | '_ \ / _` |          |  _  / \___ \           | |\/| | |  | | . ` | | || |      | | |  ___/ | || |  | |      #
#     | |  | | (_| | |_) | (_| |          | | \ \ ____) |          | |  | | |__| | |\  |_| || |____ _| |_| |    _| || |__| |       #
#    |_|  |_|\__,_| .__/ \__,_|          |_|  \_\_____/           |_|  |_|\____/|_| \_|_____\_____|_____|_|   |_____\____/         #
#                | |            ______                   ______                                                                    #
#               |_|           |______|                 |______|                                                                    #
#                                                                                                                                  #
####################################################################################################################################

############################################################################################################
#                        _____                            _                 _                              #
#                       |  __ \                          | |               (_)                             #
#                      | |  | | ___ _ __   ___ _ __   __| | ___ _ __   ___ _  __ _ ___                     # 
#                     | |  | |/ _ \ '_ \ / _ \ '_ \ / _` |/ _ \ '_ \ / __| |/ _` / __|                     #
#                    | |__| |  __/ |_) |  __/ | | | (_| |  __/ | | | (__| | (_| \__ \                      #
#                   |_____/ \___| .__/ \___|_| |_|\__,_|\___|_| |_|\___|_|\__,_|___/                       #
#                              | |                                                                         #
#                             |_|                                                                          #                  
#################################---Instalando e chamando dependências---###################################

# Importando os scripts das funções que vamos utilizar
files.sources = list.files("Functions")
sapply(paste0("Functions/", files.sources), source)

# Instalando os pacotes faltantes e chamando junto com os já instalados
ler_libs(c("maptools", "spdep", "cartography", "tmap", "leaflet", "dplyr", "rgdal", "RColorBrewer", "R.utils", "htmltools","htmlwidgets"))

###############################################################################################
#                       _____ _                 _            /\/|                             #
#                     / ____(_)               | |          |/\/                               #
#                   | (___  _ _ __ ___  _   _| | __ _  ___ ___   ___  ___                     # 
#                   \___ \| | '_ ` _ \| | | | |/ _` |/ __/ _ \ / _ \/ __|                     #
#                  ____) | | | | | | | |_| | | (_| | (_| (_) |  __/\__ \                      #
#                |_____/|_|_| |_| |_|\__,_|_|\__,_|\___\___/ \___||___/                       #
#                                                   )_)                                       # 
##################################---Simulando os dados---#####################################

# Simulando valores de incidência
Valores <- seq(0.23, 4.17, 0.01) %>% sample(size = 498, replace = TRUE)

# Simulando os dados de localização de postos de saude(n=5), hospitais(n=3), eventos cardiovarculares(n=30), obitos(n=15)
# random_positions(LATITUDE_1, LONGITUDE_1, LATITUDE_2, LONGITUDE_2, NUMERO_OBS)
ps_p <- random_positions(-30.029140, -51.234022, -30.114543, -51.158496, 5)
ps_h <- random_positions(-30.029140, -51.234022, -30.114543, -51.158496, 3)
ps_e <- random_positions(-30.029140, -51.234022, -30.114543, -51.158496, 30)
ps_o <- random_positions(-30.029140, -51.234022, -30.114543, -51.158496, 15)


###############################################################################################
#                    _______        _                             _                           #
#                  |__   __|      | |                           | |                           #
#                    | |_ __ __ _| |_ __ _ _ __ ___   ___ _ __ | |_ ___                       #
#                   | | '__/ _` | __/ _` | '_ ` _ \ / _ \ '_ \| __/ _ \                       #
#                  | | | | (_| | || (_| | | | | | |  __/ | | | || (_) |                       #
#                 |_|_|  \__,_|\__\__,_|_| |_| |_|\___|_| |_|\__\___/                         #
#                                                                                             #
##########################---Importando e formatando os dados---###############################

# Shape File
# Importando o Shape File
shp <- readOGR("Shape Files", stringsAsFactors=FALSE, encoding="UTF-8")

# Definindo o enconding como UTF-8 para pegar os caracteres corretos
Encoding(shp$NM_MUNICIP) <- "UTF-8"

# Retirando LAGOA DOS PATOS
shp <- shp[!shp$NM_MUNICIP=="LAGOA DOS PATOS",]


# CID
# Importando o CID para mostrar a causa dos óbitos no mapa 
cid <- read.csv2("Essential Datasets/CID.csv", stringsAsFactors = FALSE, encoding = "UTF-8", 
                 col.names = c("COD","CAUSA")) %>% sample_n(15) %>% mutate(ID=eval(666200+1:15))


# Dados  
# Formatando data.frame para dar o merge entre o banco com os valores e o banco com os dados do mapa
dados <- data.frame("Codigos" = as.numeric(shp$CD_GEOCMU),
                    "Nomes" = shp$NM_MUNICIP,
                    "Valores" = Valores,
                    stringsAsFactors = FALSE)

# Deletando LAGOA DOS PATOS
dados <- dados[!dados$Nomes=="LAGOA DOS PATOS",]


# Realizando o merge entre banco com os valores e o banco de dados do mapa
# Utilizando as variáveis de código de município presente nos dois bancos
dados_mapa <- merge(x = shp,y = dados, by.x = "CD_GEOCMU", by.y = "Codigos")

# Adicionando as coordenadas geográficas 
proj4string(dados_mapa) <- CRS("+proj=utm +datum=WGS84 +no_defs")




#################################################################################
#                              _____            _                               #
#                             |  __ \          (_)                              #
#                            | |  | | ___  ___ _  __ _ _ __                     #
#                           | |  | |/ _ \/ __| |/ _` | '_ \                     #
#                          | |__| |  __/\__ \ | (_| | | | |                     #
#                         |_____/ \___||___/_|\__, |_| |_|                      #
#                                             __/ |                             #
#                                            |___/                              #
#########################---Preparando e formatando o mapa---####################

# Importando icones dos marcadores no mapa
p_ico <- makeIcon("Icons/postos.png")
h_ico <- makeIcon("Icons/hospitais.png")
e_ico <- makeIcon("Icons/eventos.png")
o_ico <- makeIcon("Icons/obitos.png")

# Selecionando a paleta de cores e os valores a serem vinculados as cores
pal <- colorNumeric(palette = "Spectral", domain = dados_mapa$Valores, reverse = TRUE) #cores do mapa

# Pop-up informativo do município
municipio_popup <- paste0("<strong>Municipio: </strong>", 
                          dados_mapa$Nomes, 
                          "<br><strong>% prevalência de SCA: </strong>", 
                          dados_mapa$Valores)

# Pop-up informativo do óbito 
obito_popup <- paste0("<strong>Idade: </strong>", 
                      round(ps_o$ID/6500, 0)-sample(1:nrow(ps_o),15),
                      "<br><strong>Causa: (</strong>",
                      
                      cid$COD, "<strong>) </strong>", cid$CAUSA , "<strong>.</strong>")

######################################################################################
#                                __  __                                              #                          
#                               |  \/  |                                             #                          
#                              | \  / | __ _ _ __   __ _                             #                        
#                             | |\/| |/ _` | '_ \ / _` |                             #                      
#                            | |  | | (_| | |_) | (_| |                              #                        
#                           |_|  |_|\__,_| .__/ \__,_|                               #                      
#                                       | |                                          #                      
#                                      |_|                                           #                     
##################---Construção, plotagem e exportação do mapa---#####################
# Construindo o mapa
mapa_sca <- leaflet(data = dados_mapa) %>%
  addTiles() %>%
  addPolygons(fillColor = ~pal(dados_mapa$Valores), 
              fillOpacity = 0.5, 
              color = "#BDBDC3", 
              weight = 1.5, 
              popup = municipio_popup) %>%
  addMarkers(lng = ~ps_p$LONG, lat = ~ps_p$LAT, icon = p_ico, 
             label="Posto de Saúde", clusterOptions = markerClusterOptions()) %>%
  addMarkers(lng = ~ps_h$LONG, lat = ~ps_h$LAT, icon = h_ico, 
             label="Hospital", clusterOptions = markerClusterOptions()) %>%
  addMarkers(lng = ~ps_e$LONG, lat = ~ps_e$LAT, icon = e_ico,
             label="Participante com doença cardíaca", clusterOptions = markerClusterOptions()) %>%
  addMarkers(lng = ~ps_o$LONG, lat = ~ps_o$LAT, popup = obito_popup, icon = o_ico,
             label="Participante falecido", clusterOptions = markerClusterOptions()) %>%
  addLegend("bottomright", pal = pal, values = ~dados_mapa$Valores,
            title = "Incidência de SCA", 
            labFormat = labelFormat(suffix = "%", digits=2),
            opacity = 1) %>% 
  setView(-53,-30.5,7) %>% 
  addMiniMap(toggleDisplay = TRUE) %>% 
  addEasyButton(easyButton(
    icon="fa-globe", title="Zoom to Level 1",
    onClick=JS("function(btn, map){ map.setZoom(1); }"))) %>%
  addEasyButton(easyButton(
    icon="fa-crosshairs", title="Locate Me",
    onClick=JS("function(btn, map){ map.locate({setView: true}); }")))

#Plotando o mapa
mapa_sca

# Salvando em HTML
htmlwidgets::saveWidget(mapa_sca, "mapa_rs_municipio.html", 
                        selfcontained = TRUE)
