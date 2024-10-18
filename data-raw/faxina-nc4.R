# ## Lista com caminho e nomes dos arquivo a serem lidos
# list_of_files <- list.files("data-raw/nc4/",
#                             pattern = ".nc4",
#                             full.names = TRUE)
#
# ## buscando o contorno do Brasil
# br <- geobr::read_country(showProgress = FALSE)
# pol_br <- br$geom |> purrr::pluck(1) |> as.matrix()
# def_pol <- function(x, y, pol){
#   as.logical(sp::point.in.polygon(point.x = x,
#                                   point.y = y,
#                                   pol.x = pol[,1],
#                                   pol.y = pol[,2]))
# }
#
# ## função para ler arquivos NCD4
# ncdf_reader <- function(file_path){
#   nc_file <- ncdf4::nc_open(file_path)
#   df <- data.frame(
#     "longitude"=ncdf4::ncvar_get(nc_file,varid="longitude"),
#     "latitude"=ncdf4::ncvar_get(nc_file,varid="latitude"),
#     "time"=ncdf4::ncvar_get(nc_file,varid="time"),
#     "xco2"=ncdf4::ncvar_get(nc_file,varid="xco2"),
#     "xco2_quality_flag"=ncdf4::ncvar_get(nc_file,varid="xco2_quality_flag"),
#     "xco2_incerteza"=ncdf4::ncvar_get(nc_file,varid="xco2_uncertainty"),
#     "path" = file_path
#   )
#   ncdf4::nc_close(nc_file)
#   return(df)
# }
# ncdf_reader(list_of_files[1])
# dff_1 <- purrr::map_df(list_of_files[1:1000],ncdf_reader)
# readr::write_rds(dff_1,"data-raw/dff_1.rds")
# dff_2 <- purrr::map_df(list_of_files[1001:2000],ncdf_reader)
# readr::write_rds(dff_2,"data-raw/dff_2.rds")
# dff_3 <- purrr::map_df(list_of_files[2001:length(list_of_files)],ncdf_reader)
# readr::write_rds(dff_3,"data-raw/dff_3.rds")
#
# df1 <- readr::read_rds("data-raw/dff_1.rds")
# df1 <- df1 |>
#   dplyr::filter(longitude >= -80 &
#                   longitude <= -32,
#                 latitude >= -34 &
#                   latitude <= 10)
# dplyr::glimpse(df1)
#
# df2 <- readr::read_rds("data-raw/dff_2.rds")
# df2 <- df2 |>
#   dplyr::filter(longitude >= -80 &
#                   longitude <= -32,
#                 latitude >= -34 &
#                   latitude <= 10)
# dplyr::glimpse(df2)
#
# df3 <- readr::read_rds("data-raw/dff_3.rds")
# df3 <- df3 |>
#   dplyr::filter(longitude >= -80 &
#                   longitude <= -32,
#                 latitude >= -34 &
#                   latitude <= 10)
# dplyr::glimpse(df3)
# dff <- rbind(df1,df2,df3)
#
# dff <- dff |>
#   dplyr::mutate(
#     flag_br = def_pol(longitude, latitude, pol_br)
#   )
# dplyr::glimpse(dff)
#
# regiao <- geobr::read_region(showProgress = FALSE)
# pol_norte <- regiao$geom |> purrr::pluck(1) |> as.matrix()
# pol_nordeste <- regiao$geom |> purrr::pluck(2) |> as.matrix()
# pol_sudeste <- regiao$geom |> purrr::pluck(3) |> as.matrix()
# pol_sul <- regiao$geom |> purrr::pluck(4) |> as.matrix()
# pol_centroeste<- regiao$geom |> purrr::pluck(5) |> as.matrix()
#
# # Retirando alguns pontos
# pol_br <- pol_br[pol_br[,1]<=-34,]
# pol_br <- pol_br[!((pol_br[,1]>=-38.8 & pol_br[,1]<=-38.6) &
#                      (pol_br[,2]>= -19 & pol_br[,2]<= -16)),]
#
# pol_nordeste <- pol_nordeste[pol_nordeste[,1]<=-34,]
# pol_nordeste <- pol_nordeste[!((pol_nordeste[,1]>=-38.7 & pol_nordeste[,1]<=-38.6) & pol_nordeste[,2]<= -15),]
#
# # Retirando alguns pontos
# pol_br <- pol_br[pol_br[,1]<=-34,]
# pol_br <- pol_br[!((pol_br[,1]>=-38.8 & pol_br[,1]<=-38.6) &
#                      (pol_br[,2]>= -19 & pol_br[,2]<= -16)),]
#
# pol_nordeste <- pol_nordeste[pol_nordeste[,1]<=-34,]
# pol_nordeste <- pol_nordeste[!((pol_nordeste[,1]>=-38.7 & pol_nordeste[,1]<=-38.6) & pol_nordeste[,2]<= -15),]
#
# # Recriando o flag_nordeste
# dff <- dff |>
#   dplyr::mutate(
#     flag_br = def_pol(longitude, latitude, pol_br),
#     flag_nordeste = def_pol(longitude, latitude, pol_nordeste)
#   )
#
# br |>
#   ggplot2::ggplot() +
#   ggplot2::geom_sf(fill="white", color="#FEBF57",
#                    size=.15, show.legend = FALSE) +
#   ggplot2::geom_point(data= dff |>
#                         dplyr::sample_n(10000) |>
#                         dplyr::filter(flag_br|flag_nordeste) ,
#                       ggplot2::aes(x=longitude,y=latitude),
#                       shape=3,
#                       col="red",
#                       alpha=0.2)

# dff <- dff |>
#   dplyr::filter(flag_br|flag_nordeste)
# data_set <- readr::read_rds("data/nasa-xco2.rds") |>
#   # tibble::tibble() |>
#   dplyr::filter(
#     xco2 > 0,
#     xco2_quality_flag == 0) |>
#     dplyr::mutate(
#       path = stringr::str_remove(path, "data-raw/nc4/|\\.nc4"),
#       date = lubridate::as_date(stringr::str_sub(path,12,17)),
#       year = lubridate::year(date),
#       month = lubridate::month(date),
#       day = lubridate::day(date),
#       .after = "time"
#     )
# dplyr::glimpse(data_set)

# # Criando a coluna para os estados.
# # carregando os poligonos e funções
# source("R/my-function.R")
#
# # Leitura do banco de dados
# data_set <- readr::read_rds("data/nasa-xco2.rds")
#
# # Classificando pontos
# state <- 0
# x <- data_set |> dplyr::pull(longitude)
# y <- data_set |> dplyr::pull(latitude)
# for(i in 1:nrow(data_set)) state[i] <- get_geobr_state(x[i],y[i])
# data_set <- data_set |> cbind(state)
# dplyr::glimpse(data_set)


# readr::write_rds(data_set,"data/nasa-xco2.rds")











