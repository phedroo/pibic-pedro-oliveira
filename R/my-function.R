# Criando as funções básicas
get_geobr_pol <- function(object_geom, i) {
  object_geom$geom |> purrr::pluck(i) |> as.matrix()
}

get_geobr_biomes_pol <- function(i) {
  biomes$geom |> purrr::pluck(i) |> as.matrix()
}

get_geobr_conservation_pol <- function(i) {
  conservation$geom |> purrr::pluck(i) |> as.matrix()
}

get_geobr_indigenous_pol <- function(i) {
  indigenous$geom |> purrr::pluck(i) |> as.matrix()
}

# Carregando os polígonos
states <- geobr::read_state(showProgress = FALSE)
biomes <- geobr::read_biomes(showProgress = FALSE)
conservation <- geobr::read_conservation_units(showProgress = FALSE)
indigenous <- geobr::read_indigenous_land(showProgress = FALSE)
citys <- geobr::read_municipality(showProgress = FALSE)

cat("Polygons loaded [states, citysbiomes, conservarion and indigenous]\n")

# Criando lista de polígonos com todos os estados
abbrev_states <- states$abbrev_state
list_pol <- purrr::map(1:27, get_geobr_pol,object_geom=states)
names(list_pol) <- abbrev_states
cat("List of polygons loaded [list_pol]\n")


## Classificação de pertencimento de ponto em polígono
def_pol <- function(x, y, pol){
  as.logical(sp::point.in.polygon(point.x = x,
                                  point.y = y,
                                  pol.x = pol[,1],
                                  pol.y = pol[,2]))
}

### Função para buscar se um ponto pertentece a lista de polígonos
get_geobr_state <- function(x,y){
  x <- as.vector(x[1])
  y <- as.vector(y[1])
  resul <- "Other"
  lgv <- FALSE
  for(i in 1:27){
    lgv <- def_pol(x,y,list_pol[[i]])
    if(lgv){
      resul <- names(list_pol[i])
    }else{
      resul <- resul
    }
  }
  return(as.vector(resul))
}


get_geobr_city <- function(x, y, state){
  # Garantir que x, y e state são vetores de mesma dimensão
  if(length(x) != length(y) | length(x) != length(state)){
    stop("As dimensões de x, y e state devem ser iguais.")
  }
  resul <- vector("character", length(x))  # Vetor para armazenar os resultados
  for(i in seq_along(x)){
    xi <- x[i]
    yi <- y[i]
    mstate <- state[i]

    obj <- citys |>
      filter(abbrev_state == mstate)

    name_munis <- citys |>
      filter(abbrev_state == mstate) |>
      pull(name_muni)

    for(j in seq_along(name_munis)){
      pol <- obj$geom |> pluck(j) |> as.matrix()
      lgv <- def_pol(xi, yi, pol)
      if(lgv) {
        resul[i] <- name_munis[j]
        break  # Para de procurar assim que encontrar a cidade correspondente
      }
    }
  }
  return(resul)
}

####
names_biomes<- biomes |>
  dplyr::filter(name_biome!='Sistema Costeiro') |>
  dplyr::pull(name_biome)

list_pol_biomes <- purrr::map(1:6, get_geobr_biomes_pol)
names(list_pol_biomes) <- names_biomes

get_geobr_biomes <- function(x,y){
  x <- as.vector(x[1])
  y <- as.vector(y[1])
  resul <- "Other"
  lgv <- FALSE
  for(i in 1:6){
    lgv <- def_pol(x,y,list_pol_biomes[[i]])
    if(lgv){
      resul <- names(list_pol_biomes[i])
    }else{
      resul <- resul
    }
  }
  return(as.vector(resul))
}

###
list_pol_conservation <- purrr::map(1:1934, get_geobr_conservation_pol)
list_pol_indigenous <- purrr::map(1:615, get_geobr_indigenous_pol)
names(list_pol_biomes) <- names_biomes

get_geobr_conservation <- function(x,y){
  x <- as.vector(x[1])
  y <- as.vector(y[1])
  lgv <- FALSE
  for(i in 1:1934){
    lgv <- def_pol(x,y,list_pol_conservation[[i]])
    if(lgv) break
  }
  return(lgv)
}

get_geobr_indigenous <- function(x,y){
  x <- as.vector(x[1])
  y <- as.vector(y[1])
  lgv <- FALSE
  for(i in 1:615){
    lgv <- def_pol(x,y,list_pol_indigenous[[i]])
    if(lgv) break
  }
  return(lgv)
}


# Função para ler 01 arquivo csv
my_file_read <- function(sector_name){
  read.csv(sector_name) %>%
    select(!starts_with("other")) %>%
    mutate(directory = sector_name)
}

### função download precipitação
power_data_download <- function(lon,lat, startdate, enddate){
  df <- nasapower::get_power(
    community = 'ag',
    lonlat = c(lon,lat),
    pars = c('ALLSKY_SFC_SW_DWN','RH2M','T2M','PRECTOTCORR','WS2M','WD2M'),
    dates = c(startdate,enddate),
    temporal_api = 'daily'
  )
  write.csv(df,paste0('precipitation/data-raw/',lon,'_',lat,'.csv'))
}

### função para o download dos dados do BR no CT
download_arquivo <- function(url, dir){
  download.file(url, dir)
  return(dir)
}


# função para extrais o contorno do territorio nacional
get_contorno <- function(indice, lista){
  obj <- lista |> purrr::pluck(indice) |> as.matrix() |>
    as.data.frame()
  return(obj)
}

# função que busca a coordenada mais próxima de um ponto krigado no mapa do
# fogo
get_coord <- function(x, y, type= "Long"){
  df <- df_raster_aux |>
    mutate(distancia = sqrt((x-Long)^2 + (y-Lat)^2)) |>
    arrange(distancia) |>
    slice(1)
  if(type == "Long") return(as.numeric(df[,1]))
  if(type == "Lat") return(as.numeric(df[,2]))
  if(type == "Area") return(as.numeric(df[,3]))
  if(type == "distancia") return(as.numeric(df[,4]))
}

rds_reader <- function(path){
  readr::read_rds(path) |>
    mutate(
       path = stringr::str_remove(path,"output/maps-kgr/kgr-|\\.rds"),
       year = as.numeric(stringr::str_sub(path,-8,-5)),
       state = stringr::str_extract(path,"(?<=-)[^-]+(?=-)"),
       variable = stringr::str_extract(path, "^[^\\-]+")
    ) |>
    rename(
      value = var1.pred, value_std = var1.var
    ) |>
    select(-path) |>
    relocate(variable, state, year
    )
}


# get_coord_2 <- function(x, y, type= "Long"){
#   df <- ko_beta_aux |>
#     mutate(distancia = sqrt((x-X)^2 + (y-Y)^2)) |>
#     arrange(distancia) |>
#     slice(1)
#   if(type == "Long") return(as.numeric(df[,1]))
#   if(type == "Lat") return(as.numeric(df[,2]))
#   if(type == "Area") return(as.numeric(df[,3]))
#   if(type == "distancia") return(as.numeric(df[,4]))
# }


# generalização da função para todos os estados do país
get_pol_in_pol <- function(indice, lista, gradeado){
  poligono <- lista |> purrr::pluck(indice) |> as.matrix()
  flag <- def_pol(gradeado$X, gradeado$Y, poligono)
  return(flag)
}

# definição da equação de regressão linear e seus diagnósticos
linear_reg <- function(df, output="beta1"){
  # Modelo para cada pixel
  modelo <- lm(xco2_mean ~ dia, data=df)
  beta_1 <- c(summary(modelo)$coefficients[2])

  # Definindo o modelo
  if(output=="beta1"){
    return(beta_1*365) # <-------------------- BETA LINE POR 365
  }

  # Salvando o valor P
  if(output=="p_value"){
    if(is.nan(beta_1)){
      beta_1 <- 0
      p <- 1
    }else{
      p <- summary(modelo)$coefficients[2,4]
      if(is.nan(p)) p <- 1
    }
    return(p)
  }

  # Criando gráfico
  if(output=="plot"){
    plot <- df |>
      ggplot2::ggplot(ggplot2::aes(x=dia,y=xco2_mean)) +
      ggplot2::geom_point() +
      ggplot2::theme_bw()
    return(plot)
  }
  if(output=="hist"){
    hist <- df |>
      ggplot2::ggplot(ggplot2::aes(x=xco2_mean, y=..density..)) +
      ggplot2::geom_histogram(bins=10, color="black", fill="lightgray") +
      ggplot2::geom_density()+
      ggplot2::theme_bw()
    return(hist)
  }

  # Anomalia é o Xco2 do regional menos o Xco2 do pixel, melhor é o contrário.
  if(output == "partial"){
    partial <- df |>
      dplyr::summarise(xco2 = mean(xco2_mean), na.mr=TRUE) |>
      dplyr::pull(xco2)
    return(partial)
  }

  if(output == "n"){
    return(nrow(df))
  }
}

# definição da equação de regressão linear e seus diagnósticos
linear_reg_ch4 <- function(df, output="beta1"){
  # Modelo para cada pixel
  modelo <- lm(ch4_mean ~ year, data=df)
  beta_1 <- c(summary(modelo)$coefficients[2])

  # Definindo o modelo
  if(output=="beta1"){
    return(beta_1*365) # <-------------------- BETA LINE POR 365
  }

  # Salvando o valor P
  if(output=="p_value"){
    if(is.nan(beta_1)){
      beta_1 <- 0
      p <- 1
    }else{
      p <- summary(modelo)$coefficients[2,4]
      if(is.nan(p)) p <- 1
    }
    return(p)
  }

  if(output == "n"){
    return(nrow(df))
  }
}

get_tif <- function(file){
  df<-file |>
    raster() |>
    rasterToPoints() |>
    as.data.frame()
  names(df) <- c("x","y","Umidade")
  n_split <- lengths(stringr::str_split(file,"/"))
  nomes <- stringr::str_split(file,"/",simplify = TRUE)[,n_split] |>
    stringr::str_remove(".tif")
  df$ano <- nomes
  return(df)
}



# # Função para pegar os chutes iniciais do variograma
# get_psill <- function(vari){
#   Gamma <- vari$gamma
#   return(-min(Gamma)+median(Gamma))
# }
#
# get_nugget <- function(vari){
#   Gamma <- vari$gamma
#   return(min(Gamma))
# }
#
#
# get_range <- function(vari){
#   Gamma <- vari$gamma
#   Dist <- vari$dist
#   Dist2 <- Dist*Dist
#
#   reg <- lm(Gamma ~ Dist + Dist2)
#   reg_anova <- summary(reg)
#   c <- reg_anova$coefficients[1]
#   b <- reg_anova$coefficients[2]
#   a <- reg_anova$coefficients[3]
#
#   Xv <- - b/2/a
#   # plot(Gamma~Dist)
#   # curve(a*x^2 + b*x +c,add=TRUE)
#
#   if(Xv < 30 & Xv > 0 ){
#     return(Xv)
#   }else{
#     return(median(Dist))
#   }
# }

mapas_contorno1 <- read_state(code_state="MS")
mapas_contorno2 <- read_state(code_state="MT")
mapas_contorno3 <- read_state(code_state="MG")
mapas_contorno4 <- read_state(code_state="PA")
mapas_contorno5 <- read_state(code_state="GO")
