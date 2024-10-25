Variabilidade espaçotemporal da emissão de GEE e a pecuária Brasileira
================
Oliveira PHM; Panosso AR
2024-03-28

# [Pré-processamento Xco2](https://arpanosso.github.io/pibic-pedro-oliveira/Docs/pre-processamento-xco2.html)

# Análise Para XCO2

## Carregando Pacotes

``` r
# ### Pacotes necessários
library(tidyverse)
library(ggridges)
library(geobr)
source("r/my-function.R")
```

    ## Polygons loaded [states, citysbiomes, conservarion and indigenous]
    ## List of polygons loaded [list_pol]

## Filtrando a Base

``` r
### Base de dados inicial
data_set <- read_rds("data/nasa-xco2.rds") |> 
  filter(year >= 2015 & year <= 2020) |> 
  mutate(
    state = ifelse(state=="DF","GO",state)
  ) |> 
  select(-flag_nordeste, -flag_br)
glimpse(data_set)
```

    ## Rows: 1,135,303
    ## Columns: 12
    ## $ longitude         <dbl> -50.28787, -50.34569, -50.35614, -50.35908, -50.3602…
    ## $ latitude          <dbl> -12.87398, -12.67460, -12.65862, -12.62923, -12.6399…
    ## $ time              <dbl> 1420131440, 1420131444, 1420131445, 1420131445, 1420…
    ## $ date              <date> 2015-01-01, 2015-01-01, 2015-01-01, 2015-01-01, 201…
    ## $ year              <dbl> 2015, 2015, 2015, 2015, 2015, 2015, 2015, 2015, 2015…
    ## $ month             <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
    ## $ day               <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
    ## $ xco2              <dbl> 396.6765, 396.2824, 394.5461, 396.7095, 397.1642, 39…
    ## $ xco2_quality_flag <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    ## $ xco2_incerteza    <dbl> 0.5534429, 0.5050880, 0.6011925, 0.5258151, 0.549804…
    ## $ path              <chr> "oco2_LtCO2_150101_B11100Ar_230524221540s.nc4", "oco…
    ## $ state             <chr> "GO", "GO", "GO", "GO", "GO", "GO", "GO", "MT", "MT"…

## Análise de tendência dos dados de xCO2

``` r
data_set |> 
  sample_n(10000) |> 
  ggplot(aes(x=year, y=xco2)) +
  geom_point() +
  geom_point(shape=21,color="black",fill="gray") +
  geom_smooth(method = "lm") +
  ggpubr::stat_regline_equation(ggplot2::aes(
  label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~"))) +
  theme_bw()
```

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
mod_trend_xco2 <- lm(xco2~year, 
          data= data_set |> 
            drop_na())
```

``` r
# ## Avaliando o sinal de XCO2
#Carrega a base filtrada
estados <- c("MG","MT","MS","GO","PA")
data_set_me <- data_set |>
  filter( 
    state %in% estados
  )
# Resumo da base
glimpse(data_set_me)
```

    ## Rows: 415,559
    ## Columns: 12
    ## $ longitude         <dbl> -50.28787, -50.34569, -50.35614, -50.35908, -50.3602…
    ## $ latitude          <dbl> -12.87398, -12.67460, -12.65862, -12.62923, -12.6399…
    ## $ time              <dbl> 1420131440, 1420131444, 1420131445, 1420131445, 1420…
    ## $ date              <date> 2015-01-01, 2015-01-01, 2015-01-01, 2015-01-01, 201…
    ## $ year              <dbl> 2015, 2015, 2015, 2015, 2015, 2015, 2015, 2015, 2015…
    ## $ month             <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
    ## $ day               <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
    ## $ xco2              <dbl> 396.6765, 396.2824, 394.5461, 396.7095, 397.1642, 39…
    ## $ xco2_quality_flag <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    ## $ xco2_incerteza    <dbl> 0.5534429, 0.5050880, 0.6011925, 0.5258151, 0.549804…
    ## $ path              <chr> "oco2_LtCO2_150101_B11100Ar_230524221540s.nc4", "oco…
    ## $ state             <chr> "GO", "GO", "GO", "GO", "GO", "GO", "GO", "MT", "MT"…

``` r
#Gráficos
# Histogramas
data_set_me |>
  ggplot(aes(x=xco2)) +
  geom_histogram(color="black",fill="gray",
                 bins = 30) +
  facet_wrap(~year, scales = "free") +
  theme_bw()
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
data_set_me |>
  mutate(
  fct_year = fct_rev(as.factor(year)),
  ) |>
  ggplot(aes(y=fct_year)) +
  geom_density_ridges(rel_min_height = 0.03,
                      aes(x=xco2, fill=state),
                      alpha = .6, color = "black"
  ) +
  scale_fill_viridis_d() +
  theme_ridges() +
  theme(
    legend.position = "top"
  )
```

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
#Estatística Descritiva
data_set_me |>
  group_by(year,state) |>
  summarise(
    N = length(xco2),
    MIN = min(xco2),
    MEAN = mean(xco2),
    MEDIAN = median(xco2),
    MAX = max(xco2),
    VARIANCIA  = var(xco2),
    STD_DV = sd(xco2),
    CV = 100*STD_DV/MEAN,
    SKW = agricolae::skewness(xco2),
    KRT = agricolae::kurtosis(xco2),
  ) |>
  writexl::write_xlsx("output/estat-desc-xco2.xlsx")
```

## Análise Geoestatística

### PASSO 1

``` r
my_year = 2018
my_state = "MT"
# Criar o adensamento de pontos
x<-data_set_me$longitude
y<-data_set_me$latitude
dis <- 0.05 #Distância entre pontos
grid <- expand.grid(X=seq(min(x),max(x),dis), Y=seq(min(y),max(y),dis)) |>
  mutate(
    flag_my_state = my_state,
    flag_st = def_pol(X, Y, list_pol[[my_state]]),
    flag_df = def_pol(X, Y, list_pol[["DF"]]),
    flag = ifelse(flag_my_state == "GO", (flag_st|flag_df),flag_st)
  ) |>  filter(flag) |>
  dplyr::select(-starts_with("flag"))
sp::gridded(grid) = ~ X + Y
# plot(grid$X,grid$Y,col="red",pch=4)
# points(x,y)
```

### Agregação dos dados

``` r
dist_agg <- .15 #Distância entre pontos
grid_agg <- expand.grid(
  X=seq(min(x),max(x),dist_agg), 
  Y=seq(min(y),max(y),dist_agg)) |>
  mutate(
    flag_my_state = my_state,
    flag_st = def_pol(X, Y, list_pol[[my_state]]),
    flag_df = def_pol(X, Y, list_pol[["DF"]]),
    flag = ifelse(flag_my_state == "GO", (flag_st|flag_df),flag_st)
  ) |>  filter(flag) |>
  dplyr::select(-starts_with("flag"))

data_set_me |>
  filter(year == 2015,state == my_state) |> 
  ggplot(aes(longitude,latitude)) +
  geom_point() +
  geom_point(data=grid_agg, aes(X,Y))
```

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
# Isolando o banco de dados, pelo ano
data_set_aux  <- data_set_me |>
  filter(
    year == my_year,
    state == my_state) |>
  dplyr::select(longitude, latitude, xco2)

vct_xco2 <- vector();dist_xco2 <- vector();
lon_grid <- vector();lat_grid <- vector();
for(i in 1:nrow(data_set_aux)){
  d <- sqrt((data_set_aux$longitude[i] - grid_agg$X)^2 + 
              (data_set_aux$lat[i] - grid_agg$Y)^2
  )
  min_index <- order(d)[1]
  vct_xco2[i] <- data_set_aux$xco2[min_index]
  dist_xco2[i] <- d[order(d)[1]]
  lon_grid[i] <- grid_agg$X[min_index]
  lat_grid[i] <- grid_agg$Y[min_index]
}
data_set_aux$dist_xco2 <- dist_xco2
data_set_aux$xco2_new <- vct_xco2
data_set_aux$lon_grid <- lon_grid
data_set_aux$lat_grid <- lat_grid
data_set_aux |> 
  group_by(lon_grid,lat_grid) |> 
  summarise(
    xco2 = mean(xco2)
  ) |> 
  rename(longitude = lon_grid, latitude = lat_grid) -> data_set_aux
```

### PASSO 2 - Construção do Semivariograma Experimental

``` r
# Alteração no df
sp::coordinates(data_set_aux) = ~ longitude + latitude

# Fórmule é a variável que modelar, e o 1 da fórmula indica que ela
# não sofre transformação
form <- xco2 ~ 1
```

``` r
# Criando o Semivariograma Experimental.
vari_exp <- gstat::variogram(form, data = data_set_aux,
                      cressie = FALSE,
                      cutoff = 15, # distância máxima do semivariograma
                      width = .8) # distancia entre pontos
vari_exp  |>
  ggplot(aes(x=dist, y=gamma)) +
  geom_point() +
  labs(x="lag (º)",
       y=expression(paste(gamma,"(h)")))
```

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

### Passo 3 - Ajuste dos modelos

``` r
patamar=1.8
alcance=4
epepita=.5
modelo_1 <- gstat::fit.variogram(vari_exp,gstat::vgm(patamar,"Sph",alcance,epepita))
modelo_2 <- gstat::fit.variogram(vari_exp,gstat::vgm(patamar,"Exp",alcance,epepita))
modelo_3 <- gstat::fit.variogram(vari_exp,gstat::vgm(patamar,"Gau",alcance,epepita))
sqr.f1<-round(attr(modelo_1, "SSErr"),4); c01<-round(modelo_1$psill[[1]],4); c0_c11<-round(sum(modelo_1$psill),4);a1<-round(modelo_1$range[[2]],2)
sqr.f2<-round(attr(modelo_2, "SSErr"),4); c02<-round(modelo_2$psill[[1]],4); c0_c12<-round(sum(modelo_2$psill),4);a2<-round(3*modelo_2$range[[2]],2)
sqr.f3<-round(attr(modelo_3, "SSErr"),4); c03<-round(modelo_3$psill[[1]],4); c0_c13<-round(sum(modelo_3$psill),4);a3<-round(modelo_3$range[[2]]*(3^.5),2)

df_aux <- vari_exp |>
  mutate(
    gamma_m1 = ifelse(dist <= a1, c01 + (c0_c11-c01)*(3/2*(dist/a1)-1/2*(dist/a1)^3),c0_c11),
    gamma_m2 = c02 + (c0_c12-c02)*(1-exp(-3*(dist/a2))),
    gamma_m3 = c03 + (c0_c13-c03)*(1-exp(-3*(dist/a3)^2)),
    residuo_total = (gamma-mean(gamma))^2,
    residuo_mod_1 = (gamma - gamma_m1)^2,
    residuo_mod_2 = (gamma - gamma_m2)^2,
    residuo_mod_3 = (gamma - gamma_m3)^2
  ) |>
  summarise(
    r2_1=(sum(residuo_total) - sum(residuo_mod_1))/sum(residuo_total),
    r2_2=(sum(residuo_total) - sum(residuo_mod_2))/sum(residuo_total),
    r2_3=(sum(residuo_total) - sum(residuo_mod_3))/sum(residuo_total),
  )
r21<-as.vector(round(df_aux[1],4))
r22<-as.vector(round(df_aux[2],4))
r23<-as.vector(round(df_aux[3],4))

plot(vari_exp,
     model=modelo_1,
     col=1,pl=F,
     pch=16,
     cex=1.2,cex.main=7,
     ylab=list("Semivariância",cex=1.3),
     xlab=list("Distância de Separação h (m)",cex=1.3),
     main =paste("Esf(C0= ",c01,"; C0+C1= ",
                 c0_c11, "; a= ", a1,"; r2 = ",
                 r21,")",sep=""))
```

![](README_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
plot(vari_exp,model=modelo_2, col=1,pl=F,pch=16,cex=1.2,cex.main=7,ylab=list("Semivariância",cex=1.3),xlab=list("Distância de Separação h (m)",cex=1.3),main =paste("Exp(C0= ",c02,"; C0+C1= ", c0_c12, "; a= ", a2,"; r2 = ", r22,")",sep=""))
```

![](README_files/figure-gfm/unnamed-chunk-14-2.png)<!-- -->

``` r
plot(vari_exp,model=modelo_3, col=1,pl=F,pch=16,cex=1.2,cex.main=7,ylab=list("Semivariância",cex=1.3),xlab=list("Distância de Separação h (m)",cex=1.3),main =paste("Gau(C0= ",c03,"; C0+C1= ", c0_c13, "; a= ", a3,"; r2 = ", r23,")",sep=""))
```

![](README_files/figure-gfm/unnamed-chunk-14-3.png)<!-- -->

### Passo 4 - escolha do melhor modelo

``` r
# LOOCV - Leave one out cross validation
conjunto_validacao <- data_set_aux |>
  as_tibble() |>
  sample_n(50)
sp::coordinates(conjunto_validacao) = ~longitude + latitude
modelos<-list(modelo_1,modelo_2,modelo_3)
for(j in 1:3){
  est<-0
  for(i in 1:nrow(conjunto_validacao)){
    valid <- gstat::krige(formula=form, conjunto_validacao[-i,], conjunto_validacao, model=modelos[[j]])
    est[i]<-valid$var1.pred[i]
  }
  obs<-as.data.frame(conjunto_validacao)[,3]
  RMSE<-round((sum((obs-est)^2)/length(obs))^.5,3)
  mod<-lm(obs~est)
  b<-round(mod$coefficients[2],3)
  se<-round(summary(mod)$coefficients[4],3)
  r2<-round(summary(mod)$r.squared,3)
  a<-round(mod$coefficients[1],3)
  plot(est,obs,xlab="Estimado", ylab="Observado",pch=j,col="blue",
       main=paste("Modelo = ",modelos[[j]][2,1],"; Coef. Reg. = ", b, " (SE = ",se, ", r2 = ", r2,")\ny intersept = ",a,"RMSE = ",RMSE ))
  abline(lm(obs~est));
  abline(0,1,lty=3)
}
```

    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]

![](README_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]

![](README_files/figure-gfm/unnamed-chunk-15-2.png)<!-- -->

    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]

![](README_files/figure-gfm/unnamed-chunk-15-3.png)<!-- -->

### Passo 5 - Selecionado o melhor modelo, vamos guardá-lo

``` r
modelo <- modelo_3 ## sempre modificar
# Salvando os parâmetros dos melhores modelo
model <- modelo |> slice(2) |> pull(model)
rss <- round(attr(modelo, "SSErr"),4) 
c0 <- round(modelo$psill[[1]],4) 
c0_c1 <- round(sum(modelo$psill),4)
a <- ifelse(model == "Gau", round(modelo$range[[2]]*(3^.5),2),
            ifelse(model == "Exp",round(3*modelo$range[[2]],2),
            round(modelo$range[[2]],2)))
r2 <- ifelse(model == "Gau", r23,
            ifelse(model == "Exp",r22,
            r21)) |> pluck(1)
tibble(
  my_state, my_year, model, c0, c0_c1, a, rss, r2
) |> mutate(gde = c0/c0_c1, .after = "a") |>
  rename(state=my_state,year=my_year) |> 
  write_csv(paste0("output/best-fit/",my_state,"-",my_year,"-xco2.csv"))

ls_csv <- list.files("output/best-fit/",full.names = TRUE,pattern = "-xco2.csv")
map_df(ls_csv, read_csv) |> 
  writexl::write_xlsx("output/semivariogram-models-xco2.xlsx")
png(filename = paste0("output/semivariogram-img/semivar-xco2-",
                      my_state,"-",my_year,".png"),
    width = 800, height = 600)
plot(vari_exp,model=modelo,cex.lab=2, col=1,pl=F,pch=16,cex=2.2,ylab=list("Semivariância",cex=2.3),xlab=list("Distância de Separação h (m)",cex=2.3,cex.axis=4))
dev.off()
```

    ## png 
    ##   2

### Passo 6 - Krigagem Ordinária - interpolação em locais não amostrados

``` r
# ko_variavel <- gstat::krige(formula=form, data_set_aux, grid, model=modelo,
#                      block=c(0.1,0.1),
#                      nsim=0,
#                      na.action=na.pass,
#                      debug.level=-1
# )
```

## Passo 7 Visualização dos padrões espaciais e armazenamento dos dados e imagem.

``` r
# mapa <- as.tibble(ko_variavel) |>
#   ggplot(aes(x=X, y=Y)) +
#   geom_tile(aes(fill = var1.pred)) +
#   scale_fill_viridis_c() +
#   coord_equal() +
#   labs(x="Longitude",
#        y="Latitude",
#        fill="xco2") +
#   theme_bw()
# mapa
# ggsave(paste0("output/maps-kgr/kgr-xco2-",my_state,"-",my_year,".png"), plot = mapa, width = 10, height = 8, dpi = 300)
# df <- ko_variavel |>
#   as.tibble() |>
#   mutate(var1.var = sqrt(var1.var)) 
# write_rds(df,paste0("output/maps-kgr/kgr-xco2-",my_state,"-",my_year,".rds"))
```

# Análise Para XCH4

## Carregando Pacotes

## Filtrando a Base

``` r
### Base de dados inicial
data_set <- read_rds("data/gosat-xch4.rds") |> 
  filter(year >= 2015 & year <= 2020) |> 
  mutate(
    state = ifelse(state=="DF","GO",state)
  ) |> 
  select(-flag_nordeste, -flag_br)
glimpse(data_set)
```

    ## Rows: 52,796
    ## Columns: 12
    ## $ longitude         <dbl> -50.55648, -50.55732, -52.16461, -52.16571, -52.1661…
    ## $ latitude          <dbl> 0.6428701, 0.6460943, -6.9492564, -6.9488077, -6.945…
    ## $ time              <dbl> 1422806474, 1422806479, 1422806571, 1422806576, 1422…
    ## $ date              <date> 2015-02-01, 2015-02-01, 2015-02-01, 2015-02-01, 201…
    ## $ year              <dbl> 2015, 2015, 2015, 2015, 2015, 2015, 2015, 2015, 2015…
    ## $ month             <dbl> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2…
    ## $ day               <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
    ## $ xch4              <dbl> 1834.315, 1842.071, 1840.691, 1826.608, 1834.456, 18…
    ## $ xch4_quality_flag <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    ## $ xch4_incerteza    <dbl> 9.699241, 9.678691, 10.075063, 9.945088, 9.987506, 1…
    ## $ path              <chr> "UoL-GHG-L2-CH4-GOSAT-OCPR-20150201-fv9.0.nc", "UoL-…
    ## $ state             <chr> "AP", "AP", "PA", "PA", "PA", "MT", "MT", "MT", "MT"…

## Análise de tendência dos dados xCH4

``` r
data_set |> 
  sample_n(10000) |> 
  ggplot(aes(x=year, y=xch4)) +
  geom_point() +
  geom_point(shape=21,color="black",fill="gray") +
  geom_smooth(method = "lm") +
  ggpubr::stat_regline_equation(ggplot2::aes(
  label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~"))) +
  theme_bw()
```

![](README_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
mod_trend_xch4<- lm(xch4 ~ year, 
          data= data_set |> 
            drop_na())
```

``` r
# ## Avaliando o sinal de XCH4
#Carrega a base filtrada
data_set_me <- data_set |>
  filter( 
    state %in% estados
  )
# Resumo da base
glimpse(data_set_me)
```

    ## Rows: 24,344
    ## Columns: 12
    ## $ longitude         <dbl> -52.16461, -52.16571, -52.16616, -50.32345, -50.3242…
    ## $ latitude          <dbl> -6.949256, -6.948808, -6.945048, -9.851810, -9.84787…
    ## $ time              <dbl> 1422806571, 1422806576, 1422806580, 1422806626, 1422…
    ## $ date              <date> 2015-02-01, 2015-02-01, 2015-02-01, 2015-02-01, 201…
    ## $ year              <dbl> 2015, 2015, 2015, 2015, 2015, 2015, 2015, 2015, 2015…
    ## $ month             <dbl> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2…
    ## $ day               <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
    ## $ xch4              <dbl> 1840.691, 1826.608, 1834.456, 1839.762, 1828.547, 18…
    ## $ xch4_quality_flag <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    ## $ xch4_incerteza    <dbl> 10.075063, 9.945088, 9.987506, 10.116740, 10.031814,…
    ## $ path              <chr> "UoL-GHG-L2-CH4-GOSAT-OCPR-20150201-fv9.0.nc", "UoL-…
    ## $ state             <chr> "PA", "PA", "PA", "MT", "MT", "MT", "MT", "MT", "MT"…

``` r
data_set_me |>
  filter(year == 2015) |>
  sample_n(1000) |> 
  ggplot(aes(x = longitude, latitude)) +
  geom_point()
```

![](README_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

``` r
#Gráficos
# Histogramas
data_set_me |>
  ggplot(aes(x=xch4)) +
  geom_histogram(color="black",fill="gray",
                 bins = 30) +
  facet_wrap(~year, scales = "free") +
  theme_bw()
```

![](README_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

``` r
data_set_me |>
  mutate(
  fct_year = fct_rev(as.factor(year)),
  ) |>
  ggplot(aes(y=fct_year)) +
  geom_density_ridges(rel_min_height = 0.03,
                      aes(x=xch4, fill=state),
                      alpha = .6, color = "black"
  ) +
  scale_fill_viridis_d() +
  theme_ridges() +
  theme(
    legend.position = "top"
  )
```

![](README_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

``` r
#Estatística Descritiva
data_set_me |>
  group_by(year,state) |>
  summarise(
    N = length(xch4),
    MIN = min(xch4),
    MEAN = mean(xch4),
    MEDIAN = median(xch4),
    MAX = max(xch4),
    VARIANCIA  = var(xch4),
    STD_DV = sd(xch4),
    CV = 100*STD_DV/MEAN,
    SKW = agricolae::skewness(xch4),
    KRT = agricolae::kurtosis(xch4),
  ) |>
  writexl::write_xlsx("output/estat-desc-xch4.xlsx")
```

## Análise Geoestatística

### PASSO 1

``` r
my_year = 2020
my_state = "PA"
# Criar o adensamento de pontos
x<-data_set_me$longitude
y<-data_set_me$latitude
dis <- 0.05 #Distância entre pontos
grid <- expand.grid(X=seq(min(x),max(x),dis), Y=seq(min(y),max(y),dis)) |>
  mutate(
    flag_my_state = my_state,
    flag_st = def_pol(X, Y, list_pol[[my_state]]),
    flag_df = def_pol(X, Y, list_pol[["DF"]]),
    flag = ifelse(flag_my_state == "GO", (flag_st|flag_df),flag_st)
  ) |>  filter(flag) |>
  dplyr::select(-starts_with("flag"))
sp::gridded(grid) = ~ X + Y
# plot(grid$X,grid$Y,col="red",pch=4)
# points(x,y)
```

### Agregação dos dados

``` r
dist_agg <- .15 #Distância entre pontos
grid_agg <- expand.grid(
  X=seq(min(x),max(x),dist_agg), 
  Y=seq(min(y),max(y),dist_agg)) |>
  mutate(
    flag_my_state = my_state,
    flag_st = def_pol(X, Y, list_pol[[my_state]]),
    flag_df = def_pol(X, Y, list_pol[["DF"]]),
    flag = ifelse(flag_my_state == "GO", (flag_st|flag_df),flag_st)
  ) |>  filter(flag) |>
  dplyr::select(-starts_with("flag"))

data_set_me |>
  filter(year == 2015,state == my_state) |> 
  ggplot(aes(longitude,latitude)) +
  geom_point() +
  geom_point(data=grid_agg, aes(X,Y))
```

![](README_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

``` r
# Isolando o banco de dados, pelo ano
data_set_aux  <- data_set_me |>
  filter(
    year == my_year,
    state == my_state) |>
  dplyr::select(longitude, latitude, xch4)

# vct_xch4 <- vector();dist_xch4 <- vector();
# lon_grid <- vector();lat_grid <- vector();
# for(i in 1:nrow(data_set_aux)){
#   d <- sqrt((data_set_aux$longitude[i] - grid_agg$X)^2 + 
#               (data_set_aux$lat[i] - grid_agg$Y)^2
#   )
#   min_index <- order(d)[1]
#   vct_xch4[i] <- data_set_aux$xch4[min_index]
#   dist_xch4[i] <- d[order(d)[1]]
#   lon_grid[i] <- grid_agg$X[min_index]
#   lat_grid[i] <- grid_agg$Y[min_index]
# }
# data_set_aux$dist_xch4 <- dist_xch4
# data_set_aux$xch4_new <- vct_xch4
# data_set_aux$lon_grid <- lon_grid
# data_set_aux$lat_grid <- lat_grid
# data_set_aux |> 
#   group_by(lon_grid,lat_grid) |> 
#   summarise(
#     xch4 = mean(xch4)
#   ) |> 
#   rename(longitude = lon_grid, latitude = lat_grid) -> data_set_aux
```

### PASSO 2 - Construção do Semivariograma Experimental

``` r
# Alteração no df
sp::coordinates(data_set_aux) = ~ longitude + latitude

# Fórmule é a variável que modelar, e o 1 da fórmula indica que ela
# não sofre transformação
form <- xch4 ~ 1
```

``` r
# Criando o Semivariograma Experimental.
vari_exp <- gstat::variogram(form, data = data_set_aux,
                      cressie = FALSE,
                      cutoff = 8, # distância máxima do semivariograma
                      width = .7) # distancia entre pontos
vari_exp  |>
  ggplot(aes(x=dist, y=gamma)) +
  geom_point() +
  labs(x="lag (º)",
       y=expression(paste(gamma,"(h)")))
```

![](README_files/figure-gfm/unnamed-chunk-31-1.png)<!-- -->

### Passo 3 - Ajuste dos modelos

``` r
patamar=200
alcance=6
epepita=1
modelo_1 <- gstat::fit.variogram(vari_exp,gstat::vgm(patamar,"Sph",alcance,epepita))
modelo_2 <- gstat::fit.variogram(vari_exp,gstat::vgm(patamar,"Exp",alcance,epepita))
modelo_3 <- gstat::fit.variogram(vari_exp,gstat::vgm(patamar,"Gau",alcance,epepita))
sqr.f1<-round(attr(modelo_1, "SSErr"),4); c01<-round(modelo_1$psill[[1]],4); c0_c11<-round(sum(modelo_1$psill),4);a1<-round(modelo_1$range[[2]],2)
sqr.f2<-round(attr(modelo_2, "SSErr"),4); c02<-round(modelo_2$psill[[1]],4); c0_c12<-round(sum(modelo_2$psill),4);a2<-round(3*modelo_2$range[[2]],2)
sqr.f3<-round(attr(modelo_3, "SSErr"),4); c03<-round(modelo_3$psill[[1]],4); c0_c13<-round(sum(modelo_3$psill),4);a3<-round(modelo_3$range[[2]]*(3^.5),2)

df_aux <- vari_exp |>
  mutate(
    gamma_m1 = ifelse(dist <= a1, c01 + (c0_c11-c01)*(3/2*(dist/a1)-1/2*(dist/a1)^3),c0_c11),
    gamma_m2 = c02 + (c0_c12-c02)*(1-exp(-3*(dist/a2))),
    gamma_m3 = c03 + (c0_c13-c03)*(1-exp(-3*(dist/a3)^2)),
    residuo_total = (gamma-mean(gamma))^2,
    residuo_mod_1 = (gamma - gamma_m1)^2,
    residuo_mod_2 = (gamma - gamma_m2)^2,
    residuo_mod_3 = (gamma - gamma_m3)^2
  ) |>
  summarise(
    r2_1=(sum(residuo_total) - sum(residuo_mod_1))/sum(residuo_total),
    r2_2=(sum(residuo_total) - sum(residuo_mod_2))/sum(residuo_total),
    r2_3=(sum(residuo_total) - sum(residuo_mod_3))/sum(residuo_total),
  )
r21<-as.vector(round(df_aux[1],4))
r22<-as.vector(round(df_aux[2],4))
r23<-as.vector(round(df_aux[3],4))

plot(vari_exp,
     model=modelo_1,
     col=1,pl=F,
     pch=16,
     cex=1.2,cex.main=7,
     ylab=list("Semivariância",cex=1.3),
     xlab=list("Distância de Separação h (m)",cex=1.3),
     main =paste("Esf(C0= ",c01,"; C0+C1= ",
                 c0_c11, "; a= ", a1,"; r2 = ",
                 r21,")",sep=""))
```

![](README_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

``` r
plot(vari_exp,model=modelo_2, col=1,pl=F,pch=16,cex=1.2,cex.main=7,ylab=list("Semivariância",cex=1.3),xlab=list("Distância de Separação h (m)",cex=1.3),main =paste("Exp(C0= ",c02,"; C0+C1= ", c0_c12, "; a= ", a2,"; r2 = ", r22,")",sep=""))
```

![](README_files/figure-gfm/unnamed-chunk-32-2.png)<!-- -->

``` r
plot(vari_exp,model=modelo_3, col=1,pl=F,pch=16,cex=1.2,cex.main=7,ylab=list("Semivariância",cex=1.3),xlab=list("Distância de Separação h (m)",cex=1.3),main =paste("Gau(C0= ",c03,"; C0+C1= ", c0_c13, "; a= ", a3,"; r2 = ", r23,")",sep=""))
```

![](README_files/figure-gfm/unnamed-chunk-32-3.png)<!-- -->

### Passo 4 - escolha do melhor modelo

``` r
# LOOCV - Leave one out cross validation
conjunto_validacao <- data_set_aux |>
  as_tibble() |>
  sample_n(50)
sp::coordinates(conjunto_validacao) = ~longitude + latitude
modelos<-list(modelo_1,modelo_2,modelo_3)
for(j in 1:3){
  est<-0
  for(i in 1:nrow(conjunto_validacao)){
    valid <- gstat::krige(formula=form, conjunto_validacao[-i,], conjunto_validacao, model=modelos[[j]])
    est[i]<-valid$var1.pred[i]
  }
  obs<-as.data.frame(conjunto_validacao)[,3]
  RMSE<-round((sum((obs-est)^2)/length(obs))^.5,3)
  mod<-lm(obs~est)
  b<-round(mod$coefficients[2],3)
  se<-round(summary(mod)$coefficients[4],3)
  r2<-round(summary(mod)$r.squared,3)
  a<-round(mod$coefficients[1],3)
  plot(est,obs,xlab="Estimado", ylab="Observado",pch=j,col="blue",
       main=paste("Modelo = ",modelos[[j]][2,1],"; Coef. Reg. = ", b, " (SE = ",se, ", r2 = ", r2,")\ny intersept = ",a,"RMSE = ",RMSE ))
  abline(lm(obs~est));
  abline(0,1,lty=3)
}
```

    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]

![](README_files/figure-gfm/unnamed-chunk-33-1.png)<!-- -->

    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]

![](README_files/figure-gfm/unnamed-chunk-33-2.png)<!-- -->

    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]

![](README_files/figure-gfm/unnamed-chunk-33-3.png)<!-- -->

### Passo 5 - Selecionado o melhor modelo, vamos guardá-lo

``` r
modelo <- modelo_2 ## sempre modificar
# Salvando os parâmetros dos melhores modelo
model <- modelo |> slice(2) |> pull(model)
rss <- round(attr(modelo, "SSErr"),4) 
c0 <- round(modelo$psill[[1]],4) 
c0_c1 <- round(sum(modelo$psill),4)
a <- ifelse(model == "Gau", round(modelo$range[[2]]*(3^.5),2),
            ifelse(model == "Exp",round(3*modelo$range[[2]],2),
            round(modelo$range[[2]],2)))
r2 <- ifelse(model == "Gau", r23,
            ifelse(model == "Exp",r22,
            r21)) |> pluck(1)
tibble(
  my_state, my_year, model, c0, c0_c1, a, rss, r2
) |> mutate(gde = c0/c0_c1, .after = "a") |>
  rename(state=my_state,year=my_year) |> 
  write_csv(paste0("output/best-fit/",my_state,"-",my_year,"-xch4.csv"))

ls_csv <- list.files("output/best-fit/",full.names = TRUE,pattern = "-xch4.csv")
map_df(ls_csv, read_csv) |> 
  writexl::write_xlsx("output/semivariogram-models-xch4.xlsx")
png(filename = paste0("output/semivariogram-img/semivar-xch4-",
                      my_state,"-",my_year,".png"),
    width = 800, height = 600)
plot(vari_exp,model=modelo,cex.lab=2, col=1,pl=F,pch=16,cex=2.2,ylab=list("Semivariância",cex=2.3),xlab=list("Distância de Separação h (m)",cex=2.3,cex.axis=4))
dev.off()
```

    ## png 
    ##   2

### Passo 6 - Krigagem Ordinária - interpolação em locais não amostrados

``` r
# ko_variavel <- gstat::krige(formula=form, data_set_aux, grid, model=modelo,
#                      block=c(0.1,0.1),
#                      nsim=0,
#                      na.action=na.pass,
#                      debug.level=-1
# )
```

## Passo 7 Visualização dos padrões espaciais e armazenamento dos dados e imagem.

``` r
# mapa <- as.tibble(ko_variavel) |>
#   ggplot(aes(x=X, y=Y)) +
#   geom_tile(aes(fill = var1.pred)) +
#   scale_fill_viridis_c() +
#   coord_equal() +
#   labs(x="Longitude",
#        y="Latitude",
#        fill="xch4") +
#   theme_bw()
# mapa
# ggsave(paste0("output/maps-kgr/kgr-xch4-",my_state,"-",my_year,".png"), plot = mapa, width = 10, height = 8, dpi = 300)
# df <- ko_variavel |>
#   as.tibble() |>
#   mutate(var1.var = sqrt(var1.var)) 
# write_rds(df,paste0("output/maps-kgr/kgr-xch4-",my_state,"-",my_year,".rds"))
```

# Análise para SIF

## Carregando Pacotes

## Filtrando a Base

``` r
### Base de dados inicial
data_set <- read_rds("data/oco2-sif.rds") |> 
  filter(year >= 2015 & year <= 2020,
         sif >= -1 & sif <= 5) |> 
  mutate(
    state = ifelse(state=="DF","GO",state)
  ) |> 
  select(-flag_nordeste, -flag_br)
glimpse(data_set)
```

    ## Rows: 34,787
    ## Columns: 25
    ## $ longitude                                                     <dbl> -52.5, -…
    ## $ longitude_bnds                                                <chr> "-53.0:-…
    ## $ latitude                                                      <dbl> -4.5, -3…
    ## $ latitude_bnds                                                 <chr> "-5.0:-4…
    ## $ time_yyyymmddhhmmss                                           <dbl> 2.01501e…
    ## $ time_bnds_yyyymmddhhmmss                                      <chr> "2015010…
    ## $ altitude_km                                                   <dbl> 2955.042…
    ## $ alt_bnds_km                                                   <chr> "0.0:591…
    ## $ fluorescence_radiance_757nm_uncert_idp_ph_sec_1_m_2_sr_1_um_1 <dbl> 7.538797…
    ## $ fluorescence_radiance_757nm_idp_ph_sec_1_m_2_sr_1_um_1        <dbl> 2.977973…
    ## $ xco2_moles_mole_1                                             <dbl> 0.000393…
    ## $ aerosol_total_aod                                             <dbl> 0.157814…
    ## $ fluorescence_offset_relative_771nm_idp                        <dbl> 0.020921…
    ## $ fluorescence_at_reference_ph_sec_1_m_2_sr_1_um_1              <dbl> 4.183452…
    ## $ fluorescence_radiance_771nm_idp_ph_sec_1_m_2_sr_1_um_1        <dbl> 3.720872…
    ## $ fluorescence_offset_relative_757nm_idp                        <dbl> 0.016250…
    ## $ fluorescence_radiance_771nm_uncert_idp_ph_sec_1_m_2_sr_1_um_1 <dbl> 5.870890…
    ## $ XCO2                                                          <dbl> 385.1973…
    ## $ xco2                                                          <dbl> 393.0661…
    ## $ date                                                          <dttm> 2015-01…
    ## $ year                                                          <dbl> 2015, 20…
    ## $ month                                                         <dbl> 1, 1, 1,…
    ## $ day                                                           <int> 1, 1, 1,…
    ## $ sif                                                           <dbl> 1.110144…
    ## $ state                                                         <chr> "PA", "P…

``` r
# ## Avaliando o sinal de XCH4
#Carrega a base filtrada
data_set_me <- data_set |>
  filter( 
    state %in% estados
  )
# Resumo da base
glimpse(data_set_me)
```

    ## Rows: 14,294
    ## Columns: 25
    ## $ longitude                                                     <dbl> -52.5, -…
    ## $ longitude_bnds                                                <chr> "-53.0:-…
    ## $ latitude                                                      <dbl> -4.5, -3…
    ## $ latitude_bnds                                                 <chr> "-5.0:-4…
    ## $ time_yyyymmddhhmmss                                           <dbl> 2.01501e…
    ## $ time_bnds_yyyymmddhhmmss                                      <chr> "2015010…
    ## $ altitude_km                                                   <dbl> 2955.042…
    ## $ alt_bnds_km                                                   <chr> "0.0:591…
    ## $ fluorescence_radiance_757nm_uncert_idp_ph_sec_1_m_2_sr_1_um_1 <dbl> 7.538797…
    ## $ fluorescence_radiance_757nm_idp_ph_sec_1_m_2_sr_1_um_1        <dbl> 2.977973…
    ## $ xco2_moles_mole_1                                             <dbl> 0.000393…
    ## $ aerosol_total_aod                                             <dbl> 0.157814…
    ## $ fluorescence_offset_relative_771nm_idp                        <dbl> 0.020921…
    ## $ fluorescence_at_reference_ph_sec_1_m_2_sr_1_um_1              <dbl> 4.183452…
    ## $ fluorescence_radiance_771nm_idp_ph_sec_1_m_2_sr_1_um_1        <dbl> 3.720872…
    ## $ fluorescence_offset_relative_757nm_idp                        <dbl> 0.016250…
    ## $ fluorescence_radiance_771nm_uncert_idp_ph_sec_1_m_2_sr_1_um_1 <dbl> 5.870890…
    ## $ XCO2                                                          <dbl> 385.1973…
    ## $ xco2                                                          <dbl> 393.0661…
    ## $ date                                                          <dttm> 2015-01…
    ## $ year                                                          <dbl> 2015, 20…
    ## $ month                                                         <dbl> 1, 1, 1,…
    ## $ day                                                           <int> 1, 1, 1,…
    ## $ sif                                                           <dbl> 1.110144…
    ## $ state                                                         <chr> "PA", "P…

``` r
data_set_me |>
  filter(year == 2015) |>
  sample_n(1000) |> 
  ggplot(aes(x = longitude, latitude)) +
  geom_point()
```

![](README_files/figure-gfm/unnamed-chunk-39-1.png)<!-- -->

``` r
#Gráficos
# Histogramas
data_set_me |>
  ggplot(aes(x=sif)) +
  geom_histogram(color="black",fill="gray",
                 bins = 30) +
  facet_wrap(~year, scales = "free") +
  theme_bw()
```

![](README_files/figure-gfm/unnamed-chunk-40-1.png)<!-- -->

``` r
data_set_me |>
  mutate(
  fct_year = fct_rev(as.factor(year)),
  ) |>
  ggplot(aes(y=fct_year)) +
  geom_density_ridges(rel_min_height = 0.03,
                      aes(x=sif, fill=state),
                      alpha = .6, color = "black"
  ) +
  scale_fill_viridis_d() +
  theme_ridges() +
  theme(
    legend.position = "top"
  )
```

![](README_files/figure-gfm/unnamed-chunk-41-1.png)<!-- -->

``` r
#Estatística Descritiva
data_set_me |>
  group_by(year,state) |>
  summarise(
    N = length(sif),
    MIN = min(sif),
    MEAN = mean(sif),
    MEDIAN = median(sif),
    MAX = max(sif),
    VARIANCIA  = var(sif),
    STD_DV = sd(sif),
    CV = 100*STD_DV/MEAN,
    SKW = agricolae::skewness(sif),
    KRT = agricolae::kurtosis(sif),
  ) |>
  writexl::write_xlsx("output/estat-desc-sif.xlsx")
```

## Análise Geoestatística

### PASSO 1

``` r
# my_year = 2015
# my_state = "MT"
# Criar o adensamento de pontos
x<-data_set_me$longitude
y<-data_set_me$latitude
dis <- 0.05 #Distância entre pontos
grid <- expand.grid(X=seq(min(x),max(x),dis), Y=seq(min(y),max(y),dis)) |>
  mutate(
    flag_my_state = my_state,
    flag_st = def_pol(X, Y, list_pol[[my_state]]),
    flag_df = def_pol(X, Y, list_pol[["DF"]]),
    flag = ifelse(flag_my_state == "GO", (flag_st|flag_df),flag_st)
  ) |>  filter(flag) |>
  dplyr::select(-starts_with("flag"))
sp::gridded(grid) = ~ X + Y
# plot(grid$X,grid$Y,col="red",pch=4)
# points(x,y)
```

### Agregação dos dados

``` r
dist_agg <- .15 #Distância entre pontos
grid_agg <- expand.grid(
  X=seq(min(x),max(x),dist_agg), 
  Y=seq(min(y),max(y),dist_agg)) |>
  mutate(
    flag_my_state = my_state,
    flag_st = def_pol(X, Y, list_pol[[my_state]]),
    flag_df = def_pol(X, Y, list_pol[["DF"]]),
    flag = ifelse(flag_my_state == "GO", (flag_st|flag_df),flag_st)
  ) |>  filter(flag) |>
  dplyr::select(-starts_with("flag"))

data_set_me |>
  filter(year == 2015,state == my_state) |> 
  ggplot(aes(longitude,latitude)) +
  geom_point() +
  geom_point(data=grid_agg, aes(X,Y))
```

![](README_files/figure-gfm/unnamed-chunk-44-1.png)<!-- -->

``` r
# Isolando o banco de dados, pelo ano
data_set_aux  <- data_set_me |>
  filter(
    year == my_year,
    state == my_state) |>
  group_by(latitude, longitude) |> 
  summarise(
    sif = mean(sif, na.rm = TRUE)
  ) |> 
  dplyr::select(longitude, latitude, sif)

# vct_sif <- vector();dist_sif <- vector();
# lon_grid <- vector();lat_grid <- vector();
# for(i in 1:nrow(data_set_aux)){
#   d <- sqrt((data_set_aux$longitude[i] - grid_agg$X)^2 + 
#               (data_set_aux$lat[i] - grid_agg$Y)^2
#   )
#   min_index <- order(d)[1]
#   vct_sif[i] <- data_set_aux$sif[min_index]
#   dist_sif[i] <- d[order(d)[1]]
#   lon_grid[i] <- grid_agg$X[min_index]
#   lat_grid[i] <- grid_agg$Y[min_index]
# }
# data_set_aux$dist_sif <- dist_sif
# data_set_aux$sif_new <- vct_sif
# data_set_aux$lon_grid <- lon_grid
# data_set_aux$lat_grid <- lat_grid
# data_set_aux |> 
#   group_by(lon_grid,lat_grid) |> 
#   summarise(
#     sif = mean(sif)
#   ) |> 
#   rename(longitude = lon_grid, latitude = lat_grid) -> data_set_aux
```

### PASSO 2 - Construção do Semivariograma Experimental

``` r
# Alteração no df
sp::coordinates(data_set_aux) = ~ longitude + latitude

# Fórmule é a variável que modelar, e o 1 da fórmula indica que ela
# não sofre transformação
form <- sif ~ 1
```

``` r
# Criando o Semivariograma Experimental.
vari_exp <- gstat::variogram(form, data = data_set_aux,
                      cressie = FALSE,
                      cutoff = 6, # distância máxima do semivariograma
                      width = 0.02,
                      cloud = FALSE) # distancia entre ponts
vari_exp  |>
  ggplot(aes(x=dist, y=gamma)) +
  geom_point() +
  labs(x="lag (º)",
       y=expression(paste(gamma,"(h)")))
```

![](README_files/figure-gfm/unnamed-chunk-47-1.png)<!-- -->

### Passo 3 - Ajuste dos modelos

``` r
patamar=0.05
alcance=4
epepita=0.01
modelo_1 <- gstat::fit.variogram(vari_exp,gstat::vgm(patamar,"Sph",alcance,epepita))
modelo_2 <- gstat::fit.variogram(vari_exp,gstat::vgm(patamar,"Exp",alcance,epepita))
modelo_3 <- gstat::fit.variogram(vari_exp,gstat::vgm(patamar,"Gau",alcance,epepita))
sqr.f1<-round(attr(modelo_1, "SSErr"),4); c01<-round(modelo_1$psill[[1]],4); c0_c11<-round(sum(modelo_1$psill),4);a1<-round(modelo_1$range[[2]],2)
sqr.f2<-round(attr(modelo_2, "SSErr"),4); c02<-round(modelo_2$psill[[1]],4); c0_c12<-round(sum(modelo_2$psill),4);a2<-round(3*modelo_2$range[[2]],2)
sqr.f3<-round(attr(modelo_3, "SSErr"),4); c03<-round(modelo_3$psill[[1]],4); c0_c13<-round(sum(modelo_3$psill),4);a3<-round(modelo_3$range[[2]]*(3^.5),2)

df_aux <- vari_exp |>
  mutate(
    gamma_m1 = ifelse(dist <= a1, c01 + (c0_c11-c01)*(3/2*(dist/a1)-1/2*(dist/a1)^3),c0_c11),
    gamma_m2 = c02 + (c0_c12-c02)*(1-exp(-3*(dist/a2))),
    gamma_m3 = c03 + (c0_c13-c03)*(1-exp(-3*(dist/a3)^2)),
    residuo_total = (gamma-mean(gamma))^2,
    residuo_mod_1 = (gamma - gamma_m1)^2,
    residuo_mod_2 = (gamma - gamma_m2)^2,
    residuo_mod_3 = (gamma - gamma_m3)^2
  ) |>
  summarise(
    r2_1=(sum(residuo_total) - sum(residuo_mod_1))/sum(residuo_total),
    r2_2=(sum(residuo_total) - sum(residuo_mod_2))/sum(residuo_total),
    r2_3=(sum(residuo_total) - sum(residuo_mod_3))/sum(residuo_total),
  )
r21<-as.vector(round(df_aux[1],4))
r22<-as.vector(round(df_aux[2],4))
r23<-as.vector(round(df_aux[3],4))

plot(vari_exp,
     model=modelo_1,
     col=1,pl=F,
     pch=16,
     cex=1.2,cex.main=7,
     ylab=list("Semivariância",cex=1.3),
     xlab=list("Distância de Separação h (m)",cex=1.3),
     main =paste("Esf(C0= ",c01,"; C0+C1= ",
                 c0_c11, "; a= ", a1,"; r2 = ",
                 r21,")",sep=""))
```

![](README_files/figure-gfm/unnamed-chunk-48-1.png)<!-- -->

``` r
plot(vari_exp,model=modelo_2, col=1,pl=F,pch=16,cex=1.2,cex.main=7,ylab=list("Semivariância",cex=1.3),xlab=list("Distância de Separação h (m)",cex=1.3),main =paste("Exp(C0= ",c02,"; C0+C1= ", c0_c12, "; a= ", a2,"; r2 = ", r22,")",sep=""))
```

![](README_files/figure-gfm/unnamed-chunk-48-2.png)<!-- -->

``` r
plot(vari_exp,model=modelo_3, col=1,pl=F,pch=16,cex=1.2,cex.main=7,ylab=list("Semivariância",cex=1.3),xlab=list("Distância de Separação h (m)",cex=1.3),main =paste("Gau(C0= ",c03,"; C0+C1= ", c0_c13, "; a= ", a3,"; r2 = ", r23,")",sep=""))
```

![](README_files/figure-gfm/unnamed-chunk-48-3.png)<!-- -->

### Passo 4 - escolha do melhor modelo

``` r
# LOOCV - Leave one out cross validation
conjunto_validacao <- data_set_aux |>
  as_tibble() |>
  sample_n(10)
sp::coordinates(conjunto_validacao) = ~longitude + latitude
modelos<-list(modelo_1,modelo_2,modelo_3)
for(j in 1:3){
  est<-0
  for(i in 1:nrow(conjunto_validacao)){
    valid <- gstat::krige(formula=form, conjunto_validacao[-i,], conjunto_validacao, model=modelos[[j]])
    est[i]<-valid$var1.pred[i]
  }
  obs<-as.data.frame(conjunto_validacao)[,3]
  RMSE<-round((sum((obs-est)^2)/length(obs))^.5,3)
  mod<-lm(obs~est)
  b<-round(mod$coefficients[2],3)
  se<-round(summary(mod)$coefficients[4],3)
  r2<-round(summary(mod)$r.squared,3)
  a<-round(mod$coefficients[1],3)
  plot(est,obs,xlab="Estimado", ylab="Observado",pch=j,col="blue",
       main=paste("Modelo = ",modelos[[j]][2,1],"; Coef. Reg. = ", b, " (SE = ",se, ", r2 = ", r2,")\ny intersept = ",a,"RMSE = ",RMSE ))
  abline(lm(obs~est));
  abline(0,1,lty=3)
}
```

    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]

![](README_files/figure-gfm/unnamed-chunk-49-1.png)<!-- -->

    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]

![](README_files/figure-gfm/unnamed-chunk-49-2.png)<!-- -->

    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]
    ## [using ordinary kriging]

![](README_files/figure-gfm/unnamed-chunk-49-3.png)<!-- -->

### Passo 5 - Selecionado o melhor modelo, vamos guardá-lo

``` r
modelo <- modelo_3 ## sempre modificar
# Salvando os parâmetros dos melhores modelo
model <- modelo |> slice(2) |> pull(model)
rss <- round(attr(modelo, "SSErr"),4) 
c0 <- round(modelo$psill[[1]],4) 
c0_c1 <- round(sum(modelo$psill),4)
a <- ifelse(model == "Gau", round(modelo$range[[2]]*(3^.5),2),
            ifelse(model == "Exp",round(3*modelo$range[[2]],2),
            round(modelo$range[[2]],2)))
r2 <- ifelse(model == "Gau", r23,
            ifelse(model == "Exp",r22,
            r21)) |> pluck(1)
tibble(
  my_state, my_year, model, c0, c0_c1, a, rss, r2
) |> mutate(gde = c0/c0_c1, .after = "a") |>
  rename(state=my_state,year=my_year) |> 
  write_csv(paste0("output/best-fit/",my_state,"-",my_year,"-sif.csv"))

ls_csv <- list.files("output/best-fit/",full.names = TRUE,pattern = "-sif.csv")
map_df(ls_csv, read_csv) |> 
  writexl::write_xlsx("output/semivariogram-models-sif.xlsx")
png(filename = paste0("output/semivariogram-img/semivar-sif-",
                      my_state,"-",my_year,".png"),
    width = 800, height = 600)
plot(vari_exp,model=modelo,cex.lab=2, col=1,pl=F,pch=16,cex=2.2,ylab=list("Semivariância",cex=2.3),xlab=list("Distância de Separação h (m)",cex=2.3,cex.axis=4))
dev.off()
```

    ## png 
    ##   2

### Passo 6 - Krigagem Ordinária - interpolação em locais não amostrados

``` r
# ko_variavel <- gstat::krige(formula=form, data_set_aux, grid, model=modelo,
#                      block=c(0.1,0.1),
#                      nsim=0,
#                      na.action=na.pass,
#                      debug.level=-1
# )
```

## Passo 7 Visualização dos padrões espaciais e armazenamento dos dados e imagem.

``` r
# mapa <- as.tibble(ko_variavel) |>
#   ggplot(aes(x=X, y=Y)) +
#   geom_tile(aes(fill = var1.pred)) +
#   scale_fill_viridis_c() +
#   coord_equal() +
#   labs(x="Longitude",
#        y="Latitude",
#        fill="sif") +
#   theme_bw()
# mapa
# ggsave(paste0("output/maps-kgr/kgr-sif-",my_state,"-",my_year,".png"), plot = mapa, width = 10, height = 8, dpi = 300)
# df <- ko_variavel |>
#   as.tibble() |>
#   mutate(var1.var = sqrt(var1.var)) 
# write_rds(df,paste0("output/maps-kgr/kgr-sif-",my_state,"-",my_year,".rds"))
```

# Compilando os mapas krigados.

### Rodar somente após todos os mapas gerados

``` r
list_rds <- list.files("output/maps-kgr/",
           pattern = ".rds",
           full.names = TRUE)
kgr_maps <- map_df(list_rds, rds_reader)

kgr_maps_group <- kgr_maps |>
  group_by(variable, state, X, Y) |>
  summarise(
    n = n()
  ) |> select(-n)

kgr_maps |>
  group_by(variable, state) |>
  summarise(
    n = n()
  )
```

    ## # A tibble: 15 × 3
    ## # Groups:   variable [3]
    ##    variable state      n
    ##    <chr>    <chr>  <int>
    ##  1 sif      GO     70068
    ##  2 sif      MG    111828
    ##  3 sif      MS     72708
    ##  4 sif      MT    180720
    ##  5 sif      PA    241032
    ##  6 xch4     GO     70086
    ##  7 xch4     MG    113928
    ##  8 xch4     MS     69198
    ##  9 xch4     MT    178626
    ## 10 xch4     PA    244432
    ## 11 xco2     GO     70134
    ## 12 xco2     MG    120156
    ## 13 xco2     MS     74136
    ## 14 xco2     MT    180936
    ## 15 xco2     PA    246126

``` r
# state <- kgr_maps_group |> pull(state)
# x <- kgr_maps_group |> pull(X)
# y <- kgr_maps_group |> pull(Y)
# 
# v_city <- 0
# for(i in 1:nrow(kgr_maps_group)){
#   obj <- citys |>
#     filter(abbrev_state == state[i])
#   name_muni <- citys |>
#     filter(abbrev_state == state[i]) |>
#     pull(name_muni)
#   vlg <- FALSE
#   for(j in seq_along(name_muni)){
#     pol <- obj$geom |> pluck(j) |> as.matrix()
#     lgv <- def_pol(x[i],y[i],pol)
#     if(lgv) {
#       v_city[i] <- name_muni[j]
#       # print(paste0(v_city[i],"-",i," of 380,971"))
#       break
#     }
#   }
# }
# kgr_maps_group <- kgr_maps_group |>
#   add_column(
#     city = v_city
#   )

kgr_maps_group <- read_rds("data/kgr-maps-cities-groups.rds")

kgr_maps <- kgr_maps |>
  full_join(
    kgr_maps_group |>
      ungroup() |> 
      select(X, Y, city) ,by = c("X","Y"),
  )
```

# Retirando atendência

``` r
# readr::write_rds(kgr_maps_group,"data/kgr-maps-cities-groups.rds")
a_co2 <- mod_trend_xco2$coefficients[[1]]
b_co2 <- mod_trend_xco2$coefficients[[2]]
a_ch4 <- mod_trend_xch4$coefficients[[1]]
b_ch4 <- mod_trend_xch4$coefficients[[2]]

glimpse(kgr_maps)
```

    ## Rows: 2,325,318
    ## Columns: 8
    ## $ variable  <chr> "sif", "sif", "sif", "sif", "sif", "sif", "sif", "sif", "sif…
    ## $ state     <chr> "GO", "GO", "GO", "GO", "GO", "GO", "GO", "GO", "GO", "GO", …
    ## $ year      <dbl> 2015, 2015, 2015, 2015, 2015, 2015, 2015, 2015, 2015, 2015, …
    ## $ X         <dbl> -50.90, -50.85, -51.00, -50.95, -50.90, -51.00, -50.95, -50.…
    ## $ Y         <dbl> -19.45, -19.45, -19.40, -19.40, -19.40, -19.35, -19.35, -19.…
    ## $ value     <dbl> 0.5935694, 0.5899778, 0.5990964, 0.5951511, 0.5913777, 0.596…
    ## $ value_std <dbl> 0.1179227, 0.1181349, 0.1155714, 0.1157388, 0.1159298, 0.113…
    ## $ city      <chr> "Itajá", "Itajá", "Itajá", "Itajá", "Itajá", "Itajá", "Itajá…

``` r
kgr_maps_detrend <- kgr_maps |> 
  group_by(variable) |> 
  mutate(
    value_est = ifelse(variable =="xco2",a_co2+b_co2*year,
                       ifelse(variable =="xch4",a_ch4+b_ch4*year,value)),
    delta = ifelse(variable =="sif",value,value_est-value),
    value_detrend = ifelse(variable =="xco2",(a_co2-delta)-(mean(value)-a_co2),
                       ifelse(variable =="xch4",(a_ch4-delta)-(mean(value)-a_ch4),value)),
    value = value_detrend
  ) |> ungroup() |> 
  select(-value_est,-delta,-value_detrend)
```

## Cálculo do Beta para cada municípios

``` r
kgr_maps_beta <- kgr_maps_detrend |> 
  group_by(variable,state,city,year) |> 
  summarise(
    media = mean(value,na.rm = TRUE),
    desv_pad = mean(value_std)
  ) |> 
  group_by(variable,state,city) |>
  nest() |> 
  ungroup()

get_reg_lin <- function(df, par_return = "beta"){
  y <- df$year
  x <- df$media
  mod <- lm(y~x)
  value <- mod$coefficients[[2]]
  if(par_return == "beta") return(value)
}
kgr_maps_beta <- kgr_maps_beta |> 
  mutate(
    beta = map(data,get_reg_lin)
  ) |> 
  select(-data) |> 
  unnest()

glimpse(kgr_maps_beta)
```

    ## Rows: 4,299
    ## Columns: 4
    ## $ variable <chr> "sif", "sif", "sif", "sif", "sif", "sif", "sif", "sif", "sif"…
    ## $ state    <chr> "GO", "GO", "GO", "GO", "GO", "GO", "GO", "GO", "GO", "GO", "…
    ## $ city     <chr> "Abadia De Goiás", "Abadiânia", "Acreúna", "Adelândia", "Alex…
    ## $ beta     <dbl> 5.693984, 5.854820, 5.068227, 6.334005, 5.680407, 4.805122, 7…

## Mapeando o beta por cidades

``` r
city_kgr_beta <- left_join(citys |> filter(abbrev_state %in% estados),
           kgr_maps_beta |>
             pivot_wider(names_from = variable,
                         values_from = beta,names_prefix = "beta_") |> 
             rename(abbrev_state = state,name_muni =city),
           by=c("abbrev_state","name_muni"))
```

``` r
city_kgr_beta |>
     ggplot() +
     geom_sf(aes(fill=beta_sif), color="transparent",
             size=.05, show.legend = TRUE)  +
   geom_sf(data=mapas_contorno1, fill="transparent", color="red", size=3, show.legend = FALSE) +
   geom_sf(data=mapas_contorno2, fill="transparent", color="red", size=3, show.legend = FALSE) +
   geom_sf(data=mapas_contorno3, fill="transparent", color="red", size=3, show.legend = FALSE) +
   geom_sf(data=mapas_contorno4, fill="transparent", color="red", size=3, show.legend = FALSE) +
   geom_sf(data=mapas_contorno5, fill="transparent", color="red", size=3, show.legend = FALSE) +
     theme_bw() +
   theme(
     axis.text.x = element_text(size = rel(.9), color = "black"),
     axis.title.x = element_text(size = rel(1.1), color = "black"),
     axis.text.y = element_text(size = rel(.9), color = "black"),
     axis.title.y = element_text(size = rel(1.1), color = "black"),
     legend.text = element_text(size = rel(1), color = "black"),
     legend.title = element_text(face = 'bold', size = rel(1.2)),
     legend.position = c(1.29, .5)
     ) +
   labs(fill = 'Beta_sif',
         x = 'Longitude',
         y = 'Latitude') +
     scale_fill_viridis_c(option = "inferno")
```

![](README_files/figure-gfm/unnamed-chunk-57-1.png)<!-- -->

``` r
city_kgr_beta |>
     ggplot() +
     geom_sf(aes(fill=beta_xch4), color="transparent",
             size=.05, show.legend = TRUE)  +
   geom_sf(data=mapas_contorno1, fill="transparent", color="red", size=3, show.legend = FALSE) +
   geom_sf(data=mapas_contorno2, fill="transparent", color="red", size=3, show.legend = FALSE) +
   geom_sf(data=mapas_contorno3, fill="transparent", color="red", size=3, show.legend = FALSE) +
   geom_sf(data=mapas_contorno4, fill="transparent", color="red", size=3, show.legend = FALSE) +
   geom_sf(data=mapas_contorno5, fill="transparent", color="red", size=3, show.legend = FALSE) +
     theme_bw() +
   theme(
     axis.text.x = element_text(size = rel(.9), color = "black"),
     axis.title.x = element_text(size = rel(1.1), color = "black"),
     axis.text.y = element_text(size = rel(.9), color = "black"),
     axis.title.y = element_text(size = rel(1.1), color = "black"),
     legend.text = element_text(size = rel(1), color = "black"),
     legend.title = element_text(face = 'bold', size = rel(1.2)),
     legend.position = c(1.29, .5)
     ) +
   labs(fill = 'Beta_xch4',
         x = 'Longitude',
         y = 'Latitude') +
     scale_fill_viridis_c(option = "inferno")
```

![](README_files/figure-gfm/unnamed-chunk-58-1.png)<!-- -->

``` r
city_kgr_beta |>
     ggplot() +
     geom_sf(aes(fill=beta_xco2), color="transparent",
             size=.05, show.legend = TRUE)  +
   geom_sf(data=mapas_contorno1, fill="transparent", color="red", size=3, show.legend = FALSE) +
   geom_sf(data=mapas_contorno2, fill="transparent", color="red", size=3, show.legend = FALSE) +
   geom_sf(data=mapas_contorno3, fill="transparent", color="red", size=3, show.legend = FALSE) +
   geom_sf(data=mapas_contorno4, fill="transparent", color="red", size=3, show.legend = FALSE) +
   geom_sf(data=mapas_contorno5, fill="transparent", color="red", size=3, show.legend = FALSE) +
     theme_bw() +
   theme(
     axis.text.x = element_text(size = rel(.9), color = "black"),
     axis.title.x = element_text(size = rel(1.1), color = "black"),
     axis.text.y = element_text(size = rel(.9), color = "black"),
     axis.title.y = element_text(size = rel(1.1), color = "black"),
     legend.text = element_text(size = rel(1), color = "black"),
     legend.title = element_text(face = 'bold', size = rel(1.2)),
     legend.position = c(1.29, .5)
     ) +
   labs(fill = 'Beta_xco2',
         x = 'Longitude',
         y = 'Latitude') +
     scale_fill_viridis_c(option = "inferno")
```

![](README_files/figure-gfm/unnamed-chunk-59-1.png)<!-- -->

## Inspeção por estado

``` r
variables <- c("beta_sif","beta_xch4","beta_xco2")
my_plot_map_beta <- function(.estado, .variable){
  city_kgr_beta |>
    filter(abbrev_state == .estado) |> 
    ggplot() +
     geom_sf(aes_string(fill=.variable), color="black",
              size=.05, show.legend = TRUE)  +
    theme_bw() +
    theme(
      axis.text.x = element_text(size = rel(.9), color = "black"),
      axis.title.x = element_text(size = rel(1.1), color = "black"),
      axis.text.y = element_text(size = rel(.9), color = "black"),
      axis.title.y = element_text(size = rel(1.1), color = "black"),
      legend.text = element_text(size = rel(1), color = "black"),
      legend.title = element_text(face = 'bold', size = rel(1.2)),
      legend.position = c(1.29, .5)
    ) +
     labs(fill =.variable,
          x = 'Longitude',
          y = 'Latitude') +
    scale_fill_gradient(low="yellow",high = "red")
}
my_plot_map_beta("PA", "beta_xch4")
```

![](README_files/figure-gfm/unnamed-chunk-60-1.png)<!-- -->

``` r
map(estados,my_plot_map_beta,.variable="beta_xco2")
```

    ## [[1]]

![](README_files/figure-gfm/unnamed-chunk-60-2.png)<!-- -->

    ## 
    ## [[2]]

![](README_files/figure-gfm/unnamed-chunk-60-3.png)<!-- -->

    ## 
    ## [[3]]

![](README_files/figure-gfm/unnamed-chunk-60-4.png)<!-- -->

    ## 
    ## [[4]]

![](README_files/figure-gfm/unnamed-chunk-60-5.png)<!-- -->

    ## 
    ## [[5]]

![](README_files/figure-gfm/unnamed-chunk-60-6.png)<!-- -->

``` r
map(estados,my_plot_map_beta,.variable="beta_xch4")
```

    ## [[1]]

![](README_files/figure-gfm/unnamed-chunk-60-7.png)<!-- -->

    ## 
    ## [[2]]

![](README_files/figure-gfm/unnamed-chunk-60-8.png)<!-- -->

    ## 
    ## [[3]]

![](README_files/figure-gfm/unnamed-chunk-60-9.png)<!-- -->

    ## 
    ## [[4]]

![](README_files/figure-gfm/unnamed-chunk-60-10.png)<!-- -->

    ## 
    ## [[5]]

![](README_files/figure-gfm/unnamed-chunk-60-11.png)<!-- -->

``` r
map(estados,my_plot_map_beta,.variable="beta_sif")
```

    ## [[1]]

![](README_files/figure-gfm/unnamed-chunk-60-12.png)<!-- -->

    ## 
    ## [[2]]

![](README_files/figure-gfm/unnamed-chunk-60-13.png)<!-- -->

    ## 
    ## [[3]]

![](README_files/figure-gfm/unnamed-chunk-60-14.png)<!-- -->

    ## 
    ## [[4]]

![](README_files/figure-gfm/unnamed-chunk-60-15.png)<!-- -->

    ## 
    ## [[5]]

![](README_files/figure-gfm/unnamed-chunk-60-16.png)<!-- --> \#
Incorporação dos dados do CT

## IMPORTANDO A BASE DE DADOS

``` r
brazil_ids <- read_rds("data/df_nome.rds")
nomes_uf <- c(brazil_ids$nome_uf |> unique(),"Brazil")
emissions_sources <- read_rds('data/emissions_sources.rds')
```

``` r
city_kgr_beta_emission <- city_kgr_beta |> 
  left_join(
    emissions_sources |>
      filter(year == 2022,                 #%in% 2015:2022
             sigla_uf %in% estados,
             !source_name %in% nomes_uf,
             str_detect(activity_units, 'animal'),
             gas == 'co2e_100yr') |>
      group_by(city_ref, sigla_uf)  |>
      summarise(emissions_quantity =
                  sum(emissions_quantity, na.rm = TRUE)) |>
      rename(abbrev_state = sigla_uf,name_muni =city_ref) |> 
      ungroup(),
    by=c("name_muni","abbrev_state")
  )
```

## CARACTERIZANDO MUNICÍPIO

``` r
cities <- citys

# DADOS IBGE:
# Quantidade de Cabeças de MS: 18433728
# Quantidade de Cabeças de MT: 34246313
# Quantidade de Cabeças de MG: 22993105
# Quantidade de Cabeças de GO: 24410182
# Quantidade de Cabeças de PA: 24791060

# Quantidade de municípios de MS: 79
# Quantidade de municípios de MT: 141
# Quantidade de municípios de MG: 853
# Quantidade de municípios de GO: 246
# Quantidade de municípios de PA: 144

estado <- 'GO'
quant_head <- 24410182

emissions_sources |>
  filter(year == 2022,
         str_detect(activity_units, 'animal'),
         # sector_name == 'agriculture',
         !source_name %in% nomes_uf,
         sigla_uf == estado,
         gas == 'co2e_100yr') |>
  group_by(sigla_uf) |>                  #!!
  summarise(
    emission_total = sum(emissions_quantity)/1e6,
    mean_head = sum(emissions_quantity)/quant_head
    ) |>
  arrange(- emission_total)
```

    ## # A tibble: 1 × 3
    ##   sigla_uf emission_total mean_head
    ##   <chr>             <dbl>     <dbl>
    ## 1 GO                 36.9      1.51

## CRIANDO TEMA GRAFICO

``` r
my_theme <- theme(
       # axis.text.x = element_text(size = rel(1.25)),
        axis.title.x = element_text(size = rel(1.4)),
       # axis.text.y = element_text(size = rel(1.3)),
        axis.title.y = element_text(size = rel(1.4)),
        legend.text = element_text(size = rel(0.9)),
       # legend.title = element_text(size = rel(1.7)),
       title = element_text(face = 'bold'),
       legend.position = "top",
       legend.background = element_rect(fill = "#fffff0", color = "black"))

my_theme_add <- function(.my_theme){
 theme(
       # axis.text.x = element_text(size = rel(1.25)),
        axis.title.x = element_text(size = rel(1.4)),
       # axis.text.y = element_text(size = rel(1.3)),
        axis.title.y = element_text(size = rel(1.4)),
        legend.text = element_text(size = rel(0.9)),
       # legend.title = element_text(size = rel(1.7)),
       title = element_text(face = 'bold'),
       legend.position = "top",
       legend.background = element_rect(fill = "transparent", color = "black"))
}
```

## MAPEAR

### CONTRUINDO MAPA COM CLASSES

``` r
#my_state <- "MS" ### <-
my_plot_map <- function(.estados){
  citys |>
  select(abbrev_state) |>
  filter(
    abbrev_state == .estados) |>
  ggplot() +
  geom_sf(fill="white", color="black",
          size=.15, show.legend = F) +
  geom_point(data = emissions_sources |>
               filter(year == 2022, #>2014 & year <2023
                      sigla_uf == .estados,
                      str_detect(activity_units, 'animal'),
                      gas == 'co2e_100yr') |>
               mutate(
                 classe_emissao = case_when(
                   emissions_quantity <0.1e6 ~ '< 0.1 Mton',
                   emissions_quantity <0.4e6 ~ '< 0.4 Mton',
                   emissions_quantity <1e6 ~ '< 1 Mton',
                   emissions_quantity >=1e6 ~ '>= 1 Mton'
                 )
               ),
             size = 1.5,
             aes(lon,lat, col = classe_emissao)) +
    theme_bw() +
  theme(
    axis.text.x = element_text(size = rel(1), color = "#222222"),
    axis.title.x = element_text(size = rel(1.3), color = "#222222"),
    axis.text.y = element_text(size = rel(1), color = "#222222"),
    axis.title.y = element_text(size = rel(1.3), color = "#222222"),
    legend.text = element_text(size = rel(1.3), color = "#222222"),
    legend.title = element_text(size = rel(1.5)),
    ) +
   labs(col = 'CO'[2]~'eq emission',
        x = 'Longitude',
        y = 'Latitude'
     #title = paste0("CO2e emission for", my_state),
        # caption = 'Data Source: Climate TRACE',
     )
}

map(estados,my_plot_map)
```

    ## [[1]]

![](README_files/figure-gfm/unnamed-chunk-65-1.png)<!-- -->

    ## 
    ## [[2]]

![](README_files/figure-gfm/unnamed-chunk-65-2.png)<!-- -->

    ## 
    ## [[3]]

![](README_files/figure-gfm/unnamed-chunk-65-3.png)<!-- -->

    ## 
    ## [[4]]

![](README_files/figure-gfm/unnamed-chunk-65-4.png)<!-- -->

    ## 
    ## [[5]]

![](README_files/figure-gfm/unnamed-chunk-65-5.png)<!-- -->

``` r
# ggsave('GO.png', dpi = 3000, width = 9, height = 5.5)
```

``` r
# estado <- "MG" ### <-
# estados <- c("MS", "PA", "GO", "MT", "MG")
#
# my_plot_map_join <- function(.estados){
#   left_join(city |> filter(abbrev_state == .estados),
#           emissions_sources |>
#             filter(year == 2022, #>2014 & year <2023
#                    sigla_uf == .estados,
#                    !source_name %in% nomes_uf,
#                    str_detect(activity_units, 'animal'),            #filtering the four subsectors for cattle
#                    gas == 'co2e_100yr') |>
#             group_by(city_ref, sigla_uf)  |>
#             summarise(emissions_quantity =
#                         sum(emissions_quantity, na.rm = TRUE)) |>
#             rename(name_muni = city_ref) ,
#           by="name_muni") |>
#   mutate(emissions_quantity = replace_na(emissions_quantity,0)) |>
#   mutate(
#     classe_emissao = case_when(
#       emissions_quantity <0.1e6 ~ '< 0.1 Mton',
#       emissions_quantity <0.4e6 ~ '< 0.4 Mton',
#       emissions_quantity <0.7e6 ~ '< 0.7 Mton',
#       emissions_quantity >=1 ~ '>= 1 Mton'
#     )
#   ) |>
#     ggplot() +
#     geom_sf(aes(fill=classe_emissao), color="black",
#             size=.15, show.legend = TRUE)  +
#     theme_bw() +
#   theme(
#     axis.text.x = element_text(size = rel(1), color = "#222222"),
#     axis.title.x = element_text(size = rel(1.3), color = "#222222"),
#     axis.text.y = element_text(size = rel(1), color = "#222222"),
#     axis.title.y = element_text(size = rel(1.3), color = "#222222"),
#     legend.text = element_text(size = rel(1.3), color = "#222222"),
#     legend.title = element_text(size = rel(1.5)),
#     ) +
#    labs(fill = 'CO'[2]'e emission',
#         x = 'Longitude',
#         y = 'Latitude'
#      #title = paste0("CO2e emission for", my_state),
#         # caption = 'Data Source: Climate TRACE',
#      ) +
#     scale_fill_viridis_d()
# }
# map(estados,my_plot_map_join)

# ggsave('MGg_col.png', dpi = 2000, width = 9, height = 5.5)


#Gerando os mapas em uma única figura



 left_join(citys |> filter(abbrev_state %in% estados),
           emissions_sources |>
             filter(year == 2022,                 #%in% 2015:2022
                    sigla_uf %in% estados,
                    !source_name %in% nomes_uf,
                    str_detect(activity_units, 'animal'),            #filtering the four subsectors for cattle
                    gas == 'co2e_100yr') |>
             group_by(city_ref, sigla_uf)  |>
             summarise(emissions_quantity =
                         sum(emissions_quantity, na.rm = TRUE)) |>
             rename(name_muni = city_ref),
           by="name_muni") |>
   mutate(emissions_quantity = replace_na(emissions_quantity, 0)) |>
   mutate(
     classe_emissao = case_when(
       emissions_quantity <.1e6 ~ ' <  0,1 Mton',
       emissions_quantity <.5e6 ~ ' <  0,5 Mton',
       emissions_quantity < 1e6 ~ ' <  1,0 Mton',
       emissions_quantity >=1e6 ~ '>= 1,0 Mton'
     )
   ) |>
     ggplot() +
     geom_sf(aes(fill=classe_emissao), color="black",
             size=.05, show.legend = TRUE)  +
   geom_sf(data=mapas_contorno1, fill="transparent", color="red", size=3, show.legend = FALSE) +
   geom_sf(data=mapas_contorno2, fill="transparent", color="red", size=3, show.legend = FALSE) +
   geom_sf(data=mapas_contorno3, fill="transparent", color="red", size=3, show.legend = FALSE) +
   geom_sf(data=mapas_contorno4, fill="transparent", color="red", size=3, show.legend = FALSE) +
   geom_sf(data=mapas_contorno5, fill="transparent", color="red", size=3, show.legend = FALSE) +
     theme_bw() +
   theme(
     axis.text.x = element_text(size = rel(.9), color = "black"),
     axis.title.x = element_text(size = rel(1.1), color = "black"),
     axis.text.y = element_text(size = rel(.9), color = "black"),
     axis.title.y = element_text(size = rel(1.1), color = "black"),
     legend.text = element_text(size = rel(1), color = "black"),
     legend.title = element_text(face = 'bold', size = rel(1.2)),
     legend.position = c(1.29, .5)
     ) +
   labs(fill = 'Classe de emissão',
         x = 'Longitude',
         y = 'Latitude') +
     scale_fill_viridis_d()
```

![](README_files/figure-gfm/unnamed-chunk-66-1.png)<!-- -->

``` r
 # ggsave('Maps_states_red.png')
 ggsave('mapa_nova_classe.png')
```

## Análise de correlação

``` r
my_corrplot <- function(.estado){
  pl<- city_kgr_beta_emission |> 
    as_data_frame() |>
    filter(abbrev_state == .estado) |> 
    select(beta_sif:emissions_quantity) |> 
    relocate(emissions_quantity) |> 
    drop_na() |> 
    cor(method = "spearman") |> 
    corrplot::corrplot(method = "ellipse",type = "upper" )
  print(pl)
}
map(estados,my_corrplot)
```

![](README_files/figure-gfm/unnamed-chunk-67-1.png)<!-- -->

    ## $corr
    ##                    emissions_quantity    beta_sif  beta_xch4  beta_xco2
    ## emissions_quantity         1.00000000  0.09228143  0.1042044  0.1116891
    ## beta_sif                   0.09228143  1.00000000  0.4444834 -0.2416790
    ## beta_xch4                  0.10420437  0.44448337  1.0000000 -0.3589533
    ## beta_xco2                  0.11168911 -0.24167901 -0.3589533  1.0000000
    ## 
    ## $corrPos
    ##                 xName              yName x y        corr
    ## 1  emissions_quantity emissions_quantity 1 4  1.00000000
    ## 2            beta_sif emissions_quantity 2 4  0.09228143
    ## 3            beta_sif           beta_sif 2 3  1.00000000
    ## 4           beta_xch4 emissions_quantity 3 4  0.10420437
    ## 5           beta_xch4           beta_sif 3 3  0.44448337
    ## 6           beta_xch4          beta_xch4 3 2  1.00000000
    ## 7           beta_xco2 emissions_quantity 4 4  0.11168911
    ## 8           beta_xco2           beta_sif 4 3 -0.24167901
    ## 9           beta_xco2          beta_xch4 4 2 -0.35895333
    ## 10          beta_xco2          beta_xco2 4 1  1.00000000
    ## 
    ## $arg
    ## $arg$type
    ## [1] "upper"

![](README_files/figure-gfm/unnamed-chunk-67-2.png)<!-- -->

    ## $corr
    ##                    emissions_quantity    beta_sif  beta_xch4   beta_xco2
    ## emissions_quantity         1.00000000 -0.08791411 -0.1354180 -0.08506265
    ## beta_sif                  -0.08791411  1.00000000  0.2824124  0.04865846
    ## beta_xch4                 -0.13541799  0.28241237  1.0000000  0.11557149
    ## beta_xco2                 -0.08506265  0.04865846  0.1155715  1.00000000
    ## 
    ## $corrPos
    ##                 xName              yName x y        corr
    ## 1  emissions_quantity emissions_quantity 1 4  1.00000000
    ## 2            beta_sif emissions_quantity 2 4 -0.08791411
    ## 3            beta_sif           beta_sif 2 3  1.00000000
    ## 4           beta_xch4 emissions_quantity 3 4 -0.13541799
    ## 5           beta_xch4           beta_sif 3 3  0.28241237
    ## 6           beta_xch4          beta_xch4 3 2  1.00000000
    ## 7           beta_xco2 emissions_quantity 4 4 -0.08506265
    ## 8           beta_xco2           beta_sif 4 3  0.04865846
    ## 9           beta_xco2          beta_xch4 4 2  0.11557149
    ## 10          beta_xco2          beta_xco2 4 1  1.00000000
    ## 
    ## $arg
    ## $arg$type
    ## [1] "upper"

![](README_files/figure-gfm/unnamed-chunk-67-3.png)<!-- -->

    ## $corr
    ##                    emissions_quantity    beta_sif  beta_xch4   beta_xco2
    ## emissions_quantity          1.0000000 -0.10998852 -0.5746164  0.13324288
    ## beta_sif                   -0.1099885  1.00000000  0.1056883  0.06848972
    ## beta_xch4                  -0.5746164  0.10568834  1.0000000 -0.18835195
    ## beta_xco2                   0.1332429  0.06848972 -0.1883519  1.00000000
    ## 
    ## $corrPos
    ##                 xName              yName x y        corr
    ## 1  emissions_quantity emissions_quantity 1 4  1.00000000
    ## 2            beta_sif emissions_quantity 2 4 -0.10998852
    ## 3            beta_sif           beta_sif 2 3  1.00000000
    ## 4           beta_xch4 emissions_quantity 3 4 -0.57461643
    ## 5           beta_xch4           beta_sif 3 3  0.10568834
    ## 6           beta_xch4          beta_xch4 3 2  1.00000000
    ## 7           beta_xco2 emissions_quantity 4 4  0.13324288
    ## 8           beta_xco2           beta_sif 4 3  0.06848972
    ## 9           beta_xco2          beta_xch4 4 2 -0.18835195
    ## 10          beta_xco2          beta_xco2 4 1  1.00000000
    ## 
    ## $arg
    ## $arg$type
    ## [1] "upper"

![](README_files/figure-gfm/unnamed-chunk-67-4.png)<!-- -->

    ## $corr
    ##                    emissions_quantity    beta_sif   beta_xch4   beta_xco2
    ## emissions_quantity         1.00000000 -0.12242464 -0.15930352  0.01000652
    ## beta_sif                  -0.12242464  1.00000000  0.04591749 -0.14554713
    ## beta_xch4                 -0.15930352  0.04591749  1.00000000 -0.08746528
    ## beta_xco2                  0.01000652 -0.14554713 -0.08746528  1.00000000
    ## 
    ## $corrPos
    ##                 xName              yName x y        corr
    ## 1  emissions_quantity emissions_quantity 1 4  1.00000000
    ## 2            beta_sif emissions_quantity 2 4 -0.12242464
    ## 3            beta_sif           beta_sif 2 3  1.00000000
    ## 4           beta_xch4 emissions_quantity 3 4 -0.15930352
    ## 5           beta_xch4           beta_sif 3 3  0.04591749
    ## 6           beta_xch4          beta_xch4 3 2  1.00000000
    ## 7           beta_xco2 emissions_quantity 4 4  0.01000652
    ## 8           beta_xco2           beta_sif 4 3 -0.14554713
    ## 9           beta_xco2          beta_xch4 4 2 -0.08746528
    ## 10          beta_xco2          beta_xco2 4 1  1.00000000
    ## 
    ## $arg
    ## $arg$type
    ## [1] "upper"

![](README_files/figure-gfm/unnamed-chunk-67-5.png)<!-- -->

    ## $corr
    ##                    emissions_quantity   beta_sif   beta_xch4   beta_xco2
    ## emissions_quantity         1.00000000 -0.2509624  0.34834898 -0.04646986
    ## beta_sif                  -0.25096240  1.0000000 -0.24474686  0.19587754
    ## beta_xch4                  0.34834898 -0.2447469  1.00000000  0.06893047
    ## beta_xco2                 -0.04646986  0.1958775  0.06893047  1.00000000
    ## 
    ## $corrPos
    ##                 xName              yName x y        corr
    ## 1  emissions_quantity emissions_quantity 1 4  1.00000000
    ## 2            beta_sif emissions_quantity 2 4 -0.25096240
    ## 3            beta_sif           beta_sif 2 3  1.00000000
    ## 4           beta_xch4 emissions_quantity 3 4  0.34834898
    ## 5           beta_xch4           beta_sif 3 3 -0.24474686
    ## 6           beta_xch4          beta_xch4 3 2  1.00000000
    ## 7           beta_xco2 emissions_quantity 4 4 -0.04646986
    ## 8           beta_xco2           beta_sif 4 3  0.19587754
    ## 9           beta_xco2          beta_xch4 4 2  0.06893047
    ## 10          beta_xco2          beta_xco2 4 1  1.00000000
    ## 
    ## $arg
    ## $arg$type
    ## [1] "upper"

    ## [[1]]
    ## [[1]]$corr
    ##                    emissions_quantity    beta_sif  beta_xch4  beta_xco2
    ## emissions_quantity         1.00000000  0.09228143  0.1042044  0.1116891
    ## beta_sif                   0.09228143  1.00000000  0.4444834 -0.2416790
    ## beta_xch4                  0.10420437  0.44448337  1.0000000 -0.3589533
    ## beta_xco2                  0.11168911 -0.24167901 -0.3589533  1.0000000
    ## 
    ## [[1]]$corrPos
    ##                 xName              yName x y        corr
    ## 1  emissions_quantity emissions_quantity 1 4  1.00000000
    ## 2            beta_sif emissions_quantity 2 4  0.09228143
    ## 3            beta_sif           beta_sif 2 3  1.00000000
    ## 4           beta_xch4 emissions_quantity 3 4  0.10420437
    ## 5           beta_xch4           beta_sif 3 3  0.44448337
    ## 6           beta_xch4          beta_xch4 3 2  1.00000000
    ## 7           beta_xco2 emissions_quantity 4 4  0.11168911
    ## 8           beta_xco2           beta_sif 4 3 -0.24167901
    ## 9           beta_xco2          beta_xch4 4 2 -0.35895333
    ## 10          beta_xco2          beta_xco2 4 1  1.00000000
    ## 
    ## [[1]]$arg
    ## [[1]]$arg$type
    ## [1] "upper"
    ## 
    ## 
    ## 
    ## [[2]]
    ## [[2]]$corr
    ##                    emissions_quantity    beta_sif  beta_xch4   beta_xco2
    ## emissions_quantity         1.00000000 -0.08791411 -0.1354180 -0.08506265
    ## beta_sif                  -0.08791411  1.00000000  0.2824124  0.04865846
    ## beta_xch4                 -0.13541799  0.28241237  1.0000000  0.11557149
    ## beta_xco2                 -0.08506265  0.04865846  0.1155715  1.00000000
    ## 
    ## [[2]]$corrPos
    ##                 xName              yName x y        corr
    ## 1  emissions_quantity emissions_quantity 1 4  1.00000000
    ## 2            beta_sif emissions_quantity 2 4 -0.08791411
    ## 3            beta_sif           beta_sif 2 3  1.00000000
    ## 4           beta_xch4 emissions_quantity 3 4 -0.13541799
    ## 5           beta_xch4           beta_sif 3 3  0.28241237
    ## 6           beta_xch4          beta_xch4 3 2  1.00000000
    ## 7           beta_xco2 emissions_quantity 4 4 -0.08506265
    ## 8           beta_xco2           beta_sif 4 3  0.04865846
    ## 9           beta_xco2          beta_xch4 4 2  0.11557149
    ## 10          beta_xco2          beta_xco2 4 1  1.00000000
    ## 
    ## [[2]]$arg
    ## [[2]]$arg$type
    ## [1] "upper"
    ## 
    ## 
    ## 
    ## [[3]]
    ## [[3]]$corr
    ##                    emissions_quantity    beta_sif  beta_xch4   beta_xco2
    ## emissions_quantity          1.0000000 -0.10998852 -0.5746164  0.13324288
    ## beta_sif                   -0.1099885  1.00000000  0.1056883  0.06848972
    ## beta_xch4                  -0.5746164  0.10568834  1.0000000 -0.18835195
    ## beta_xco2                   0.1332429  0.06848972 -0.1883519  1.00000000
    ## 
    ## [[3]]$corrPos
    ##                 xName              yName x y        corr
    ## 1  emissions_quantity emissions_quantity 1 4  1.00000000
    ## 2            beta_sif emissions_quantity 2 4 -0.10998852
    ## 3            beta_sif           beta_sif 2 3  1.00000000
    ## 4           beta_xch4 emissions_quantity 3 4 -0.57461643
    ## 5           beta_xch4           beta_sif 3 3  0.10568834
    ## 6           beta_xch4          beta_xch4 3 2  1.00000000
    ## 7           beta_xco2 emissions_quantity 4 4  0.13324288
    ## 8           beta_xco2           beta_sif 4 3  0.06848972
    ## 9           beta_xco2          beta_xch4 4 2 -0.18835195
    ## 10          beta_xco2          beta_xco2 4 1  1.00000000
    ## 
    ## [[3]]$arg
    ## [[3]]$arg$type
    ## [1] "upper"
    ## 
    ## 
    ## 
    ## [[4]]
    ## [[4]]$corr
    ##                    emissions_quantity    beta_sif   beta_xch4   beta_xco2
    ## emissions_quantity         1.00000000 -0.12242464 -0.15930352  0.01000652
    ## beta_sif                  -0.12242464  1.00000000  0.04591749 -0.14554713
    ## beta_xch4                 -0.15930352  0.04591749  1.00000000 -0.08746528
    ## beta_xco2                  0.01000652 -0.14554713 -0.08746528  1.00000000
    ## 
    ## [[4]]$corrPos
    ##                 xName              yName x y        corr
    ## 1  emissions_quantity emissions_quantity 1 4  1.00000000
    ## 2            beta_sif emissions_quantity 2 4 -0.12242464
    ## 3            beta_sif           beta_sif 2 3  1.00000000
    ## 4           beta_xch4 emissions_quantity 3 4 -0.15930352
    ## 5           beta_xch4           beta_sif 3 3  0.04591749
    ## 6           beta_xch4          beta_xch4 3 2  1.00000000
    ## 7           beta_xco2 emissions_quantity 4 4  0.01000652
    ## 8           beta_xco2           beta_sif 4 3 -0.14554713
    ## 9           beta_xco2          beta_xch4 4 2 -0.08746528
    ## 10          beta_xco2          beta_xco2 4 1  1.00000000
    ## 
    ## [[4]]$arg
    ## [[4]]$arg$type
    ## [1] "upper"
    ## 
    ## 
    ## 
    ## [[5]]
    ## [[5]]$corr
    ##                    emissions_quantity   beta_sif   beta_xch4   beta_xco2
    ## emissions_quantity         1.00000000 -0.2509624  0.34834898 -0.04646986
    ## beta_sif                  -0.25096240  1.0000000 -0.24474686  0.19587754
    ## beta_xch4                  0.34834898 -0.2447469  1.00000000  0.06893047
    ## beta_xco2                 -0.04646986  0.1958775  0.06893047  1.00000000
    ## 
    ## [[5]]$corrPos
    ##                 xName              yName x y        corr
    ## 1  emissions_quantity emissions_quantity 1 4  1.00000000
    ## 2            beta_sif emissions_quantity 2 4 -0.25096240
    ## 3            beta_sif           beta_sif 2 3  1.00000000
    ## 4           beta_xch4 emissions_quantity 3 4  0.34834898
    ## 5           beta_xch4           beta_sif 3 3 -0.24474686
    ## 6           beta_xch4          beta_xch4 3 2  1.00000000
    ## 7           beta_xco2 emissions_quantity 4 4 -0.04646986
    ## 8           beta_xco2           beta_sif 4 3  0.19587754
    ## 9           beta_xco2          beta_xch4 4 2  0.06893047
    ## 10          beta_xco2          beta_xco2 4 1  1.00000000
    ## 
    ## [[5]]$arg
    ## [[5]]$arg$type
    ## [1] "upper"

### Verificando maiores cidades emissoras

``` r
# Only for analysis

emissions_sources |>
  #glimpse() |>
  #select(sigla_uf, emissions_quantity, city_ref, gas, activity_units, sector_name, sub_sector) |>
  filter(sigla_uf == "PA",
         gas == 'co2e_100yr',
         year == 2015,
         sector_name == 'agriculture',
         !source_name %in% nomes_uf,
         # !sub_sector %in% c("forest-land-clearing",
         #                    "forest-land-degradation",
         #                    "shrubgrass-fires",
         #                    "forest-land-fires",
         #                    "wetland-fires",
         #                    "removals"),
         # str_detect(activity_units, 'animal'),
         # sub_sector == 'enteric-fermentation-cattle-pasture',
                         # 'manure-left-on-pasture-cattle'),
         city_ref == 'São Félix Do Xingu'                       #change the municipality here
          ) #|>
```

    ## # A tibble: 3 × 32
    ##   source_id source_name        source_type iso3_country original_inventory_sec…¹
    ##       <int> <chr>              <chr>       <chr>        <chr>                   
    ## 1  10844136 São Félix do Xingu <NA>        BRA          cropland-fires          
    ## 2  21122398 São Félix do Xingu <NA>        BRA          enteric-fermentation-ca…
    ## 3  20395095 São Félix do Xingu <NA>        BRA          manure-left-on-pasture-…
    ## # ℹ abbreviated name: ¹​original_inventory_sector
    ## # ℹ 27 more variables: start_time <date>, end_time <date>, lat <dbl>,
    ## #   lon <dbl>, geometry_ref <chr>, gas <chr>, emissions_quantity <dbl>,
    ## #   temporal_granularity <chr>, created_date <date>, modified_date <date>,
    ## #   directory <chr>, activity <dbl>, activity_units <chr>,
    ## #   emissions_factor <dbl>, emissions_factor_units <chr>, capacity <dbl>,
    ## #   capacity_units <chr>, capacity_factor <dbl>, year <dbl>, …

``` r
  # group_by(sub_sector, sector_name) |>
  # summarise(
  #    emission = sum(emissions_quantity, na.rm = T)
  #  )
```

## VISUALIZANDO MAIORES EMISSORES PARA O SETOR DE AGRICULTURA OU P/ ANIMAL

``` r
emissions_sources |>
  filter(
    year == 2022,                   #%in% 2015:2022
    sigla_uf %in% estados, # <-----
    str_detect(activity_units, 'animal'),
    # sector_name == 'agriculture',
    !source_name %in% nomes_uf,
    gas == 'co2e_100yr'
    ) |>
  group_by(city_ref, sigla_uf, sub_sector) |>
  summarise(
    emission = sum(emissions_quantity, na.rm = T)
  ) |>
  group_by(city_ref,sigla_uf) |>
  mutate(
    emission_total = sum(emission, na.rm = T)
  ) |>
  ungroup() |>
  group_by(sigla_uf) |>
  mutate(
    city_ref = city_ref |> fct_reorder(emission_total) |>
      fct_lump(n = 3, w = emission_total)) |>
  filter(city_ref != "Other") |>
  mutate(
      sub_sector = case_when(
        sub_sector == "enteric-fermentation-cattle-feedlot" ~ "FEGC",
        sub_sector == "enteric-fermentation-cattle-pasture" ~ "FEGP",
        sub_sector == "manure-left-on-pasture-cattle"  ~ "EP",
        sub_sector == "manure-management-cattle-feedlot" ~ "GEC",
        sub_sector == 'cropland-fires' ~ 'CF',
        sub_sector == 'synthetic-fertilizer-application' ~ 'SF application'
      )) |>
  ggplot(aes(emission/1e6, #passar de ton para Mton
             city_ref,
             fill = sub_sector)) +
  geom_col(col="black", lwd = 0.1) +
  xlab(bquote(Emissião~CO[2]~e~(Mton))) +
  labs(#x = 'Emission (Mton)',
       y = 'Cidade',
       fill = 'Subsetor') +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = rel(1)),
    # axis.title.x = element_text(size = rel(2)),
    axis.text.y = element_text(size = rel(1.3)),
    # axis.title.y = element_text(size = rel(2)),
    legend.text = element_text(size = rel(1)),
    #legend.title = element_text(size = rel(1.7)),
    title = element_text(face = 'bold'),
    legend.position = 'top',
    legend.background = element_rect(fill = "transparent", color = "black")) +
      scale_fill_viridis_d(option ='plasma') +
  facet_wrap(~sigla_uf,scales = "free",ncol = 2) +
  annotate("text",
           x=2,
           y=1,
           label = ".",
           size=0.1)
```

![](README_files/figure-gfm/unnamed-chunk-69-1.png)<!-- -->

``` r
ggsave('top3cidades_emissão_states.png')
```

``` r
# Only for exemplification in the report

estado <- 'MG'

my_plot_col_states <- function(.estados){
  emissions_sources |>
  filter(
    year == 2022,
    sigla_uf == .estados,
    !source_name %in% nomes_uf,
    #str_detect(activity_units, 'animal'),
    sector_name == 'agriculture',
    gas == 'co2e_100yr'
    ) |>
  group_by(city_ref, sub_sector) |>
  summarise(
    emission = sum(emissions_quantity, na.rm = T)
  ) |>
  group_by(city_ref) |>
  mutate(
    emission_total = sum(emission, na.rm = T)
  ) |>
  ungroup() |>
  mutate(
    city_ref = city_ref |> fct_reorder(emission_total) |>
      fct_lump(n = 5, w = emission_total)) |>
  filter(city_ref != "Other") |>
    mutate(
      sub_sector = case_when(
        sub_sector == "cropland-fires"  ~ "Cropland fires",
        sub_sector == "enteric-fermentation-cattle-feedlot" ~ "EFC feedlot",
        sub_sector == "enteric-fermentation-cattle-pasture" ~ "EFC pasture",
        sub_sector == "manure-left-on-pasture-cattle"  ~ "ML pasture cattle",
        sub_sector == "manure-management-cattle-feedlot" ~ "MMC feedlot",
        sub_sector == "rice-cultivation" ~ "rice cultivation",
        sub_sector == "synthetic-fertilizer-application" ~ "SF application",
      )
    ) |>
  ggplot(aes(emission/1e6,
             city_ref,
             fill = sub_sector)) +
  geom_col(col="black", lwd = 0.1) +
    xlab(bquote(Emission~CO[2]~e~(Mton))) +
  labs(#x = 'Emission (Mton)',
       y = 'City',
       fill = '') +
  theme_bw() +
  map(my_theme,my_theme_add) +
  theme(legend.position = "top", ##retirar legenda = ''
    legend.background = element_rect(fill = "transparent")) + ##?
  scale_fill_viridis_d(option = "plasma") +
  annotate("text",
           x=2,
           y=1,
           label = ".",
           size=0.1)
  }
  # geom_text(stat = 'identity',
  #           position = 'identity',
  #           size = 4,
  #           angle = 90,
  #           vjust = 2,
  #           data = NULL,
  #           label = 'emission')

map(estados,my_plot_col_states)
```

    ## [[1]]

![](README_files/figure-gfm/unnamed-chunk-70-1.png)<!-- -->

    ## 
    ## [[2]]

![](README_files/figure-gfm/unnamed-chunk-70-2.png)<!-- -->

    ## 
    ## [[3]]

![](README_files/figure-gfm/unnamed-chunk-70-3.png)<!-- -->

    ## 
    ## [[4]]

![](README_files/figure-gfm/unnamed-chunk-70-4.png)<!-- -->

    ## 
    ## [[5]]

![](README_files/figure-gfm/unnamed-chunk-70-5.png)<!-- -->

``` r
# ggsave('MG_legenda_setor_agr.png', dpi = 3000)
```

## SUBSETORES

### CARACTERIZANDO SUBSETORES PARA EMISSÕES DE GADO E RANKEANDO AS 5 CIDADES COM SUBSETORES DE MAIOR EMISSÃO DENTRE OS 5 ESTADOS

``` r
# my_plot_subsector_states <- function(.estados){
  emissions_sources |>
  filter(
         year == 2022,
         str_detect(activity_units, 'animal'),
         gas == 'co2e_100yr',
         !source_name %in% nomes_uf,
         sigla_uf %in% estados) |>
  group_by(city_ref, original_inventory_sector, sigla_uf) |>
  summarise(
    emission = sum(emissions_quantity, na.rm = T)
  ) |>
  # ungroup() |>
  group_by(city_ref,sigla_uf) |>
  mutate(
    emission_total = sum(emission, na.rm = T)
  ) |>
  ungroup() |>
  mutate(
    city_ref = city_ref |> fct_reorder(emission_total) |>
      fct_lump(n = 10, w = emission_total)) |>
  mutate(
       original_inventory_sector = original_inventory_sector |>
         as_factor() |>
         fct_relevel("manure-left-on-pasture-cattle",
          "enteric-fermentation-cattle-feedlot",
          "manure-management-cattle-feedlot",
          "enteric-fermentation-cattle-pasture")
    ) |>
  filter(city_ref != "Other") |>
  mutate(
      original_inventory_sector = case_when(
        original_inventory_sector == "enteric-fermentation-cattle-feedlot" ~ "FEGC",
        original_inventory_sector == "enteric-fermentation-cattle-pasture" ~ "FEGP",
        original_inventory_sector == "manure-left-on-pasture-cattle"  ~ "EP",
        original_inventory_sector == "manure-management-cattle-feedlot" ~ "GEC",
      )) |>
  ggplot(aes(emission/1e6, #passar para Mega Ton
             city_ref,
             fill = original_inventory_sector)) +
  geom_col(col="black") +
  labs(x = 'Emissão (Mton)',
       y = 'City',
       fill = 'Subsetor') +
  theme_bw() +
  # map(my_theme,my_theme_add) +
  theme(legend.position = "top") +
  scale_fill_viridis_d(option = "plasma") +
    # facet_wrap(~city_ref,,ncol = 2) +
  annotate("text",
           x=5,
           y=1,
           label = ".",
           size=0.1)
```

![](README_files/figure-gfm/unnamed-chunk-71-1.png)<!-- -->

``` r
# }

# map(estados,my_plot_subsector_states)

# ggsave('top10.png', dpi = 2000)
```

``` r
# emissions_sources |>
#   filter(
#     year == 2022,
#     sigla_uf %in% estados,
#     !source_name %in% nomes_uf,
#     gas == 'co2e_100yr',
#     # sector_name == 'agriculture'
#     str_detect(activity_units, 'animal')
#     ) |>
#   select(original_inventory_sector, emissions_quantity, city_ref) |>
#   group_by(city_ref, original_inventory_sector) |>
#   summarise(
#     emission = sum(emissions_quantity, na.rm = T)
#   ) |>
#   arrange( - emission) |>
#   group_by(city_ref) |>
#   mutate(
#     emission_total = sum(emission, na.rm = T)
#   ) |>
#   ungroup() |>
#   mutate(
#     city_ref = city_ref |> fct_reorder(emission_total) |>
#       fct_lump(n = 5, w = emission_total)) |>
#   filter(city_ref != "Other") |>
#   ggplot(aes(emission/1e6,
#              city_ref,
#              fill = original_inventory_sector)) +
#   geom_col(col="black") +
#   labs(x = 'Emission (Mton)',
#        y = 'City',
#        fill = 'Subsector') +
#   theme_bw() +
#   map(my_theme,my_theme_add) +
#   theme(legend.position = 'top') +
#   scale_fill_viridis_d(option = 'plasma')
```

## AGRUPAR POR ESTADO, EMISSÃO E SETOR

``` r
emissions_sources |>
  filter(
    year == 2022,
    sigla_uf %in% estados,
    !source_name %in% nomes_uf,
    gas == 'co2e_100yr',
    str_detect(activity_units, 'animal')) |>
  mutate(
      original_inventory_sector = case_when(
        original_inventory_sector == "enteric-fermentation-cattle-feedlot" ~ "FEGC",
        original_inventory_sector == "enteric-fermentation-cattle-pasture" ~ "FEGP",
        original_inventory_sector == "manure-left-on-pasture-cattle"  ~ "EP",
        original_inventory_sector == "manure-management-cattle-feedlot" ~ "GEC"
      )) |>
  select(original_inventory_sector, emissions_quantity, sigla_uf) |>
  group_by(sigla_uf, original_inventory_sector) |>
  arrange( desc(emissions_quantity)) |>
  summarise(
    emission = sum(emissions_quantity, na.rm = T)
  ) |>
  mutate(emission_total = sum(emission)) |>
  arrange( - emission) |>
  ungroup() |>
  mutate(
    sigla_uf = sigla_uf |>
  fct_reorder(emission_total)) |>
  ggplot(aes(emission/1e6,
             sigla_uf,
             fill = original_inventory_sector)) +
  geom_col(color ="black", lwd = 0.1) +
  xlab(bquote(Emissão~de~CO[2]~e~(Mton))) +
  labs(#x = 'Emissão de CO2 (Mton)',
       y = 'Estado',
       fill = 'Subsetor') +
  theme_bw() +
  map(my_theme,my_theme_add) +
  map(my_theme,my_theme_add) +
  theme(legend.position = 'top') +
  scale_fill_viridis_d(option = 'plasma')
```

![](README_files/figure-gfm/unnamed-chunk-73-1.png)<!-- -->

``` r
# ggsave('States_emission.png')
```

## TOTAL DE EMISSÃO PARA OS ESTADOS/BR

``` r
emissions_sources |>
  filter(year == 2022,
         str_detect(activity_units, 'animal'),          #cattle
         # sector_name == 'agriculture',
         !source_name %in% nomes_uf,
         sigla_uf %in% estados,
         gas == 'co2e_100yr') |>
  group_by(sigla_uf) |>                  #!! group_by(iso3_country)  #to BR
  summarise(
    soma_emissao = sum(emissions_quantity)
    ) |>
  arrange(- soma_emissao)
```

    ## # A tibble: 5 × 2
    ##   sigla_uf soma_emissao
    ##   <chr>           <dbl>
    ## 1 MG          48937939.
    ## 2 MS          43469987.
    ## 3 MT          40816950.
    ## 4 GO          36888763.
    ## 5 PA          13610501.

## SERIE TEMPORAL, 2015 A 2022

``` r
emissions_sources |>
  filter(
    # year <= 2022,
    gas == 'co2e_100yr',
    !source_name %in% nomes_uf,
    str_detect(activity_units, 'animal')
  ) |>  # pull(sigla_uf) |> unique()
  group_by(year) |>
  summarise(
    soma_emissao= sum(emissions_quantity, na.rm = TRUE)/1e6 #,
    # media_emissao = mean(emissions_quantity, na.rm = TRUE)/1e6,
    # sd_emissao = sd(emissions_quantity/1e6, na.rm = TRUE)
  )  |>
  mutate(
    sigla_uf = "Br"
  ) |>
  rbind(emissions_sources |>
            filter(sigla_uf %in% estados,
            str_detect(activity_units, 'animal'),
            gas == 'co2e_100yr',
            !source_name %in% nomes_uf
          ) |>
          group_by(year, sigla_uf) |>
          summarise(
            soma_emissao= sum(emissions_quantity)/1e6 #,
            # media_emissao = mean(emissions_quantity)/1e6,
            # sd_emissao = sd(emissions_quantity/1e6)
          )
  ) |>
  filter(sigla_uf != "Br") |>
  ggplot(aes(x=year,y=soma_emissao,
             fill=sigla_uf))+
  #geom_point()+
  #geom_smooth(method = 'lm')+
  #ggpubr::stat_cor()+
  geom_col(position = "dodge", color="black") +    #erro com position = "identity" [CIC] ; position = "dodge" [RF]
  theme_bw() +
  map(my_theme, my_theme_add) +
  theme(
    legend.text = element_text(size = rel(1.3))
  ) +
  labs(x = 'Ano',
       y = 'Emissão total',
       fill="") +
  scale_fill_viridis_d()
```

![](README_files/figure-gfm/unnamed-chunk-75-1.png)<!-- -->

``` r
ggsave('TemporalEmissions-states.png')
```
