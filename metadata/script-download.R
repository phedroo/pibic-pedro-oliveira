# Links de download gerados pela NASA
links <- list.files(path = "metadata/",
                    pattern = ".txt",
                    full.names = TRUE) |>
  readr::read_table(col_names = FALSE,
                    col_types = "c",
                    skip =3) |>
  dplyr::pull(X1)

# Definindo os caminhos e nomes dos arquivos
n_split <- lengths(stringr::str_split(links[1],"/"))
files_csv <- stringr::str_split(links,"/",simplify = TRUE)[,n_split]
files_csv <- paste0("data-raw/nc4/",files_csv)

# Definindo o plano de multisession
future::plan("multisession")

# Criando a função para fazer o download dos arquivos
download_arquivos <- function(url, dir){
  download.file(url, dir, method="wget",
                extra= c("--user=alan.panosso --password FMB675fmb675@"))
  return(dir)
}

# Teste o tempo de download
tictoc::tic()
purrr::map2(links[1:1], files_csv[1:1],
            purrr::possibly(download_arquivos, ""))
tictoc::toc()

# Criando a função maybe_
# maybe_download_nasa_prog <- function(url, dir, prog){
#   prog()
#   f <- purrr::possibly(download_arquivos, "")
#   f(url, dir)
# }
#
# # Rodando com a barra de progresso
# progressr::with_progress({
#   prog <- progressr::progressor(along = links)
#   furrr::future_map2(links, files_csv,
#                      maybe_download_nasa_prog, prog=prog)
# })
#
# ### BAixando os que faltaram
# links_f <- list.files(path = "metadata/",
#                     pattern = ".nc4",
#                     full.names = TRUE) |>
#   readr::read_table(col_names = FALSE,
#                     col_types = "c") |>
#   dplyr::pull(X1)
#
# # Definindo os caminhos e nomes dos arquivos
# n_split <- lengths(stringr::str_split(links_f[1],"/"))
# files_csv_f <- stringr::str_split(links_f,"/",simplify = TRUE)[,n_split]
# files_csv_f <- paste0("data-raw/nc4/",files_csv_f)
#
# # Rodando com a barra de progresso
# progressr::with_progress({
#   prog <- progressr::progressor(along = links_f)
#   furrr::future_map2(links_f, files_csv_f,
#                      maybe_download_nasa_prog, prog=prog)
# })
#
