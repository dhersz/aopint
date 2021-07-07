#' Atualizar .Renviron com chaves do projeto
#'
#' Inclui as chaves do projeto no seu .Renviron. Dessa forma elas podem ser
#' utilizadas usando `Sys.getenv("chave")`. É necessário reiniciar a sessão do R
#' depois de atualizar pra poder usar as chaves assim.
#'
#' @section Chaves existentes:
#' Atualmente inclui as chaves:
#'
#' - `GOOGLE1`, `GOOGLE2` e `GOOGLE3`, da API do Google Maps;
#' - `MAPBOX`, do Mapbox.
#'
#' @examples
#' if (interactive()) {
#'
#'   atualizar_renviron()
#'
#'   # depois de reiniciar o R
#'   Sys.getenv("GOOGLE1")
#' }
#'
#' @export
atualizar_renviron <- function() {

  path <- normalizePath("~/.Renviron")

  # checa se existe um .Renviron. se não, cria um

  if (!file.exists(path)) arquivo_criado <- file.create(path)

  if (!file.exists(path) && !arquivo_criado)
    stop("Houve um problema ao criar o ~/.Renviron")

  # pega o conteúdo dos arquivos de chaves do projeto e transforma em pares de
  # NOME=valor

  chaves_google <- readLines("../../data-raw/google_key.txt")
  chaves_google <- paste(
    c("GOOGLE1", "GOOGLE2", "GOOGLE3"),
    chaves_google,
    sep = "="
  )

  chaves_mapbox <- readLines("../../data-raw/mapbox_key.txt")
  chaves_mapbox <- paste0("\"", chaves_mapbox, "\"")
  chaves_mapbox <- paste("MAPBOX", chaves_mapbox, sep = "=")

  # lê o conteúdo do .Renviron e adiciona as chaves do projeto no final

  renviron <- readLines(path, encoding = "UTF-8")

  # checa se o .Renviron já foi editado por essa função antes
  # se já, desconsidera o que foi editado por ela anteriorment

  linha_esp <- "# nao editar abaixo desta linha, atualizado pelo aopint"

  editado_antes <- grepl(linha_esp, renviron)

  if (any(editado_antes)) {
    indice_linha <- match(TRUE, editado_antes)
    renviron <- renviron[seq.int(from = 1, to = indice_linha - 1)]
  }

  # adicionar linhas no .Renviron
  # fazer questão de adicionar uma linha vazia antes

  if (identical(renviron, character(0)) || renviron[length(renviron)] != "")
    renviron <- c(renviron, "")

  renviron <- c(renviron, linha_esp, chaves_google, chaves_mapbox)

  writeLines(renviron, path)

  invisible(path)

}
