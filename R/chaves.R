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
#' - `MAPBOX`, do Mapbox;
#' - `EARTHDATA_LOGIN` e `EARTHDATA_PASS`, da Earthdata da NASA.
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

  checa_wd_valido()

  path <- normalizePath("~/.Renviron")

  # checa se existe um .Renviron. se não, cria um

  if (!file.exists(path)) arquivo_criado <- file.create(path)

  if (!file.exists(path) && !arquivo_criado)
    stop("Houve um problema ao criar o ~/.Renviron")

  # pega o conteúdo do arquivo de chaves do projeto - retira a primeira linha
  # que é um comentário

  chaves_aop <- readLines("../../data-raw/.chaves.txt")
  chaves_aop <- chaves_aop[-1]

  # lê o conteúdo do .Renviron e adiciona as chaves do projeto no final

  renviron <- readLines(path, encoding = "UTF-8")

  # checa se o .Renviron já foi editado por essa função antes
  # se já, desconsidera o que foi editado por ela anteriormente

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

  renviron <- c(renviron, linha_esp, chaves_aop)

  writeLines(renviron, path)

  invisible(path)

}



#' Adicionar chaves ao projeto
#'
#' Adiciona chaves de API para serem utilizadas no projeto.
#'
#' @param ... Uma sequência de argumentos nomeados na forma:
#' `NOME_DA_CHAVE = "valor"`
#'
#' @section Detalhes:
#' É considerada uma boa prática nomear variáveis de ambiente com letras
#' maiúsculas. A função processa os nomes pra deixá-los em maiúsculas, caso
#' tenha sido dado em minúsculas.
#'
#' @examples
#' if (interactive()) {
#'
#'   adicionar_chaves(CHAVEEXEMPLO = "valor_ficticio")
#'
#'   atualizar_renviron()
#'
#' }
#'
#' @export
adicionar_chaves <- function(...) {

  checa_wd_valido()

  novas_chaves <- list(...)
  nomes_chaves <- setdiff(names(novas_chaves), "")

  if (is.null(nomes_chaves) || length(nomes_chaves) < length(novas_chaves))
    stop("Todas as chaves precisam ter um nome único!", call. = FALSE)

  # lê o arquivo de chaves e checa se alguma chave a ser adicionada já existe.
  # se sim, retorna um erro informando qual chave já existe

  arquivo_chaves <- "../../data-raw/.chaves.txt"
  conteudo_arquivo <- readLines(arquivo_chaves, encoding = "UTF-8")

  chaves_conflitantes <- vapply(
    nomes_chaves,
    function(i) {
      se_existe <- grepl(paste0("^", i, "="), conteudo_arquivo)
      any(se_existe)
    },
    logical(1)
  )
  chaves_conflitantes <- nomes_chaves[chaves_conflitantes]

  if (!identical(chaves_conflitantes, character(0)))
    stop(
      "Essas chaves já existem: ",
      paste(chaves_conflitantes, collapse = ", "),
      call. = FALSE
    )

  # transforma a lista 'novas_chaves' em um vetor de caracteres no formato
  # NOME="valor"

  novas_chaves <- paste0(
    toupper(names(novas_chaves)),
    "=\"",
    unlist(novas_chaves),
    "\""
  )

  # adiciona as chaves ao arquivo de chaves
  # como o arquivo é somente leitura, primeiro habilita escrita nele

  Sys.chmod(arquivo_chaves, "200")
  on.exit(Sys.chmod(arquivo_chaves, "000"), add = TRUE)

  conteudo_arquivo <- c(conteudo_arquivo, unlist(novas_chaves))
  writeLines(conteudo_arquivo, arquivo_chaves)

  invisible(arquivo_chaves)

}



#' Remover chaves do projeto
#'
#' Remove chaves do arquivo de chaves do projeto
#'
#' @param chaves Um vetor de strings com o nome das chaves a serem retiradas.
#'   Lembre-se que as chaves estão todos com os nomes em letras maiúsculas.
#'
#' @examples
#' if (interactive()) {
#'
#'   remover_chaves(c("CHAVEEXEMPLO", "OUTROEXEMPLO"))
#'
#'   atualizar_renviron()
#'
#' }
#'
#' @export
remover_chaves <- function(chaves) {

  checa_wd_valido()

  if (!is.character(chaves)) stop("'chaves' deve ser um vetor de strings!")

  # lê o arquivo de chaves e vê se as chaves dadas estão nele
  # se alguma não estiver, retorna um erro

  arquivo_chaves <- "../../data-raw/.chaves.txt"
  conteudo_arquivo <- readLines(arquivo_chaves, encoding = "UTF-8")

  chaves_no_arquivo <- vapply(
    chaves,
    function(i) any(grepl(paste0("^", i, "="), conteudo_arquivo)),
    logical(1)
  )

  if (!all(chaves_no_arquivo))
    stop(
      "Essas chaves não existem: ",
      paste(chaves[!chaves_no_arquivo], collapse = ", "),
      call. = FALSE
    )

  # encontra os índices das chaves e retira elas do conteúdo do arquivo
  # como o arquivo é somente leitura, primeiro habilita escrita nele

  indice_chaves <- vapply(
    chaves,
    function(i) grep(paste0("^", i, "="), conteudo_arquivo),
    integer(1)
  )

  conteudo_arquivo <- conteudo_arquivo[-indice_chaves]

  Sys.chmod(arquivo_chaves, "200")
  on.exit(Sys.chmod(arquivo_chaves, "000"), add = TRUE)

  writeLines(conteudo_arquivo, arquivo_chaves)

  invisible(arquivo_chaves)

}
