#' Checar se o working directory é válido
#'
#' Checa se o working directory é válido: se ele está dois níveis abaixo da
#' pasta raiz do projeto (ou seja, se você por exemplo num projeto dentro da sua
#' página `git_seunome` pessoal).
#'
#' @keywords internal
checa_wd_valido <- function() {

  dois_niveis_acima <- normalizePath("../..")

  if (!grepl("Proj_acess_oport$", dois_niveis_acima))
    stop(
      "Working directoty inválido!\n",
      "As funções do {aopint} só funcionam dentro de um projeto na sua pasta ",
      "'git_seunome'.\n",
      "Por exemplo: wd == Proj_acess_oport/git_daniel/aopint - válido\n",
      "             wd == Proj_acess_oport/git_daniel        - inválido\n",
      call. = FALSE
    )

  invisible(TRUE)

}
