
# aopint 🍺

Pacotinho pra agilizar a vida da galera do AOP. As funções dele só
funcionam se você estiver em um projeto um nível abaixo da sua pasta
`git_seunome`. Por exemplo, as funções funcionam se eu estiver no
projeto `git_daniel/aopcoisas`, e não funcionam se eu estiver em um
projeto no meu `Meus Documentos`.

## Instalação

``` r
install.packages("aopint", repos = "https://dhersz.r-universe.dev")

# ou
remotes::install_github("dhersz/aopint")
```

## Funções

-   `atualizar_renviron()` - atualiza o seu `~/.Renviron` pra que ele
    contenha as chaves das APIs usadas no projeto.

``` r
atualizar_renviron()

# depois de reiniciar o R você pode por exemplo pegar o valor de uma chave da
# API do Google Maps fácil fácil
chave <- Sys.getenv("GOOGLE1")
```

-   `adicionar_chaves()`, `remover_chaves()` - adiciona ou remove chaves
    em um arquivo pra que elas possam depois ser adicionados ao seu
    `~/.Renviron`.

``` r
adicionar_chaves(CHAVE_ESPECIAL = "valor_secreto")
atualizar_renviron()

# depois de reiniciar o R, você pode pegar o valor dela
Sys.getenv("CHAVE_ESPECIAL")
#> [1] "valor_secreto"

remover_chaves("CHAVE_ESPECIAL")
atualizar_renviron()

# depois de reiniciar o R
Sys.getenv("CHAVE_ESPECIAL")
#> [1] ""
```
