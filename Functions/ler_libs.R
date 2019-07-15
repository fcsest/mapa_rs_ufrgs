ler_libs <- function(packages){
  # Cria vetor com os nomes dos packages chamados que não estão instalados:
  instalar <- packages[!(packages %in% installed.packages()[, "Package"])]
  
  # Caso exista algum package não instalado na lista:
  if(length(instalar) > 0){
    # Instala os packages e suas dependências (packages dos quais eles dependem):
    install.packages(pkgs = instalar, dependencies = TRUE)
  }
  
  # Aplica a função "require" em cada package, com a opção "character.only = TRUE" para passar os nomes como strings:
  invisible(sapply(packages, require, character.only = TRUE))
}