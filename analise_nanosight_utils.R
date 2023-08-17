# Para poder fazer subsets de linhas e colunas (necessário ter uma função para usar com arg em lapply/sapply)
index_lapply_sapply <- function(...) {
  ...[]
} 
#Para padronizar ids
extract_standardized_id <- function(ids) {
  standardized_ids <- character()
  
  for (i in seq_along(ids)) {
    id <- ids[i]
    id_number <- sub('.*?(\\d{5}).*', '\\1', id, perl = TRUE)
    
    if (grepl("(?i)\\d{5}$", id)) {
      if (grepl("(?i)^[wW][12]", id)) {
        id_number <- paste0(id_number, "_", tolower(sub("^[wW]([12]).*", "\\1", id, perl = TRUE)))
        standardized_ids <- c(standardized_ids, id_number)
      } else {
        warning(paste("Sample at position", i, "(", id, ") does not contain 'w1' or 'w2' in its name. It has not been outputted."))
      }
    } else if (grepl("(?i)^[wW][12]", id)) {
      id_number <- paste0(id_number, "_", tolower(sub("^[wW]([12]).*", "\\1", id, perl = TRUE)))
      standardized_ids <- c(standardized_ids, id_number)
    } else if (grepl("(?i)([wW][12])", id)) {
      id_number <- paste0(id_number, "_", tolower(sub(".*?([wW][12]).*", "\\1", id, perl = TRUE)))
      standardized_ids <- c(standardized_ids, id_number)
    } else {
      warning(paste("Sample at position", i, "(", id, ") does not match the expected format. It has not been outputted."))
    }
  }
  
  return(standardized_ids)
}
#Para obter diluicao das amostras da Belle que nao seguiram a diluicao padrao dela (valor diluicao deve ser com / e nao :)
##Func nao esta sendo utilizada pq o caso de uso era mais simples, mas caso se complique no futuro, ela sera util, entao se mantem aqui
#obter_diluicao_excecoes <- function(id, valor_diluicao) {
#  if (length(id) == 1 && length(valor_diluicao) == 1) {
#    row <- as.numeric(which(id_sample == id))
#    diluicao[row, ] <- valor_diluicao
#  } else {
#    for (i in 1:length(id)) {
#      row <- as.numeric(which(id_sample == id[i]))
#      diluicao[row, ] <- valor_diluicao[i]
#    }
#  }
#  assign("diluicao", diluicao, envir = .GlobalEnv)
#}
##Exemplo de uso
#ids <- c() #preencher com as ids que a diluicao e diferente, entre aspas, n tem problema se tiver so uma
# valores_diluicao <- c() #preencher com os valores de diluicao, entre aspas, posicao deve bater com a deve bater com a id, n tem problema se tiver so um 
#obter_diluicao_excecoes(ids, valores_diluicao) 
