#Para gerar pdfs de qqplots e testes de normalidade
##Importar bibliotecas necessarias
library(broom)
##Obtem prefixos para os nomes dos dados (a tabela que vai ser usada) baseado na wave
get_data_prefix <- function(trajetoria) {
  if (trajetoria == "controles") {
    return("grupo_controles")
  } else if (trajetoria == "incidentes") {
    return("grupo_incidentes")
  } else if (trajetoria == "persistentes") {
    return("grupo_persistentes")
  } else if (trajetoria == "remitentes") {
    return("grupo_remitentes")
  } else {
    stop("Invalid argument. Must be either 'controles', 'incidentes', 'persistentes' or 'remitentes'.")
  }
}
##Gera os nomes de pdfs e dos dados que serao utilizados, usando os prefixos para os nomes dos dados obtidos no passo anterior
generate_names <- function() {
  trajetoria <- c("controles", "incidentes", "persistentes", "remitentes")
  csv_prefix <- "teste_norm"
  pdf_prefix <- "qq"
  
  col_names <- c("concentracao_real", "tamanho_mean_average", "tamanho_mode_average", "scaled_age")
  
  data_names <- sapply(trajetoria, function(trajetoria) paste0(get_data_prefix(trajetoria), "$", col_names))
  pdf_names <- sapply(trajetoria, function(trajetoria) paste0(pdf_prefix, "_", col_names, "_", trajetoria, ".pdf"))
  csv_names <- sapply(trajetoria, function(trajetoria) paste0(csv_prefix, "_", col_names, "_", trajetoria, ".csv"))
  return(list(data_names = data_names, pdf_names = pdf_names, csv_names = csv_names))
}
##Gera graficos dos qqs em pdf 
gera_qq_em_pdf <- function() {
  names <- generate_names()
  pdf_names <- names$pdf_names
  data_names <- names$data_names
  
  generate_plot <- function(pdf_name, data_name) {
    pdf(pdf_name)
    qqnorm(eval(parse(text = data_name), envir = parent.frame()))
    qqline(eval(parse(text = data_name), envir = parent.frame()))
    dev.off()
  }
  mapply(generate_plot, pdf_names, data_names)
}
## Faz testes de normalidade e gera csv deles
teste_norm_em_csv <- function() {
  names <- generate_names()
  csv_names <- names$csv_names
  data_names <- names$data_names
  
  generate_csv <- function(csv_name, data_name) {
    tidy(shapiro.test(eval(parse(text = data_name), envir = parent.frame()))) %>% write.csv(csv_name, row.names = FALSE)
  }
  mapply(generate_csv, csv_names, data_names)
}

