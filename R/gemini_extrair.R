#' Gera Texto com a API Google Gemini
#'
#' Esta função gera texto com base nas instruções e perguntas fornecidas, utilizando a API Google Gemini.
#'
#' @param x String. O texto base sobre o qual as instruções serão aplicadas.
#' @param instrucoes String. Instruções sobre o tema do texto a ser gerado. O padrão é "Escreva um texto sobre o seguinte tema:".
#' @param perguntas Vetor de Strings. Perguntas específicas para direcionar a geração do texto.
#' @param chaves Vetor de Strings. As chaves esperadas no resultado JSON.
#' @param api_key String. A chave da API Google Gemini. Se não for fornecida, a função tentará usar a variável de ambiente "GEMINI_KEY".
#' @param temperatura Numeric. Define a aleatoriedade do texto gerado. Valores mais baixos tornam o texto mais focado e determinístico (padrão = 0.7).
#' @param model_version String. A versão do modelo a ser usada (padrão = "gemini-pro").
#'
#' @return Lista. Retorna uma lista contendo as respostas geradas pela API.
#' @export
#'
#' @examples
#' \dontrun{
#' resultado <- gemini_extrair(
#'   x = "Power BI",
#'   instrucoes = "Texto de vendas sobre o tema",
#'   perguntas = c("Texto de vendas"),
#'   chaves = c("resposta"),
#'   api_key = "sua_api_key"
#' )
#' print(resultado)
#' }
#' 
gemini_extrair <- function(x,
                           instrucoes = NULL,
                           perguntas = NULL,
                           chaves = NULL,
                           api_key = NULL,
                           temperatura = 0.2,
                           model_version = "gemini-pro") {
  if (is.null(x) || !is.character(x) || length(x) == 0) {
    stop("A entrada 'x' deve ser uma string n\u00e3o nula.")
  }
  
  if (is.null(api_key)) {
    api_key <- Sys.getenv("GEMINI_KEY")
    
    if (api_key == "") {
      stop(
        "\u00c9 necess\u00e1rio uma chave de API do Google Gemini para usar esta fun\u00e7\u00e3o. Para obter uma chave, visite https://ai.google.dev/gemini-api/docs/api-key?hl=pt-br"
      )
      
    }
  }
  
  if (length(api_key) == 1) {
    stop("A chave GEMINI_KEY deve ser uma string n\\u00e3o nula.")
  }
  
  if (length(perguntas) != length(chaves)) {
    stop("Perguntas e chaves devem ter o mesmo tamanho.")
  }
  
  if (is.null(instrucoes)) {
    instrucoes <- "Escreva um texto sobre o seguinte tema:"
  }
  
  if (is.null(perguntas)) {
    perguntas <- "1. Qual \\u00e9 o tema do texto?"
  }
  
  if (is.null(chaves)) {
    chaves <- c("tema")
  }
  
  perguntas <- stringr::str_c(perguntas, collapse = "\n")
  colunas <- stringr::str_c(chaves, collapse = ", ")
  
  url <-
    paste0(
      "https://generativelanguage.googleapis.com/v1beta/models/",
      model_version,
      ":generateContent?key=",
      api_key
    )
  h <- c(`Content-Type` = "application/json")
  
  texto <-
    glue::glue(
      "{instrucoes}, demarcada por tr\\u00eas ap\\u00f3strofes: ```{x}```. Responda \\u00e0s seguintes perguntas:
                      {perguntas}
                      retorne as respostas em formato json com as seguintes chaves: {colunas}"
    )
  
  body <- list(
    contents = list(list(parts = list(list(
      text = texto
    )))),
    generation_config = list(temperature = temperatura)
  )
  
  i <- 1
  repeat {
    tryCatch({
      r1 <-
        url %>%
        httr2::request() %>%
        httr2::req_headers(!!!h) %>%
        httr2::req_body_json(body) %>%
        httr2::req_perform()
      
      if (httr2::resp_status(r1) == 200) {
        break
      } else if (httr2::resp_status(r1) == 429) {
        message("Muitas requisi\u00e7\u00f5es. Aguardando 1 minuto para continuar...")
        Sys.sleep(65)
      } else {
        stop("Erro na requisi\u00e7\u00e3o: ", httr2::resp_status(r1))
      }
    }, error = function(e) {
      if (grepl("HTTP 429", e$message)) {
        message("Muitas requisi\u00e7\u00e3o Aguardando 1 minuto para continuar...")
        Sys.sleep(65)
      } else {
        stop(e$message)
      }
    })
    
    if (i == 10) {
      message("N\u00famero m\u00e1ximo de tentativas alcan\u00e7ado")
      break
    }
    
    i <- i + 1
  }
  
  r1 %>%
    httr2::resp_body_json() %>%
    purrr::pluck("candidates", 1, "content", "parts", 1, "text") %>%
    stringr::str_extract("(?<=\n)\\{\\X+?\\}(?=\n``)")
}
