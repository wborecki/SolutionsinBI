#' Processa ementas utilizando o Gemini e salva os resultados em arquivos JSON
#'
#' Esta função processa uma lista de ementas utilizando o serviço Gemini, extrai informações conforme instruções fornecidas, 
#' responde a perguntas específicas e salva os resultados em arquivos JSON. Se um identificador não for fornecido, ele será gerado automaticamente.
#' A função verifica e evita duplicatas baseando-se em arquivos existentes no diretório especificado.
#'
#' @param ementa Lista de ementas a serem processadas.
#' @param identificador Vetor opcional de identificadores para as ementas. Se não fornecido, será gerado automaticamente.
#' @param instrucoes Instruções específicas para o processamento das ementas.
#' @param perguntas Perguntas a serem respondidas com base nas ementas.
#' @param chaves Palavras-chave para a extração de informações.
#' @param api_key Chave da API do serviço Gemini. Se não fornecida, a função tentará usar a variável de ambiente GEMINI_KEY.
#' @param temperatura Parâmetro de temperatura para ajustar a aleatoriedade das respostas da IA. O valor padrão é 0.2.
#' @param diretorio Diretório onde os arquivos JSON serão salvos. O padrão é o diretório atual.
#'
#' @return Nenhum valor é retornado. Os resultados são salvos em arquivos JSON no diretório especificado.
#' @export
#'
#' @examples
#' \dontrun{
#' ementas <- c("Ementa 1...", "Ementa 2...")
#' instrucoes <- "Instruções gerais"
#' perguntas <- c("Pergunta 1", "Pergunta 2")
#' chaves <- c("Chave 1", "Chave 2")
#'
#' gemini_ementa(ementas, 
#'               instrucoes = "Instruções específicas", 
#'               perguntas = "Perguntas específicas", 
#'               chaves = "Palavras-chave",
#'               api_key = "SUA_API_KEY")
#' }
gemini_ementa <- function(ementa, 
                          identificador = NULL, 
                          instrucoes, 
                          perguntas, 
                          chaves, 
                          api_key = NULL, 
                          temperatura = 0.2,
                          diretorio = "." ) {
  
  # Verifica se o diretório existe; se não, cria-o
  if (!dir.exists(diretorio)) {
    dir.create(diretorio, recursive = TRUE)
  }
  
  # Se o identificador não for fornecido, cria uma sequência numérica
  if (is.null(identificador)) {
    id <- seq_along(ementa)
  } else {
    id <- identificador
  }
  
  # Verifica arquivos JSON existentes no diretório
  arquivos_existentes <- list.files(diretorio, full.names = TRUE, pattern = "IA_.*\\.json$")
  
  # Remove ementas existentes para evitar duplicatas
  if (length(arquivos_existentes) > 0) {
    ementa <- ementa[!id %in% stringr::str_extract(arquivos_existentes, "(?<=_)[^.]*")]
    id <- id[!id %in% stringr::str_extract(arquivos_existentes, "(?<=_)[^.]*")]
  }
  
  # Cria uma barra de progresso para acompanhar o processamento
  pb <- progress::progress_bar$new(
    format = "[:bar] :percent eta: :eta",  
    total = length(id),   
    clear = FALSE,   
    width = 60   
  )
  
  # Define o caminho do arquivo de saída
  arquivos <- file.path(diretorio, paste0("IA_", id, ".json"))
  
 # Itera sobre as listas 'ementa' e 'id', processando cada par
  purrr::walk2(ementa, arquivos, ~{
    
    pb$tick()  # Avança a barra de progresso
    
    # Processa a ementa utilizando a função 'gemini_extrair' e salva os dados em um arquivo JSON
    dados <- SolutionsinBI::gemini_extrair(
      x = .x,
      instrucoes = instrucoes,
      perguntas = perguntas,
      chaves = chaves,
      api_key = api_key,
      temperatura = temperatura
    ) %>%
      write(.y)  # Escreve os dados processados no arquivo
  })
}