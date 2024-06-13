#' Lê e Combina Arquivos JSON
#'
#' Esta função lê múltiplos arquivos JSON de um diretório especificado e os combina em um único data frame.
#'
#' @param arquivos Vetor de strings. Caminhos para os arquivos JSON a serem lidos. Se NULL, todos os arquivos JSON no diretório serão lidos.
#' @param diretorio String. Diretório onde os arquivos JSON estão localizados. O padrão é o diretório atual.
#'
#' @return Um data frame contendo os dados combinados de todos os arquivos JSON.
#' @export
#'
#' @examples
#' \dontrun{
#' # Ler todos os arquivos JSON no diretório atual
#' df <- gemini_ler(diretorio = ".")
#'
#' # Ler arquivos JSON específicos
#' df <- gemini_ler(arquivos = c("file1.json", "file2.json"))
#' }
#' 
gemini_ler <- function(arquivos, diretorio = ".") {
  
  if (is.null(arquivos)) {
    arquivos <- list.files(diretorio, full.names = T)
  }
  
  purrr::map_dfr(arquivos, purrr::possibly( ~ {
    nome_arquivo <- basename(.x)
    
    if (file.size(.x) == 0) {
      message(paste("Arquivo vazio:", nome_arquivo))
    }
    
    jsonlite::fromJSON(.x) %>%
      purrr::map( ~ ifelse(is.null(.x), NA, .x)) %>%
      as.data.frame() %>%
      dplyr::mutate_all(as.character) %>%
      dplyr::mutate(arquivo = nome_arquivo, .before = 1)
    
  }, NULL),.progress = TRUE)
  
}                 
