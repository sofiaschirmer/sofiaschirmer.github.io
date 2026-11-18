
# Lê publications.csv e escreve a lista agrupada por ano
# (mais recente primeiro)

render_pubs <- function(path = "publications.csv",
                        doi_label = "DOI") {
  
  if (!file.exists(path)) {
    stop("Arquivo não encontrado: ", path)
  }
  
  d <- read.csv(
    path,
    stringsAsFactors = FALSE,
    encoding = "UTF-8",
    check.names = FALSE
  )
  
  # Verifica as colunas
  required <- c("year", "authors", "title",
                "journal", "volume", "DOI")
  
  if (!all(required %in% names(d))) {
    stop("Confira as colunas do publications.csv")
  }
  
  # Ordena por ano
  d$year <- as.numeric(d$year)
  d <- d[order(-d$year), ]
  
  # Agrupa por ano
  for (y in unique(d$year)) {
    
    cat("\n## ", y, "\n\n", sep = "")
    
    for (i in which(d$year == y)) {
      
      journal <- trimws(d$journal[i])
      journal <- sub("[,;]+$", "", journal)
      
      volume <- trimws(d$volume[i])
      volume <- sub("\\.$", "", volume)
      
      doi <- trimws(d$DOI[i])
      
      link <- ""
      
      if (!is.na(doi) && nzchar(doi)) {
        link <- paste0(
          "[", doi_label,
          "](https://doi.org/", doi, ")"
        )
      }
      
      cat(
        "- ", d$authors[i],
        " (", y, "). **", d$title[i],
        "**. *", journal, "*. ",
        volume, ". ",
        link, "\n",
        sep = ""
      )
    }
  }
}

