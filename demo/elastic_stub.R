# Заглушка Elasticsearch для демо-режима.
#
# В продакшене справочники (пользователи, группы логистов, глоссарий,
# клиенты, перевозчики, ТС) читались из Elasticsearch через пакет
# elasticsearchr:  elastic(url, index) %search% (query(...) + select_fields(...)).
#
# В демо-режиме те же справочники лежат в PostgreSQL в схеме search_index —
# по одной таблице на индекс, колонки названы так же, как «сплющенные»
# поля документов ("data.id", "data.title", ...). Этот файл повторяет
# минимальный API elasticsearchr, чтобы helpers.R работал без изменений.

elastic <- function(cluster_url, index, doc_type = NULL) {
  structure(list(cluster_url = cluster_url, index = index), class = "elastic_rescource")
}

query <- function(json, size = 0) {
  structure(list(query = json), class = c("elastic_query", "elastic_api"))
}

select_fields <- function(json) {
  includes <- jsonlite::fromJSON(json)$includes
  structure(list(includes = includes), class = c("elastic_fields", "elastic_api"))
}

`+.elastic_api` <- function(e1, e2) {
  structure(c(unclass(e1), unclass(e2)), class = c("elastic_search", "elastic_api"))
}

`%search%` <- function(rescource, search) {
  stopifnot(inherits(rescource, "elastic_rescource"))
  table_name <- rescource$index
  sql <- sprintf('SELECT * FROM search_index."%s"', table_name)
  result <- DBI::dbGetQuery(con, sql)
  includes <- if (inherits(search, "elastic_api")) search$includes else NULL
  if (!is.null(includes)) {
    missing <- setdiff(includes, names(result))
    if (length(missing) > 0) {
      stop(sprintf("search_index.%s: нет колонок %s", table_name, paste(missing, collapse = ", ")))
    }
    result <- result[, includes, drop = FALSE]
  }
  result
}
