# Читаем из конфигурационного файла
# TODO: оформить в функцию
main_config_file_path <- "./config.yml"
local_config_file_path <- "./config.local.yml"
main_config <- config::get(file = main_config_file_path)
if (!exists("DEMO_MODE")) DEMO_MODE <- tolower(Sys.getenv("ERP_REPORTS_DEMO", "false")) %in% c("true", "1", "yes")
# В демо-режиме локальный конфиг с боевыми адресами игнорируется —
# подключение целиком описывается переменными окружения (см. demo/run_demo.ps1)
if (!DEMO_MODE && file.exists(local_config_file_path)) {
  local_config <- config::get(file = local_config_file_path)
  cfg <- config::merge(main_config, local_config)
} else {
  cfg <- main_config
}


validate_report_config <- function(settings, demo_mode = FALSE) {
  required <- c("host", "port", "user", "dbname")
  present <- function(value) length(value) == 1L && !is.na(value) && nzchar(trimws(as.character(value)))
  for (database in c("db_erp", "db_profiles", "db_tenders", "db_auth")) {
    missing <- required[!vapply(required, function(field) present(settings[[database]][[field]]), logical(1))]
    if (length(missing)) {
      stop(sprintf("Missing database configuration: %s.%s", database, paste(missing, collapse = ", ")), call. = FALSE)
    }
    port <- suppressWarnings(as.integer(settings[[database]]$port))
    if (is.na(port) || port < 1L || port > 65535L) stop("Invalid database port configuration.", call. = FALSE)
  }
  if (!demo_mode && !present(settings$db_elastic$url)) {
    stop("Missing Elasticsearch configuration: db_elastic.url", call. = FALSE)
  }
  invisible(TRUE)
}
validate_report_config(cfg, DEMO_MODE)

# Подключаемся к PostgreSQL только после проверки явной конфигурации.
con = DBI::dbConnect(RPostgres::Postgres(),
                host = cfg$db_erp$host,
                user = cfg$db_erp$user,
                password = cfg$db_erp$password,
                dbname = cfg$db_erp$dbname,
                port = cfg$db_erp$port                
                )

con1 = dbConnect(RPostgres::Postgres(),
                host = cfg$db_profiles$host,
                user = cfg$db_profiles$user,
                password = cfg$db_profiles$password,
                dbname = cfg$db_profiles$dbname,
                port = cfg$db_profiles$port)

con2 = dbConnect(RPostgres::Postgres(),
                 host = cfg$db_tenders$host,
                 user = cfg$db_tenders$user,
                 password = cfg$db_tenders$password,
                 dbname = cfg$db_tenders$dbname,
                 port = cfg$db_tenders$port)

con3 = dbConnect(RPostgres::Postgres(),
                 host = cfg$db_auth$host,
                 user = cfg$db_auth$user,
                 password = cfg$db_auth$password,
                 dbname = cfg$db_auth$dbname,
                 port = cfg$db_auth$port)

# con_analytics <- dbConnect(RPostgres::Postgres(),
#                            host = cfg$db_analytics$host,
#                            user = cfg$db_analytics$user,
#                            password = cfg$db_analytics$password,
#                            dbname = cfg$db_analytics$dbname,
#                            port = cfg$db_analytics$port)


getProblemDocsTable <- function(){
  
  my_query <- query('{
"bool": {
      "must": [
        {
  "match": {"isActual": "true"}
        }
      ]
}
}')
  
  selected_field_expeditors_group <- select_fields('{
  "includes": ["data.id", "data.name"]
}')
  
  expeditors_group <- elastic(cfg$db_elastic$url,
                              "expeditors-group-version-index") %search% (my_query + selected_field_expeditors_group)
  
  requests <- dbGetQuery(con,
                         "SELECT requests.created_at, requests.responsible_expeditor_division,
           requests.responsible_expeditor_title,
           requests.charterer_rate_amount
           FROM erp_requests.requests
           LEFT JOIN erp_requests.transportations
           ON requests.id = transportations.request_id
           WHERE requests.is_actual=true AND
           requests.status='carrierFound' AND
           transportations.status='docsProblem';") %>%

    as_tibble() %>%

    transmute(Дата = as.Date(created_at),
                  Группа = responsible_expeditor_division,
                  Логист = responsible_expeditor_title,
                  Ставка_клиента = charterer_rate_amount)

  compare <- requests %>% 
    left_join(expeditors_group, by = c("Группа" = "data.id")) %>%
    select(Дата, Логист, data.name, Ставка_клиента) %>%
    rename("Группа" = 3) %>%
    arrange(desc(Дата))

  # Суммарная ставка клиента по дням и логистам
  static <- compare %>%

    group_by(Логист) %>%

    summarize(Группа = first(Группа), Ставка = round(sum(Ставка_клиента), digits = 0)) %>%
    arrange(desc(Ставка))

  static$Группа[is.na(static$Группа)] <- "Без группы"

  dynamics <- compare %>%

    group_by(Дата, Логист) %>%

    summarize(Группа = first(Группа), Ставка = round(sum(Ставка_клиента), digits = 0)) %>%
    arrange(Дата)

  resultList <- list("static" = static, "dynamics" = dynamics)

  return(resultList)
}

getMissingDocsTable <- function(){
  
  my_query <- query('{
"bool": {
      "must": [
        {
  "match": {"isActual": "true"}
        }
      ]
}
}')
  
  selected_field_expeditors_group <- select_fields('{
  "includes": ["data.id", "data.name"]
}')
  
  expeditors_group <- elastic(cfg$db_elastic$url,
                              "expeditors-group-version-index") %search% (my_query + selected_field_expeditors_group)
  
  requests = dbGetQuery(con,
                                 "SELECT requests.created_at,
                     requests.responsible_expeditor_division,
                     requests.responsible_expeditor_title,
                     transportations.carrier_version_framework_agreement_number,
                     inner_application_contract_charterer.category is not null inner_application_contract_charterer,
                     signed_application_contract_carrier.category is not null signed_application_contract_carrier
                     FROM erp_requests.requests
                   LEFT JOIN erp_requests.transportations
                   ON requests.id = transportations.request_id
          
                   LEFT JOIN erp_requests.transportation_documents as inner_application_contract_charterer
                    ON requests.id = inner_application_contract_charterer.transportation_request_id
                        AND inner_application_contract_charterer.category='inner_application_contract_charterer'
                   
                   LEFT JOIN erp_requests.transportation_documents as signed_application_contract_carrier
                    ON requests.id = signed_application_contract_carrier.transportation_request_id
                        AND signed_application_contract_carrier.category='signed_application_contract_carrier'
                   WHERE requests.is_actual=true
                   AND requests.created_at >= '2020-12-31 21:00:00'::timestamp
                    AND requests.status not in ('cancelled', 'transportationCancelled')
                   ORDER BY 1;") %>%

    as_tibble() %>%

    transmute(Дата = as.Date(created_at),
                  Группа = responsible_expeditor_division,
                  Логист = responsible_expeditor_title,
                  Рамочный_договор = carrier_version_framework_agreement_number,
                  Заявка = inner_application_contract_charterer,
                  Договор = signed_application_contract_carrier) %>%

    left_join(expeditors_group, by = c("Группа" = "data.id")) %>%

    select(Дата, Логист, data.name, Рамочный_договор, Заявка, Договор)

  colnames(requests) = c("Дата", "Логист", "Группа", "Рам.Договор", "Заявка", "Договор")


  # Число отсутствующих заявок клиентов по дням и логистам
  missing_applications_everyday = requests %>%

    filter(Заявка != TRUE) %>%

    group_by(Дата, Логист, Группа) %>%

    summarize(Дата = first(Дата), Логист = first(Логист), Группа = first(Группа), Отсутствует = n()) %>%
    arrange(desc(Отсутствует))

  # Число отсутствующих договоров перевозчиков по дням и логистам
  missing_contracts_everyday = requests %>%

    filter(Договор != TRUE & is.na(Рам.Договор)) %>%

    group_by(Дата, Логист, Группа) %>%

    summarize(Дата = first(Дата), Логист = first(Логист), Группа = first(Группа), Отсутствует = n()) %>%
    arrange(desc(Отсутствует))

  # Общее число заявок по дням и логистам
  general_everyday = requests %>%

    group_by(Дата, Логист, Группа) %>%

    summarize(Дата = first(Дата), Логист = first(Логист), Группа = first(Группа), Всего = n()) %>%
    arrange(desc(Всего))

  # Итоговая таблица
  result = general_everyday %>% left_join(missing_applications_everyday, by = c("Дата" = "Дата", "Логист" = "Логист", "Группа" = "Группа")) %>%
    mutate(Отношение = round(Отсутствует / Всего * 100, digits = 2 )) %>% rename(Группа = 3) %>%
    arrange(desc(Всего))

  result$Отсутствует[is.na(result$Отсутствует)] <- 0
  result$Отношение[is.na(result$Отношение)] <- 0

  # Итоговая таблица
  result1 = general_everyday %>% left_join(missing_contracts_everyday, by = c("Дата" = "Дата", "Логист" = "Логист", "Группа" = "Группа")) %>%
    mutate(Отношение = round(Отсутствует / Всего * 100, digits = 2 )) %>% rename(Группа = 3) %>%
    arrange(desc(Всего))

  result1$Отсутствует[is.na(result1$Отсутствует)] <- 0
  result1$Отношение[is.na(result1$Отношение)] <- 0

  resultList <- list("requests" = result, "contracts" = result1)

  return(resultList)

}

# Проверенные документы - кредиторы
getVerifiedDocs <- function(startDate, endDate){
  
  my_query <- query('{
"bool": {
      "must": [
        {
  "match": {"isActual": "true"}
        }
      ]
}
}')
  
  selected_field_user <- select_fields('{
  "includes": ["data.id", "data.title"]
}')
  
  user = elastic(cfg$db_elastic$url,
                 "user-profile-version-index") %search% (my_query + selected_field_user) %>%
    group_by(data.id) %>%
    summarise(data.title = first(data.title))

  verified_requests_by_day <- dbGetQuery(con,
                                         paste("SELECT requests.short_id,
                                         requests.charterer_version_title as client,
                                         transportations.IssueDateTime,
                                         do_specialist.title
           FROM erp_requests.requests
           INNER JOIN (SELECT request_id,
                       (jsonb_path_query_array(transportations.statuses_history, '$[*] ? (@.Status == \"docsProblem\" || @.Status == \"paid\" || @.Status == \"docsApproved\")')->0->>'IssueDateTime')::timestamp AS IssueDateTime,
                       jsonb_path_query_array(transportations.statuses_history, '$[*] ? (@.Status == \"docsProblem\" || @.Status == \"paid\" || @.Status == \"docsApproved\")')->0->>'UserId' AS UserId
           FROM erp_requests.transportations
           WHERE
           jsonb_path_exists(transportations.statuses_history::jsonb, '$[*] ? (@.Status == \"docsProblem\" || @.Status == \"paid\" || @.Status == \"docsApproved\")') = true

           ) as transportations

           ON requests.id = transportations.request_id
           
           left join erp_users.user_profiles as do_specialist
            on transportations.UserId = do_specialist.original_id 
            and do_specialist.is_actual = true
           
           WHERE requests.is_actual=true 
                   AND transportations.IssueDateTime >=", paste0("'", startDate, "'::timestamp"), "AND transportations.IssueDateTime <=", paste0("'",endDate, "'::timestamp;")
                                         )) %>%
  as_tibble()
  
  result <- verified_requests_by_day %>%
#    left_join(user, by = c("userid" = "data.id")) %>%
    transmute(Дата = as.Date(strftime(issuedatetime, format="%Y-%m-%d")),
                  Специалист = title,
                  Номер_заявки = short_id,
                  Клиент = client,
                  Тип = "Оригинал")
  # group_by(Дата, Специалист) %>%
  # summarize(Заявки = n(),
  #                 Номера_заявок = toString(unique(Номер_заявки)))
  
  verified_scans_by_day <- dbGetQuery(con,
                                      paste("SELECT requests.short_id,
                                      requests.charterer_version_title as client,
                                      jsonb_path_query_array(transportations.docs_scan_statuses_history, '$[*] ? (@.Status == \"approved\")')->0->>'IssueDateTime' AS IssueDateTime,
                                      do_specialist.title
           FROM erp_requests.requests
           INNER JOIN erp_requests.transportations
           ON requests.id = transportations.request_id 
           
            left join erp_users.user_profiles as do_specialist
            on jsonb_path_query_array(transportations.docs_scan_statuses_history, '$[*] ? (@.Status == \"approved\")')->0->>'UserId' = do_specialist.original_id 
            and do_specialist.is_actual = true
           
           WHERE requests.is_actual=true and
                                       transportations.docs_scan_status = 'approved'
                                            AND (jsonb_path_query_array(transportations.docs_scan_statuses_history, '$[*] ? (@.Status == \"docsProblem\" || @.Status == \"paid\" || @.Status == \"docsApproved\")')->0->>'IssueDateTime')::timestamp >=", paste0("'", startDate, "'::timestamp"), "AND (jsonb_path_query_array(transportations.docs_scan_statuses_history, '$[*] ? (@.Status == \"docsProblem\" || @.Status == \"paid\" || @.Status == \"docsApproved\")')->0->>'IssueDateTime')::timestamp <=", paste0("'",endDate, "'::timestamp;")
                                            )) %>%
  
    as_tibble()
  
  result_scan <- verified_scans_by_day %>%
#    left_join(user, by = c("userid" = "data.id")) %>%
    transmute(Дата = as.Date(strftime(issuedatetime, format="%Y-%m-%d")),
                  Специалист = title,
                  Номер_заявки = short_id,
                  Клиент = client,
                  Тип = "Скан")
  # group_by(Дата, Специалист) %>%
  # summarize(Заявки = n(),
  #                 Номера_заявок = toString(unique(Номер_заявки)))
  
  origins_scans <- rbind(result, result_scan) %>%
    arrange(Дата) %>%
    group_by(Специалист, Клиент, Номер_заявки) %>%
    summarize(Тип = first(Тип))
  
  result_general <- origins_scans %>%
    left_join(rbind(result, result_scan), by = c("Специалист" = "Специалист", "Клиент" = "Клиент", "Номер_заявки" = "Номер_заявки", "Тип" = "Тип"))
  
  result <- result_general %>%
    filter(Тип == "Оригинал") %>%
    group_by(Дата, Специалист, Клиент) %>%
    summarize(Заявки = n(),
                    Номера_заявок = toString(unique(Номер_заявки)))
  
  result_scan <- result_general %>%
    filter(Тип == "Скан") %>%
    group_by(Дата, Специалист, Клиент) %>%
    summarize(Заявки = n(),
                    Номера_заявок = toString(unique(Номер_заявки)))
  
  result_general <- result %>%
    left_join(result_scan, by = c("Специалист" = "Специалист", "Клиент" = "Клиент", "Дата" = "Дата"))
  
  result_general$Заявки.x[is.na(result_general$Заявки.x)] <- 0
  result_general$Заявки.y[is.na(result_general$Заявки.y)] <- 0
  
  result_general$Номера_заявок.x[is.na(result_general$Номера_заявок.x)] <- ""
  result_general$Номера_заявок.y[is.na(result_general$Номера_заявок.y)] <- ""
  
  result_general <- result_general %>% mutate(Всего = as.numeric(Заявки.x) + as.numeric(Заявки.y)) %>%
    rename("Оригиналы" = 4, "Номера_оригиналов" = 5, "Сканы" = 6, "Номера_сканов" = 7) %>%
    filter(Специалист != "System")
}

# Выставленные счета - дебиторы
getBills <- function(startDate, endDate){

  # специалисты документооборота — из индекса пользователей (раньше список uuid был захардкожен)
  workflow_specialist_1C <- elastic(cfg$db_elastic$url, "user-profile-version-index") %search%
    (query('{"bool": {"must": [{"match": {"isActual": "true"}}]}}') + select_fields('{"includes": ["data.id", "data.title"]}')) %>%
    group_by(data.id) %>% summarise(data.title = first(data.title))

  bills_by_day <- dbGetQuery(con,
                             paste("select requests.charterer_version_title,
                               requests.short_id,
                               bills.number_doc,
                               bills.invoice_amount,
                               bills.created_at_original,
                               bills.created_at,
                               bills.user_id
                               
FROM erp_requests.requests

INNER JOIN (select bills.number_doc,
                   bills.invoice_amount,
                   bills.created_at_original,
                   bills.created_at,
                   bills.user_id,
                   unnest(bills.request_ids) as request_id 

            from erp_bills.bills where bills.is_actual AND bills.type = 'Charterer') as bills

ON requests.id = bills.request_id 

where requests.is_actual
                   AND bills.created_at >=", paste0("'", startDate, "'::timestamp"), "AND bills.created_at <=", paste0("'", endDate, "'::timestamp;"))) %>%

    as_tibble() %>%

    transmute(Клиент = charterer_version_title,
                    Номер_заявки = short_id,
                    Номер_счёта = number_doc,
                    Сумма_счёта = invoice_amount,
                    Дата = as.Date(strftime(created_at_original, format="%Y-%m-%d")),
                    ДатаВыс = as.Date(strftime(created_at, format="%Y-%m-%d")),
                    Дебитор = user_id) %>%

    left_join(workflow_specialist_1C, by = c("Дебитор" = "data.id")) %>%
    select(-Дебитор) %>%
    rename("Специалист" = 7) %>%
    group_by(Клиент, Номер_счёта, Сумма_счёта, Дата, ДатаВыс, Специалист) %>%
    summarize(Клиент = first(Клиент),
                    Номер_счёта = first(Номер_счёта),
                    Сумма_счёта = first(Сумма_счёта),
                    Дата = first(Дата),
                    ДатаВыс = first(ДатаВыс),
                    Заявки = n(),
                    Номера_заявок = toString(unique(Номер_заявки)),
                    Специалист = first(Специалист)
    ) %>%
    arrange(Дата, Клиент) %>%
    mutate(Количество_счетов = 1)
}

getDZ_new_flow <- function(){
  my_query <- query('{
"bool": {
      "must": [
        {
  "match": {"isActual": "true"}
        }
      ]
}
}')
  
  selected_field_user <- select_fields('{
  "includes": ["data.id", "data.title"]
}')
  
  user = elastic(cfg$db_elastic$url,
                 "user-profile-version-index") %search% (my_query + selected_field_user) %>%
    group_by(data.id) %>%
    summarise(data.title = first(data.title))
  
  bills_by_day <- dbGetQuery(con,
                             "SELECT
         bills.client_juridical_person_short_title,
         bills.client_juridical_person_responsible_for_bill_processing_id,
         bills.client_account_manager_id,
         bills.send_date,
         bills.delivery_date,
         bills.verification_date,
         bills.return_date,
         bills.number,
         bills.amount,
         bills.formed_at,
         bills.planned_payment_date,
         clients.grace_of_payments_days,
         clients.tsd_settings_doc_verification_days,
         requests.short_id,
         requests.client_rate
         
FROM document_flow.bills

LEFT JOIN (SELECT clients.id, 
            juridicals.grace_of_payments_days, 
            juridicals.tsd_settings_doc_verification_days
            FROM erp_clients.clients
            INNER JOIN erp_clients.juridicals
            ON clients.id = juridicals.client_id 
            AND clients.version = juridicals.client_version 
            WHERE clients.is_actual = TRUE
            ) as clients
ON bills.client_id = clients.id

left join (select bills.number,
				  string_agg(requests.short_id, ', ') as short_id,
				  sum(requests.client_rate) as client_rate
				  
				  from document_flow.bills
				  left join document_flow.requests
				  on bills.id = requests.bill_id and bills.is_actual and requests.is_actual
				  
				  group by number
				  
			) as requests
on bills.number = requests.number


 WHERE bills.is_actual = TRUE AND bills.is_paid = FALSE") %>%
    
    as_tibble() %>%
    
    left_join(user, by = c("client_juridical_person_responsible_for_bill_processing_id" = "data.id")) %>%
    
    left_join(user, by = c("client_account_manager_id" = "data.id")) %>%
    
    transmute(Клиент = client_juridical_person_short_title,
                    Менеджер = data.title.y,
                    Номер_счёта = number,
                    Дата_счёта = as.Date(strftime(formed_at, format="%Y-%m-%d")),
                    Сумма_счёта = amount,
                    Сотрудник = data.title.x,
                    Дата_отпр = as.Date(strftime(send_date, format="%Y-%m-%d")),
                    Дата_дос = as.Date(strftime(delivery_date, format="%Y-%m-%d")),
                    Отсрочка_платежа = grace_of_payments_days,
                    Сроки_вериф = tsd_settings_doc_verification_days,
                    Дата_вер = as.Date(strftime(verification_date, format="%Y-%m-%d")),                    
                    Верификация_счёта_плановая_дата = Дата_дос + Сроки_вериф,
                    # Номер_заявки = short_id,
                    # Ставка_клиента = client_rate,
                    # Дата_возвр = as.Date(strftime(return_date, format="%Y-%m-%d")),
                    Просрочка_по_верификации = case_when(
                      is.na(Дата_вер) ~ as.numeric(Sys.Date() - Верификация_счёта_плановая_дата),
                      !is.na(Дата_вер) ~ as.numeric(Дата_вер - Верификация_счёта_плановая_дата)
                    ),
                    Дата_оплаты_по_договору = case_when(
                      is.na(planned_payment_date) ~ Дата_дос + Отсрочка_платежа,
                      !is.na(planned_payment_date) ~ as.Date(strftime(planned_payment_date, format="%Y-%m-%d"))),
                    ПДЗ = as.numeric(Sys.Date() - Дата_оплаты_по_договору),
                    Категория = case_when(
                      as.numeric(ПДЗ) > 0 & as.numeric(ПДЗ) <8 ~ "B. До 7 дней",
                      as.numeric(ПДЗ) >= 8 & as.numeric(ПДЗ) <=15 ~ "C. От 8 до 15 дней",
                      as.numeric(ПДЗ) > 15 & as.numeric(ПДЗ) <=30 ~ "D. От 16 до 30 дней",
                      as.numeric(ПДЗ) > 30 & as.numeric(ПДЗ) <=60 ~ "F. От 31 до 60 дней",
                      as.numeric(ПДЗ) > 60 & as.numeric(ПДЗ) <=90 ~ "G. От 61 до 90 дней",
                      as.numeric(ПДЗ) > 90 ~ "H. Свыше 90 дней",
                      is.na(ПДЗ) | as.numeric(ПДЗ) <= 0 ~ "A. Дебиторская задолженность"),
                    Источник = "Вкладка «Счета»"
                    ) %>%
    select(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 14, 16, 17)
}

# Дебиторская задолженность
getDZ <- function(){
  
  my_query <- query('{
"bool": {
      "must": [
        {
  "match": {"isActual": "true"}
        }
      ]
}
}')
  
  selected_field_user <- select_fields('{
  "includes": ["data.id", "data.title"]
}')
  
  user = elastic(cfg$db_elastic$url,
                 "user-profile-version-index") %search% (my_query + selected_field_user) %>%
    group_by(data.id) %>%
    summarise(data.title = first(data.title))

  bills_by_day <- dbGetQuery(con,
                             "select DISTINCT ON (bills.number_doc)
         documents_packets.created_by,
         documents_packets.send_date,
         documents_packets.delivery_date,
         documents_packets.verification_date,
         documents_packets.returned_date,
         requests.short_id,
         bills.number_doc,
         bills.invoice_amount,
         bills.created_at_original,
         bills.created_at,
         requests.charterer_rate_amount,
         juridical_persons.short_title,
         juridical_persons.agreement_number,
         juridical_persons.signed_by_uid,
         juridical_persons.grace_of_payments_days,
         juridical_persons.tsd_settings_doc_verification_days,
         bills.status,
         bills.date
 
 FROM erp_requests.requests
 
right join (
    select
        bfor.date,
        unnest(erp_bills.request_ids) as request_id,
        erp_bills.status,
         erp_bills.number_doc,
         erp_bills.invoice_amount,
         erp_bills.created_at_original,
         erp_bills.created_at
    from erp_bills.bills erp_bills
    
    left join (
        select
            distinct on (bfor.id, bfor.bill_id)
            id, date, bill_id
        from (
            select id, date, unnest(bill_ids) as bill_id from erp_bills.factoring_of_receivables
        ) bfor
    ) as bfor on erp_bills.id::uuid = bfor.bill_id
    where erp_bills.is_actual and erp_bills.type = 'Charterer'
) as bills
on requests.id = bills.request_id
 
 left JOIN (
 	 select 
 	     document_packets_requests.id,
 	 	 documents_packets.created_by,
         documents_packets.send_date,
         documents_packets.delivery_date,
         documents_packets.verification_date,
         documents_packets.returned_date
     from erp_documents_packets.document_packets_requests
     inner join erp_documents_packets.documents_packets
     ON document_packets_requests.documents_packet_id = documents_packets.id 
     AND document_packets_requests.documents_packet_version = documents_packets.version
     and documents_packets.is_actual
 ) as documents_packets
 ON requests.id = documents_packets.id AND requests.is_actual    
 
 
 left join (
    select
        juridical_persons.id,
        juridical_persons.short_title,
        juridical_persons.agreement->>'Number' AS agreement_number,
        clients.signed_by_uid,
        juridicals.grace_of_payments_days,
        juridicals.tsd_settings_doc_verification_days
    from erp_juridical_persons.juridical_persons
    left join erp_clients.juridicals on juridical_persons.id = juridicals.juridical_person_id and juridical_persons.version = juridicals.juridical_person_version
    left join erp_clients.clients on juridicals.client_id = clients.id and juridicals.client_version = clients.version
    where juridical_persons.is_actual and clients.is_actual=true
) as juridical_persons
on requests.juridical_person_version_id = juridical_persons.id
 
 WHERE bills.status in ('PartialPaid', 'ForPayment', 'NotPaid') AND requests.is_actual;") %>%
    as_tibble() %>%
    
    left_join(user, by = c("created_by" = "data.id")) %>%
    
    left_join(user, by = c("signed_by_uid" = "data.id")) %>%

    transmute(Клиент = short_title,
                    Договор = agreement_number,
                    Номер_заявки = short_id,
                    Номер_счёта = number_doc,
                    Сумма_счёта = invoice_amount,
                    Дата_счёта = as.Date(strftime(created_at_original, format="%Y-%m-%d")),
                    Дата_выставления_счёта = as.Date(strftime(created_at, format="%Y-%m-%d")),
                    Ставка_клиента = charterer_rate_amount,
                    Дата_отпр = as.Date(strftime(send_date, format="%Y-%m-%d")),
                    Дата_дос = as.Date(strftime(delivery_date, format="%Y-%m-%d")),
                    Дата_возвр = as.Date(strftime(returned_date, format="%Y-%m-%d")),
                    Дата_вер = as.Date(strftime(verification_date, format="%Y-%m-%d")),
                    Сотрудник = data.title.x,
                    Менеджер = data.title.y,
                    Отсрочка_платежа = grace_of_payments_days,
                    Сроки_вериф = tsd_settings_doc_verification_days,
                    Статус = status
    ) %>%
    
    transmute(Клиент = Клиент,
                    Менеджер = Менеджер,
                    Договор = Договор,
                    Номер_заявки = Номер_заявки,
                    Номер_счёта = Номер_счёта,
                    Дата_счёта = Дата_счёта,
                    Дата_выставления_счёта = Дата_выставления_счёта,
                    Сумма_счёта = Сумма_счёта,
                    Ставка_клиента = Ставка_клиента,
                    Разница_по_суммам = round(Сумма_счёта - Ставка_клиента, digits = 2),
                    Отсрочка_платежа = Отсрочка_платежа,
                    Сроки_вериф = Сроки_вериф,
                    Дата_отпр = Дата_отпр,
                    Дата_дос = Дата_дос,
                    Дата_возвр = Дата_возвр,
                    Верификация_счёта_плановая_дата = Дата_дос + Сроки_вериф,
                    Дата_вер = Дата_вер,
                    Просрочка_по_верификации = case_when(
                      is.na(Дата_вер) ~ as.numeric(Sys.Date() - Верификация_счёта_плановая_дата),
                      !is.na(Дата_вер) ~ as.numeric(Дата_вер - Верификация_счёта_плановая_дата)
                    ),
                    Дата_оплаты_по_договору = Дата_дос + Отсрочка_платежа,
                    Частично_оплачен = case_when(Статус == "PartialPaid" ~ "Да",
                                                       Статус != "PartialPaid" ~ "Нет"),
                    ПДЗ = as.numeric(Sys.Date() - Дата_оплаты_по_договору),
                    Неделя_для_оплат = format(Дата_оплаты_по_договору, "%V"),
                    Сотрудник = Сотрудник,
                    Категория = case_when(
                      as.numeric(ПДЗ) > 0 & as.numeric(ПДЗ) <8 ~ "B. До 7 дней",
                      as.numeric(ПДЗ) >= 8 & as.numeric(ПДЗ) <=15 ~ "C. От 8 до 15 дней",
                      as.numeric(ПДЗ) > 15 & as.numeric(ПДЗ) <=30 ~ "D. От 16 до 30 дней",
                      as.numeric(ПДЗ) > 30 & as.numeric(ПДЗ) <=60 ~ "F. От 31 до 60 дней",
                      as.numeric(ПДЗ) > 60 & as.numeric(ПДЗ) <=90 ~ "G. От 61 до 90 дней",
                      as.numeric(ПДЗ) > 90 ~ "H. Свыше 90 дней",
                      is.na(ПДЗ) | as.numeric(ПДЗ) <= 0 ~ "A. Дебиторская задолженность"),
                    Источник = "Вкладка «Документооборот»"
    ) %>%
    arrange(Дата_счёта, Менеджер, Клиент) %>%
    
    select(1, 2, 5, 6, 8, 23, 13, 14, 11, 12, 17, 18, 19, 24, 25)
    # select(1, 2, 5, 6, 8, 23, 7, 13, 14, 11, 12, 17, 18, 19, 24)

  bills_by_day$Менеджер[is.na(bills_by_day$Менеджер)] <- "Менеджер не указан"
  # bills_by_day$Дата_дос[is.na(bills_by_day$Дата_дос)] <- "Не доставлен"
  # bills_by_day$Дата_дос[is.na(bills_by_day$Дата_дос)] <- "Возврата не было"
  # bills_by_day$Дата_вер[is.na(bills_by_day$Дата_вер)] <- "Не верифицирован"

  # bills_by_day$Категория <- cut(as.numeric(bills_by_day$ПДЗ), c(-Inf, 0, 8, 15, 30, Inf), labels=c("ДЗ", "От 0 до 8 дней", "От 8 до 15 дней", "От 15 до 30 дней", "От 30 до 60 дней"))
  # bills_by_day$Категория[is.na(bills_by_day$Категория)] <- "Ещё не доставлены клиенту"


  return(bills_by_day)

}

dz_result <- function() {
  rbind(getDZ(), getDZ_new_flow()) %>%
    
    group_by(Номер_счёта) %>%
    
    summarise(Клиент = last(Клиент),
                  Менеджер = last(Менеджер),
                  Дата_счёта = last(Дата_счёта),
                  Сумма_счёта = last(Сумма_счёта),
                  Сотрудник = last(Сотрудник),
                  Дата_отпр = last(Дата_отпр),
                  Дата_дос = last(Дата_дос),
                  Отсрочка_платежа = last(Отсрочка_платежа),
                  Сроки_вериф = last(Сроки_вериф),
                  Дата_вер = last(Дата_вер),                    
                  Просрочка_по_верификации = last(Просрочка_по_верификации),
                  Дата_оплаты_по_договору = last(Дата_оплаты_по_договору),
                  Категория = last(Категория),
                  Источник = last(Источник)
  ) %>% 
    select(2, 3, 1, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)
}

# Сорванные рейсы
getBreakdowns <- function(){
  
  my_query <- query('{
"bool": {
      "must": [
        {
  "match": {"isActual": "true"}
        }
      ]
}
}')
  
  selected_field_expeditors_group <- select_fields('{
  "includes": ["data.id", "data.name"]
}')
  
  expeditors_group <- elastic(cfg$db_elastic$url,
                              "expeditors-group-version-index") %search% (my_query + selected_field_expeditors_group)
  
  requests_all <- dbGetQuery(con, "SELECT requests.relative_request_short_id
                                        FROM erp_requests.requests WHERE
                                        requests.is_actual = true;")
  
  breakdowns_by_day <- dbGetQuery(con,
                                  "SELECT jsonb_path_query_first(requests.tsd, '$[*] ? (@.Key == \"torg_2_discrepancy_act\")') AS torg_2_discrepancy_act,
                                jsonb_path_query_array(transportations.statuses_history, '$[*] ? (@.Status == \"docsProblem\")')->-1->>'IssueDateTime' AS docsProblem,
                                requests.created_at,
                             requests.short_id,
                             requests.external_number,
                             requests.juridical_person_version_short_title as client,
                             requests.type,
                     requests.responsible_expeditor_division,
                     requests.responsible_expeditor_title,
                     requests.charterer_rate_amount,
                     requests.notes->-1->>'Text' AS notes,
                     requests.vehicle_requirements_characteristic_preset,
                     requests.cargo_brutto_in_tonns,
                     requests.cargo_volume_in_cubometres,
                     requests.status,
                     requests.trip_comment,
                     requests.relative_request_short_id,
                     requests.cancellation_info_has_complaint_work,
                     first_loading.locality as start_point,
                     first_loading.from_at as loading_plan,
                     last_unloading.locality as finish_point,
                     transportations.juridical_person_version_short_title as carrier,
                     transportations.carrier_rate_info_amount,
                     transportations.prepayment_amount,
                     transportations.carrier_rate_info_with_nds,
                     transportations.profit_amount,
                     transportations.carrier_version_framework_agreement_number,
                     inner_application_contract_charterer.category is not null inner_application_contract_charterer,
                     signed_application_contract_carrier.category is not null signed_application_contract_carrier,

                     juridical_persons.inn,
                     juridical_persons.juridical_address

                     FROM erp_requests.requests
                   LEFT JOIN erp_requests.transportations
                   ON requests.id = transportations.request_id

                   LEFT JOIN erp_requests.transportation_documents as inner_application_contract_charterer
                    ON requests.id = inner_application_contract_charterer.transportation_request_id
                        AND inner_application_contract_charterer.category='inner_application_contract_charterer'
                   
                   LEFT JOIN erp_requests.transportation_documents as signed_application_contract_carrier
                    ON requests.id = signed_application_contract_carrier.transportation_request_id
                        AND signed_application_contract_carrier.category='signed_application_contract_carrier'
                   
left join (
    select distinct on (s.request_id)
          request_id, id, locality, \"from\" as from_at
    from erp_requests.request_start_route_points s
    order by s.request_id desc, s.from asc
) as first_loading
on requests.id = first_loading.request_id

left join (
    select distinct on (s.request_id)
          request_id, id, locality, \"from\" as from_at
    from erp_requests.request_finish_route_points s
    order by s.request_id desc, s.from desc
) as last_unloading
on requests.id = last_unloading.request_id

                    LEFT JOIN (SELECT transportations.request_id, juridical_persons.id, juridical_persons.version, juridical_persons.inn, juridical_persons.juridical_address FROM erp_juridical_persons.juridical_persons INNER JOIN erp_requests.transportations ON juridical_persons.id = transportations.juridical_person_version_id AND juridical_persons.is_actual) as juridical_persons
                    ON requests.id = juridical_persons.request_id

                   WHERE requests.is_actual=true
                    AND requests.status in ('cancelled', 'transportationCancelled', 'active');") %>%

    as_tibble() %>%

    left_join(expeditors_group, by = c("responsible_expeditor_division" = "data.id")) %>%

    transmute(Номер_заявки = short_id,
                          Внешний_номер_заявки = external_number,
                          Дата_создания_заявки = as.Date(strftime(created_at, format="%Y-%m-%d")),
                          Клиент = client,
                          Тип_заявки = case_when(type == "Spot" ~ "Спот",
                                                 type == "Warranty" ~ "Гарантия"),
                          Статус_заявки = case_when(status == "cancelled" ~ "Заявка отменена",
                                                    status == "transportationCancelled" ~ "Перевозка отменена",
                                                    status == "active" ~ "Заявка просрочена"),
                          Статус_обработки = case_when(!stringr::str_detect(notes, 'Обработано|обработано') ~ "Не обработано",
                                                       stringr::str_detect(notes, 'Обработано|обработано') ~ "Обработано"),
                          Наличие_заявки_клиента = case_when(inner_application_contract_charterer == TRUE ~ "Есть",
                                                             inner_application_contract_charterer == FALSE ~ "Отсутствует"),
                          Наличие_договора_заявки_с_перевоз = case_when(signed_application_contract_carrier == TRUE & is.na(carrier_version_framework_agreement_number) ~ "Есть",
                                                                        signed_application_contract_carrier == FALSE & is.na(carrier_version_framework_agreement_number) ~ "Отсутствует"),
                          Наличие_договора_заявки_с_перевозом = case_when(!is.null(carrier_version_framework_agreement_number) ~ "-"),
                          Место_погрузки = start_point,
                          Место_выгрузки = finish_point,
                          Тип_ТС = vehicle_requirements_characteristic_preset,
                          Тоннаж_ТС = cargo_brutto_in_tonns,
                          Объём_ТС = cargo_volume_in_cubometres,
                          Ответственный_логист = responsible_expeditor_title,
                          Старший_группы = data.name,
                          Перевозчик = carrier,
                          ИНН_перевозчика = inn,
                          Адрес_перевозчика = juridical_address,
                          Ставка_клиента = charterer_rate_amount,
                          Ставка_перевозчика = carrier_rate_info_amount,
                          Сумма_предоплаты = prepayment_amount,
                          Маржа = profit_amount,
                          НДС = case_when(carrier_rate_info_with_nds == TRUE ~ "с НДС",
                                          carrier_rate_info_with_nds == FALSE ~ "без НДС"),
                          Рентабельность = round(profit_amount / charterer_rate_amount * 100, digits = 2),
                          ТСД = as.Date(docsproblem),
                          Претензия = case_when(cancellation_info_has_complaint_work == TRUE ~ "Да",
                                                cancellation_info_has_complaint_work == FALSE ~ "Нет"),
                          Торг = case_when(is.na(torg_2_discrepancy_act) ~ "Отсутствует",
                                           TRUE ~ "Есть"),
                          Заметки = notes,
                          Причина_создания = trip_comment,
                          Связанная_заявка = relative_request_short_id,
                          Дата_загрузки = as.Date(strftime(loading_plan, format="%Y-%m-%d"))
    ) %>%
    
    filter(!is.na(Ответственный_логист)) %>%
    
    filter((Статус_заявки == "Заявка отменена" & is.na(Связанная_заявка)) | (Статус_заявки == "Перевозка отменена" & is.na(Связанная_заявка)) | (is.na(Связанная_заявка) & Статус_заявки == "Заявка просрочена" & (Sys.Date() - Дата_загрузки > 0))) %>%
     
    select(-Дата_загрузки)
  
  canceling_byDay <- breakdowns_by_day %>%
    anti_join(requests_all, by = c("Номер_заявки" = "relative_request_short_id")) 
}

# Выработка СБ - водители
getSB <- function(startDate, endDate){
  
  my_query <- query('{
"bool": {
      "must": [
        {
  "match": {"isActual": "true"}
        }
      ]
}
}')
  
  selected_field_user <- select_fields('{
  "includes": ["data.id", "data.title"]
}')
  
  user = elastic(cfg$db_elastic$url,
                 "user-profile-version-index") %search% (my_query + selected_field_user) %>%
    group_by(data.id) %>%
    summarise(data.title = first(data.title))

  # Водители, создание
  drivers_creation =
    dbGetQuery(con1,
               paste("SELECT created_at,
             created_by
             FROM profiles.drivers
               WHERE created_at >=", startDate,
                     "AND created_at <=", endDate,
                     ";")) %>%

    mutate(Время = as.POSIXct(created_at / 1000, origin = "1970-01-01")) %>%

    inner_join(user, by = c("created_by" = "data.id")) %>%

    transmute(Дата = as.Date(strftime(Время, format="%Y-%m-%d")),
                  Сотрудник = data.title) %>%

    group_by(Дата, Сотрудник) %>%

    summarise(Водители = n())
  
  
  # Водители, изменение
  drivers_change1 =
    dbGetQuery(con1,
               paste("SELECT updated_at,
             last_updated_by
             FROM profiles.drivers
               WHERE updated_at >=", startDate,
                     "AND updated_at <=", endDate,
                     ";")) %>%
    
    mutate(Время = as.POSIXct(updated_at / 1000, origin = "1970-01-01")) %>%
    
    inner_join(user, by = c("last_updated_by" = "data.id")) %>%
    
    transmute(Дата = as.Date(strftime(Время, format="%Y-%m-%d")),
                  Сотрудник = data.title) %>%
    
    group_by(Дата, Сотрудник) %>%
    
    summarise(Водители = n())


  # Водители, изменение
  drivers_change =
    dbGetQuery(con1,
               paste("SELECT updated_at,
             last_updated_by
             FROM profiles.archived_drivers
               WHERE updated_at >=", startDate,
                     "AND updated_at <=", endDate,
                     ";")) %>%

    mutate(Время = as.POSIXct(updated_at / 1000, origin = "1970-01-01")) %>%

    inner_join(user, by = c("last_updated_by" = "data.id")) %>%

    transmute(Дата = as.Date(strftime(Время, format="%Y-%m-%d")),
                  Сотрудник = data.title) %>%

    group_by(Дата, Сотрудник) %>%

    summarise(Водители = n())

  drivers_result <- drivers_creation %>%
    full_join(drivers_change, by = c("Дата" = "Дата", "Сотрудник" = "Сотрудник"))  %>%
    full_join(drivers_change1, by = c("Дата" = "Дата", "Сотрудник" = "Сотрудник"))
    

  drivers_result$Водители.x[is.na(drivers_result$Водители.x)] <- 0
  drivers_result$Водители.y[is.na(drivers_result$Водители.y)] <- 0
  drivers_result$Водители[is.na(drivers_result$Водители)] <- 0
  
  drivers_result <- drivers_result %>%
    transmute(Дата = Дата,
                  Сотрудник = Сотрудник,
                  Всего_водители = Водители.x + Водители.y + Водители,
                  Создано_водители = Водители.x,
                  Изменено_водители = Водители.y + Водители
    )

  print("Водители получены")  
  
  return(drivers_result)
}


# Выработка СБ - перевозчики 
get_SB_carriers <- function(startDate, endDate){
  
  my_query <- query('{
"bool": {
      "must": [
        {
  "match": {"isActual": "true"}
        }
      ]
}
}')
  
  selected_field_user <- select_fields('{
  "includes": ["data.id", "data.title"]
}')
  
  user = elastic(cfg$db_elastic$url,
                 "user-profile-version-index") %search% (my_query + selected_field_user) %>%
    group_by(data.id) %>%
    summarise(data.title = first(data.title))
  
  # Перевозчики, создание
  carriers_creation =
    dbGetQuery(con,
               paste("SELECT (created_at AT TIME ZONE 'Europe/Moscow')::date as \"Дата создания\",
             created_by as \"Специалист СБ\"
             FROM erp_carriers.carriers
               WHERE created_at >=", paste0("'", startDate, "'::timestamp"),
                     "AND created_at <=", paste0("'", endDate, "'::timestamp"),
                     "AND is_actual;")) %>%
    
    inner_join(user, by = c("Специалист СБ" = "data.id")) %>%
    
    transmute(Дата = `Дата создания`,
                  Сотрудник = data.title) %>%
    
    group_by(Дата, Сотрудник) %>%
    
    summarise(Перевозчики = n())
  
  
  # Перевозчики, дата последнего изменения
  carriers_change1 =
    dbGetQuery(con,
               paste("SELECT (updated_at AT TIME ZONE 'Europe/Moscow')::date as \"Дата изменения\",
             updated_by as \"Специалист СБ\"
             FROM erp_carriers.carriers
               WHERE updated_at >=", paste0("'", startDate, "'::timestamp"),
                     "AND updated_at <=", paste0("'", endDate, "'::timestamp"),
                     "AND is_actual;")) %>%
    
    inner_join(user, by = c("Специалист СБ" = "data.id")) %>%
    
    transmute(Дата = `Дата изменения`,
                  Сотрудник = data.title) %>%
    
    group_by(Дата, Сотрудник) %>%
    
    summarise(Перевозчики = n())
  
  
  # Перевозчики, даты предыдущих изменений
  carriers_change =
    dbGetQuery(con,
               paste("SELECT (updated_at AT TIME ZONE 'Europe/Moscow')::date as \"Дата изменения\",
             updated_by as \"Специалист СБ\"
             FROM erp_carriers.carriers
               WHERE updated_at >=", paste0("'", startDate, "'::timestamp"),
                     "AND updated_at <=", paste0("'", endDate, "'::timestamp"),
                     "AND NOT is_actual;")) %>%
    
    inner_join(user, by = c("Специалист СБ" = "data.id")) %>%
    
    transmute(Дата = `Дата изменения`,
                  Сотрудник = data.title) %>%
    
    group_by(Дата, Сотрудник) %>%
    
    summarise(Перевозчики = n())
  
  carriers_result <- carriers_creation %>%
    full_join(carriers_change, by = c("Дата" = "Дата", "Сотрудник" = "Сотрудник"))  %>%
    full_join(carriers_change1, by = c("Дата" = "Дата", "Сотрудник" = "Сотрудник"))
  
  
  carriers_result$Перевозчики.x[is.na(carriers_result$Перевозчики.x)] <- 0
  carriers_result$Перевозчики.y[is.na(carriers_result$Перевозчики.y)] <- 0
  carriers_result$Перевозчики[is.na(carriers_result$Перевозчики)] <- 0
  
  carriers_result <- carriers_result %>%
    transmute(Дата = Дата,
                  Сотрудник = Сотрудник,
                  Всего_перевозчики = Перевозчики.x + Перевозчики.y + Перевозчики,
                  Создано_перевозчики = Перевозчики.x,
                  Изменено_перевозчики = Перевозчики.y + Перевозчики
    )
  
  print("Перевозчики получены")  
  
  return(carriers_result)
}


# Выработка СБ - ТС 
get_SB_vehicles <- function(startDate, endDate){
  
  my_query <- query('{
"bool": {
      "must": [
        {
  "match": {"isActual": "true"}
        }
      ]
}
}')
  
  selected_field_user <- select_fields('{
  "includes": ["data.id", "data.title"]
}')
  
  user = elastic(cfg$db_elastic$url,
                 "user-profile-version-index") %search% (my_query + selected_field_user) %>%
    group_by(data.id) %>%
    summarise(data.title = first(data.title))
  
  # ТС, создание
  vehicles_creation =
    dbGetQuery(con,
               paste("SELECT (created_at AT TIME ZONE 'Europe/Moscow')::date as \"Дата создания\",
             created_by as \"Специалист СБ\"
             FROM erp_vehicles.vehicles
               WHERE created_at >=", paste0("'", startDate, "'::timestamp"),
                     "AND created_at <=", paste0("'", endDate, "'::timestamp"),
                     "AND is_actual;")) %>%
    
    inner_join(user, by = c("Специалист СБ" = "data.id")) %>%
    
    transmute(Дата = `Дата создания`,
                  Сотрудник = data.title) %>%
    
    group_by(Дата, Сотрудник) %>%
    
    summarise(ТС = n())
  
  
  # ТС, дата последнего изменения
  vehicles_change1 =
    dbGetQuery(con,
               paste("SELECT (updated_at AT TIME ZONE 'Europe/Moscow')::date as \"Дата изменения\",
             updated_by as \"Специалист СБ\"
             FROM erp_vehicles.vehicles
               WHERE updated_at >=", paste0("'", startDate, "'::timestamp"),
                     "AND updated_at <=", paste0("'", endDate, "'::timestamp"),
                     "AND is_actual;")) %>%
    
    inner_join(user, by = c("Специалист СБ" = "data.id")) %>%
    
    transmute(Дата = `Дата изменения`,
                  Сотрудник = data.title) %>%
    
    group_by(Дата, Сотрудник) %>%
    
    summarise(ТС = n())
  
  
  # ТС, даты предыдущих изменений
  vehicles_change =
    dbGetQuery(con,
               paste("SELECT (updated_at AT TIME ZONE 'Europe/Moscow')::date as \"Дата изменения\",
             updated_by as \"Специалист СБ\"
             FROM erp_vehicles.vehicles
               WHERE updated_at >=", paste0("'", startDate, "'::timestamp"),
                     "AND updated_at <=", paste0("'", endDate, "'::timestamp"),
                     "AND NOT is_actual;")) %>%
    
    inner_join(user, by = c("Специалист СБ" = "data.id")) %>%
    
    transmute(Дата = `Дата изменения`,
                  Сотрудник = data.title) %>%
    
    group_by(Дата, Сотрудник) %>%
    
    summarise(ТС = n())
  
  vehicles_result <- vehicles_creation %>%
    full_join(vehicles_change, by = c("Дата" = "Дата", "Сотрудник" = "Сотрудник"))  %>%
    full_join(vehicles_change1, by = c("Дата" = "Дата", "Сотрудник" = "Сотрудник"))
  
  
  vehicles_result$ТС.x[is.na(vehicles_result$ТС.x)] <- 0
  vehicles_result$ТС.y[is.na(vehicles_result$ТС.y)] <- 0
  vehicles_result$ТС[is.na(vehicles_result$ТС)] <- 0
  
  vehicles_result <- vehicles_result %>%
    transmute(Дата = Дата,
                  Сотрудник = Сотрудник,
                  Всего_тс = ТС.x + ТС.y + ТС,
                  Создано_тс = ТС.x,
                  Изменено_тс = ТС.y + ТС
    )
  
  print("ТС получены")  
  
  return(vehicles_result)
}



# Конверсия перевозчиков
getNewOldCarriers <- function(){
  requests <- dbGetQuery(con,
                         "SELECT
                                requests.created_at,
                                requests.short_id,
                                requests.responsible_expeditor_title,
                                transportations.juridical_person_version_short_title
                     FROM erp_requests.requests
                     INNER JOIN erp_requests.transportations
                     ON requests.id = transportations.request_id
                     WHERE requests.is_actual=true
                     AND requests.status not in ('cancelled', 'transportationCancelled');") %>%

    as_tibble() %>%

    transmute(Дата = as.Date(created_at),
                  Номер = short_id,
                  Логист = responsible_expeditor_title,
                  Перевозчик = juridical_person_version_short_title) %>%
    arrange(Дата)

  new_carriers <- requests %>%
    group_by(Перевозчик) %>%
    summarise(Дата = first(Дата),
                  Номер = first(Номер),
                  Логист = first(Логист)) %>%
    mutate(Статус = "Новый")

  result <- requests %>%
    left_join(new_carriers, by = c("Перевозчик" = "Перевозчик",
                                   "Дата" = "Дата",
                                   "Номер" = "Номер",
                                   "Логист" = "Логист"))

  result$Статус[is.na(result$Статус)] <- "Существующий"

  return(result)
}


# Рейсы - запрос к slave
getRequests_slave <- function(startDate, endDate, date_type){
  
  my_query <- query('{
"bool": {
      "must": [
        {
  "match": {"isActual": "true"}
        }
      ]
}
}')
  
  selected_field_expeditors_group <- select_fields('{
  "includes": ["data.id", "data.name"]
}')
  
  expeditors_group <- elastic(cfg$db_elastic$url,
                              "expeditors-group-version-index") %search% (my_query + selected_field_expeditors_group)
  
  selected_field_user <- select_fields('{
  "includes": ["data.id", "data.title"]
}')
  
  user = elastic(cfg$db_elastic$url,
                 "user-profile-version-index") %search% (my_query + selected_field_user) %>%
    group_by(data.id) %>%
    summarise(data.title = first(data.title))
  
  #   carrier_selected_field <- select_fields('{
  #   "includes": ["data.id", "data.createdAt", "data.type"]
  # }')
  #   
  #   # Перевозчики
  #   carriers = elastic(cfg$db_elastic$url,
  #                      "carrier-version-index") %search% (my_query + carrier_selected_field)
  
  #   vehicles_selected_field <- select_fields('{
  #   "includes": ["id", "data.typesOfBody", "data.payload", "data.cubicCapacity"]
  # }')
  #   
  #   vehicles <- elastic(cfg$db_elastic$url,
  #                       "vehicle-version-index") %search% (my_query + vehicles_selected_field) %>%
  #     mutate(data.typesOfBody = as.character(data.typesOfBody))
  
    glossary_selected_field <- select_fields('{
    "includes": ["data.key", "data.title"]
  }')

    glossary <- elastic(cfg$db_elastic$url,
                        "glossary-element-version-index") %search% (my_query + glossary_selected_field)
  
    requests_by_day <- DBI::dbGetQuery(con,
                                   paste("SELECT distinct on (\"Номер заявки\") * 
                                   from (SELECT requests.short_id as \"Номер заявки\",
requests.external_number as \"Внешний номер заявки\",
(requests.created_at AT TIME ZONE 'Europe/Moscow')::date as \"Дата создания заявки\",
requests.charterer_version_title as Клиент,
juridical_persons.short_title as \"Юридическое лицо клиента\",
juridical_persons.agreement->>'Number' as \"Номер договора с клиентом\",

(case
 when requests.type = 'Warranty' then 'Гарантия'
 when requests.type = 'Spot' then 'Спот'
 end) as \"Тип заявки\",

(case
 when first_loading.region = last_unloading.region then 'Регион'
 else 'Межрегион'
 end) as География,

(case
 when requests.status = 'carrierFound' then 'Перевозчик найден'
 when requests.status = 'paid' then 'Клиент всё оплатил'
 when (requests.status = 'active') and (now()::date - first_loading.from_at::date > 0) then 'Заявка просрочена'
 when (requests.status = 'active') and (now()::date - first_loading.from_at::date <= 0) then 'Заявка активна'
 when requests.status = 'cancelled' then 'Заявка отменена'
 when requests.status = 'transportationCancelled' then 'Перевозка отменена'
 when requests.status = 'docsReceived' then 'Документы получены клиентом'
 when requests.status = 'docsApproved' then 'Документы подтверждены клиентом'
 when requests.status = 'docsGrouped' then 'Документы готовы к отправке'
 when requests.status = 'docsProblem' then 'Есть проблемы с документами'
 when requests.status = 'docsSent' then 'Документы высланы клиенту'

 end) as \"Статус заявки\",

(case
 when transportations.status = 'ready' then 'Перевозка началась'
 when transportations.status = 'onStartPoint' then 'Водитель прибыл на загрузку'
 when transportations.status = 'loaded' then 'Водитель в пути'
 when transportations.status = 'onTheWay' then 'Пройдена половина пути, всё хорошо'
 when transportations.status = 'onFinishPoint' then 'Водитель прибыл на разгрузку'
 when transportations.status = 'unloaded' then 'Водитель разгрузился, ждём документы'
 when transportations.status = 'docsReceived' then 'Получены документы от водителя'
 when transportations.status = 'paid' then 'Перевозка оплачена'
 when transportations.status = 'cancelled' then 'Перевозка отменена'
 when transportations.status = 'docsProblem' then 'У водителя проблемы с документами'
 when transportations.status = 'docsApproved' then 'Документы водителя подтверждены'

 end) as \"Статус перевозки\",

(case
 when inner_application_contract_charterer.category is not null = TRUE then 'Есть'
 else 'Отсутствует'
 end) as \"Наличие заявки клиента (прикреплена)\",

(case
 when signed_application_contract_carrier.category is not null AND transportations.carrier_version_framework_agreement_number IS NULL then 'Есть'
 when signed_application_contract_carrier.category is null AND transportations.carrier_version_framework_agreement_number IS NULL then 'Отсутствует'
 when transportations.carrier_version_framework_agreement_number is not null = true then '-'
 end) as \"Наличие подписанного договора-заявки с перевозчиком (прикреплен)\",

(case
 when transportations.carrier_version_framework_agreement_number is not null = TRUE then transportations.carrier_version_framework_agreement_number
 else '-'
 end) as \"Наличие заявки клиента (прикреплена)\",

first_loading.region as \"Регион погрузки\",
first_loading.locality as \"Место или город погрузки\",
last_unloading.region as \"Регион выгрузки\",
last_unloading.locality as \"Место или город выгрузки\",
requests.vehicle_requirements_types_of_body[1] as \"Тип ТС из заявки клиента\",
requests.vehicle_requirements_carrying_capacity_and_volume as \"Объём / Тоннаж ТС из заявки клиента\",
requests.cargo_cost as \"Стоимость груза\",

requests.created_by as \"Создатель заявки\",
transportations.created_by as \"Создатель перевозки\",
requests.responsible_expeditor_title as \"Ответственный логист\",
requests.responsible_expeditor_division as \"Старший группы\",

transportations.juridical_person_version_short_title as \"Юридическое лицо перевозчика\",
transportation_driver_versions.full_name as \"ФИО водителя\",
transportation_driver_versions.phone_numbers as \"Номер телефона водителя\",
truck_vehicle.number_plate as \"Номер тягача\",
trailer_vehicle.number_plate as \"Номер полуприцепа\",

(first_loading.from_at AT TIME ZONE 'Europe/Moscow')::date as \"Дата загрузки, план\",
(first_loading.from_at AT TIME ZONE 'Europe/Moscow')::time as \"Время загрузки, план\",

to_char(first_loading.fact_arrival_date_time AT TIME ZONE 'Europe/Moscow', 'DD.MM.YYYY HH24:MI:SS') as \"Дата загрузки, факт\",

(last_unloading.from_at AT TIME ZONE 'Europe/Moscow')::date as \"Дата выгрузки, план\",
(last_unloading.from_at AT TIME ZONE 'Europe/Moscow')::time as \"Время выгрузки, план\",

to_char(last_unloading.fact_departure_date_time AT TIME ZONE 'Europe/Moscow', 'DD.MM.YYYY HH24:MI:SS') as \"Дата выгрузки, факт\",

last_unloading.consignee as \"Грузополучатель\",
concat(first_loading.address, ' - ', last_unloading.address) as Маршрут,

requests.charterer_rate_amount as \"Ставка клиента\",

(case
 when requests.charterer_version_title ilike '%НДС%' then requests.charterer_rate_amount
 else requests.charterer_rate_amount / 1.2
 end) as \"Ставка клиента очищенная\",

transportations.carrier_rate_info_amount as \"Ставка перевозчика\",

(case
 when transportations.carrier_rate_info_with_nds = TRUE then transportations.carrier_rate_info_amount / 1.2
 else transportations.carrier_rate_info_amount
 end) as \"Ставка перевозчика очищенная\",

transportations.prepayment_amount as \"Сумма предоплаты перевозчику\",
transportations.profit_amount as \"Маржа очищенная, ERP\",

(case
 when requests.charterer_version_title ilike '%НДС%' then requests.charterer_rate_amount
 else requests.charterer_rate_amount / 1.2
 end) - (case
         when transportations.carrier_rate_info_with_nds = TRUE then transportations.carrier_rate_info_amount / 1.2
         else transportations.carrier_rate_info_amount
         end) as \"Маржа очищенная, расчёт\",

(case
 when transportations.carrier_rate_info_with_nds = TRUE then 'с НДС'
 when transportations.carrier_rate_info_with_nds = FALSE then 'без НДС'
 end) as \"НДС / без НДС (по перевозчику)\",

COALESCE(transportations.profit_amount / NULLIF(requests.charterer_rate_amount, 0), 0) * 100 as \"Рентабельность, ERP\",

COALESCE(((case
           when requests.charterer_version_title ilike '%НДС%' then requests.charterer_rate_amount
           else requests.charterer_rate_amount / 1.2
           end) - (case
                   when transportations.carrier_rate_info_with_nds = TRUE then transportations.carrier_rate_info_amount / 1.2
                   else transportations.carrier_rate_info_amount
                   end)) / NULLIF((case
                                   when requests.charterer_version_title ilike '%НДС%' then requests.charterer_rate_amount
                                   else requests.charterer_rate_amount / 1.2
                                   end), 0), 0) * 100 as \"Рентабельность очищенная, расчёт\",


transportations.grace_period as \"Отсрочка платежа перевозчику, бд\",
requests.charterer_version_doc_received_days as \"Срок сдачи ТСД перевозчика, кд\",
(requests.shipping_documents_info_planned_doc_received_date AT TIME ZONE 'Europe/Moscow')::date as \"Дата сдачи ТСД расчётная\",
(requests.shipping_documents_info_docs_received_date AT TIME ZONE 'Europe/Moscow')::date as \"Дата сдачи ТСД факт\",

(case
 when requests.shipping_documents_info_docs_received_date is not null then requests.shipping_documents_info_planned_doc_received_date::date - requests.shipping_documents_info_docs_received_date::date
 when requests.shipping_documents_info_docs_received_date is null then now()::date - requests.shipping_documents_info_planned_doc_received_date::date
 end) as \"Отклонение срока сдачи ТСД\",

((jsonb_path_query_array(transportations.statuses_history, '$[*] ? (@.Status == \"docsProblem\")')->-1->>'IssueDateTime')::timestamp AT TIME ZONE 'Europe/Moscow')::date AS \"Проблема с ТСД, дата\",
jsonb_path_query_array(transportations.statuses_history, '$[*] ? (@.Status == \"docsProblem\")')->-1->>'UserId' AS \"Автор проблемы\",
jsonb_path_query_array(transportations.statuses_history, '$[*] ? (@.Status == \"docsProblem\")')->-1->>'Reason' AS \"Комментарий проблемы\",


(case
 when jsonb_path_query_first(requests.tsd, '$[*] ? (@.Key == \"torg_2_discrepancy_act\")') is not null then 'Есть'
 when jsonb_path_query_first(requests.tsd, '$[*] ? (@.Key == \"torg_2_discrepancy_act\")') is null then 'Нет'
 end) as \"Наличие Торг-2\",

requests.charterer_certificate_info_number as \"Номер акта клиента в 1С\",
(requests.charterer_certificate_info_created_at AT TIME ZONE 'Europe/Moscow')::date as \"Дата акта клиента в 1С\",
(requests.charterer_certificate_info_ready_at AT TIME ZONE 'Europe/Moscow')::date as \"Дата проведения акта клиента в 1С\",

(transportations.carrier_grace_of_payment_finish_grace_period_date AT TIME ZONE 'Europe/Moscow')::date as \"Дата оплаты перевозчику - расчетная\",
transportations.carrier_certificate_info_number as \"Номер акта перевозчика в 1С\",
(transportations.carrier_certificate_info_created_at AT TIME ZONE 'Europe/Moscow')::date as \"Дата акта перевозчика в 1С\",
(transportations.carrier_certificate_info_ready_at AT TIME ZONE 'Europe/Moscow')::date as \"Дата проведения акта перевозчика в 1С\",
(transportations.carrier_bill_info_paid_at AT TIME ZONE 'Europe/Moscow')::date as \"Дата оплаты перевозчику - факт, 1С\",

requests.notes->-1->>'Text' AS \"Последняя заметка\",
requests.trip_comment as \"Причина создания\",
requests.relative_request_short_id as \"Связанная заявка\",

juridical_persons.responsible_employee_processing_doc_id as Кредитор,
juridical_persons.responsible_employee_processing_accounts_id as Дебитор,
requests.charterer_version_signed_by_uid as Менеджер,
juridical_persons.responsible_assistant_id as \"Ассистент менеджера\",

(case
 when bills.status = 'Paid' then 'Оплачено'
 else 'Не оплачено'
 end) as \"Статус оплаты клиентом\",

(case
 when transportations.docs_scan_status = 'approved' then 'Да'
 else '-'
 end) as \"Проверено по сканам\",

(case
 when bills.date is not null then 'Да'
 else 'Нет'
 end) as \"Зафакторено\",

(bills.date AT TIME ZONE 'Europe/Moscow')::date as \"Дата факторинга\",

requests.charterer_description as \"Комментарий клиента\",

(case
  when transportations.prepayment_type is null then 'Без предоплаты'
  when transportations.prepayment_type IN ('Fast', 'FastPostpayment') then 'Быстрая'
  when transportations.prepayment_type = 'Regular' then 'Обычная'
  else transportations.prepayment_type
end) as \"Тип предоплаты\",

transportations.carrier_rate_info_commission_in_percent as \"Комиссия перевозчика в %\",

transportations.carrier_rate_info_original_amount as \"Исходная ставка перевозчика\",

(transportations.created_at AT TIME ZONE 'Europe/Moscow')::date as \"Дата создания перевозки\",

(carriers.created_at AT TIME ZONE 'Europe/Moscow')::date as \"Дата создания перевозчика\"

FROM erp_requests.requests

left join (
  select transportations.request_id, carriers.created_at 
  from erp_requests.transportations
  left join erp_carriers.carriers
  on transportations.carrier_version_id = carriers.id
  and is_actual = TRUE
) as carriers
on requests.id = carriers.request_id

left join (
  select
  bfor.date,
  unnest(erp_bills.request_ids) as request_id,
  erp_bills.status
  from erp_bills.bills erp_bills
  left join (
    select
    distinct on (bfor.id, bfor.bill_id)
    id, date, bill_id
    from (
      select id, date, unnest(bill_ids) as bill_id from erp_bills.factoring_of_receivables
    ) bfor
  ) as bfor on erp_bills.id::uuid = bfor.bill_id
  where erp_bills.is_actual and erp_bills.type = 'Charterer'
) as bills
on requests.id = bills.request_id

LEFT JOIN erp_requests.transportations
ON requests.id = transportations.request_id

LEFT JOIN erp_requests.transportation_documents as inner_application_contract_charterer
ON requests.id = inner_application_contract_charterer.transportation_request_id
AND inner_application_contract_charterer.category='inner_application_contract_charterer'

LEFT JOIN erp_requests.transportation_documents as signed_application_contract_carrier
ON requests.id = signed_application_contract_carrier.transportation_request_id
AND signed_application_contract_carrier.category='signed_application_contract_carrier'

left join (
  select distinct on (s.request_id)
  request_id, id, locality, address, \"from\" as from_at, fact_arrival_date_time, region
  from erp_requests.request_start_route_points s
  order by s.request_id desc, s.from asc
) as first_loading
on requests.id = first_loading.request_id

left join (
  select distinct on (s.request_id)
  request_id, id, locality, address, \"from\" as from_at, fact_arrival_date_time, region, consignee, fact_departure_date_time
  from erp_requests.request_finish_route_points s
  order by s.request_id desc, s.from desc
) as last_unloading
on requests.id = last_unloading.request_id

LEFT JOIN erp_requests.transportation_driver_versions
ON requests.id = transportation_driver_versions.transportation_request_id

LEFT JOIN erp_requests.transportation_vehicle_versions as truck_vehicle
ON requests.id = truck_vehicle.transportation_request_id
AND (truck_vehicle.type ='tractor' OR truck_vehicle.type ='truck')

LEFT JOIN erp_requests.transportation_vehicle_versions as trailer_vehicle
ON requests.id = trailer_vehicle.transportation_request_id
AND (trailer_vehicle.type ='trailer' OR trailer_vehicle.type ='semitrailer')

left join (
  select
  juridical_persons.id,
  juridical_persons.version,
  juridical_persons.is_actual,
  juridical_persons.agreement,
  juridical_persons.short_title,
  juridicals.client_id,
  juridicals.juridical_person_id,
  juridicals.id as juridical_id,
  juridicals.responsible_employee_processing_doc_id,
  responsible_employee_processing_accounts_id,
  responsible_assistant_id
  from erp_juridical_persons.juridical_persons
  left join erp_clients.juridicals on juridical_persons.id = juridicals.juridical_person_id and juridical_persons.version = juridicals.juridical_person_version
  left join erp_clients.clients on juridicals.client_id = clients.id and juridicals.client_version = clients.version
  where juridical_persons.is_actual and clients.is_actual=true
) as juridical_persons
on requests.juridical_person_version_id = juridical_persons.id

WHERE requests.is_actual=true
                   AND", date_type, ">= $1", "AND", date_type, "<= $2",
                                         "ORDER BY", date_type, ") as main_req"
                                   ), params = list(startDate, endDate)) %>%
      
      left_join(expeditors_group, by = c("Старший группы" = "data.id")) %>%
      
      mutate("Старший группы" = data.name) %>% select(-data.name) %>%
      
      left_join(glossary, by = c("Тип ТС из заявки клиента" = "data.key")) %>% 
      
      mutate("Тип ТС из заявки клиента" = data.title) %>% select(-data.title) %>%
      
      left_join(glossary, by = c("Объём / Тоннаж ТС из заявки клиента" = "data.key")) %>% 
      
      mutate("Объём / Тоннаж ТС из заявки клиента" = gsub("<sup>3</sup>", "\U00B3 ", data.title)) %>% select(-data.title) %>%
      
      left_join(user, by = c("Автор проблемы" = "data.id")) %>%
      
      mutate("Автор проблемы" = data.title) %>% select(-data.title) %>%
      
      left_join(user, by = c("Кредитор" = "data.id")) %>%
      
      mutate("Кредитор" = data.title) %>% select(-data.title) %>%
      
      left_join(user, by = c("Дебитор" = "data.id")) %>%
      
      mutate("Дебитор" = data.title) %>% select(-data.title) %>%
      
      left_join(user, by = c("Менеджер" = "data.id")) %>%
      
      mutate("Менеджер" = data.title) %>% select(-data.title) %>%
      
      left_join(user, by = c("Создатель заявки" = "data.id")) %>%
      
      mutate("Создатель заявки" = data.title) %>% select(-data.title) %>%
      
      left_join(user, by = c("Создатель перевозки" = "data.id")) %>%
      
      mutate("Создатель перевозки" = data.title) %>% select(-data.title) %>%
      
      left_join(user, by = c("Ассистент менеджера" = "data.id")) %>%
      
      mutate("Ассистент менеджера" = data.title) %>% select(-data.title)
  
}

# Открытые рейсы
getActualRequests <- function(){
  
  my_query <- query('{
"bool": {
      "must": [
        {
  "match": {"isActual": "true"}
        }
      ]
}
}')
  
  glossary_selected_field <- select_fields('{
  "includes": ["data.key", "data.title"]
}')
  
  glossary <- elastic(cfg$db_elastic$url,
                      "glossary-element-version-index") %search% (my_query + glossary_selected_field)

  requests_by_day <- dbGetQuery(con,
                                "SELECT requests.short_id,
                                requests.juridical_person_version_short_title as client,
                     requests.responsible_expeditor_title,
                     requests.charterer_rate_amount,
                     requests.cargo_cargo_type,
                     requests.cargo_brutto_in_tonns,
                     requests.cargo_volume_in_cubometres,
                     requests.vehicle_requirements_types_of_body,
                     requests.vehicle_requirements_carrying_capacity_and_volume,
                     first_loading.locality as start_point,
                     first_loading.from_at as loading_plan,
                     last_unloading.locality as finish_point
                     
                     FROM erp_requests.requests
                     
left join (
    select distinct on (s.request_id)
          request_id, id, locality, address, \"from\" as from_at, fact_arrival_date_time, region
    from erp_requests.request_start_route_points s
    order by s.request_id desc, s.from asc
) as first_loading
on requests.id = first_loading.request_id

left join (
    select distinct on (s.request_id)
          request_id, id, locality, address, \"from\" as from_at, fact_arrival_date_time, region, consignee, fact_departure_date_time
    from erp_requests.request_finish_route_points s
    order by s.request_id desc, s.from desc
) as last_unloading
on requests.id = last_unloading.request_id
                     
                     WHERE requests.is_actual=true
                                      AND requests.created_at >= '2022-01-31 21:00:00'::timestamp 
                                AND requests.status = 'active';") %>%
    as_tibble() %>%

    arrange(loading_plan) %>%
    
    mutate(vehicle_requirements_types_of_body = as.character(gsub('\\{|\\}|\\"|\\"', "", vehicle_requirements_types_of_body))) %>%

    # mutate(vehicle_requirements_types_of_body = unlist(strsplit(vehicle_requirements_types_of_body, split=","))[1]) %>%

    left_join(glossary, by = c("cargo_cargo_type" = "data.key")) %>%

    left_join(glossary, by = c("vehicle_requirements_carrying_capacity_and_volume" = "data.key")) %>%

    left_join(glossary, by = c("vehicle_requirements_types_of_body" = "data.key")) %>%

    transmute(Номер_заявки = short_id,
                          Клиент = client,
                          Ответственный_логист = responsible_expeditor_title,
                          Дата_загрузки_план = as.Date(strftime(loading_plan, format="%Y-%m-%d")),
                          Время_загрузки_план = strftime(loading_plan, format="%H:%M:%S"),
                          Маршрут = paste(start_point, "\U2192", finish_point),
                          Груз = case_when(!is.na(data.title.x) ~ paste(data.title.x, "/", cargo_volume_in_cubometres, "м\U00B3 /", cargo_brutto_in_tonns, "тонн"),
                                           TRUE ~ paste("-", "/", cargo_volume_in_cubometres, "м\U00B3 /", cargo_brutto_in_tonns, "тонн")),
                          ТС = paste(data.title, "/", gsub("<sup>3</sup>", "\U00B3 ", data.title.y)),
                          Ставка_клиента = charterer_rate_amount,
    )  %>%

    filter(!is.na(Ответственный_логист))

}

#####################################
### Выработка логистов - BI-отчёт ###
#####################################


# Справочник «логист → группа → должность». Раньше был захардкожен здесь,
# теперь читается из базы (в демо — search_index.logist_groups).
expeditors_group <- dbGetQuery(con, 'select "data.id", "data.name", "position" as "Должность" from search_index.logist_groups')

# По дате создания
getLogisticKPI <- function(startDate, endDate){

  #   my_query <- query('{
  # "bool": {
  #       "must": [
  #         {
  #   "match": {"isActual": "true"}
  #         }
  #       ]
  # }
  # }')
  #
  #   selected_field_expeditors_group <- select_fields('{
  #   "includes": ["data.id", "data.name"]
  # }')
  #
  #   expeditors_group <- elastic(cfg$db_elastic$url,
  #                               "expeditors-group-version-index") %search% (my_query + selected_field_expeditors_group)

  requests <- dbGetQuery(con,
                             paste("SELECT date_trunc('day', transportations.created_at AT TIME ZONE 'Europe/Moscow') as Дата,
                     logist.title as Логист,
                     requests.juridical_person_version_short_title as Клиент,
                     count(*) AS Рейсы,
                     SUM(requests.charterer_rate_amount) as Оборот,
              	    SUM(transportations.profit_amount) as Маржа,
					COALESCE(SUM(transportations.profit_amount) / SUM(NULLIF(requests.charterer_rate_amount, 0)), 0) as Рентабельность,

                  SUM(case
              		when signed_application_contract_carrier.category is not null AND transportations.carrier_version_framework_agreement_number IS NULL then 1
              		when signed_application_contract_carrier.category is null AND transportations.carrier_version_framework_agreement_number IS NULL then 0
              	    end) as Договоры

                     FROM erp_requests.requests

                   LEFT JOIN erp_requests.transportations
                   ON requests.id = transportations.request_id

           LEFT JOIN erp_requests.transportation_documents as signed_application_contract_carrier
            ON requests.id = signed_application_contract_carrier.transportation_request_id
                AND signed_application_contract_carrier.category='signed_application_contract_carrier'
                
left join erp_users.user_profiles as logist
on requests.responsible_expeditor_id = logist.original_id 
and logist.is_actual = true

                   WHERE requests.is_actual=true
                   AND requests.status not in ('cancelled', 'transportationCancelled', 'active')
                   AND transportations.created_at >=", paste0("'", startDate, "'::timestamp"), "AND transportations.created_at <=", paste0("'",endDate, "'::timestamp"),

                                   "group by Дата, Логист, Клиент;")) %>%

    as_tibble() %>%
    
    left_join(expeditors_group, by = c("Логист" = "data.id")) %>%

    transmute(Рейсы = as.numeric(Рейсы),
                          Дата = as.Date(strftime(Дата, format="%Y-%m-%d")),
                          Клиент = Клиент,
                          Группа = data.name,
                          Логист = Логист,
                          Должность = Должность,
                          Оборот = Оборот,
                          # Ставка_перевозчика = case_when(nds == TRUE ~ carrier_rate_info_amount/ 1.2,
                          #                                nds == FALSE ~ carrier_rate_info_amount),
                          # Маржа_расчёт = Ставка_клиента - Ставка_перевозчика,
                          # Рентабельность_расчёт = Маржа_расчёт / Ставка_клиента
                          Маржа = Маржа,
                          Рентабельность = Рентабельность,
                          Договоры = Договоры) %>%

    mutate(Должность = case_when(Должность == "ст. логист" ~ "логист",
                                          TRUE ~ Должность))
    
  requests$Группа[is.na(requests$Группа)] <- "Без группы"
  
  # requests[mapply(is.infinite, requests)] <- 0

  return(requests)

}


# По дате загрузки
getLogisticKPI1 <- function(startDate, endDate){

  #   my_query <- query('{
  # "bool": {
  #       "must": [
  #         {
  #   "match": {"isActual": "true"}
  #         }
  #       ]
  # }
  # }')
  #
  #   selected_field_expeditors_group <- select_fields('{
  #   "includes": ["data.id", "data.name"]
  # }')
  #
  #   expeditors_group <- elastic(cfg$db_elastic$url,
  #                               "expeditors-group-version-index") %search% (my_query + selected_field_expeditors_group)

  requests <- dbGetQuery(con,
                         paste("SELECT date_trunc('day', first_loading.from_at AT TIME ZONE 'Europe/Moscow') as Дата,
                     logist.title as Логист,
                     requests.juridical_person_version_short_title as Клиент,
                     count(*) AS Рейсы,
                     SUM(requests.charterer_rate_amount) as Оборот,
              	    SUM(transportations.profit_amount) as Маржа,
					COALESCE(SUM(transportations.profit_amount) / SUM(NULLIF(requests.charterer_rate_amount, 0)), 0) as Рентабельность,

                  SUM(case
              		when signed_application_contract_carrier.category is not null AND transportations.carrier_version_framework_agreement_number IS NULL then 1
              		when signed_application_contract_carrier.category is null AND transportations.carrier_version_framework_agreement_number IS NULL then 0
              	    end) as Договоры

                     FROM erp_requests.requests

                   LEFT JOIN erp_requests.transportations
                   ON requests.id = transportations.request_id

           LEFT JOIN erp_requests.transportation_documents as inner_application_contract_charterer
            ON requests.id = inner_application_contract_charterer.transportation_request_id
                AND inner_application_contract_charterer.category='inner_application_contract_charterer'
           
           LEFT JOIN erp_requests.transportation_documents as signed_application_contract_carrier
            ON requests.id = signed_application_contract_carrier.transportation_request_id
                AND signed_application_contract_carrier.category='signed_application_contract_carrier'

                left join (
    select distinct on (s.request_id)
          request_id, id, locality, address, \"from\" as from_at, fact_arrival_date_time, region
    from erp_requests.request_start_route_points s
    order by s.request_id desc, s.from asc
) as first_loading
on requests.id = first_loading.request_id


left join erp_users.user_profiles as logist
on requests.responsible_expeditor_id = logist.original_id 
and logist.is_actual = true


                   WHERE requests.is_actual=true
                   AND requests.status not in ('cancelled', 'transportationCancelled', 'active')
                   AND first_loading.from_at >=", paste0("'", startDate, "'::timestamp"), "AND first_loading.from_at <=", paste0("'",endDate, "'::timestamp"),

                               "group by Дата, Логист, Клиент;")) %>%

    as_tibble() %>%

    left_join(expeditors_group, by = c("Логист" = "data.id")) %>%

    transmute(Рейсы = as.numeric(Рейсы),
                   Дата = as.Date(strftime(Дата, format="%Y-%m-%d")),
                   Клиент = Клиент,
                   Группа = data.name,
                   Логист = Логист,
                   Должность = Должность,
                   Оборот = Оборот,
                   # Ставка_перевозчика = case_when(nds == TRUE ~ carrier_rate_info_amount/ 1.2,
                   #                                nds == FALSE ~ carrier_rate_info_amount),
                   # Маржа_расчёт = Ставка_клиента - Ставка_перевозчика,
                   # Рентабельность_расчёт = Маржа_расчёт / Ставка_клиента
                   Маржа = Маржа,
                   Рентабельность = Рентабельность,
                   Договоры = Договоры) %>%

    mutate(Должность = case_when(Должность == "ст. логист" ~ "логист",
                                          TRUE ~ Должность))

  requests$Группа[is.na(requests$Группа)] <- "Без группы"

  # requests[mapply(is.infinite, requests)] <- 0

  return(requests)

}


# По дате выгрузки
getLogisticKPI2 <- function(startDate, endDate){

  #   my_query <- query('{
  # "bool": {
  #       "must": [
  #         {
  #   "match": {"isActual": "true"}
  #         }
  #       ]
  # }
  # }')
  #
  #   selected_field_expeditors_group <- select_fields('{
  #   "includes": ["data.id", "data.name"]
  # }')
  #
  #   expeditors_group <- elastic(cfg$db_elastic$url,
  #                               "expeditors-group-version-index") %search% (my_query + selected_field_expeditors_group)

  requests <- dbGetQuery(con,
                         paste("SELECT date_trunc('day', last_unloading.from_at AT TIME ZONE 'Europe/Moscow') as Дата,
                     logist.title as Логист,
                     requests.juridical_person_version_short_title as Клиент,
                     count(*) AS Рейсы,
                     SUM(requests.charterer_rate_amount) as Оборот,
              	    SUM(transportations.profit_amount) as Маржа,
					COALESCE(SUM(transportations.profit_amount) / SUM(NULLIF(requests.charterer_rate_amount, 0)), 0) as Рентабельность,

                  SUM(case
              		when signed_application_contract_carrier.category is not null AND transportations.carrier_version_framework_agreement_number IS NULL then 1
              		when signed_application_contract_carrier.category is null AND transportations.carrier_version_framework_agreement_number IS NULL then 0
              	    end) as Договоры

                     FROM erp_requests.requests

                   LEFT JOIN erp_requests.transportations
                   ON requests.id = transportations.request_id

           LEFT JOIN erp_requests.transportation_documents as inner_application_contract_charterer
            ON requests.id = inner_application_contract_charterer.transportation_request_id
                AND inner_application_contract_charterer.category='inner_application_contract_charterer'
           
           LEFT JOIN erp_requests.transportation_documents as signed_application_contract_carrier
            ON requests.id = signed_application_contract_carrier.transportation_request_id
                AND signed_application_contract_carrier.category='signed_application_contract_carrier'

                   left join (
    select distinct on (s.request_id)
          request_id, id, locality, address, \"from\" as from_at, fact_arrival_date_time, region, consignee, fact_departure_date_time
    from erp_requests.request_finish_route_points s
    order by s.request_id desc, s.from desc
) as last_unloading
on requests.id = last_unloading.request_id

left join erp_users.user_profiles as logist
on requests.responsible_expeditor_id = logist.original_id 
and logist.is_actual = true

                   WHERE requests.is_actual=true
                   AND requests.status not in ('cancelled', 'transportationCancelled', 'active')
                   AND last_unloading.from_at >=", paste0("'", startDate, "'::timestamp"), "AND last_unloading.from_at <=", paste0("'",endDate, "'::timestamp"),

                               "group by Дата, Логист, Клиент;")) %>%

    as_tibble() %>%

    left_join(expeditors_group, by = c("Логист" = "data.id")) %>%

    transmute(Рейсы = as.numeric(Рейсы),
                   Дата = as.Date(strftime(Дата, format="%Y-%m-%d")),
                   Клиент = Клиент,
                   Группа = data.name,
                   Логист = Логист,
                   Должность = Должность,
                   Оборот = Оборот,
                   # Ставка_перевозчика = case_when(nds == TRUE ~ carrier_rate_info_amount/ 1.2,
                   #                                nds == FALSE ~ carrier_rate_info_amount),
                   # Маржа_расчёт = Ставка_клиента - Ставка_перевозчика,
                   # Рентабельность_расчёт = Маржа_расчёт / Ставка_клиента
                   Маржа = Маржа,
                   Рентабельность = Рентабельность,
                   Договоры = Договоры) %>%

    mutate(Должность = case_when(Должность == "ст. логист" ~ "логист",
                                          TRUE ~ Должность))

  requests$Группа[is.na(requests$Группа)] <- "Без группы"

  # requests[mapply(is.infinite, requests)] <- 0

  return(requests)

}


# Кредиторская задолженность

accountsPayable <- function(){
  requests <- dbGetQuery(con,
                             "SELECT requests.short_id,
                               requests.charterer_version_doc_received_days,
                               requests.shipping_documents_info_planned_doc_received_date,
                               requests.shipping_documents_info_docs_received_date,

                     transportations.juridical_person_version_short_title as carrier,
                     transportations.carrier_rate_info_amount,
                     transportations.prepayment_amount,
                     transportations.carrier_rate_info_with_nds,
                     transportations.profit_amount,
                     transportations.carrier_version_framework_agreement_number,
                     transportations.grace_period,
                     transportations.carrier_certificate_info_number,
                     transportations.carrier_certificate_info_created_at,
                     transportations.carrier_certificate_info_ready_at,
                     transportations.carrier_version_id,
                     transportations.carrier_bill_info_paid_at,
                     transportations.carrier_grace_of_payment_finish_grace_period_date,
                     transportations.status as trans_status

                               FROM erp_requests.requests

                               INNER JOIN erp_requests.transportations
                               ON requests.id = transportations.request_id

                               WHERE requests.is_actual 
                               AND requests.status not in ('cancelled', 'transportationCancelled', 'active')
                               AND transportations.carrier_bill_info_paid_at is null;") %>%

    as_tibble()

    requests$prepayment_amount[is.na(requests$prepayment_amount)] <- 0

    requests %>% transmute(Перевозчик = carrier,
                        Номер_заявки = short_id,
                        Статус_заявки = case_when(trans_status == "ready" ~ "Перевозка началась",
                                                     trans_status == "onStartPoint" ~ "Водитель прибыл на загрузку",
                                                     trans_status == "loaded" ~ "Водитель в пути",
                                                     trans_status == "onTheWay" ~ "Пройдена половина пути, всё хорошо",
                                                     trans_status == "onFinishPoint" ~ "Водитель прибыл на разгрузку",
                                                     trans_status == "unloaded" ~ "Водитель разгрузился, ждём документы",
                                                     trans_status == "docsReceived" ~ "Получены документы от водителя",
                                                     trans_status == "paid" ~ "Перевозка оплачена",
                                                     trans_status == "cancelled" ~ "Перевозка отменена",
                                                     trans_status == "docsProblem" ~ "У водителя проблемы с документами",
                                                     trans_status == "docsApproved" ~ "Документы водителя подтверждены"
                        ),
                        Ставка_перевозчика = carrier_rate_info_amount,
                        Сумма_предоплаты = prepayment_amount,
                        С_учётом_предоплаты = Ставка_перевозчика - Сумма_предоплаты,
                        Дата_оплаты_перевозчику_расчёт = as.Date(strftime(carrier_grace_of_payment_finish_grace_period_date, format="%Y-%m-%d")),
                        ПKЗ = as.numeric(Sys.Date() - as.Date(Дата_оплаты_перевозчику_расчёт)),
                        Категория = case_when(
                          as.numeric(ПKЗ) > 0 & as.numeric(ПKЗ) <8 ~ "B. До 7 дней",
                          as.numeric(ПKЗ) >= 8 & as.numeric(ПKЗ) <=15 ~ "C. От 8 до 15 дней",
                          as.numeric(ПKЗ) > 15 & as.numeric(ПKЗ) <=30 ~ "D. От 16 до 30 дней",
                          as.numeric(ПKЗ) > 30 & as.numeric(ПKЗ) <=60 ~ "F. От 31 до 60 дней",
                          as.numeric(ПKЗ) > 60 & as.numeric(ПKЗ) <=90 ~ "G. От 61 до 90 дней",
                          as.numeric(ПKЗ) > 90 ~ "H. Свыше 90 дней",
                          is.na(ПKЗ) | as.numeric(ПKЗ) <= 0 ~ "A. Кредиторская задолженность")
    )
}

# Доля спота
getSpot <- function(startDate, endDate){

  clientRequests <- dbGetQuery(con2,
                                paste("SELECT date_trunc('day', t1.created_at AT TIME ZONE 'Europe/Moscow') as Дата,
	   t1.Клиент,
	   count(*) as Заявки 
	   
from (select distinct on (tnx_cargo_order_name) * FROM (

    SELECT
       (case 
       		when source_name = 'inbev' then external_id_params->>'tnx_cargo_order_name'
            else external_id
       end) as tnx_cargo_order_name,
        COALESCE(start_trading_on_source_at, created_on_source_at, created_at) AS created_at,
       (case 
            when charterer ->> 'title' = 'Клиент 07' then 'Клиент 07'
            when source_name = 'inbev' then 'Клиент 01'
            when charterer ->> 'title' = 'Клиент 08' then 'Клиент 08'
            when charterer ->> 'title' = 'Клиент 10' then 'Клиент 10'
            when charterer ->> 'title' = 'Клиент 11' or charterer ->> 'title' = 'Клиент 09' then 'Клиент 09'
            when charterer ->> 'title' = 'Клиент 12' then 'Клиент 12'
            when charterer ->> 'title' = 'Клиент 02' then 'Клиент 02'
            when charterer ->> 'title' = 'Клиент 03' then 'Клиент 03'
            when charterer ->> 'title' = 'Клиент 04' then 'Клиент 04'
            when charterer ->> 'title' = 'Клиент 13' then 'Клиент 13'
            when source_name = 'uniconf' then 'Клиент 06'
        end) as Клиент
        
        FROM orders
    WHERE trade_type = 'AUCTION'
    AND (charterer ->> 'title' in ('Клиент 07', 'Клиент 08', 'Клиент 01', 'Клиент 01','Клиент 10', 'Клиент 11', 'Клиент 12', 'Клиент 02', 'Клиент 03', 'Клиент 04', 'Клиент 13', 'Клиент 09')
    OR source_name = 'uniconf')
) AS t
									  WHERE t.created_at >=", paste0("'", startDate, "'::timestamp"), "AND t.created_at <=", paste0("'",endDate, "'::timestamp"),
                                      
                                      "ORDER BY t.tnx_cargo_order_name, t.created_at desc
                                     
                 ) as t1
                 
                 group by Дата, Клиент;")) 
  
  eRPRequests <- dbGetQuery(con,
                            paste("SELECT date_trunc('day', created_at AT TIME ZONE 'Europe/Moscow') as Дата,
                                requests.juridical_person_version_short_title as Клиент,
                                count(*) as Заявки 

                                FROM erp_requests.requests
                             WHERE type = 'Spot'
                             AND date_trunc('day', created_at) >=", paste0("'", startDate, "'::timestamp"), "AND date_trunc('day', created_at) <=", paste0("'",endDate, "'::timestamp"),
                                  "AND is_actual = TRUE
                             group by Дата, Клиент;")) %>%
    # mutate(Дата = as.Date(strftime(created_at, format="%Y-%m-%d")),
    #            Клиент = client) %>%
    # group_by(Дата, Клиент) %>%
    # summarise(Заявки = n())
    mutate(Дата = as.Date(strftime(Дата, format="%Y-%m-%d")))

   result <- clientRequests %>%
    full_join(eRPRequests, by = c("Дата" = "Дата", "Клиент" = "Клиент")) %>%
    mutate(Отношение = Заявки.y / Заявки.x) %>%
     arrange(Дата)

   result$Заявки.x[is.na(result$Заявки.x)] <- 0
   result$Заявки.y[is.na(result$Заявки.y)] <- 0
   # result$Отношение[is.na(result$Отношение)] <- 0

  return(result)

}

# Спотовые заявки ИнБев
getDetailSpotForInbev <- function(startDate, endDate){
  
  my_query <- query('{
"bool": {
      "must": [
        {
  "match": {"isActual": "true"}
        }
      ]
}
}')
  
  glossary_selected_field <- select_fields('{
  "includes": ["data.key", "data.title"]
}')
  
  glossary <- elastic(cfg$db_elastic$url,
                      "glossary-element-version-index") %search% (my_query + glossary_selected_field)
  
  requests_by_day <- dbGetQuery(con2,
                                paste("SELECT distinct on (tnx_cargo_order_name) * FROM (
    SELECT
        external_id_params->>'tnx_cargo_order_name' AS tnx_cargo_order_name,
        status,
        trade_type,
        COALESCE(start_trading_on_source_at, created_on_source_at, created_at) AS created_at,
        base_price_net,
        matched_price_net,
        targets->0->'address'->>'locality' AS loading_locality,
        targets->0->'address'->>'full_formatted' AS loading_address,
        targets->0->'visit_windows'->0->>'from' AS loading_datetime,
        targets->0->'address'->'dadata'->> 'region_with_type' AS start_region,

        targets->-1->'address'->>'locality' AS unloading_locality,
        targets->-1->'address'->>'full_formatted' AS unloading_address,
        targets->-1->'visit_windows'->0->>'from' AS unloading_datetime,
        targets->1->'address'->'dadata'->> 'region_with_type' AS finish_region,

        vehicle_requirements->>'types_of_body' AS types_of_body,
        vehicle_requirements->>'payload_in_tons' AS payload_in_tons,
        cargos->0->>'type' AS cargos_type
    FROM orders
    WHERE source_name = 'inbev'
) AS t
                             WHERE t.created_at >=", paste0("'", startDate, "'::timestamp"), "AND t.created_at <=", paste0("'",endDate, "'::timestamp"),
                             "ORDER BY t.tnx_cargo_order_name, t.created_at desc;")) %>%
    
    as_tibble() %>%
    
    mutate(types_of_body = gsub('\\[|\\]|\\"|\\"', "", types_of_body)) %>%
    
    left_join(glossary, by = c("types_of_body" = "data.key")) %>%
    
    left_join(glossary, by = c("cargos_type" = "data.key")) %>%
    
    transmute(Клиент = 'Клиент 01',
              Дата_торгов = as.Date(strftime(created_at, format="%Y-%m-%d")),
              День_недели = wday(strftime(created_at, format="%Y-%m-%d")),
              Маршрут = paste(loading_locality, "\U2192", unloading_locality),
              Тип = case_when(trade_type == "AUCTION" ~ "Спот",
                              trade_type == "FIXED" ~ "Гарантия",
                              TRUE ~ trade_type
              ),
              "Тип груза" = data.title.y,
              "Номер заявки" = tnx_cargo_order_name,
              "Регион погрузки" = start_region,
              "Пункт погрузки" = loading_locality,
              # "Количество точек на погрузке" = " ",
              "Адрес погрузки" = loading_address,
              "Дата погрузки" = as.Date(strftime(loading_datetime, format="%Y-%m-%d")),
              "Время погрузки" = strftime(loading_datetime, format="%H:%M:%S"),
              
              "Регион выгрузки" = finish_region,
              "Пункт выгрузки" = unloading_locality,
              # "Количество точек на выгрузке" = " ",
              "Адрес выгрузки" = unloading_address,
              "Дата выгрузки" = as.Date(strftime(unloading_datetime, format="%Y-%m-%d")),
              "Время выгрузки" = strftime(unloading_datetime, format="%H:%M:%S"),
              "Цена с НДС (начальная)" = base_price_net * 1.2,
              "Цена без НДС (начальная)" = base_price_net,
              "Грузоподъёмность" = payload_in_tons,
              "Тип ТС" = data.title.x,
              # Статус = status,
              "Цена с ндс (конечная)" = matched_price_net * 1.2,
              "Цена без ндс (конечная)" = matched_price_net,
              Статус = case_when(status == "CREATED" | status == "ACTIVE" ~ "Заявка создана/активна",
                                 status == "REPLY_MATCHED" ~ "Выиграли",
                                 status == "LOST" ~ "Участвовали",
                                 status == "CANCELLED" ~ "Клиент отменил",
                                 status == "UNDEFINED" ~ "Неизвестный статус",
                                 status == "UNAVAILABLE" ~ "Недоступна",
                                 TRUE ~ status
              )
              ) %>%
    
    mutate(День_недели = case_when(День_недели == 2 ~ "Понедельник",
                                   День_недели == 3 ~ "Вторник",
                                   День_недели == 4 ~ "Среда",
                                   День_недели == 5 ~ "Четверг ",
                                   День_недели == 6 ~ "Пятница",
                                   День_недели == 7 ~ "Суббота",
                                   День_недели == 1 ~ "Воскресенье"
    )) %>%
    
    arrange(Дата_торгов)
  
  }

# Спотовые заявки Клиент 08
getDetailSpotForBaltika <- function(startDate, endDate){
  
  my_query <- query('{
"bool": {
      "must": [
        {
  "match": {"isActual": "true"}
        }
      ]
}
}')
  
  glossary_selected_field <- select_fields('{
  "includes": ["data.key", "data.title"]
}')
  
  glossary <- elastic(cfg$db_elastic$url,
                      "glossary-element-version-index") %search% (my_query + glossary_selected_field)
  
  requests_by_day <- dbGetQuery(con2,
                                paste("SELECT external_id_params->>'baltika_lot_pickup_list_number' AS external_id,
                                COALESCE(start_trading_on_source_at, created_on_source_at, created_at) AS created_at,
                                base_price_net,
                                matched_price_net,
                                matched_price_net,
                                targets->0->'address'->>'locality' AS loading_locality,
                                targets->0->'address'->>'full_formatted' AS loading_address,
                                targets->0->'visit_windows'->0->>'from' AS loading_datetime,
                                targets->0->'address'->'dadata'->> 'region_with_type' AS start_region,

                                targets->-1->'address'->>'locality' AS unloading_locality,
                                targets->-1->'address'->>'full_formatted' AS unloading_address,
                                targets->-1->'visit_windows'->0->>'from' AS unloading_datetime,
                                targets->1->'address'->'dadata'->> 'region_with_type' AS finish_region,

                                
                                vehicle_requirements->>'types_of_body' AS types_of_body,
                                vehicle_requirements->>'payload_in_tons' AS payload_in_tons,
                                cargos->0->>'type' AS cargos_type,
                                charterer_notes->-1->>'text' as charterer_notes
                                FROM orders
                             WHERE source_name = 'baltika'
                             AND COALESCE(start_trading_on_source_at, created_on_source_at, created_at) >=", paste0("'", startDate, "'::timestamp"), "AND COALESCE(start_trading_on_source_at, created_on_source_at, created_at) <=", paste0("'",endDate, "'::timestamp"),
                             "ORDER BY COALESCE(start_trading_on_source_at, created_on_source_at, created_at);")) %>%

    as_tibble() %>%

    mutate(types_of_body = gsub('\\[|\\]|\\"|\\"', "", types_of_body)) %>%
    
    left_join(glossary, by = c("types_of_body" = "data.key")) %>%
    
    left_join(glossary, by = c("cargos_type" = "data.key")) %>%
    
    transmute(Клиент = 'Клиент 08',
                    Дата_торгов = as.Date(strftime(created_at, format="%Y-%m-%d")),
                    День_недели = wday(strftime(created_at, format="%Y-%m-%d")),
                    Маршрут = paste(loading_locality, "\U2192", unloading_locality),
                    "Тип заявки" = "Спот",
                    "Тип груза" = data.title.y,
                    "Номер заявки" = external_id,
                    "Пункт погрузки" = loading_locality,
                    # "Количество точек на погрузке" = " ",
                    "Регион погрузки" = start_region,
                    "Адрес погрузки" = loading_address,
                    "Дата погрузки" = as.Date(strftime(loading_datetime, format="%Y-%m-%d")),
                    "Время погрузки" = strftime(loading_datetime, format="%H:%M:%S"),
                    
                    "Регион выгрузки" = finish_region,
                    "Пункт выгрузки" = unloading_locality,
                    # "Количество точек на выгрузке" = " ",
                    "Адрес выгрузки" = unloading_address,
                    "Дата выгрузки" = as.Date(strftime(unloading_datetime, format="%Y-%m-%d")),
                    "Время выгрузки" = strftime(unloading_datetime, format="%H:%M:%S"),
                    "Цена с НДС (начальная)" = base_price_net * 1.2,
                    "Цена без НДС (начальная)" = base_price_net,
                    "Грузоподъёмность" = payload_in_tons,
                    "Тип ТС" = data.title.x,
                    # Статус = status,
                    "Цена с ндс (конечная)" = matched_price_net * 1.2,
                    "Цена без ндс (конечная)" = matched_price_net) %>%
    
    mutate(День_недели = case_when(День_недели == 2 ~ "Понедельник",
                                              День_недели == 3 ~ "Вторник",
                                              День_недели == 4 ~ "Среда",
                                              День_недели == 5 ~ "Четверг ",
                                              День_недели == 6 ~ "Пятница",
                                              День_недели == 7 ~ "Суббота",
                                              День_недели == 1 ~ "Воскресенье"
    )) %>%
    
    arrange(Дата_торгов)
}


# Спотовые заявки с площадки Транспореона
getDetailSpotForTransporeon <- function(startDate, endDate){
  
  my_query <- query('{
"bool": {
      "must": [
        {
  "match": {"isActual": "true"}
        }
      ]
}
}')
  
  glossary_selected_field <- select_fields('{
  "includes": ["data.key", "data.title"]
}')
  
  glossary <- elastic(cfg$db_elastic$url,
                      "glossary-element-version-index") %search% (my_query + glossary_selected_field)
  
  requests_by_day <- dbGetQuery(con2,
                                paste("SELECT external_id,
                                charterer ->> 'title' AS charterer,
                                COALESCE(start_trading_on_source_at, created_on_source_at, created_at) AS created_at,
                                base_price_net,
                                matched_price_net,
                                targets->0->'address'->>'locality' AS loading_locality,
                                targets->0->'address'->>'full_formatted' AS loading_address,
                                targets->0->'visit_windows'->0->>'from' AS loading_datetime,
                                targets->0->'address'->'dadata'->> 'region_with_type' AS start_region,

                                targets->-1->'address'->>'locality' AS unloading_locality,
                                targets->-1->'address'->>'full_formatted' AS unloading_address,
                                targets->-1->'visit_windows'->0->>'from' AS unloading_datetime,
                                targets->1->'address'->'dadata'->> 'region_with_type' AS finish_region,

                                vehicle_requirements->>'types_of_body' AS types_of_body,
                                vehicle_requirements->>'payload_in_tons' AS payload_in_tons,
                                cargos->0->>'type' AS cargos_type,
                                trade_type,
                                status
                                FROM orders
                             WHERE source_name = 'transporeon'
                             AND trade_type IN ('AUCTION', 'FIXED')
                             AND COALESCE(start_trading_on_source_at, created_on_source_at, created_at) >=", paste0("'", startDate, "'::timestamp"), "AND COALESCE(start_trading_on_source_at, created_on_source_at, created_at) <=", paste0("'",endDate, "'::timestamp"),
                             "ORDER BY COALESCE(start_trading_on_source_at, created_on_source_at, created_at);")) %>%

    as_tibble() %>%

    mutate(types_of_body = gsub('\\[|\\]|\\"|\\"', "", types_of_body)) %>%
    
    left_join(glossary, by = c("types_of_body" = "data.key")) %>%
    
    left_join(glossary, by = c("cargos_type" = "data.key")) %>%
    
    transmute(Клиент = case_when(charterer == "Клиент 02" ~ 'Клиент 02',
                                 charterer == "Клиент 03" ~ 'Клиент 03',
                                 charterer == "Клиент 04" ~ 'Клиент 04',
                                 TRUE ~ charterer
                                 ),
                    Дата_торгов = as.Date(strftime(created_at, format="%Y-%m-%d")),
                    День_недели = wday(strftime(created_at, format="%Y-%m-%d")),
                    Маршрут = paste(loading_locality, "\U2192", unloading_locality),
                    Тип = case_when(trade_type == "AUCTION" ~ "Спот",
                                    trade_type == "FIXED" ~ "Гарантия",
                                    TRUE ~ trade_type
                    ),
                    "Тип груза" = data.title.y,
                    "Номер заявки" = external_id,
                    "Регион погрузки" = start_region,
                    "Пункт погрузки" = loading_locality,
                    # "Количество точек на погрузке" = " ",
                    "Адрес погрузки" = loading_address,
                    "Дата погрузки" = as.Date(strftime(loading_datetime, format="%Y-%m-%d")),
                    "Время погрузки" = strftime(loading_datetime, format="%H:%M:%S"),
                    
                    "Регион выгрузки" = finish_region,
                    "Пункт выгрузки" = unloading_locality,
                    # "Количество точек на выгрузке" = " ",
                    "Адрес выгрузки" = unloading_address,
                    "Дата выгрузки" = as.Date(strftime(unloading_datetime, format="%Y-%m-%d")),
                    "Время выгрузки" = strftime(unloading_datetime, format="%H:%M:%S"),
                    "Цена с НДС (начальная)" = base_price_net * 1.2,
                    "Цена без НДС (начальная)" = base_price_net,
                    "Грузоподъёмность" = payload_in_tons,
                    "Тип ТС" = data.title.x,
                    # Статус = status,
                    "Цена с ндс (конечная)" = matched_price_net * 1.2,
                    "Цена без ндс (конечная)" = matched_price_net,
                    Статус = case_when(status == "CREATED" | status == "ACTIVE" ~ "Заявка создана/активна",
                                       status == "REPLY_MATCHED" ~ "Выиграли",
                                       status == "LOST" ~ "Участвовали",
                                       status == "CANCELLED" ~ "Клиент отменил",
                                       status == "UNDEFINED" ~ "Неизвестный статус",
                                       status == "UNAVAILABLE" ~ "Недоступна",
                                       TRUE ~ status
                    )
                    ) %>%
    
    mutate(День_недели = case_when(День_недели == 2 ~ "Понедельник",
                                              День_недели == 3 ~ "Вторник",
                                              День_недели == 4 ~ "Среда",
                                              День_недели == 5 ~ "Четверг ",
                                              День_недели == 6 ~ "Пятница",
                                              День_недели == 7 ~ "Суббота",
                                              День_недели == 1 ~ "Воскресенье"
    )) %>%
    
    arrange(Дата_торгов)
}



# Спотовые заявки ПепсиКо
getDetailSpotForPepsi <- function(startDate, endDate){
  
  my_query <- query('{
"bool": {
      "must": [
        {
  "match": {"isActual": "true"}
        }
      ]
}
}')
  
  glossary_selected_field <- select_fields('{
  "includes": ["data.key", "data.title"]
}')
  
  glossary <- elastic(cfg$db_elastic$url,
                      "glossary-element-version-index") %search% (my_query + glossary_selected_field)
  
  requests_by_day <- dbGetQuery(con2,
                                paste("SELECT external_id,
                                COALESCE(start_trading_on_source_at, created_on_source_at, created_at) AS created_at,
                                base_price_net,
                                matched_price_net,
                                targets->0->'address'->>'locality' AS loading_locality,
                                targets->0->'address'->>'full_formatted' AS loading_address,
                                targets->0->'visit_windows'->0->>'from' AS loading_datetime,
                                targets->0->'address'->'dadata'->> 'region_with_type' AS start_region,
                                
                                targets->-1->'address'->>'locality' AS unloading_locality,
                                targets->-1->'address'->>'full_formatted' AS unloading_address,
                                targets->-1->'visit_windows'->0->>'from' AS unloading_datetime,
                                targets->1->'address'->'dadata'->> 'region_with_type' AS finish_region,

                                vehicle_requirements->>'types_of_body' AS types_of_body,
                                vehicle_requirements->>'payload_in_tons' AS payload_in_tons,
                                cargos->0->>'type' AS cargos_type
                                FROM orders
                             WHERE source_name = 'pepsico'
                            AND COALESCE(start_trading_on_source_at, created_on_source_at, created_at) >=", paste0("'", startDate, "'::timestamp"), "AND COALESCE(start_trading_on_source_at, created_on_source_at, created_at) <=", paste0("'",endDate, "'::timestamp"),
                            "ORDER BY COALESCE(start_trading_on_source_at, created_on_source_at, created_at);")) %>%

    as_tibble() %>%

    mutate(types_of_body = gsub('\\[|\\]|\\"|\\"', "", types_of_body)) %>%
    
    left_join(glossary, by = c("types_of_body" = "data.key")) %>%
    
    left_join(glossary, by = c("cargos_type" = "data.key")) %>%
    
    transmute(Клиент = 'Клиент 07',
                    Дата_торгов = as.Date(strftime(created_at, format="%Y-%m-%d")),
                    День_недели = wday(strftime(created_at, format="%Y-%m-%d")),
                    Маршрут = paste(loading_locality, "\U2192", unloading_locality),
                    "Тип заявки" = "Спот",
                    "Тип груза" = data.title.y,
                    "Номер заявки" = external_id,
                    "Регион погрузки" = start_region,
                    "Пункт погрузки" = loading_locality,
                    # "Количество точек на погрузке" = " ",
                    "Адрес погрузки" = loading_address,
                    "Дата погрузки" = as.Date(strftime(loading_datetime, format="%Y-%m-%d")),
                    "Время погрузки" = strftime(loading_datetime, format="%H:%M:%S"),
                    
                    "Регион выгрузки" = finish_region,
                    "Пункт выгрузки" = unloading_locality,
                    # "Количество точек на выгрузке" = " ",
                    "Адрес выгрузки" = unloading_address,
                    "Дата выгрузки" = as.Date(strftime(unloading_datetime, format="%Y-%m-%d")),
                    "Время выгрузки" = strftime(unloading_datetime, format="%H:%M:%S"),
                    "Цена с НДС (начальная)" = base_price_net * 1.2,
                    "Цена без НДС (начальная)" = base_price_net,
                    "Грузоподъёмность" = payload_in_tons,
                    "Тип ТС" = data.title.x,
                    # Статус = status,
                    "Цена с ндс (конечная)" = matched_price_net * 1.2,
                    "Цена без ндс (конечная)" = matched_price_net) %>%
    
    mutate(День_недели = case_when(День_недели == 2 ~ "Понедельник",
                                              День_недели == 3 ~ "Вторник",
                                              День_недели == 4 ~ "Среда",
                                              День_недели == 5 ~ "Четверг ",
                                              День_недели == 6 ~ "Пятница",
                                              День_недели == 7 ~ "Суббота",
                                              День_недели == 1 ~ "Воскресенье"
    )) %>%
    
    arrange(Дата_торгов)
}


# Спотовые заявки Торгтранс
getDetailSpotForTorgtrans <- function(startDate, endDate){
  
  my_query <- query('{
"bool": {
      "must": [
        {
  "match": {"isActual": "true"}
        }
      ]
}
}')
  
  glossary_selected_field <- select_fields('{
  "includes": ["data.key", "data.title"]
}')
  
  glossary <- elastic(cfg$db_elastic$url,
                      "glossary-element-version-index") %search% (my_query + glossary_selected_field)
  
  requests_by_day <- dbGetQuery(con2,
                                paste("SELECT external_id,
                                COALESCE(start_trading_on_source_at, created_on_source_at, created_at) AS created_at,
                                charterer ->> 'title' AS charterer,
                                base_price_net,
                                matched_price_net,
                                targets->0->'address'->>'locality' AS loading_locality,
                                targets->0->'address'->>'full_formatted' AS loading_address,
                                targets->0->'visit_windows'->0->>'from' AS loading_datetime,
                                targets->0->'address'->'dadata'->> 'region_with_type' AS start_region,

                                targets->-1->'address'->>'locality' AS unloading_locality,
                                targets->-1->'address'->>'full_formatted' AS unloading_address,
                                targets->-1->'visit_windows'->0->>'from' AS unloading_datetime,
                                targets->1->'address'->'dadata'->> 'region_with_type' AS finish_region,

                                
                                vehicle_requirements->>'types_of_body' AS types_of_body,
                                vehicle_requirements->>'payload_in_tons' AS payload_in_tons,
                                cargos->0->>'type' AS cargos_type,
                                charterer_notes->-1->>'text' as charterer_notes
                                FROM orders
                             WHERE source_name = 'torgtrans'
                             AND charterer ->> 'title' in ('Клиент 09', 'Клиент 10', 'Клиент 11', 'Клиент 12')
                             AND COALESCE(start_trading_on_source_at, created_on_source_at, created_at) >=", paste0("'", startDate, "'::timestamp"), "AND COALESCE(start_trading_on_source_at, created_on_source_at, created_at) <=", paste0("'",endDate, "'::timestamp"),
                             "ORDER BY COALESCE(start_trading_on_source_at, created_on_source_at, created_at);")) %>%
    
    as_tibble() %>%
    
    mutate(types_of_body = gsub('\\[|\\]|\\"|\\"', "", types_of_body)) %>%
    
    left_join(glossary, by = c("types_of_body" = "data.key")) %>%
    
    left_join(glossary, by = c("cargos_type" = "data.key")) %>%
    
    transmute(Клиент = case_when(charterer == "Клиент 10" ~ 'Клиент 10',
                                 charterer == "Клиент 11" ~ 'Клиент 11',
                                 charterer == "Клиент 12" ~ 'Клиент 12'),
                    Дата_торгов = as.Date(strftime(created_at, format="%Y-%m-%d")),
                    День_недели = wday(strftime(created_at, format="%Y-%m-%d")),
                    Маршрут = paste(loading_locality, "\U2192", unloading_locality),
                    "Тип заявки" = "Спот",
                    "Тип груза" = data.title.y,
                    "Номер заявки" = external_id,
                    "Регион погрузки" = start_region,
                    "Пункт погрузки" = loading_locality,
                    # "Количество точек на погрузке" = " ",
                    "Адрес погрузки" = loading_address,
                    "Дата погрузки" = as.Date(strftime(loading_datetime, format="%Y-%m-%d")),
                    "Время погрузки" = strftime(loading_datetime, format="%H:%M:%S"),
                    
                    "Регион выгрузки" = finish_region,
                    "Пункт выгрузки" = unloading_locality,
                    # "Количество точек на выгрузке" = " ",
                    "Адрес выгрузки" = unloading_address,
                    "Дата выгрузки" = as.Date(strftime(unloading_datetime, format="%Y-%m-%d")),
                    "Время выгрузки" = strftime(unloading_datetime, format="%H:%M:%S"),
                    "Цена с НДС (начальная)" = base_price_net * 1.2,
                    "Цена без НДС (начальная)" = base_price_net,
                    "Грузоподъёмность" = payload_in_tons,
                    "Тип ТС" = data.title.x,
                    # Статус = status,
                    "Цена с ндс (конечная)" = matched_price_net * 1.2,
                    "Цена без ндс (конечная)" = matched_price_net) %>%
    
    mutate(День_недели = case_when(День_недели == 2 ~ "Понедельник",
                                              День_недели == 3 ~ "Вторник",
                                              День_недели == 4 ~ "Среда",
                                              День_недели == 5 ~ "Четверг ",
                                              День_недели == 6 ~ "Пятница",
                                              День_недели == 7 ~ "Суббота",
                                              День_недели == 1 ~ "Воскресенье"
    )) %>%
    
    arrange(Дата_торгов)
}


# Спотовые заявки МПК
getDetailSpotForMPK <- function(startDate, endDate){
  
  my_query <- query('{
"bool": {
      "must": [
        {
  "match": {"isActual": "true"}
        }
      ]
}
}')
  
  glossary_selected_field <- select_fields('{
  "includes": ["data.key", "data.title"]
}')
  
  glossary <- elastic(cfg$db_elastic$url,
                      "glossary-element-version-index") %search% (my_query + glossary_selected_field)
  
  requests_by_day <- dbGetQuery(con2,
                                paste("SELECT external_id,
                                COALESCE(start_trading_on_source_at, created_on_source_at, created_at) AS created_at,
                                base_price_gross,
                                matched_price_gross,
                                targets->0->'address'->>'locality' AS loading_locality,
                                targets->0->'address'->>'full_formatted' AS loading_address,
                                targets->0->'visit_windows'->0->>'from' AS loading_datetime,
                                targets->0->'address'->'dadata'->> 'region_with_type' AS start_region,

                                targets->-1->'address'->>'locality' AS unloading_locality,
                                targets->-1->'address'->>'full_formatted' AS unloading_address,
                                targets->-1->'visit_windows'->0->>'from' AS unloading_datetime,
                                targets->1->'address'->'dadata'->> 'region_with_type' AS finish_region,

                                vehicle_requirements->>'types_of_body' AS types_of_body,
                                vehicle_requirements->>'payload_in_tons' AS payload_in_tons,
                                cargos->0->>'type' AS cargos_type,
                                charterer_notes->-1->>'text' as charterer_notes
                                FROM orders
                             WHERE source_name = 'mosbrew'
                             AND COALESCE(start_trading_on_source_at, created_on_source_at, created_at) >=", paste0("'", startDate, "'::timestamp"), "AND COALESCE(start_trading_on_source_at, created_on_source_at, created_at) <=", paste0("'",endDate, "'::timestamp"),
                             "ORDER BY COALESCE(start_trading_on_source_at, created_on_source_at, created_at);")) %>%
    
    as_tibble() %>%
    
    mutate(types_of_body = gsub('\\[|\\]|\\"|\\"', "", types_of_body)) %>%
    
    left_join(glossary, by = c("types_of_body" = "data.key")) %>%
    
    left_join(glossary, by = c("cargos_type" = "data.key")) %>%
    
    transmute(Клиент = 'Клиент 13',
                    Дата_торгов = as.Date(strftime(created_at, format="%Y-%m-%d")),
                    День_недели = wday(strftime(created_at, format="%Y-%m-%d")),
                    Маршрут = paste(loading_locality, "\U2192", unloading_locality),
                    "Тип заявки" = "Спот",
                    "Тип груза" = data.title.y,
                    "Номер заявки" = external_id,
                    "Регион погрузки" = start_region,
                    "Пункт погрузки" = loading_locality,
                    # "Количество точек на погрузке" = " ",
                    "Адрес погрузки" = loading_address,
                    "Дата погрузки" = as.Date(strftime(loading_datetime, format="%Y-%m-%d")),
                    "Время погрузки" = strftime(loading_datetime, format="%H:%M:%S"),
                    
                    "Регион выгрузки" = finish_region,
                    "Пункт выгрузки" = unloading_locality,
                    # "Количество точек на выгрузке" = " ",
                    "Адрес выгрузки" = unloading_address,
                    "Дата выгрузки" = as.Date(strftime(unloading_datetime, format="%Y-%m-%d")),
                    "Время выгрузки" = strftime(unloading_datetime, format="%H:%M:%S"),
                    "Цена с НДС (начальная)" = base_price_gross,
                    "Цена без НДС (начальная)" = base_price_gross / 1.2,
                    "Грузоподъёмность" = payload_in_tons,
                    "Тип ТС" = data.title.x,
                    # Статус = status,
                    "Цена с ндс (конечная)" = matched_price_gross,
                    "Цена без ндс (конечная)" = matched_price_gross / 1.2) %>%
    
    mutate(День_недели = case_when(День_недели == 2 ~ "Понедельник",
                                              День_недели == 3 ~ "Вторник",
                                              День_недели == 4 ~ "Среда",
                                              День_недели == 5 ~ "Четверг ",
                                              День_недели == 6 ~ "Пятница",
                                              День_недели == 7 ~ "Суббота",
                                              День_недели == 1 ~ "Воскресенье"
    )) %>%
    
    arrange(Дата_торгов)
}


# Спотовые заявки Кондитеры
getDetailSpotForUniconf <- function(startDate, endDate){
  
  my_query <- query('{
"bool": {
      "must": [
        {
  "match": {"isActual": "true"}
        }
      ]
}
}')
  
  glossary_selected_field <- select_fields('{
  "includes": ["data.key", "data.title"]
}')
  
  glossary <- elastic(cfg$db_elastic$url,
                      "glossary-element-version-index") %search% (my_query + glossary_selected_field)
  
  requests_by_day <- dbGetQuery(con2,
                                paste("SELECT external_id,
                                COALESCE(start_trading_on_source_at, created_on_source_at, created_at) AS created_at,
                                base_price_gross,
                                matched_price_gross,
                                targets->0->'address'->>'locality' AS loading_locality,
                                targets->0->'address'->>'full_formatted' AS loading_address,
                                targets->0->'visit_windows'->0->>'from' AS loading_datetime,
                                targets->0->'address'->'dadata'->> 'region_with_type' AS start_region,

                                targets->-1->'address'->>'locality' AS unloading_locality,
                                targets->-1->'address'->>'full_formatted' AS unloading_address,
                                targets->-1->'visit_windows'->0->>'from' AS unloading_datetime,
                                targets->1->'address'->'dadata'->> 'region_with_type' AS finish_region,

                                vehicle_requirements->>'types_of_body' AS types_of_body,
                                vehicle_requirements->>'payload_in_tons' AS payload_in_tons,
                                cargos->0->>'type' AS cargos_type,
                                charterer_notes->-1->>'text' as charterer_notes
                                FROM orders
                             WHERE source_name = 'uniconf'
                             AND COALESCE(start_trading_on_source_at, created_on_source_at, created_at) >=", paste0("'", startDate, "'::timestamp"), "AND COALESCE(start_trading_on_source_at, created_on_source_at, created_at) <=", paste0("'",endDate, "'::timestamp"),
                                      "ORDER BY COALESCE(start_trading_on_source_at, created_on_source_at, created_at);")) %>%
    
    as_tibble() %>%
    
    mutate(types_of_body = gsub('\\[|\\]|\\"|\\"', "", types_of_body)) %>%
    
    left_join(glossary, by = c("types_of_body" = "data.key")) %>%
    
    left_join(glossary, by = c("cargos_type" = "data.key")) %>%
    
    transmute(Клиент = 'Клиент 06',
                    Дата_торгов = as.Date(strftime(created_at, format="%Y-%m-%d")),
                    День_недели = wday(strftime(created_at, format="%Y-%m-%d")),
                    Маршрут = paste(loading_locality, "\U2192", unloading_locality),
                    "Тип заявки" = "Спот",
                    "Тип груза" = data.title.y,
                    "Номер заявки" = external_id,
                    "Регион погрузки" = start_region,
                    "Пункт погрузки" = loading_locality,
                    # "Количество точек на погрузке" = " ",
                    "Адрес погрузки" = loading_address,
                    "Дата погрузки" = as.Date(strftime(loading_datetime, format="%Y-%m-%d")),
                    "Время погрузки" = strftime(loading_datetime, format="%H:%M:%S"),
                    
                    "Регион выгрузки" = finish_region,
                    "Пункт выгрузки" = unloading_locality,
                    # "Количество точек на выгрузке" = " ",
                    "Адрес выгрузки" = unloading_address,
                    "Дата выгрузки" = as.Date(strftime(unloading_datetime, format="%Y-%m-%d")),
                    "Время выгрузки" = strftime(unloading_datetime, format="%H:%M:%S"),
                    "Цена с НДС (начальная)" = base_price_gross,
                    "Цена без НДС (начальная)" = base_price_gross / 1.2,
                    "Грузоподъёмность" = payload_in_tons,
                    "Тип ТС" = data.title.x,
                    # Статус = status,
                    "Цена с ндс (конечная)" = matched_price_gross,
                    "Цена без ндс (конечная)" = matched_price_gross / 1.2) %>%
    
    mutate(День_недели = case_when(День_недели == 2 ~ "Понедельник",
                                              День_недели == 3 ~ "Вторник",
                                              День_недели == 4 ~ "Среда",
                                              День_недели == 5 ~ "Четверг ",
                                              День_недели == 6 ~ "Пятница",
                                              День_недели == 7 ~ "Суббота",
                                              День_недели == 1 ~ "Воскресенье"
    )) %>%
    
    arrange(Дата_торгов)
}

# Для отчёта "Факторинг"
getBillsForFactoring <- function(){

  workflow_specialist_1C <- dbGetQuery(con,
    'SELECT original_id AS "data.id", title AS "data.title" FROM erp_users.user_profiles WHERE is_actual') %>%
    distinct(data.id, .keep_all = TRUE)
  
  my_query <- query('{
"bool": {
      "must": [
        {
  "match": {"isActual": "true"}
        }
      ]
}
}')
  
  selected_field_user <- select_fields('{
  "includes": ["data.id", "data.title"]
}')
  
  user = elastic(cfg$db_elastic$url,
                 "user-profile-version-index") %search% (my_query + selected_field_user) %>%
    group_by(data.id) %>%
    summarise(data.title = first(data.title))

  selected_field_client <- select_fields('{
  "includes": ["data.id", "data.docVerification.days"]
}')

  client <- elastic(cfg$db_elastic$url,
                    "charterer-version-index") %search% (my_query + selected_field_client)

  bills_by_day <- dbGetQuery(con,
                             "select requests.juridical_person_version_short_title,
                               requests.charterer_version_signed_by_uid,
                               requests.short_id,
                               bills.number_doc,
                               bills.invoice_amount,
                               bills.created_at_original,
                               bills.created_at,
                               bills.user_id,
                               bills.status,
                               documents_packets.delivery_date,
                               requests.charterer_version_id
 FROM erp_documents_packets.document_packets_requests
 INNER JOIN erp_bills.bills
ON document_packets_requests.id = ANY(bills.request_ids) AND bills.is_actual
 INNER JOIN erp_documents_packets.documents_packets
     ON document_packets_requests.documents_packet_id = documents_packets.id AND document_packets_requests.documents_packet_version = documents_packets.version
 INNER JOIN erp_requests.requests
     ON document_packets_requests.id = requests.id AND requests.is_actual
 WHERE documents_packets.is_actual=true
AND bills.created_at_original >= '2020-12-31 21:00:00'::timestamp
                               AND bills.type = 'Charterer';") %>%

    as_tibble() %>%

    left_join(client, by = c("charterer_version_id" = "data.id")) %>%

    left_join(workflow_specialist_1C, by = c("user_id" = "data.id")) %>%

    left_join(user, by = c("charterer_version_signed_by_uid" = "data.id")) %>%

    transmute(Клиент = juridical_person_version_short_title,
                    Номер_заявки = short_id,
                    Номер_счёта = number_doc,
                    Сумма_счёта = invoice_amount,
                    Дата = as.Date(strftime(created_at_original, format="%Y-%m-%d")),
                    ДатаВыс = as.Date(strftime(created_at, format="%Y-%m-%d")),
                    Дата_дос = as.Date(strftime(delivery_date, format="%Y-%m-%d")),
                    Сроки_верификации = data.docVerification.days,
                    Верификация_счёта_плановая_дата = as.Date(strftime(Дата_дос + Сроки_верификации, format="%Y-%m-%d")),
                    Специалист = data.title.x,
                    Менеджер = data.title.y,
                    Оплата = case_when(status == "PartialPaid" ~ "Частично оплачен",
                                       status == "Paid" ~ "Оплачен",
                                       status =="NotPaid" ~ "Не оплачен",
                                       status == "ForPayment" ~ "Не оплачен",
                                       TRUE ~ status)
                    ) %>%

    group_by(Клиент, Номер_счёта, Сумма_счёта, Дата, ДатаВыс, Дата_дос, Сроки_верификации, Верификация_счёта_плановая_дата, Специалист, Менеджер, Оплата) %>%
    summarize(Клиент = first(Клиент),
                    Номер_счёта = first(Номер_счёта),
                    Сумма_счёта = first(Сумма_счёта),
                    Дата = first(Дата),
                    ДатаВыс = first(ДатаВыс),
                    Дата_дос = first(Дата_дос),
                    Сроки_верификации = first(Сроки_верификации),
                    Верификация_счёта_плановая_дата = first(Верификация_счёта_плановая_дата),
                    Заявки = n(),
                    Номера_заявок = toString(unique(Номер_заявки)),
                    Специалист = first(Специалист),
                    Менеджер = first(Менеджер),
                    Оплата = first(Оплата)
    ) %>%

    mutate(Номер_счёта = gsub(" ", "", Номер_счёта)) %>%

    arrange(Дата, Клиент)
}

getPriceIndicative <- function(startDate, endDate){
  
  my_query <- query('{
"bool": {
      "must": [
        {
  "match": {"isActual": "true"}
        }
      ]
}
}')
  
#   vehicles_selected_field <- select_fields('{
#   "includes": ["id", "data.typesOfBody", "data.payload", "data.cubicCapacity"]
# }')
#   
#   vehicles <- elastic(cfg$db_elastic$url,
#                       "vehicle-version-index") %search% (my_query + vehicles_selected_field) %>%
#     mutate(data.typesOfBody = as.character(data.typesOfBody))
  
  glossary_selected_field <- select_fields('{
  "includes": ["data.key", "data.title"]
}')
  
  glossary <- elastic(cfg$db_elastic$url,
                      "glossary-element-version-index") %search% (my_query + glossary_selected_field)

  requests_by_day <- dbGetQuery(con,
                                  paste("SELECT requests.short_id,
                                  requests.created_at,
                     requests.charterer_rate_amount,
                     requests.vehicle_requirements_types_of_body,
                     requests.vehicle_requirements_carrying_capacity_and_volume,
                     requests.charterer_version_title as client,
                     first_loading.locality as start_point,
                     first_loading.region as start_region,
                     first_loading.latitude as loading_latitude,
                     first_loading.longitude as loading_longitude,
                     last_unloading.locality as finish_point,
                     last_unloading.region as finish_region,
                     last_unloading.latitude as unloading_latitude,
                     last_unloading.longitude as unloading_longitude,
                     transportations.carrier_rate_info_amount,
                     transportations.profit_amount,
                     transportations.carrier_rate_info_with_nds as nds

                     FROM erp_requests.requests

                   LEFT JOIN erp_requests.transportations
                   ON requests.id = transportations.request_id

left join (
    select distinct on (s.request_id)
          request_id, id, locality, \"from\" as from_at, region, latitude, longitude
    from erp_requests.request_start_route_points s
    order by s.request_id desc, s.from asc
) as first_loading
on requests.id = first_loading.request_id

left join (
    select distinct on (s.request_id)
          request_id, id, locality, \"from\" as from_at, region, latitude, longitude
    from erp_requests.request_finish_route_points s
    order by s.request_id desc, s.from desc
) as last_unloading
on requests.id = last_unloading.request_id

                   WHERE requests.is_actual=true
                   AND requests.created_at >=", paste0("'", startDate, "'::timestamp"), "AND requests.created_at <=", paste0("'",endDate, "'::timestamp"),
                                        
                    "AND requests.status not in ('cancelled', 'transportationCancelled', 'active');")) %>%
    
    as_tibble() %>%
    
    # left_join(vehicles, by = c("version_id" = "id")) %>%
    
    mutate(vehicle_requirements_types_of_body = as.character(gsub('\\{|\\}|\\"|\\"', "", vehicle_requirements_types_of_body))) %>%
    
    left_join(glossary, by = c("vehicle_requirements_types_of_body" = "data.key")) %>%
    
    left_join(glossary, by = c("vehicle_requirements_carrying_capacity_and_volume" = "data.key")) %>%

  transmute(Рейсы = short_id,
            Клиент = client,
            Дата_создания_заявки = as.Date(strftime(created_at, format="%Y-%m-%d")),
            Регион_загрузки = start_region,
            Регион_выгрузки = finish_region,
            Маршрут = paste(start_point, "-", finish_point),
            Ставка_клиента_НДС = case_when(str_detect(client, "НДС") ~ charterer_rate_amount * 1.2,
                                       TRUE ~ charterer_rate_amount),
            Ставка_клиента = case_when(str_detect(client, "НДС") ~ charterer_rate_amount,
                                           TRUE ~ charterer_rate_amount / 1.2),
            Ставка_перевозчика_НДС = case_when(nds == TRUE ~ carrier_rate_info_amount,
                            nds == FALSE ~ carrier_rate_info_amount * 1.2),
            Ставка_перевозчика = case_when(nds == TRUE ~ carrier_rate_info_amount/ 1.2,
                                           nds == FALSE ~ carrier_rate_info_amount),
            Маржа = profit_amount,
            Тип_ТС = data.title.x,
            Объём_тоннаж = gsub("<sup>3</sup>", "\U00B3 ", data.title.y),
            loading_latitude = loading_latitude,
            loading_longitude = loading_longitude,
            unloading_latitude = unloading_latitude,
            unloading_longitude = unloading_longitude
  )

  # requests_by_day$Тип_ТС[is.na(requests_by_day$Тип_ТС)] <- "-"

  return(requests_by_day)


}

# Индикатив цен по Транспореону 
getPriceIndicative1 <- function(startDate, endDate){
  
  my_query <- query('{
"bool": {
      "must": [
        {
  "match": {"isActual": "true"}
        }
      ]
}
}')
  
  glossary_selected_field <- select_fields('{
  "includes": ["data.key", "data.title"]
}')
  
  glossary <- elastic(cfg$db_elastic$url,
                      "glossary-element-version-index") %search% (my_query + glossary_selected_field)
  
  requests_by_day <- dbGetQuery(con2,
                                paste("SELECT 
                                COALESCE(start_trading_on_source_at, created_on_source_at, created_at) as created_at_my,
                                base_price_net,
                                targets->0->'address'->>'locality' AS loading_address,
                                targets->-1->'address'->>'locality' AS unloading_address,
                                vehicle_requirements->>'types_of_body' AS types_of_body

                                FROM orders WHERE source_name = 'transporeon'
                                AND COALESCE(start_trading_on_source_at, created_on_source_at, created_at) >=", paste0("'", startDate, "'::timestamp"), "AND COALESCE(start_trading_on_source_at, created_on_source_at, created_at) <=", paste0("'",endDate, "'::timestamp;")
                                      )) %>%

    as_tibble() %>%

    transmute(Дата_создания_заявки = as.Date(strftime(created_at_my, format="%Y-%m-%d")),
                                 Маршрут = paste(loading_address, "-", unloading_address),
                                 base_price_net = base_price_net,
                                 base_price_gross = base_price_net * 1.2,
                                 Тип_ТС = types_of_body

    ) %>%

    mutate(Тип_ТС = gsub('\\[|\\]|\\"|\\"', "", Тип_ТС)) %>%

    left_join(glossary, by = c("Тип_ТС" = "data.key")) %>%

    mutate(Тип_ТС = data.title)

  requests_by_day$Тип_ТС[is.na(requests_by_day$Тип_ТС)] <- "-"

  return(requests_by_day)

}

# Индикатив цен по ИнБеву
getPriceIndicative2 <- function(startDate, endDate){
  
  my_query <- query('{
"bool": {
      "must": [
        {
  "match": {"isActual": "true"}
        }
      ]
}
}')
  
  glossary_selected_field <- select_fields('{
  "includes": ["data.key", "data.title"]
}')
  
  glossary <- elastic(cfg$db_elastic$url,
                      "glossary-element-version-index") %search% (my_query + glossary_selected_field)
  
  requests_by_day <- dbGetQuery(con2,
                                paste("SELECT external_id_params->>'tnx_cargo_order_name' AS external_id,
                                COALESCE(start_trading_on_source_at, created_on_source_at, created_at) as created_at_my,
                                base_price_net,
                                targets->0->'address'->>'locality' AS loading_address,
                                targets->-1->'address'->>'locality' AS unloading_address,
                                vehicle_requirements->>'types_of_body' AS types_of_body
                                FROM orders WHERE source_name = 'inbev'
                                AND COALESCE(start_trading_on_source_at, created_on_source_at, created_at) >=", paste0("'", startDate, "'::timestamp"), "AND COALESCE(start_trading_on_source_at, created_on_source_at, created_at) <=", paste0("'",endDate, "'::timestamp"),
                             "ORDER BY created_at_my;")) %>%

    as_tibble() %>%

    group_by(external_id) %>%

    summarise(created_on_source_at = last(created_at_my),
              loading_address = last(loading_address),
              unloading_address = last(unloading_address),
              types_of_body = last(types_of_body),
              base_price_net = last(base_price_net)
    ) %>%

    transmute(Дата_создания_заявки = as.Date(strftime(created_on_source_at, format="%Y-%m-%d")),
                                 Маршрут = paste(loading_address, "-", unloading_address),
                                 base_price_net = base_price_net,
                                 base_price_gross = base_price_net * 1.2,
                                 Тип_ТС = types_of_body

    ) %>%

    mutate(Тип_ТС = gsub('\\[|\\]|\\"|\\"', "", Тип_ТС)) %>%

    left_join(glossary, by = c("Тип_ТС" = "data.key")) %>%

    mutate(Тип_ТС = data.title)

  requests_by_day$Тип_ТС[is.na(requests_by_day$Тип_ТС)] <- "-"

  return(requests_by_day)

}


# Отчёт для главного бухгалтера - соответствие проверенных документов и выставленных счетов
getBillsNew <- function(){
  
  my_query <- query('{
"bool": {
      "must": [
        {
  "match": {"isActual": "true"}
        }
      ]
}
}')
  
  selected_field_user <- select_fields('{
  "includes": ["data.id", "data.title"]
}')
  
  user = elastic(cfg$db_elastic$url,
                 "user-profile-version-index") %search% (my_query + selected_field_user) %>%
    group_by(data.id) %>%
    summarise(data.title = first(data.title))

  verified_requests_by_day <- dbGetQuery(con,
                                         "SELECT requests.short_id,
                                          requests.juridical_person_version_short_title,
                                          requests.charterer_rate_amount,
jsonb_path_query_array(transportations.statuses_history, '$[*] ? (@.Status == \"paid\" || @.Status == \"docsApproved\")')->0->>'IssueDateTime' AS IssueDateTime,
           charterer_bill_info_number_doc,
           juridical_persons.responsible_employee_processing_accounts_id
           FROM erp_requests.requests
           INNER JOIN erp_requests.transportations
           ON requests.id = transportations.request_id

           INNER JOIN (SELECT juridical_persons.id, juridical_persons.is_actual, juridicals.responsible_employee_processing_doc_id, juridicals.responsible_employee_processing_accounts_id FROM erp_juridical_persons.juridical_persons INNER JOIN erp_clients.juridicals ON juridical_persons.id = juridicals.juridical_person_id AND juridical_persons.version = juridicals.juridical_person_version) as juridical_persons
                    ON requests.juridical_person_version_id = juridical_persons.id AND juridical_persons.is_actual

           WHERE requests.is_actual=true and
           jsonb_path_exists(requests.statuses_history::jsonb, '$[*] ? (@.Status == \"carrierFound\")') = true AND
           jsonb_path_exists(transportations.statuses_history::jsonb, '$[*] ? (@.Status == \"paid\" || @.Status == \"docsApproved\")') = true AND
                                       requests.created_at >= '2021-05-31 21:00:00'::timestamp;") %>%

    as_tibble() %>%

    left_join(user, by = c("responsible_employee_processing_accounts_id" = "data.id")) %>%

    transmute(Дата_проверки = as.Date(strftime(issuedatetime, format="%Y-%m-%d")),
                  Номер_заявки = short_id,
                  Ставка_клиента = charterer_rate_amount,
                  Номер_счёта = charterer_bill_info_number_doc,
                  Специалист = data.title,
                  Клиент = juridical_person_version_short_title
                  )

  workflow_specialist_1C <- dbGetQuery(con,
    'SELECT original_id AS "data.id", title AS "data.title" FROM erp_users.user_profiles WHERE is_actual') %>%
    distinct(data.id, .keep_all = TRUE)

  bills_by_day <- dbGetQuery(con,
                             "select requests.juridical_person_version_short_title,
                               requests.short_id,
                               requests.charterer_rate_amount,
                               bills.number_doc,
                               bills.invoice_amount,
                               bills.created_at_original,
                               bills.created_at,
                               bills.user_id
FROM erp_requests.requests
INNER JOIN erp_bills.bills
ON requests.id = ANY(bills.request_ids) AND bills.is_actual
where requests.is_actual
AND bills.created_at_original >= '2020-12-31 21:00:00'::timestamp
                               AND bills.type = 'Charterer';") %>%

    as_tibble() %>%

    left_join(workflow_specialist_1C, by = c("user_id" = "data.id")) %>%

    transmute(Клиент = juridical_person_version_short_title,
                    Номер_заявки = short_id,
                    Номер_счёта = number_doc,
                    Сумма_счёта = charterer_rate_amount,
                    Дата = as.Date(strftime(created_at_original, format="%Y-%m-%d")),
                    ДатаВыс = as.Date(strftime(created_at, format="%Y-%m-%d")),
                    Специалист = data.title)

  result <- verified_requests_by_day %>%
    left_join(bills_by_day, by = c("Номер_заявки" = "Номер_заявки", "Специалист" = "Специалист", "Номер_счёта" = "Номер_счёта", "Клиент" = "Клиент")) %>%

    group_by(Номер_заявки) %>%

    summarise(Специалист = first(Специалист),
                        Клиент = first(Клиент),
                        Дата_проверки = first(Дата_проверки),
                        Номер_заявки = first(Номер_заявки),
                        Ставка_клиента = first(Ставка_клиента),
                        Номер_счёта = first(Номер_счёта),
                        Сумма_счёта = first(Сумма_счёта),
                        Дата = first(Дата),
                        ДатаВыс = first(ДатаВыс)
                        )

  result$Сумма_счёта[is.na(result$Сумма_счёта)] <- 0

  return(result)

}

# Сорванные рейсы
getBreakdowns_new <- function(startDate, endDate){

  requests_all <- dbGetQuery(con,
                  paste("SELECT requests.short_id,
                  requests.created_at,
                  requests.juridical_person_version_short_title as client,
                  requests.responsible_expeditor_title,
                  requests.notes AS notes,
                  requests.status,
                  requests.relative_request_short_id,
                  request_start_route_points.from as loading_plan
                  FROM erp_requests.requests
                  LEFT JOIN erp_requests.request_start_route_points
                  ON requests.id = request_start_route_points.request_id
                  

                  WHERE requests.is_actual = true
                  AND requests.created_at >=", paste0("'", startDate, "'::timestamp"), "AND requests.created_at <=", paste0("'",endDate, "'::timestamp;"))) %>%

    as_tibble() %>%

    arrange(loading_plan) %>%

    group_by(short_id) %>%

    summarise(created_at = first(created_at),
              client = first(client),
              responsible_expeditor_title = first(responsible_expeditor_title),
              notes = first(notes),
              status = first(status),
              relative_request_short_id = first(relative_request_short_id),
              loading_plan = first(loading_plan)
              ) %>%

    transmute(Номер_заявки = short_id,
              Дата = as.Date(strftime(created_at, format="%Y-%m-%d")),
              Клиент = client,
              Статус_заявки = case_when(status == "carrierFound" ~ "Перевозчик найден",
                                                    status == "active" ~ "Заявка активна",
                                                    status == "docsReceived" ~ "Документы получены клиентом",
                                                    status == "docsApproved" ~ "Документы подтверждены клиентом",
                                                    status == "docsGrouped" ~ "Документы сгруппированы",
                                                    status == "docsProblem" ~ "Есть проблемы с документами",
                                                    status == "docsSent" ~ "Документы высланы клиенту",
                                                    status == "cancelled" ~ "Заявка отменена",
                                                    status == "transportationCancelled" ~ "Перевозка отменена",
                                                    TRUE ~ as.character(status)),
                          Ответственный_логист = responsible_expeditor_title,
                          Заметки = notes,
                          Связанная_заявка = relative_request_short_id,
                          Дата_загрузки = as.Date(strftime(loading_plan, format="%Y-%m-%d"))
    ) %>%

      filter(!is.na(Ответственный_логист))

      # Отказы от рейса
      canceling_byDay <- requests_all %>%
        filter((Статус_заявки == "Заявка отменена" | (Статус_заявки == "Перевозка отменена" & stringr::str_detect(Заметки, 'Отказ клиента'))) & is.na(Связанная_заявка)) %>%
        anti_join(requests_all, by = c("Номер_заявки" = "Связанная_заявка")) %>%
        group_by(Дата, Клиент) %>%
        summarise(Рейсы = n())

      # Срывы рейсов
      breakdowns_byDay <- requests_all %>%
        filter((Статус_заявки == "Перевозка отменена" & !stringr::str_detect(Заметки, 'Отказ клиента') & is.na(Связанная_заявка)) | (is.na(Связанная_заявка) & Статус_заявки == "Заявка отменена" & stringr::str_detect(Заметки, 'Не нашли перевозчика')) | (is.na(Связанная_заявка) & Статус_заявки == "Заявка активна" & (Sys.Date() - Дата_загрузки > 0))) %>%
        anti_join(requests_all, by = c("Номер_заявки" = "Связанная_заявка")) %>%
        group_by(Дата, Клиент) %>%
        summarise(Рейсы = n())

      # Срывы рейсов с номерами заявок
      breakdowns_byDay_detail <- requests_all %>%
        filter((Статус_заявки == "Перевозка отменена" & !stringr::str_detect(Заметки, 'Отказ клиента') & is.na(Связанная_заявка)) | (is.na(Связанная_заявка) & Статус_заявки == "Заявка отменена" & stringr::str_detect(Заметки, 'Не нашли перевозчика'))) %>%
        anti_join(requests_all, by = c("Номер_заявки" = "Связанная_заявка")) %>%
        group_by(Дата, Клиент) %>%
        summarise(Рейсы = n(), Номера_заявок = toString(unique(Номер_заявки)))

      # Просроченные заявки
      delay_byDay <- requests_all %>%
        filter((is.na(Связанная_заявка) & Статус_заявки == "Заявка активна" & (Sys.Date() - Дата_загрузки > 0))) %>%
        anti_join(requests_all, by = c("Номер_заявки" = "Связанная_заявка")) %>%
        group_by(Дата, Клиент) %>%
        summarise(Рейсы = n())

      # Все рейсы
      requests_all_byDay <- requests_all %>%
        group_by(Дата, Клиент) %>%
        summarise(Рейсы = n())

      result1 <- requests_all_byDay %>%
        left_join(canceling_byDay, by = c("Дата" = "Дата", "Клиент" = "Клиент"))

      result1$Рейсы.x[is.na(result1$Рейсы.x)] <- 0
      result1$Рейсы.y[is.na(result1$Рейсы.y)] <- 0

      result2 <- requests_all_byDay %>%
        left_join(breakdowns_byDay, by = c("Дата" = "Дата", "Клиент" = "Клиент")) %>%
        left_join(breakdowns_byDay_detail, by = c("Дата" = "Дата", "Клиент" = "Клиент")) %>%
        left_join(delay_byDay, by = c("Дата" = "Дата", "Клиент" = "Клиент"))

      result2$Рейсы.x[is.na(result2$Рейсы.x)] <- 0
      result2$Рейсы.y[is.na(result2$Рейсы.y)] <- 0
      result2$Рейсы.x.x[is.na(result2$Рейсы.x.x)] <- 0
      result2$Рейсы.y.y[is.na(result2$Рейсы.y.y)] <- 0
      result2$Номера_заявок[is.na(result2$Номера_заявок)] <- "-"


      resultList <- list("canceling" = result1, "breakdowns" = result2)

      return(resultList)
}

# Количество проведённых актов перевозчиков по сотрудникам ДО для мотивации
carriedOutActs <- function(startDate, endDate){
  requests_by_day <- dbGetQuery(con,
                                paste("SELECT 
                               requests.short_id,
                               certificates.number,
                               certificates.created_at_original,
                               certificates.created_at,
                               certificates.user_title,
                               certificates.ready_at
FROM erp_requests.requests
INNER JOIN (select certificates.number,
                   certificates.created_at_original,
                   certificates.created_at,
                   certificates.user_title,
                   certificates.ready_at,
                   unnest(certificates.request_ids) as request_id

            from erp_bills.certificates 
            
            where certificates.is_actual AND certificates.type = 'Carrier'
            
            ) as certificates
ON requests.id = certificates.request_id
where requests.is_actual
                   AND certificates.created_at >=", paste0("'", startDate, "'::timestamp"), "AND certificates.created_at <=", paste0("'",endDate, "'::timestamp;"))) %>%

    as_tibble() %>%
    
    transmute("Номер_заявки" = short_id,
              "Номер_акта_перевозчика" = number,
              "Дата_акта_перевозчика" = as.Date(strftime(created_at_original, format="%Y-%m-%d")),
              "Дата_проведения_акта_перевозчика" = as.Date(strftime(created_at, format="%Y-%m-%d")),
              "Специалист" = user_title
              ) %>%
    group_by(Номер_акта_перевозчика) %>%
    summarise(Номер_заявки = toString(unique(Номер_заявки)),
              Дата_акта_перевозчика = first(Дата_акта_перевозчика),
              Дата_проведения_акта_перевозчика = first(Дата_проведения_акта_перевозчика),
              Специалист = first(Специалист)
              )
}

# Время наибольшей загруженности
getCountRequests <- function(){
    requests_by_day <- dbGetQuery(con,
                                        "SELECT created_at
                                              FROM erp_requests.requests
                                              WHERE is_actual=true AND
                                              status not in ('cancelled', 'transportationCancelled', 'active')
                                              ORDER BY 1;") %>%
      as_tibble() %>%
      transmute(Время = lubridate::round_date(created_at, "30 minutes")
) %>%
      group_by(Время) %>%
      summarise(Заявки = n())

}


getListOfLogists <- function(){
  listOfLogist <- dbGetQuery(con3,
                                "SELECT u.\"Description\" FROM \"Users\" u
JOIN \"RelationOfUserAndRole\" ROUAR on u.\"Id\" = ROUAR.\"UserId\"
JOIN \"Roles\" R on R.\"Id\" = ROUAR.\"RoleId\"
WHERE NOT u.\"IsArchived\" AND R.\"Name\" = 'expeditor';")
}

getListOfClients <- function(){
  listOfClients <- dbGetQuery(con,
                              "SELECT juridical_persons.short_title
                              FROM erp_clients.juridicals
                              INNER JOIN erp_juridical_persons.juridical_persons
                              ON juridicals.juridical_person_id = juridical_persons.id AND juridicals.juridical_person_version = juridical_persons.version
                              INNER JOIN erp_clients.clients
                              ON juridicals.client_id = clients.id AND juridicals.client_version = clients.version
                              where clients.is_actual = true and juridical_persons.is_actual = true;") %>%

    group_by(short_title) %>%

    summarise(short_title = first(short_title))
}

# Время поиска перевозчика - по событиям из ERP и по количеству рейсов к рабочим дням логистов
getAverageTimeToCloseDeal <- function(startDate, endDate){
  
  my_query <- query('{
"bool": {
      "must": [
        {
  "match": {"isActual": "true"}
        }
      ]
}
}')
  
  selected_field_user <- select_fields('{
  "includes": ["data.id", "data.title"]
}')

  users = elastic(cfg$db_elastic$url,
                 "user-profile-version-index") %search% (my_query + selected_field_user) %>%
    group_by(data.id) %>%
    summarise(data.title = first(data.title)) 

#     carrier_selected_field <- select_fields('{
#     "includes": ["data.id", "data.createdAt", "data.securityStatus.dateLastUpdated"]
#   }')
# 
#     # Перевозчики
#     carriers = elastic(cfg$db_elastic$url,
#                        "carrier-version-index") %search% (my_query + carrier_selected_field)

  requests_by_day <- dbGetQuery(con,
                                paste("SELECT 
                                transportations.created_at AS finish_time,
                                requests.created_at AS start_time,
                                requests.short_id,
                                requests.created_by,
                                requests.charterer_rate_amount,
                                requests.responsible_expeditor_title,
                                requests.type,
                                request_start_route_points.from as loading_plan,
                                request_start_route_points.region as start_region,
                                request_finish_route_points.region as finish_region

                                FROM erp_requests.requests
                                
                                LEFT JOIN erp_requests.transportations
                                ON requests.id = transportations.request_id

                              LEFT JOIN erp_requests.request_start_route_points
                              ON requests.id = request_start_route_points.request_id
                              

                              LEFT JOIN erp_requests.request_finish_route_points
                              ON requests.id = request_finish_route_points.request_id
                              
                                
                                WHERE requests.is_actual=true
                                AND requests.created_at >=", paste0("'", startDate, "'::timestamp"), "AND requests.created_at <=", paste0("'",endDate, "'::timestamp"),
                                      "AND requests.status not in ('cancelled', 'transportationCancelled', 'active')
                                      ORDER BY request_start_route_points.from, request_finish_route_points.from;")) %>%
    
    as_tibble() %>%
    
    group_by(short_id) %>%
    
    summarise(start_time = first(start_time),
              finish_time = first(finish_time),
              loading_plan = first(loading_plan),
              created_by = first(created_by),
              charterer_rate_amount = first(charterer_rate_amount),
              responsible_expeditor_title = first(responsible_expeditor_title),
              type = first(type),
              start_region = first(start_region),
              finish_region = last(finish_region)
    ) %>%
    
    # left_join(carriers, by = c("carrier_version_id" = "data.id")) %>%
    
    left_join(users, by = c("created_by" = "data.id")) %>%
    
    transmute(Дата = as.Date(strftime(finish_time, format="%Y-%m-%d")),
                  Заявка = short_id,
                  Дата_заявки = strftime(start_time, format="%Y-%m-%d %H:%M:%S"),
                  Дата_перевозки = strftime(finish_time, format="%Y-%m-%d %H:%M:%S"),
                  Дата_загрузки = strftime(loading_plan, format="%Y-%m-%d %H:%M:%S"), 
                  Создатель = data.title,
                  Ставка = charterer_rate_amount,
                  Логист = responsible_expeditor_title,
                  Тип = case_when(type == "Spot" ~ "Спот",
                                  type == "Warranty" ~ "Гарантия"),
                  География = case_when(start_region == finish_region ~ "Город",
                                        TRUE ~ "Межгород"),
                  Маршрут = paste(start_region, "\U2192", finish_region),
                  # Дата_создания_карты_перевозчика = as.Date(as.POSIXct(data.createdAt / 1000, origin = "1970-01-01")),
                  # Условие = case_when(as.numeric(difftime(as.POSIXct(data.createdAt / 1000, origin = "1970-01-01"), strptime(active_time, format = "%Y-%m-%dT%H:%M:%S"))) > 0 ~ "Перевозчика был уже проверен",
                  #                     as.numeric(difftime(as.POSIXct(data.createdAt / 1000, origin = "1970-01-01"), strptime(active_time, format = "%Y-%m-%dT%H:%M:%S"))) < 0 ~ "Перевозчик изначально не был проверен"),
                  Время = as.numeric(difftime(strptime(finish_time, format = "%Y-%m-%d %H:%M:%S"), strptime(start_time, format = "%Y-%m-%d %H:%M:%S"), units = "hours")))
} 


getCountDrivers <- function(){
  drivers =
    dbGetQuery(con1,
               "SELECT
              	(case 
              		when security_status_value = 0 then 'Ожидает проверки'
              		when security_status_value = 1 then 'Допущен к работе'
              		when security_status_value = 2 then 'Требует перепроверки'
              		when security_status_value = 3 then 'Запрещено работать'
              		when security_status_value = 4 then 'Временно допущен'
              	end) as security_status_value,
              	count(*) AS Количество
              FROM profiles.drivers 
              WHERE NOT is_deleted
              GROUP BY security_status_value
              ORDER BY Количество DESC;") 
  
  drivers <- drivers %>%
    mutate (Доля = Количество / sum(drivers$Количество))
}

getCountCarriers <- function(){

  my_query <- query('{
"bool": {
      "must": [
        {
  "match": {"isActual": "true"}
        }
      ]
}
}')
  
  carrier_selected_field <- select_fields('{
  "includes": ["data.securityStatus.statusValue"]
}')
  
#   avg_sepal_width <- aggs('{
#   "avg_sepal_width_per_species": {
#     "terms": {
#       "field": "data.securityStatus.statusValue",
#       "size": 5
#     },
#     "aggs": {
#       "avg_sepal_width": {
#         "count": {
#           "field": "sepal_width"
#         }
#       }
#     }
#   }
# }')
  
  carriers = elastic(cfg$db_elastic$url,
                     "carrier-version-index") %search% (my_query + carrier_selected_field) %>%
    
    group_by(data.securityStatus.statusValue) %>%
    
    summarise(Количество = n()) %>%  
  
    mutate(data.securityStatus.statusValue = case_when(data.securityStatus.statusValue == 0 ~ "Ожидает проверки",
                                                       data.securityStatus.statusValue == 1 ~ "Допущен к работе",
                                                       data.securityStatus.statusValue == 2 ~ "Требует перепроверки",
                                                       data.securityStatus.statusValue == 3 ~ "Запрещено работать",
                                                       data.securityStatus.statusValue == 4 ~ "Временно допущен"
    )) %>%
    arrange(desc(Количество))
  
  carriers <- carriers %>%
    mutate (Доля = Количество / sum(carriers$Количество))
}

getCountVehicles <- function(){
  
  my_query <- query('{
"bool": {
      "must": [
        {
  "match": {"isActual": "true"}
        }
      ]
}
}')
  
    vehicles_selected_field <- select_fields('{
    "includes": ["id"]
  }')

    vehicles <- elastic(cfg$db_elastic$url,
                        "vehicle-version-index") %search% (my_query + vehicles_selected_field) %>%
    summarise(Количество = n())
}

# Движения ДС
getCash <- function(){
  
  my_query <- query('{
"bool": {
      "must": [
        {
  "match": {"isActual": "true"}
        }
      ]
}
}')
  
  selected_field_user <- select_fields('{
  "includes": ["data.id", "data.title"]
}')
  
  user = elastic(cfg$db_elastic$url,
                 "user-profile-version-index") %search% (my_query + selected_field_user) %>%
    group_by(data.id) %>%
    summarise(data.title = first(data.title))
  
  verified_requests_by_day <- dbGetQuery(con,
                                         "SELECT requests.short_id,
                                          requests.juridical_person_version_short_title,
                                          requests.charterer_rate_amount,
           juridical_persons.responsible_employee_processing_accounts_id,
           transportations.status
           FROM erp_requests.requests
           
           INNER JOIN erp_requests.transportations
           ON requests.id = transportations.request_id 

           INNER JOIN (SELECT juridical_persons.id, juridical_persons.is_actual, juridicals.responsible_employee_processing_doc_id, juridicals.responsible_employee_processing_accounts_id FROM erp_juridical_persons.juridical_persons INNER JOIN erp_clients.juridicals ON juridical_persons.id = juridicals.juridical_person_id AND juridical_persons.version = juridicals.juridical_person_version) as juridical_persons
                    ON requests.juridical_person_version_id = juridical_persons.id AND juridical_persons.is_actual
            
          LEFT JOIN erp_bills.bills
          ON requests.id = ANY(bills.request_ids) AND bills.is_actual

           WHERE requests.is_actual=true
           AND requests.status not in ('cancelled', 'transportationCancelled', 'active')
           AND bills.type = 'Charterer'
           AND bills.status != 'Paid';") %>%
    
    as_tibble() %>%
    
    group_by(short_id) %>%
    
    summarise(charterer_rate_amount = first(charterer_rate_amount),
              responsible_employee_processing_accounts_id = first(responsible_employee_processing_accounts_id),
              juridical_person_version_short_title = first(juridical_person_version_short_title),
              status = first(status)
              ) %>%
    
    left_join(user, by = c("responsible_employee_processing_accounts_id" = "data.id")) %>%
    
    transmute(
                           Номер_заявки = short_id,
                           Количество = 1,
                           Ставка_клиента = charterer_rate_amount,
                           Специалист = data.title,
                           Клиент = juridical_person_version_short_title,
                           Статус = case_when(status == "ready" ~ "Перевозка началась",
                                                        status == "onStartPoint" ~ "Водитель прибыл на загрузку",
                                                        status == "loaded" ~ "Водитель в пути",
                                                        status == "onTheWay" ~ "Пройдена половина пути, всё хорошо",
                                                        status == "onFinishPoint" ~ "Водитель прибыл на разгрузку",
                                                        status == "unloaded" ~ "Водитель разгрузился, ждём документы",
                                                        status == "docsReceived" ~ "Получены документы от водителя",
                                                        status == "paid" ~ "Перевозка оплачена",
                                                        status == "cancelled" ~ "Перевозка отменена",
                                                        status == "docsProblem" ~ "У водителя проблемы с документами",
                                                        status == "docsApproved" ~ "Документы водителя подтверждены"
                           )
                           )

}


getSpecialDistributionCenters <- function(startDate, endDate){
  
  my_query <- query('{
"bool": {
      "must": [
        {
  "match": {"isActual": "true"}
        }
      ]
}
}')
  
  selected_field_expeditors_group <- select_fields('{
  "includes": ["data.id", "data.name"]
}')
  
  expeditors_group <- elastic(cfg$db_elastic$url,
                              "expeditors-group-version-index") %search% (my_query + selected_field_expeditors_group)
  
  selected_field_user <- select_fields('{
  "includes": ["data.id", "data.title"]
}')
  
  user = elastic(cfg$db_elastic$url,
                 "user-profile-version-index") %search% (my_query + selected_field_user) %>%
    group_by(data.id) %>%
    summarise(data.title = first(data.title))
  
  requests_by_day <- dbGetQuery(con,
                                paste("SELECT requests.short_id,
                                requests.external_number,
                                requests.created_at,
                                requests.juridical_person_version_short_title as client,
                                requests.responsible_expeditor_division,
                                requests.responsible_expeditor_title,
                                requests.charterer_version_signed_by_uid as manager,

                                
                                transportations.juridical_person_version_short_title as carrier,

                                request_start_route_points.address as loading_address,
                                request_start_route_points.from as loading_plan,
                                request_start_route_points.type as loading_type,
                                request_start_route_points.fact_arrival_date_time as loading_fact,

                                request_finish_route_points.address as unloading_address,
                                request_finish_route_points.from as unloading_plan,
                                request_finish_route_points.consignee,
                                request_finish_route_points.type as unloading_type,
                                request_finish_route_points.fact_departure_date_time as unloading_fact,


                                transportation_driver_versions.full_name,
                                transportation_driver_versions.phone_numbers,
                                transportation_vehicle_versions.number_plate,
                                transportation_vehicle_versions.type as vehicle_type
                                
                                                                
                                FROM erp_requests.requests

                   LEFT JOIN erp_requests.transportations
                   ON requests.id = transportations.request_id

                   LEFT JOIN (SELECT request_start_route_points.address,
                   request_start_route_points.fact_arrival_date_time,
                   request_start_route_points.from,
                   addresses.type, request_start_route_points.request_id FROM erp_requests.request_start_route_points INNER JOIN erp_addresses.addresses ON request_start_route_points.address_id = addresses.id AND request_start_route_points.address_version = addresses.version) as request_start_route_points
                    ON requests.id = request_start_route_points.request_id


                   LEFT JOIN (SELECT request_finish_route_points.address,
                   request_finish_route_points.fact_departure_date_time,
                   request_finish_route_points.from,
                   request_finish_route_points.consignee,
                   addresses.type, request_finish_route_points.request_id FROM erp_requests.request_finish_route_points INNER JOIN erp_addresses.addresses ON request_finish_route_points.address_id = addresses.id AND request_finish_route_points.address_version = addresses.version) as request_finish_route_points
                    ON requests.id = request_finish_route_points.request_id
                    
                    
                    LEFT JOIN erp_requests.transportation_driver_versions
                    ON requests.id = transportation_driver_versions.transportation_request_id

                    LEFT JOIN erp_requests.transportation_vehicle_versions
                    ON requests.id = transportation_vehicle_versions.transportation_request_id

                   WHERE requests.is_actual=true 
                   
                   AND (request_start_route_points.type = 'dc_special' OR request_finish_route_points.type = 'dc_special' 
                   OR request_finish_route_points.consignee ilike '%Демо РЦ 04%' OR request_finish_route_points.consignee ilike '%Демо РЦ 05%'
                   OR request_finish_route_points.consignee ilike '%Демо РЦ 06%' OR request_finish_route_points.consignee ilike '%Демо РЦ 07%'
                   OR request_finish_route_points.consignee ilike '%Демо РЦ 08%'
                   OR request_finish_route_points.consignee ilike '%Демо РЦ 09%'
                   OR request_finish_route_points.consignee = 'Демо РЦ 02'
                   OR request_finish_route_points.consignee = 'Демо РЦ 03'
                   OR request_finish_route_points.consignee = 'Демо РЦ 01')

                   AND request_start_route_points.from >=", paste0("'", startDate, "'::timestamp"), "AND request_start_route_points.from <=", paste0("'",endDate, "'::timestamp"),
                    "AND requests.status in ('carrierFound', 'active');")) %>%
    
    arrange(loading_plan, unloading_plan) %>%
    
    group_by(short_id) %>%

    summarise(created_at = first(created_at),
              external_number = first(external_number),
              created_at = first(created_at),
              client = first(client),
              responsible_expeditor_division = first(responsible_expeditor_division),
              responsible_expeditor_title = first(responsible_expeditor_title),
              manager = first(manager),
              carrier = first(carrier),
              loading_address = first(loading_address),
              loading_plan = first(loading_plan),
              loading_fact = first(loading_fact),
              loading_type = first(loading_type),
              unloading_address = last(unloading_address),
              unloading_plan = last(unloading_plan),
              unloading_fact = last(unloading_fact),
              unloading_type = last(unloading_type),
              consignee = last(consignee),
              full_name = first(full_name),
              phone_numbers = first(phone_numbers),
              number_plate = first(number_plate),
              vehicle_type = first(vehicle_type)
              ) %>%
    
    left_join(expeditors_group, by = c("responsible_expeditor_division" = "data.id")) %>%
    
    left_join(user, by = c("manager" = "data.id")) %>%
    
    arrange(loading_plan) %>%
    
    transmute(Номер_заявки = short_id,
                          Внешний_номер_заявки = external_number,
                          Дата_создания_заявки = as.Date(strftime(created_at, format="%Y-%m-%d")),
                          Клиент = client,
                          Ответственный_логист = responsible_expeditor_title,
                          Грузополучатель = consignee,
                          Дата_загрузки_план = as.Date(strftime(loading_plan, format="%Y-%m-%d")),
                          Время_загрузки_план = strftime(loading_plan, format="%H:%M:%S"),
                          Дата_загрузки_факт = strftime(loading_fact, format="%Y-%m-%d %H:%M:%S"),
                          Дата_выгрузки_план = as.Date(strftime(unloading_plan, format="%Y-%m-%d")),
                          Время_выгрузки_план = strftime(unloading_plan, format="%H:%M:%S"),
                          Дата_выгрузки_факт = strftime(unloading_fact, format="%Y-%m-%d %H:%M:%S"),
                          Маршрут = paste(loading_address, "-", unloading_address),

                          Адрес_загрузки = case_when(loading_type == "dc_special" ~ loading_address,
                                                          TRUE ~ "-"),
                          
                          Адрес_выгрузки = case_when(unloading_type == "dc_special" ~ unloading_address,
                                                          TRUE ~ "-"),
                          
                          Менеджер = data.title,
                          
                          Особенность = case_when(loading_type == "dc_special" ~ "Специальный РЦ на точке загрузки",
                                                  unloading_type == "dc_special" ~ "Специальный РЦ на точке выгрузки",
                                                  TRUE ~ "Особый грузополучатель"
                          ),
                          
                          Старший_группы = data.name,
                          Перевозчик = carrier,
                          Водитель = full_name,
                          Номер_телефона = phone_numbers,
                          Номер_тягача = case_when(vehicle_type == "tractor" | vehicle_type == "truck" ~ number_plate,
                                                   TRUE ~ ""),
                          Номер_полуприцепа = case_when(vehicle_type == "semitrailer" | vehicle_type == "trailer" ~ number_plate,
                                                        TRUE ~ "")
  )

}


getSpecialDistributionCenters1 <- function(startDate, endDate){
  
  my_query <- query('{
"bool": {
      "must": [
        {
  "match": {"isActual": "true"}
        }
      ]
}
}')
  
  selected_field_expeditors_group <- select_fields('{
  "includes": ["data.id", "data.name"]
}')
  
  expeditors_group <- elastic(cfg$db_elastic$url,
                              "expeditors-group-version-index") %search% (my_query + selected_field_expeditors_group)
  
  selected_field_user <- select_fields('{
  "includes": ["data.id", "data.title"]
}')
  
  user = elastic(cfg$db_elastic$url,
                 "user-profile-version-index") %search% (my_query + selected_field_user) %>%
    group_by(data.id) %>%
    summarise(data.title = first(data.title))
  
  requests_by_day <- dbGetQuery(con,
                                paste("SELECT requests.short_id,
                                requests.external_number,
                                requests.created_at,
                                requests.juridical_person_version_short_title as client,
                                requests.responsible_expeditor_division,
                                requests.responsible_expeditor_title,
                                requests.charterer_version_signed_by_uid as manager,

                                
                                transportations.juridical_person_version_short_title as carrier,

                                request_start_route_points.address as loading_address,
                                request_start_route_points.from as loading_plan,
                                request_start_route_points.type as loading_type,
                                request_start_route_points.fact_arrival_date_time as loading_fact,


                                request_finish_route_points.address as unloading_address,
                                request_finish_route_points.from as unloading_plan,
                                request_finish_route_points.consignee,
                                request_finish_route_points.type as unloading_type,
                                request_finish_route_points.fact_departure_date_time as unloading_fact,


                                transportation_driver_versions.full_name,
                                transportation_driver_versions.phone_numbers,
                                transportation_vehicle_versions.number_plate,
                                transportation_vehicle_versions.type as vehicle_type
                                
                                                                
                                FROM erp_requests.requests

                   LEFT JOIN erp_requests.transportations
                   ON requests.id = transportations.request_id

                   LEFT JOIN (SELECT request_start_route_points.address,
                   request_start_route_points.fact_arrival_date_time,
                   request_start_route_points.from,
                   addresses.type, request_start_route_points.request_id FROM erp_requests.request_start_route_points INNER JOIN erp_addresses.addresses ON request_start_route_points.address_id = addresses.id AND request_start_route_points.address_version = addresses.version) as request_start_route_points
                    ON requests.id = request_start_route_points.request_id


                   LEFT JOIN (SELECT request_finish_route_points.address,
                   request_finish_route_points.fact_departure_date_time,
                   request_finish_route_points.from,
                   request_finish_route_points.consignee,
                   addresses.type, request_finish_route_points.request_id FROM erp_requests.request_finish_route_points INNER JOIN erp_addresses.addresses ON request_finish_route_points.address_id = addresses.id AND request_finish_route_points.address_version = addresses.version) as request_finish_route_points
                    ON requests.id = request_finish_route_points.request_id
                    
                    
                    LEFT JOIN erp_requests.transportation_driver_versions
                    ON requests.id = transportation_driver_versions.transportation_request_id

                    LEFT JOIN erp_requests.transportation_vehicle_versions
                    ON requests.id = transportation_vehicle_versions.transportation_request_id

                   WHERE requests.is_actual=true 
                   
                   AND (request_start_route_points.type = 'dc_special' OR request_finish_route_points.type = 'dc_special' 
                   OR request_finish_route_points.consignee ilike '%Демо РЦ 04%' OR request_finish_route_points.consignee ilike '%Демо РЦ 05%'
                   OR request_finish_route_points.consignee ilike '%Демо РЦ 06%' OR request_finish_route_points.consignee ilike '%Демо РЦ 07%'
                   OR request_finish_route_points.consignee ilike '%Демо РЦ 08%'
                   OR request_finish_route_points.consignee ilike '%Демо РЦ 09%'
                   OR request_finish_route_points.consignee = 'Демо РЦ 02'
                   OR request_finish_route_points.consignee = 'Демо РЦ 03'
                   OR request_finish_route_points.consignee = 'Демо РЦ 01')


                   AND request_finish_route_points.from >=", paste0("'", startDate, "'::timestamp"), "AND request_start_route_points.from <=", paste0("'",endDate, "'::timestamp"),
                   "AND requests.status in ('carrierFound', 'active');")) %>%
    
    arrange(loading_plan, unloading_plan) %>%
    
    group_by(short_id) %>% 
    
    summarise(created_at = first(created_at),
              external_number = first(external_number),
              created_at = first(created_at),
              client = first(client),
              responsible_expeditor_division = first(responsible_expeditor_division),
              responsible_expeditor_title = first(responsible_expeditor_title),
              manager = first(manager),
              carrier = first(carrier),
              loading_address = first(loading_address),
              loading_plan = first(loading_plan),
              loading_fact = first(loading_fact),
              loading_type = first(loading_type),
              unloading_address = last(unloading_address),
              unloading_plan = last(unloading_plan),
              unloading_fact = last(unloading_fact),
              unloading_type = last(unloading_type),
              consignee = last(consignee),
              full_name = first(full_name),
              phone_numbers = first(phone_numbers),
              number_plate = first(number_plate),
              vehicle_type = first(vehicle_type)
    ) %>%
    
    left_join(expeditors_group, by = c("responsible_expeditor_division" = "data.id")) %>%
    
    left_join(user, by = c("manager" = "data.id")) %>%
    
    arrange(unloading_plan) %>%
    
    transmute(Номер_заявки = short_id,
                          Внешний_номер_заявки = external_number,
                          Дата_создания_заявки = as.Date(strftime(created_at, format="%Y-%m-%d")),
                          Клиент = client,
                          Ответственный_логист = responsible_expeditor_title,
                          Грузополучатель = consignee,
                          Дата_загрузки_план = as.Date(strftime(loading_plan, format="%Y-%m-%d")),
                          Время_загрузки_план = strftime(loading_plan, format="%H:%M:%S"),
                          Дата_загрузки_факт = strftime(loading_fact, format="%Y-%m-%d %H:%M:%S"),
                          Дата_выгрузки_план = as.Date(strftime(unloading_plan, format="%Y-%m-%d")),
                          Время_выгрузки_план = strftime(unloading_plan, format="%H:%M:%S"),
                          Дата_выгрузки_факт = strftime(unloading_fact, format="%Y-%m-%d %H:%M:%S"),
                          Маршрут = paste(loading_address, "-", unloading_address),
                          
                          Адрес_загрузки = case_when(loading_type == "dc_special" ~ loading_address,
                                                     TRUE ~ "-"),
                          
                          Адрес_выгрузки = case_when(unloading_type == "dc_special" ~ unloading_address,
                                                     TRUE ~ "-"),
                          
                          Менеджер = data.title,
                          
                          Особенность = case_when(loading_type == "dc_special" ~ "Специальный РЦ на точке загрузки",
                                                  unloading_type == "dc_special" ~ "Специальный РЦ на точке выгрузки",
                                                  TRUE ~ "Особый грузополучатель"
                          ),
                          
                          Старший_группы = data.name,
                          Перевозчик = carrier,
                          Водитель = full_name,
                          Номер_телефона = phone_numbers,
                          Номер_тягача = case_when(vehicle_type == "tractor" | vehicle_type == "truck" ~ number_plate,
                                                   TRUE ~ ""),
                          Номер_полуприцепа = case_when(vehicle_type == "semitrailer" | vehicle_type == "trailer" ~ number_plate,
                                                        TRUE ~ "")
    )

}


getLateOrders <- function(){
    requests <- dbGetQuery(con,
                           "SELECT
                                requests.created_at,
                                requests.charterer_rate_amount,
                                requests.short_id,
                                requests.juridical_person_version_short_title as client,
                                requests.type,
                                requests.responsible_expeditor_title,
                     
                     request_start_route_points.locality as start_locality,
                     request_start_route_points.from as loading_plan,
                     request_start_route_points.region as start_region,
                     
                     request_finish_route_points.locality as finish_locality,
                     request_finish_route_points.region as finish_region
                     
                     FROM erp_requests.requests
                     
                    LEFT JOIN erp_requests.request_start_route_points
                    ON requests.id = request_start_route_points.request_id

                    LEFT JOIN erp_requests.request_finish_route_points
                    ON requests.id = request_finish_route_points.request_id
                

                     WHERE requests.is_actual=true
                     AND requests.status = 'active'
                           ORDER BY request_start_route_points.from, request_finish_route_points.from;") %>%
      
      as_tibble() %>%
      
      group_by(short_id) %>%
      
      summarise(created_at = first(created_at),
                client = first(client),
                charterer_rate_amount = first(charterer_rate_amount),
                type = first(type),
                responsible_expeditor_title = first(responsible_expeditor_title),
                loading_plan = first(loading_plan),
                start_locality = first(start_locality),
                start_region = first(start_region),
                finish_locality = last(finish_locality),
                finish_region = last(finish_region)
      ) %>%
      
      transmute(
        Номер = short_id,
        Дата = as.Date(strftime(created_at, format="%Y-%m-%d")),
        Клиент = client,
        Ставка_клиента = charterer_rate_amount,
        Тип = case_when(type == "Spot" ~ "Спот",
                               type == "Warranty" ~ "Гарантия"), 
        Логист = responsible_expeditor_title,
        Маршрут = paste(start_region, "\U2192", finish_region),
        Дата_загрузки_план = as.Date(strftime(loading_plan, format="%Y-%m-%d")),
        Место_погрузки = start_locality,
        Место_выгрузки = finish_locality
        ) %>%
      
      filter(Sys.Date() - Дата_загрузки_план > 0) %>%
      
      select(-Дата_загрузки_план)
      
}


getregistersForInsurance <- function(startDate, endDate){
  
  my_query <- query('{
"bool": {
      "must": [
        {
  "match": {"isActual": "true"}
        }
      ]
}
}')
  
  
  glossary_selected_field <- select_fields('{
  "includes": ["data.key", "data.title"]
}')
  
  glossary <- elastic(cfg$db_elastic$url,
                      "glossary-element-version-index") %search% (my_query + glossary_selected_field)
  
  requests_by_day <- dbGetQuery(con,
                                paste("SELECT requests.short_id,
                                requests.external_number,
                                requests.created_at,
                                logist.title as Логист,
                              requests.juridical_person_version_short_title as client,
                     requests.cargo_cargo_type,
                     requests.cargo_cost,
                     requests.vehicle_requirements_carrying_capacity_and_volume,

                     first_loading.address as start_address,
                     first_loading.from_at as loading_plan,
                     last_unloading.address as finish_address,
                     last_unloading.from_at as unloading_plan,
                      
                     truck_vehicle.title as truck_title,
                     truck_vehicle.number_plate as truck_number_plate,
                     trailer_vehicle.title as trailer_title,
                     trailer_vehicle.number_plate as trailer_number_plate,

                     transportations.status,
                     
                     transportations.juridical_person_version_short_title
                    
                     FROM erp_requests.requests
                     
                     left join erp_users.user_profiles as logist
                     on requests.responsible_expeditor_id = logist.original_id 
                     and logist.is_actual = true
                     
                    LEFT JOIN erp_requests.transportations
                   ON requests.id = transportations.request_id

left join (
    select distinct on (s.request_id)
          request_id, id, locality, address, \"from\" as from_at, fact_arrival_date_time, region
    from erp_requests.request_start_route_points s
    order by s.request_id desc, s.from asc
) as first_loading
on requests.id = first_loading.request_id

left join (
    select distinct on (s.request_id)
          request_id, id, locality, address, \"from\" as from_at, fact_arrival_date_time, region, consignee, fact_departure_date_time
    from erp_requests.request_finish_route_points s
    order by s.request_id desc, s.from desc
) as last_unloading
on requests.id = last_unloading.request_id



                    LEFT JOIN erp_requests.transportation_vehicle_versions as truck_vehicle
                    ON requests.id = truck_vehicle.transportation_request_id
                    AND (truck_vehicle.type ='tractor' OR truck_vehicle.type ='truck')
                    
                    LEFT JOIN erp_requests.transportation_vehicle_versions as trailer_vehicle
                    ON requests.id = trailer_vehicle.transportation_request_id
                    AND (trailer_vehicle.type ='trailer' OR trailer_vehicle.type ='semitrailer')


                   WHERE requests.is_actual=true 
                   AND requests.status not in ('cancelled', 'transportationCancelled', 'active')
                   AND first_loading.from_at >=", paste0("'", startDate, "'::timestamp"), "AND first_loading.from_at <=", paste0("'",endDate, "'::timestamp"),
                                      "ORDER BY first_loading.from_at;"
                                )) %>%
    
    left_join(glossary, by = c("cargo_cargo_type" = "data.key")) %>%
    
    left_join(glossary, by = c("vehicle_requirements_carrying_capacity_and_volume" = "data.key")) %>%
    
    transmute(Рейс_ЕРП = short_id,
      Номер_заявки = external_number,
      Дата_создания_перевозки = as.Date(strftime(created_at, format="%Y-%m-%d")),
      Логист = Логист,
      Перевозчик = juridical_person_version_short_title,
                          Клиент = client,
                          Тип_груза = data.title.x,
                          Стоимость_груза = cargo_cost,
                          Марка_тягача = truck_title,            
                          Номер_тягача = truck_number_plate,
                          Марка_полуприцепа = trailer_title,
                          Номер_полуприцепа = trailer_number_plate,
                          Дата_загрузки_план = as.Date(strftime(loading_plan, format="%Y-%m-%d")),
                          Время_загрузки_план = strftime(loading_plan, format="%H:%M:%S"),
                          Дата_выгрузки_план = as.Date(strftime(unloading_plan, format="%Y-%m-%d")),
                          Время_выгрузки_план = strftime(unloading_plan, format="%H:%M:%S"),
                          Пункт_отправления = start_address,
                          Пункт_назначения = finish_address,
                          Кубатура = gsub("<sup>3</sup>", "\U00B3 ", data.title.y),
                          Статус = status
    )
}

getMetrics <- function(startDate, endDate){
  requests_all =
    dbGetQuery(con,
               paste("SELECT requests.created_at,
           requests.responsible_expeditor_title,
           requests.charterer_rate_amount,
           requests.charterer_version_title,
           requests.type,
           
           requests.status,
           requests.relative_request_short_id,
           requests.short_id,
           requests.notes,
           
           transportations.profit_amount,
           request_start_route_points.from as loading_plan

           FROM erp_requests.requests
           
           LEFT JOIN erp_requests.transportations
            ON requests.id = transportations.request_id

          LEFT JOIN erp_requests.request_start_route_points
            ON requests.id = request_start_route_points.request_id
            
           WHERE requests.is_actual=true 
                   AND requests.created_at >=", paste0("'", startDate, "'::timestamp"), "AND requests.created_at <=", paste0("'",endDate, "'::timestamp"),
                     "ORDER BY request_start_route_points.from;")) %>%
    
    as_tibble() %>%
    
    left_join(expeditors_group, by = c("responsible_expeditor_title" = "data.id")) %>%
    
    group_by(short_id) %>%
    
    summarise(type = first(type),
              status = first(status),
              created_at = first(created_at),
              charterer_version_title = first(charterer_version_title),
              data.name = first(data.name),
              responsible_expeditor_title = first(responsible_expeditor_title),
              Должность = first(Должность),
              charterer_rate_amount = first(charterer_rate_amount),
              profit_amount = first(profit_amount),
              loading_plan = first(loading_plan),
              notes = first(notes),
              relative_request_short_id = first(relative_request_short_id)
    ) %>%
    
    transmute(Тип = case_when(type == "Spot" ~ "Спот",
                              type == "Warranty" ~ "Гарантия"),
                 Статус = status,
                 Дата = as.Date(strftime(created_at, format="%Y-%m-%d")),
                 Клиент = charterer_version_title,
                 Группа = data.name,
                 Логист = responsible_expeditor_title,
                 Должность = Должность,
                 Ставка_клиента = case_when(charterer_rate_amount == 0 ~ 1,
                                            TRUE ~ charterer_rate_amount),
                 Маржа = profit_amount,
                 Рентабельность = Маржа / Ставка_клиента,
                 Дата_загрузки_план = as.Date(strftime(loading_plan, format="%Y-%m-%d")),
                 Номер_заявки = short_id,
                 Заметки = notes,
                 Связанная_заявка = relative_request_short_id
    ) 
  
  requests_all$Маржа[is.na(requests_all$Маржа)] <- 0
  
  return(requests_all)

  # # Завершённые рейсы
  # completed_byDay <- requests_all %>%
  # 
  #   filter(Статус != "cancelled" | Статус != "transportationCancelled" | Статус != "active") %>%
  # 
  #   group_by(Дата, Клиент, Логист, Должность, Тип) %>%
  # 
  #   summarise(Группа = first(Группа),
  #                   Рейсы = n(),
  #                   Выручка = sum(Ставка_клиента),
  #                   Маржа = sum(Маржа),
  #                   Рентабельность = sum(Маржа) / sum(Выручка),
  #                   Стоимость_рейса = sum(Выручка) / sum(Рейсы))
  # 
  # # Отказы от рейса
  # canceling_byDay <- requests_all %>%
  # 
  #   filter((Статус == "cancelled" | (Статус == "transportationCancelled" & stringr::str_detect(Заметки, 'Отказ клиента'))) & is.na(Связанная_заявка)) %>%
  # 
  #   # anti_join(requests_all, by = c("Номер_заявки" = "Связанная_заявка")) %>%
  # 
  #   group_by(Дата, Клиент, Логист, Должность, Тип) %>%
  # 
  #   summarise(Группа = first(Группа),
  #                   Рейсы = n(),
  #                   Выручка = sum(Ставка_клиента),
  #                   Маржа = sum(Маржа),
  #                   Рентабельность = sum(Маржа) / sum(Выручка),
  #                   Стоимость_рейса = sum(Выручка) / sum(Рейсы))
  # 
  # # Срывы рейсов
  # breakdowns_byDay <- requests_all %>%
  #   filter((Статус == "transportationCancelled" & !stringr::str_detect(Заметки, 'Отказ клиента') & is.na(Связанная_заявка))) %>%
  #   # anti_join(requests_all, by = c("Номер_заявки" = "Связанная_заявка")) %>%
  #   group_by(Дата, Клиент, Логист, Должность, Тип) %>%
  #   summarise(Группа = first(Группа),
  #                   Рейсы = n(),
  #                   Выручка = sum(Ставка_клиента),
  #                   Маржа = sum(Маржа),
  #                   Рентабельность = sum(Маржа) / sum(Выручка),
  #                   Стоимость_рейса = sum(Выручка) / sum(Рейсы))
  # 
  # 
  # # Просроченные заявки
  # delay_byDay <- requests_all %>%
  #   filter((is.na(Связанная_заявка) & Статус == "active" & (Sys.Date() - Дата_загрузки_план > 0))) %>%
  #   # anti_join(requests_all, by = c("Номер_заявки" = "Связанная_заявка")) %>%
  #   group_by(Дата, Клиент, Логист, Должность, Тип) %>%
  #   summarise(Группа = first(Группа),
  #                   Рейсы = n(),
  #                   Выручка = sum(Ставка_клиента),
  #                   Маржа = sum(Маржа),
  #                   Рентабельность = sum(Маржа) / sum(Выручка),
  #                   Стоимость_рейса = sum(Выручка) / sum(Рейсы))
  # 
  # resultList <- list("completed" = completed_byDay, "canceling" = canceling_byDay, "breakdowns" = breakdowns_byDay, "expired" = delay_byDay)
  # 
  # return(resultList)
    
}


getMetrics1 <- function(startDate, endDate){
  requests_all =
    dbGetQuery(con,
               paste("SELECT requests.created_at,
           requests.responsible_expeditor_title,
           requests.charterer_rate_amount,
           requests.charterer_version_title,
           requests.type,
           
           requests.status,
           requests.relative_request_short_id,
           requests.short_id,
           requests.notes,
           
           transportations.profit_amount,
           request_start_route_points.from as loading_plan

           FROM erp_requests.requests
           
           LEFT JOIN erp_requests.transportations
            ON requests.id = transportations.request_id

          LEFT JOIN erp_requests.request_start_route_points
            ON requests.id = request_start_route_points.request_id

           WHERE requests.is_actual=true 
                   AND requests.created_at >=", paste0("'", startDate, "'::timestamp"), "AND requests.created_at <=", paste0("'",endDate, "'::timestamp"),
                     "ORDER BY request_start_route_points.from;")) %>%
    
    as_tibble() %>%
    
    left_join(expeditors_group, by = c("responsible_expeditor_title" = "data.id")) %>%
    
    group_by(short_id) %>%
    
    summarise(type = first(type),
              status = first(status),
              created_at = first(created_at),
              charterer_version_title = first(charterer_version_title),
              data.name = first(data.name),
              responsible_expeditor_title = first(responsible_expeditor_title),
              Должность = first(Должность),
              charterer_rate_amount = first(charterer_rate_amount),
              profit_amount = first(profit_amount),
              loading_plan = first(loading_plan),
              notes = first(notes),
              relative_request_short_id = first(relative_request_short_id)
              ) %>%
    
    transmute(Тип = case_when(type == "Spot" ~ "Спот",
                              type == "Warranty" ~ "Гарантия"),
                 Статус = status,
                 Дата = as.Date(strftime(created_at, format="%Y-%m-%d")),
                 Клиент = charterer_version_title,
                 Группа = data.name,
                 Логист = responsible_expeditor_title,
                 Должность = Должность,
                 Ставка_клиента = case_when(charterer_rate_amount == 0 ~ 1,
                                            TRUE ~ charterer_rate_amount),
                 Маржа = profit_amount,
                 Рентабельность = Маржа / Ставка_клиента,
                 Дата_загрузки_план = as.Date(strftime(loading_plan, format="%Y-%m-%d")),
                 Номер_заявки = short_id,
                 Заметки = notes,
                 Связанная_заявка = relative_request_short_id
    ) 
  
  requests_all$Маржа[is.na(requests_all$Маржа)] <- 0
  
  return(requests_all)
  
}


getDoublets <- function(){
  
  my_query <- query('{
"bool": {
      "must": [
        {
  "match": {"isActual": "true"}
        }
      ]
}
}')
  
  glossary_selected_field <- select_fields('{
  "includes": ["data.key", "data.title"]
}')
  
  glossary <- elastic(cfg$db_elastic$url,
                      "glossary-element-version-index") %search% (my_query + glossary_selected_field)

  erp_requests <- dbGetQuery(con,
                            "SELECT requests.short_id,
                                requests.vehicle_requirements_carrying_capacity_and_volume,
                                requests.vehicle_requirements_types_of_body,
                                requests.charterer_rate_amount,
                                requests.juridical_person_version_short_title as client,

                                first_loading.region as start_region,
                                first_loading.address as start_address,
                                first_loading.from_at as loading_plan,
                                first_loading.latitude as loading_latitude,
                                first_loading.longitude as loading_longitude,

                                last_unloading.region as finish_region,
                                last_unloading.address as finish_address,
                                last_unloading.from_at as unloading_plan,
                                last_unloading.latitude as unloading_latitude,
                                last_unloading.longitude as unloading_longitude

                                FROM erp_requests.requests

left join (
    select distinct on (s.request_id)
          request_id, id, address, \"from\" as from_at, region, latitude, longitude
    from erp_requests.request_start_route_points s
    order by s.request_id desc, s.from asc
) as first_loading
on requests.id = first_loading.request_id

left join (
    select distinct on (s.request_id)
          request_id, id, address, \"from\" as from_at, region, latitude, longitude
    from erp_requests.request_finish_route_points s
    order by s.request_id desc, s.from desc
) as last_unloading
on requests.id = last_unloading.request_id


                    WHERE requests.is_actual=true
                   AND requests.status in ('active', 'carrierFound');"
  )  %>%

    filter(Sys.time() - loading_plan < 0) %>%
    
    filter(start_region != finish_region) %>%
    
    mutate(vehicle_requirements_types_of_body = as.character(gsub('\\{|\\}|\\"|\\"', "", vehicle_requirements_types_of_body))) %>%
    
    left_join(glossary, by = c("vehicle_requirements_carrying_capacity_and_volume" = "data.key")) %>%
    
    left_join(glossary, by = c("vehicle_requirements_types_of_body" = "data.key")) %>%
    
  transmute(short_id = short_id, 
            vehicle_requirements_carrying_capacity_and_volume = gsub("<sup>3</sup>", "\U00B3 ", data.title.x),
            vehicle_requirements_types_of_body = data.title.y,
            start_region = start_region,
            start_address = start_address,
            finish_region = finish_region,
            finish_address = finish_address,
            loading_plan = loading_plan,
            unloading_plan = unloading_plan,
            loading_latitude = as.numeric(loading_latitude),
            loading_longitude = as.numeric(loading_longitude),
            unloading_latitude = as.numeric(unloading_latitude),
            unloading_longitude = as.numeric(unloading_longitude),
            charterer_rate_amount = charterer_rate_amount,
            client = client
  )


  erp_requests1 <- erp_requests %>%

    transmute(short_id = short_id,
              vehicle_requirements_carrying_capacity_and_volume1 = vehicle_requirements_carrying_capacity_and_volume,
              vehicle_requirements_types_of_body1 = vehicle_requirements_types_of_body,
              start_region1 = start_region,
              start_address1 = start_address,
              finish_region1 = finish_region,
              finish_address1 = finish_address,
              loading_plan1 = loading_plan,
              unloading_plan1 = unloading_plan,
              loading_latitude1 = loading_latitude,
              loading_longitude1 = loading_longitude,
              unloading_latitude1 = unloading_latitude,
              unloading_longitude1 = unloading_longitude,
              charterer_rate_amount1 = charterer_rate_amount,
              client1 = client
    ) 


  result <- erp_requests %>%
    inner_join(erp_requests1, by = c("finish_region" = "start_region1", "start_region" = "finish_region1", "vehicle_requirements_carrying_capacity_and_volume" = "vehicle_requirements_carrying_capacity_and_volume1", "vehicle_requirements_types_of_body" = "vehicle_requirements_types_of_body1")) %>%
    mutate(Время = as.numeric(difftime(strptime(loading_plan1, format = "%Y-%m-%d %H:%M:%S"), strptime(unloading_plan, format = "%Y-%m-%d %H:%M:%S"), units = "hours"))) %>%
    transmute(Кругорейс = paste(short_id.x, "\U2192", short_id.y),
                       Номер_прямого_рейса = short_id.x,
                       Ставка_прямого_рейса = charterer_rate_amount,
                       Клиент_прямого_рейса = client,
                       Плановые_даты_прямого_рейса = paste(loading_plan, "-", unloading_plan),
                       Маршрут_прямого_рейса_области = paste(start_region, "-", finish_region),
                       Маршрут_прямого_рейса = paste(start_address, "-", finish_address),

                       Номер_обратного_рейса = short_id.y,
                       Ставка_обратного_рейса = charterer_rate_amount1,
                       Клиент_обратного_рейса = client1,
                       Плановые_даты_обратного_рейса = paste(loading_plan1, "-", unloading_plan1),
                       Маршрут_обратного_рейса_области = paste(finish_region, "-", start_region),
                       Маршрут_обратного_рейса = paste(start_address1, "-", finish_address1),
                       Часы_между_рейсами = Время
    ) %>%

    filter(Часы_между_рейсами >= 1 & Часы_между_рейсами <=23)

  result1 <- erp_requests1 %>%
    inner_join(erp_requests, by = c("finish_region1" = "start_region", "start_region1" = "finish_region", "vehicle_requirements_carrying_capacity_and_volume1" = "vehicle_requirements_carrying_capacity_and_volume", "vehicle_requirements_types_of_body1" = "vehicle_requirements_types_of_body")) %>%
    mutate(Время = as.numeric(difftime(strptime(loading_plan, format = "%Y-%m-%d %H:%M:%S"), strptime(unloading_plan1, format = "%Y-%m-%d %H:%M:%S"), units = "hours")))  %>%
    transmute(Кругорейс = paste(short_id.x, "\U2192", short_id.y),
                       Номер_прямого_рейса = short_id.y,
                       Ставка_прямого_рейса = charterer_rate_amount,
                       Клиент_прямого_рейса = client,
                       Плановые_даты_прямого_рейса = paste(loading_plan, "-", unloading_plan),
                       Маршрут_прямого_рейса_области = paste(finish_region1, "-", start_region1),
                       Маршрут_прямого_рейса = paste(start_address, "-", finish_address),

                       Номер_обратного_рейса = short_id.x,
                       Ставка_обратного_рейса = charterer_rate_amount1,
                       Клиент_обратного_рейса = client1,
                       Плановые_даты_обратного_рейса = paste(loading_plan1, "-", unloading_plan1),
                       Маршрут_обратного_рейса_области = paste(start_region1, "-", finish_region1),
                       Маршрут_обратного_рейса = paste(start_address1, "-", finish_address1),
                       Часы_между_рейсами = Время
    ) %>%

    filter(Часы_между_рейсами >= 1 & Часы_между_рейсами <=23)

  outcome <- rbind(result, result1)
  
  result_test <- select(outcome, Кругорейс, Номер_прямого_рейса, Часы_между_рейсами) %>%
    left_join(erp_requests, by = c("Номер_прямого_рейса" = "short_id")) %>%
    mutate(loading_plan = format(loading_plan, "%d.%m.%Y, %H:%M:%S", tz = "Europe/Moscow"),
           unloading_plan = format(unloading_plan, "%d.%m.%Y, %H:%M:%S", tz = "Europe/Moscow")
           )

  
}


getDoublets1 <- function(){
  
  my_query <- query('{
"bool": {
      "must": [
        {
  "match": {"isActual": "true"}
        }
      ]
}
}')
  
  glossary_selected_field <- select_fields('{
  "includes": ["data.key", "data.title"]
}')
  
  glossary <- elastic(cfg$db_elastic$url,
                      "glossary-element-version-index") %search% (my_query + glossary_selected_field)
  
  erp_requests <- dbGetQuery(con,
                             "SELECT requests.short_id,
                                requests.vehicle_requirements_carrying_capacity_and_volume,
                                requests.vehicle_requirements_types_of_body,
                                requests.charterer_rate_amount,
                                requests.juridical_person_version_short_title as client,

                                first_loading.region as start_region,
                                first_loading.address as start_address,
                                first_loading.from_at as loading_plan,
                                first_loading.latitude as loading_latitude,
                                first_loading.longitude as loading_longitude,

                                last_unloading.region as finish_region,
                                last_unloading.address as finish_address,
                                last_unloading.from_at as unloading_plan,
                                last_unloading.latitude as unloading_latitude,
                                last_unloading.longitude as unloading_longitude

                                FROM erp_requests.requests

left join (
    select distinct on (s.request_id)
          request_id, id, address, \"from\" as from_at, region, latitude, longitude
    from erp_requests.request_start_route_points s
    order by s.request_id desc, s.from asc
) as first_loading
on requests.id = first_loading.request_id

left join (
    select distinct on (s.request_id)
          request_id, id, address, \"from\" as from_at, region, latitude, longitude
    from erp_requests.request_finish_route_points s
    order by s.request_id desc, s.from desc
) as last_unloading
on requests.id = last_unloading.request_id


                    WHERE requests.is_actual=true
                   AND requests.status in ('active', 'carrierFound');"
  )  %>%
    
    filter(Sys.time() - loading_plan < 0) %>%
    
    filter(start_region != finish_region) %>%
    
    mutate(vehicle_requirements_types_of_body = as.character(gsub('\\{|\\}|\\"|\\"', "", vehicle_requirements_types_of_body))) %>%
    
    left_join(glossary, by = c("vehicle_requirements_carrying_capacity_and_volume" = "data.key")) %>%
    
    left_join(glossary, by = c("vehicle_requirements_types_of_body" = "data.key")) %>%
    
    transmute(short_id = short_id, 
              vehicle_requirements_carrying_capacity_and_volume = gsub("<sup>3</sup>", "\U00B3 ", data.title.x),
              vehicle_requirements_types_of_body = data.title.y,
              start_region = start_region,
              start_address = start_address,
              finish_region = finish_region,
              finish_address = finish_address,
              loading_plan = loading_plan,
              unloading_plan = unloading_plan,
              loading_latitude = as.numeric(loading_latitude),
              loading_longitude = as.numeric(loading_longitude),
              unloading_latitude = as.numeric(unloading_latitude),
              unloading_longitude = as.numeric(unloading_longitude),
              charterer_rate_amount = charterer_rate_amount,
              client = client
    )
  
  
  inbev_requests <- dbGetQuery(con2,
                               "SELECT external_id_params->>'tnx_cargo_order_name' AS external_id,
                                COALESCE(start_trading_on_source_at, created_on_source_at, created_at) AS created_at,
                                base_price_net,
                                
                                targets->0->'address'->'dadata'->> 'region_with_type' AS start_region,
                                targets->0->'address'->>'full_formatted' AS start_address,
                                targets->0->'visit_windows'->0->>'from' AS loading_plan,
                                targets->0->'address'->'dadata'->> 'geo_lat' AS loading_latitude,
                                targets->0->'address'->'dadata'->> 'geo_lon' AS loading_longitude,

                                
                                targets->-1->'address'->'dadata'->> 'region_with_type' AS finish_region,
                                targets->-1->'address'->>'full_formatted' AS finish_address,
                                targets->-1->'visit_windows'->0->>'from' AS unloading_plan,
                                targets->1->'address'->'dadata'->> 'geo_lat' AS unloading_latitude,
                                targets->1->'address'->'dadata'->> 'geo_lon' AS unloading_longitude,
                                
                                vehicle_requirements->'types_of_body'->>0 AS types_of_body,
                                vehicle_requirements->>'payload_in_tons' AS payload_in_tons
                                
                                FROM orders
                             WHERE source_name IN ('inbev') AND status IN ('ACTIVE')
                             ORDER BY COALESCE(start_trading_on_source_at, created_on_source_at, created_at);") %>%
    
    as_tibble() %>%
    
    group_by(external_id) %>%
    
    summarise(created_at = last(created_at),
              start_region = last(start_region),
              start_address = last(start_address),
              loading_plan = last(loading_plan),
              finish_region = last(finish_region),
              finish_address = last(finish_address),
              unloading_plan = last(unloading_plan),
              loading_latitude = first(loading_latitude),
              loading_longitude = first(loading_longitude),
              unloading_latitude = last(unloading_latitude),
              unloading_longitude = last(unloading_longitude),
              types_of_body = last(types_of_body),
              payload_in_tons = last(payload_in_tons),
              base_price_net = last(base_price_net)
    ) %>%
    
    left_join(glossary, by = c("types_of_body" = "data.key")) %>%
    
    transmute(short_id = external_id, 
              vehicle_requirements_carrying_capacity_and_volume1 = payload_in_tons,
              vehicle_requirements_types_of_body1 = data.title,
              start_region1 = start_region,
              start_address1 = start_address,
              finish_region1 = finish_region,
              finish_address1 = finish_address,
              loading_plan1 = loading_plan,
              unloading_plan1 = unloading_plan,
              loading_latitude1 = as.numeric(loading_latitude),
              loading_longitude1 = as.numeric(loading_longitude),
              unloading_latitude1 = as.numeric(unloading_latitude),
              unloading_longitude1 = as.numeric(unloading_longitude),
              charterer_rate_amount1 = base_price_net,
              client1 = 'Клиент 01'
    )
  
  transporeon_requests <- dbGetQuery(con2,
                                     "SELECT external_id,
                                COALESCE(start_trading_on_source_at, created_on_source_at, created_at) AS created_at,
                                base_price_net,
                                charterer ->> 'title' AS charterer,
                                targets->0->'address'->'dadata'->> 'region_with_type' AS start_region,
                                targets->0->'address'->>'full_formatted' AS start_address,
                                targets->0->'visit_windows'->0->>'from' AS loading_plan,
                                targets->0->'address'->'dadata'->> 'geo_lat' AS loading_latitude,
                                targets->0->'address'->'dadata'->> 'geo_lon' AS loading_longitude,

                                
                                targets->-1->'address'->'dadata'->> 'region_with_type' AS finish_region,
                                targets->-1->'address'->>'full_formatted' AS finish_address,
                                targets->-1->'visit_windows'->0->>'from' AS unloading_plan,
                                targets->1->'address'->'dadata'->> 'geo_lat' AS unloading_latitude,
                                targets->1->'address'->'dadata'->> 'geo_lon' AS unloading_longitude,
                                
                                vehicle_requirements->'types_of_body'->>0 AS types_of_body,
                                vehicle_requirements->>'payload_in_tons' AS payload_in_tons,
                                
                                source_name
                                
                                FROM orders
                             WHERE source_name IN ('transporeon') AND status IN ('ACTIVE')
                             AND charterer ->> 'title' in ('Клиент 02', 'Клиент 03', 'Клиент 04')
                             OR source_name IN ('uniconf')
                             ORDER BY COALESCE(start_trading_on_source_at, created_on_source_at, created_at);") %>%
    
    as_tibble() %>%
    
    left_join(glossary, by = c("types_of_body" = "data.key")) %>%
    
    transmute(short_id = external_id, 
              vehicle_requirements_carrying_capacity_and_volume1 = payload_in_tons,
              vehicle_requirements_types_of_body1 = data.title,
              start_region1 = start_region,
              start_address1 = start_address,
              finish_region1 = finish_region,
              finish_address1 = finish_address,
              loading_plan1 = loading_plan,
              unloading_plan1 = unloading_plan,
              loading_latitude1 = as.numeric(loading_latitude),
              loading_longitude1 = as.numeric(loading_longitude),
              unloading_latitude1 = as.numeric(unloading_latitude),
              unloading_longitude1 = as.numeric(unloading_longitude),
              charterer_rate_amount1 = base_price_net,
              client1 = case_when(charterer == "Клиент 02" ~ 'Клиент 02',
                                 charterer == "Клиент 03" ~ 'Клиент 03',
                                 charterer == "Клиент 04" ~ 'Клиент 04',
                                 source_name == "uniconf" ~ 'Клиент 06'
                                 )
    )
  
  loadboard_requests <- rbind(inbev_requests, transporeon_requests) %>%
    filter(Sys.time() - strptime(loading_plan1, format = "%Y-%m-%dT%H:%M:%SZ") < 0)
  
  
  
  result <- erp_requests %>%
    inner_join(loadboard_requests, by = c("finish_region" = "start_region1", "start_region" = "finish_region1", "vehicle_requirements_carrying_capacity_and_volume" = "vehicle_requirements_carrying_capacity_and_volume1", "vehicle_requirements_types_of_body" = "vehicle_requirements_types_of_body1")) %>%
    mutate(Время = as.numeric(difftime(strptime(loading_plan1, format = "%Y-%m-%d %H:%M:%S"), strptime(unloading_plan, format = "%Y-%m-%d %H:%M:%S"), units = "hours"))) %>%
    transmute(Кругорейс = paste(short_id.x, "\U2192", short_id.y),
                       Номер_прямого_рейса = short_id.x,
                       Ставка_прямого_рейса = charterer_rate_amount,
                       Клиент_прямого_рейса = client,
                       Плановые_даты_прямого_рейса = paste(loading_plan, "-", unloading_plan),
                       Маршрут_прямого_рейса_области = paste(start_region, "-", finish_region),
                       Маршрут_прямого_рейса = paste(start_address, "-", finish_address),
                       
                       Номер_обратного_рейса = short_id.y,
                       Ставка_обратного_рейса = charterer_rate_amount1,
                       Клиент_обратного_рейса = client1,
                       Плановые_даты_обратного_рейса = paste(loading_plan1, "-", unloading_plan1),
                       Маршрут_обратного_рейса_области = paste(finish_region, "-", start_region),
                       Маршрут_обратного_рейса = paste(start_address1, "-", finish_address1),
                       Часы_между_рейсами = Время
    ) %>%
    
    filter(Часы_между_рейсами >= 1 & Часы_между_рейсами <=23)
  
  result1 <- loadboard_requests %>%
    inner_join(erp_requests, by = c("finish_region1" = "start_region", "start_region1" = "finish_region", "vehicle_requirements_carrying_capacity_and_volume1" = "vehicle_requirements_carrying_capacity_and_volume", "vehicle_requirements_types_of_body1" = "vehicle_requirements_types_of_body")) %>%
    mutate(Время = as.numeric(difftime(strptime(loading_plan, format = "%Y-%m-%d %H:%M:%S"), strptime(unloading_plan1, format = "%Y-%m-%d %H:%M:%S"), units = "hours")))  %>%
    transmute(Кругорейс = paste(short_id.x, "\U2192", short_id.y),
                       Номер_прямого_рейса = short_id.y,
                       Ставка_прямого_рейса = charterer_rate_amount,
                       Клиент_прямого_рейса = client,
                       Плановые_даты_прямого_рейса = paste(loading_plan, "-", unloading_plan),
                       Маршрут_прямого_рейса_области = paste(finish_region1, "-", start_region1),
                       Маршрут_прямого_рейса = paste(start_address, "-", finish_address),
                       
                       Номер_обратного_рейса = short_id.x,
                       Ставка_обратного_рейса = charterer_rate_amount1,
                       Клиент_обратного_рейса = client1,
                       Плановые_даты_обратного_рейса = paste(loading_plan1, "-", unloading_plan1),
                       Маршрут_обратного_рейса_области = paste(start_region1, "-", finish_region1),
                       Маршрут_обратного_рейса = paste(start_address1, "-", finish_address1),
                       Часы_между_рейсами = Время
    ) %>%
    
    filter(Часы_между_рейсами >= 1 & Часы_между_рейсами <=23)
  
  outcome <- rbind(result, result1)
  
  result_test <- select(outcome, Кругорейс, Номер_прямого_рейса, Часы_между_рейсами,) %>%
    left_join(rbind(erp_requests, loadboard_requests %>% rename(vehicle_requirements_carrying_capacity_and_volume = 2,
                                                                vehicle_requirements_types_of_body = 3,
                                                                start_region = 4,
                                                                start_address = 5,
                                                                finish_region = 6,
                                                                finish_address = 7,
                                                                loading_plan = 8,
                                                                unloading_plan = 9,
                                                                loading_latitude = 10,
                                                                loading_longitude = 11,
                                                                unloading_latitude = 12,
                                                                unloading_longitude = 13,
                                                                charterer_rate_amount = 14,
                                                                client = 15
                                                                )),
              by = c("Номер_прямого_рейса" = "short_id")) %>%
    
    mutate(loading_plan = strptime(loading_plan, format = "%Y-%m-%d %H:%M:%S"),
           unloading_plan = strptime(unloading_plan, format = "%Y-%m-%d %H:%M:%S")
    )
 
  
}

getDoublets2 <- function(){
  my_query <- query('{
"bool": {
      "must": [
        {
  "match": {"isActual": "true"}
        }
      ]
}
}')
  
  glossary_selected_field <- select_fields('{
  "includes": ["data.key", "data.title"]
}')
  
  glossary <- elastic(cfg$db_elastic$url,
                      "glossary-element-version-index") %search% (my_query + glossary_selected_field)
  
  inbev_requests <- dbGetQuery(con2,
                                   "SELECT external_id_params->>'tnx_cargo_order_name' AS external_id,
                                COALESCE(start_trading_on_source_at, created_on_source_at, created_at) AS created_at,
                                base_price_net,
                                
                                targets->0->'address'->'dadata'->> 'region_with_type' AS start_region,
                                targets->0->'address'->>'full_formatted' AS start_address,
                                targets->0->'visit_windows'->0->>'from' AS loading_plan,
                                targets->0->'address'->'dadata'->> 'geo_lat' AS loading_latitude,
                                targets->0->'address'->'dadata'->> 'geo_lon' AS loading_longitude,

                                targets->-1->'address'->'dadata'->> 'region_with_type' AS finish_region,
                                targets->-1->'address'->>'full_formatted' AS finish_address,
                                targets->-1->'visit_windows'->0->>'from' AS unloading_plan,
                                targets->1->'address'->'dadata'->> 'geo_lat' AS unloading_latitude,
                                targets->1->'address'->'dadata'->> 'geo_lon' AS unloading_longitude,
                                
                                vehicle_requirements->'types_of_body'->>0 AS types_of_body,
                                vehicle_requirements->>'payload_in_tons' AS payload_in_tons
                                
                                FROM orders
                             WHERE source_name IN ('inbev') AND status IN ('ACTIVE')
                             ORDER BY COALESCE(start_trading_on_source_at, created_on_source_at, created_at);") %>%
    
    as_tibble() %>%
    
    group_by(external_id) %>%
    
    summarise(created_at = last(created_at),
              start_region = last(start_region),
              start_address = last(start_address),
              loading_plan = last(loading_plan),
              finish_region = last(finish_region),
              finish_address = last(finish_address),
              unloading_plan = last(unloading_plan),
              loading_latitude = first(loading_latitude),
              loading_longitude = first(loading_longitude),
              unloading_latitude = last(unloading_latitude),
              unloading_longitude = last(unloading_longitude),
              types_of_body = last(types_of_body),
              payload_in_tons = last(payload_in_tons),
              base_price_net = last(base_price_net)
    ) %>%
    
   left_join(glossary, by = c("types_of_body" = "data.key")) %>%
    
    transmute(external_id = external_id, 
              payload_in_tons = payload_in_tons,
              types_of_body = data.title,
              start_region = start_region,
              start_address = start_address,
              finish_region = finish_region,
              finish_address = finish_address,
              loading_plan = loading_plan,
              unloading_plan = unloading_plan,
              loading_latitude = as.numeric(loading_latitude),
              loading_longitude = as.numeric(loading_longitude),
              unloading_latitude = as.numeric(unloading_latitude),
              unloading_longitude = as.numeric(unloading_longitude),
              base_price_net = base_price_net,
              client = 'Клиент 01'
    )
  
  transporeon_requests <- dbGetQuery(con2,
                               "SELECT external_id,
                                COALESCE(start_trading_on_source_at, created_on_source_at, created_at) AS created_at,
                                base_price_net,
                                charterer ->> 'title' AS charterer,
                                targets->0->'address'->'dadata'->> 'region_with_type' AS start_region,
                                targets->0->'address'->>'full_formatted' AS start_address,
                                targets->0->'visit_windows'->0->>'from' AS loading_plan,
                                targets->0->'address'->'dadata'->> 'geo_lat' AS loading_latitude,
                                targets->0->'address'->'dadata'->> 'geo_lon' AS loading_longitude,

                                targets->-1->'address'->'dadata'->> 'region_with_type' AS finish_region,
                                targets->-1->'address'->>'full_formatted' AS finish_address,
                                targets->-1->'visit_windows'->0->>'from' AS unloading_plan,
                                targets->1->'address'->'dadata'->> 'geo_lat' AS unloading_latitude,
                                targets->1->'address'->'dadata'->> 'geo_lon' AS unloading_longitude,
                                
                                vehicle_requirements->'types_of_body'->>0 AS types_of_body,
                                vehicle_requirements->>'payload_in_tons' AS payload_in_tons,
                                
                                source_name
                                
                                FROM orders
                             WHERE source_name IN ('transporeon') AND status IN ('ACTIVE')
                             AND charterer ->> 'title' in ('Клиент 02', 'Клиент 03', 'Клиент 04')
                             OR source_name IN ('uniconf')
                             ORDER BY COALESCE(start_trading_on_source_at, created_on_source_at, created_at);") %>%
    
    as_tibble() %>%
    
    left_join(glossary, by = c("types_of_body" = "data.key")) %>%
    
    transmute(external_id = external_id, 
              payload_in_tons = payload_in_tons,
              types_of_body = data.title,
              start_region = start_region,
              start_address = start_address,
              finish_region = finish_region,
              finish_address = finish_address,
              loading_plan = loading_plan,
              unloading_plan = unloading_plan,
              loading_latitude = as.numeric(loading_latitude),
              loading_longitude = as.numeric(loading_longitude),
              unloading_latitude = as.numeric(unloading_latitude),
              unloading_longitude = as.numeric(unloading_longitude),
              base_price_net = base_price_net,
              client = case_when(charterer == "Клиент 02" ~ 'Клиент 02',
                                 charterer == "Клиент 03" ~ 'Клиент 03',
                                 charterer == "Клиент 04" ~ 'Клиент 04',
                                 source_name == "uniconf" ~ 'Клиент 06'
                                 )
    )
  
  loadboard_requests <- rbind(inbev_requests, transporeon_requests) %>%
    
  filter(Sys.time() - strptime(loading_plan, format = "%Y-%m-%dT%H:%M:%SZ") < 0)
  
  
  loadboard_requests1 <- loadboard_requests %>%
    transmute(external_id = external_id,
              payload_in_tons1 = payload_in_tons,
              types_of_body1 = types_of_body,
              start_region1 = start_region,
              start_address1 = start_address,
              finish_region1 = finish_region,
              finish_address1 = finish_address,
              loading_plan1 = loading_plan,
              unloading_plan1 = unloading_plan,
              loading_latitude1 = loading_latitude,
              loading_longitude1 = loading_longitude,
              unloading_latitude1 = unloading_latitude,
              unloading_longitude1 = unloading_longitude,
              base_price_net1 = base_price_net,
              client1 = client
    ) 
  
  result <- loadboard_requests %>%
    inner_join(loadboard_requests1, by = c("finish_region" = "start_region1", "start_region" = "finish_region1",  "types_of_body" = "types_of_body1")) %>%
    mutate(Время = as.numeric(difftime(strptime(loading_plan1, format = "%Y-%m-%dT%H:%M:%SZ"), strptime(unloading_plan, format = "%Y-%m-%dT%H:%M:%SZ"), units = "hours")))  %>%
    transmute(Кругорейс = paste(external_id.x, "\U2192", external_id.y),
                       Номер_прямого_рейса = external_id.x,
                       Ставка_прямого_рейса = base_price_net,
                       Клиент_прямого_рейса = client,
                       Плановые_даты_прямого_рейса = paste(loading_plan, "-", unloading_plan),
                       Маршрут_прямого_рейса_области = paste(start_region, "-", finish_region),
                       Маршрут_прямого_рейса = paste(start_address, "-", finish_address),
                       
                       Номер_обратного_рейса = external_id.y,
                       Ставка_обратного_рейса = base_price_net1,
                       Клиент_обратного_рейса = client1,
                       Плановые_даты_обратного_рейса = paste(loading_plan1, "-", unloading_plan1),
                       Маршрут_обратного_рейса_области = paste(finish_region, "-", start_region),
                       Маршрут_обратного_рейса = paste(start_address1, "-", finish_address1),
                       Часы_между_рейсами = Время
    ) %>%
    
    filter(Часы_между_рейсами >= 1 & Часы_между_рейсами <=23)
  
  result1 <- loadboard_requests1 %>%
    inner_join(loadboard_requests, by = c("finish_region1" = "start_region", "start_region1" = "finish_region",  "types_of_body1" = "types_of_body")) %>%
    mutate(Время = as.numeric(difftime(strptime(loading_plan, format = "%Y-%m-%dT%H:%M:%SZ"), strptime(unloading_plan1, format = "%Y-%m-%dT%H:%M:%SZ"), units = "hours")))  %>%
    transmute(Кругорейс = paste(external_id.x, "\U2192", external_id.y),
                       Номер_прямого_рейса = external_id.y,
                       Ставка_прямого_рейса = base_price_net,
                       Клиент_прямого_рейса = client,
                       Плановые_даты_прямого_рейса = paste(loading_plan, "-", unloading_plan),
                       Маршрут_прямого_рейса_области = paste(finish_region1, "-", start_region1),
                       Маршрут_прямого_рейса = paste(start_address, "-", finish_address),

                       Номер_обратного_рейса = external_id.y,
                       Ставка_обратного_рейса = base_price_net1,
                       Клиент_обратного_рейса = client1,
                       Плановые_даты_обратного_рейса = paste(loading_plan1, "-", unloading_plan1),
                       Маршрут_обратного_рейса_области = paste(start_region1, "-", finish_region1),
                       Маршрут_обратного_рейса = paste(start_address1, "-", finish_address1),
                       Часы_между_рейсами = Время
    ) %>%

    filter(Часы_между_рейсами >= 1 & Часы_между_рейсами <=23)

  outcome <- rbind(result, result1)

  result <- select(outcome, Кругорейс, Номер_прямого_рейса, Часы_между_рейсами) %>%
    left_join(loadboard_requests, by = c("Номер_прямого_рейса" = "external_id")) %>%
  mutate(loading_plan = format(strptime(loading_plan, format = "%Y-%m-%dT%H:%M:%SZ"), "%d.%m.%Y, %H:%M:%S", tz = "Europe/Moscow"),
         unloading_plan = format(strptime(unloading_plan, format = "%Y-%m-%dT%H:%M:%SZ"), "%d.%m.%Y, %H:%M:%S", tz = "Europe/Moscow")
  )
}


getDoublets_result <- function(){
  rbind(getDoublets() 
        # getDoublets1(), 
        # getDoublets2() %>% rename(vehicle_requirements_carrying_capacity_and_volume = 4,
        #                                                            vehicle_requirements_types_of_body = 5,
        #                                                            charterer_rate_amount = 16)
  )
}
 

getClientDirections <- function(startDate, endDate){
  
  my_query <- query('{
"bool": {
      "must": [
        {
  "match": {"isActual": "true"}
        }
      ]
}
}')
  
  #   vehicles_selected_field <- select_fields('{
  #   "includes": ["id", "data.typesOfBody", "data.payload", "data.cubicCapacity"]
  # }')
  #   
  #   vehicles <- elastic(cfg$db_elastic$url,
  #                       "vehicle-version-index") %search% (my_query + vehicles_selected_field) %>%
  #     mutate(data.typesOfBody = as.character(data.typesOfBody))
  
  glossary_selected_field <- select_fields('{
  "includes": ["data.key", "data.title"]
}')
  
  glossary <- elastic(cfg$db_elastic$url,
                      "glossary-element-version-index") %search% (my_query + glossary_selected_field)
  
  requests_by_day <- dbGetQuery(con,
                                paste("SELECT requests.short_id,
                                  requests.created_at,
                                  requests.juridical_person_version_short_title as client,
                     requests.vehicle_requirements_types_of_body,
                     requests.vehicle_requirements_carrying_capacity_and_volume,
                     first_loading.locality as start_point,
                     first_loading.region as start_region,
                     last_unloading.locality as finish_point,
                     last_unloading.region as finish_region,
                     last_unloading.from_at as unloading_plan


                     FROM erp_requests.requests


left join (
    select distinct on (s.request_id)
          request_id, id, locality, \"from\" as from_at, region
    from erp_requests.request_start_route_points s
    order by s.request_id desc, s.from asc
) as first_loading
on requests.id = first_loading.request_id

left join (
    select distinct on (s.request_id)
          request_id, id, locality, \"from\" as from_at, region
    from erp_requests.request_finish_route_points s
    order by s.request_id desc, s.from desc
) as last_unloading
on requests.id = last_unloading.request_id


                   WHERE requests.is_actual=true
                   AND last_unloading.from_at >=", paste0("'", startDate, "'::timestamp"), "AND last_unloading.from_at <=", paste0("'",endDate, "'::timestamp"),
                                      
                                      "AND requests.status not in ('cancelled', 'transportationCancelled', 'active');")) %>%
    
    as_tibble() %>%
    
    # left_join(vehicles, by = c("version_id" = "id")) %>%
    
    mutate(vehicle_requirements_types_of_body = as.character(gsub('\\{|\\}|\\"|\\"', "", vehicle_requirements_types_of_body))) %>%
    
    left_join(glossary, by = c("vehicle_requirements_types_of_body" = "data.key")) %>%
    
    left_join(glossary, by = c("vehicle_requirements_carrying_capacity_and_volume" = "data.key")) %>%
    
    transmute(Клиент = client,
                   Дата_выгрузки = as.Date(strftime(unloading_plan, format="%Y-%m-%d")),
                   Регион_загрузки = start_region,
                   Регион_выгрузки = finish_region,
                   Маршрут = paste(start_point, "-", finish_point),
                   Тип_ТС = data.title.x,
                   Объём_тоннаж = gsub("<sup>3</sup>", "\U00B3 ", data.title.y)
    ) 
  
}

getRequestProfitability <- function(startDate, endDate){
  requests_all <- dbGetQuery(con,
                                paste("SELECT requests.short_id,
                                  transportations.created_at,
                                  requests.responsible_expeditor_title,
                     requests.charterer_rate_amount,
                     requests.charterer_version_title as client,
                     requests.type,
                     transportations.carrier_rate_info_amount,
                     transportations.profit_amount,
                     transportations.carrier_rate_info_with_nds as nds,
                     first_loading.region as start_region,
                     last_unloading.region as finish_region
                     
                     FROM erp_requests.requests

                   LEFT JOIN erp_requests.transportations
                   ON requests.id = transportations.request_id

left join (
    select distinct on (s.request_id)
          request_id, id, locality, address, \"from\" as from_at, fact_arrival_date_time, region
    from erp_requests.request_start_route_points s
    order by s.request_id desc, s.from asc
) as first_loading
on requests.id = first_loading.request_id

left join (
    select distinct on (s.request_id)
          request_id, id, locality, address, \"from\" as from_at, fact_arrival_date_time, region, consignee, fact_departure_date_time
    from erp_requests.request_finish_route_points s
    order by s.request_id desc, s.from desc
) as last_unloading
on requests.id = last_unloading.request_id


                   WHERE requests.is_actual=true
                   AND transportations.created_at >=", paste0("'", startDate, "'::timestamp"), "AND requests.created_at <=", paste0("'",endDate, "'::timestamp"),

                                      "AND requests.status not in ('cancelled', 'transportationCancelled', 'active');")) %>%
    
    left_join(expeditors_group, by = c("responsible_expeditor_title" = "data.id")) %>%
    
    transmute(Рейсы = short_id,
                   Клиент = client,
                   Тип_заявки = case_when(type == "Spot" ~ "Спот",
                                          type == "Warranty" ~ "Гарантия"),
                   География = case_when(start_region == finish_region ~ "Город",
                                                        TRUE ~ "Межгород"),
                   Логист = responsible_expeditor_title,
                   Группа = data.name,
                   Дата_создания_перевозки = as.Date(strftime(created_at, format="%Y-%m-%d")),
                   Ставка_клиента_НДС = case_when(str_detect(client, "НДС") ~ charterer_rate_amount * 1.2,
                                                  TRUE ~ charterer_rate_amount),
                   Ставка_клиента = case_when(str_detect(client, "НДС") ~ charterer_rate_amount,
                                              TRUE ~ charterer_rate_amount / 1.2),
                   Ставка_перевозчика_НДС = case_when(nds == TRUE ~ carrier_rate_info_amount,
                                                      nds == FALSE ~ carrier_rate_info_amount * 1.2),
                   Ставка_перевозчика = case_when(nds == TRUE ~ carrier_rate_info_amount/ 1.2,
                                                  nds == FALSE ~ carrier_rate_info_amount),
                   Маржа_ERP = profit_amount,
                   Маржа_расчёт = Ставка_клиента - Ставка_перевозчика,
                   Рентабельность_расчёт = Маржа_расчёт / Ставка_клиента * 100
    )

}



getRevenueProfit <- function(startDate, endDate){
  
  my_query <- query('{
"bool": {
      "must": [
        {
  "match": {"isActual": "true"}
        }
      ]
}
}')
  
  selected_field_user <- select_fields('{
  "includes": ["data.id", "data.title"]
}')
  
  user = elastic(cfg$db_elastic$url,
                 "user-profile-version-index") %search% (my_query + selected_field_user) %>%
    group_by(data.id) %>%
    summarise(data.title = first(data.title))
  
  requests_all <- dbGetQuery(con,
                             paste("SELECT date_trunc('day', transportations.created_at AT TIME ZONE 'Europe/Moscow') as Дата,                                  
                     logist.title as Логист,
                     requests.charterer_version_title as Клиент,
                     manager.title as Менеджер,
                     (case 
              		when requests.type = 'Warranty' then 'Гарантия'
              		when requests.type = 'Spot' then 'Спот'
              	    end) as \"Тип заявки\",
                     (case 
              		when first_loading.region = last_unloading.region then 'Регион'
              		else 'Межрегион'
              	    end) as География,
              	    
                     count(*) AS Рейсы,
                     SUM(requests.charterer_rate_amount) as Оборот,                  

              	    SUM(transportations.profit_amount) as Маржа
              	    
                     FROM erp_requests.requests

                   LEFT JOIN erp_requests.transportations
                   ON requests.id = transportations.request_id

left join (
    select distinct on (s.request_id)
          request_id, id, locality, address, \"from\" as from_at, fact_arrival_date_time, region
    from erp_requests.request_start_route_points s
    order by s.request_id desc, s.from asc
) as first_loading
on requests.id = first_loading.request_id

left join (
    select distinct on (s.request_id)
          request_id, id, locality, address, \"from\" as from_at, fact_arrival_date_time, region, consignee, fact_departure_date_time
    from erp_requests.request_finish_route_points s
    order by s.request_id desc, s.from desc
) as last_unloading
on requests.id = last_unloading.request_id

left join erp_users.user_profiles as logist
on requests.responsible_expeditor_id = logist.original_id 
and logist.is_actual = true

left join erp_users.user_profiles as manager
on requests.charterer_version_signed_by_uid = manager.original_id 
and manager.is_actual = true

                   WHERE requests.is_actual = true
                   AND requests.status not in ('cancelled', 'transportationCancelled', 'active')
                   AND transportations.created_at >=", paste0("'", startDate, "'::timestamp"), "AND transportations.created_at <=", paste0("'",endDate, "'::timestamp"),
                                   
                                   "group by Дата, Клиент, Менеджер, Логист, \"Тип заявки\", География;"))
  
}

getRevenueProfit1 <- function(startDate, endDate){
  
  my_query <- query('{
"bool": {
      "must": [
        {
  "match": {"isActual": "true"}
        }
      ]
}
}')
  
  selected_field_user <- select_fields('{
  "includes": ["data.id", "data.title"]
}')
  
  user = elastic(cfg$db_elastic$url,
                 "user-profile-version-index") %search% (my_query + selected_field_user) %>%
    group_by(data.id) %>%
    summarise(data.title = first(data.title))
  
  requests_all <- dbGetQuery(con,
                             paste("SELECT date_trunc('day', last_unloading.from_at AT TIME ZONE 'Europe/Moscow') as Дата,                                  
                     logist.title as Логист,
                     requests.charterer_version_title as Клиент,
                     manager.title as Менеджер,
                     (case 
              		when requests.type = 'Warranty' then 'Гарантия'
              		when requests.type = 'Spot' then 'Спот'
              	    end) as \"Тип заявки\",
                     (case 
              		when first_loading.region = last_unloading.region then 'Регион'
              		else 'Межрегион'
              	    end) as География,
              	    
                     count(*) AS Рейсы,
                     SUM(requests.charterer_rate_amount) as Оборот,                  

              	    SUM(transportations.profit_amount) as Маржа
                     
                     FROM erp_requests.requests

                   LEFT JOIN erp_requests.transportations
                   ON requests.id = transportations.request_id

left join (
    select distinct on (s.request_id)
          request_id, id, locality, address, \"from\" as from_at, fact_arrival_date_time, region
    from erp_requests.request_start_route_points s
    order by s.request_id desc, s.from asc
) as first_loading
on requests.id = first_loading.request_id

left join (
    select distinct on (s.request_id)
          request_id, id, locality, address, \"from\" as from_at, fact_arrival_date_time, region, consignee, fact_departure_date_time
    from erp_requests.request_finish_route_points s
    order by s.request_id desc, s.from desc
) as last_unloading
on requests.id = last_unloading.request_id

left join erp_users.user_profiles as logist
on requests.responsible_expeditor_id = logist.original_id 
and logist.is_actual = true

left join erp_users.user_profiles as manager
on requests.charterer_version_signed_by_uid = manager.original_id 
and manager.is_actual = true


                   WHERE requests.is_actual=true
                   AND requests.status not in ('cancelled', 'transportationCancelled', 'active')
                   AND last_unloading.from_at >=", paste0("'", startDate, "'::timestamp"), "AND last_unloading.from_at <=", paste0("'",endDate, "'::timestamp"),
                                   
                                   "group by Дата, Клиент, Менеджер, Логист, \"Тип заявки\", География;"))
  
}

# getDZ_receivable_turnover <- function(){
#   
#   requests_all <- dbGetQuery(con, 
#                              "SELECT
# bills.client_juridical_person_short_title as Контрагент,
# avg(bills.payment_date - bills.formed_at::date) as \"Оплата счёта, дни\",
# avg(bills.payment_date - requests.last_unload_at::date) as \"Оплата заявки, дни\"
# 
# FROM document_flow.requests
# 
# left join document_flow.bills
# on requests.bill_id = bills.id
# 
# WHERE bills.is_actual = TRUE AND bills.is_paid = true and requests.is_actual = true
# 
# group by Контрагент")
#   
# }


getDZ_receivable_turnover <- function(startDate, endDate){
  
  requests_all <- DBI::dbGetQuery(con,
                             "select old_flow.short_title as Контрагент,

avg(old_flow.docsReceived - old_flow.from_at::date) as \"Ожидание ТСД\",
avg(old_flow.docsApproved - old_flow.docsReceived) as \"Проверка ТСД\",
avg(old_flow.created_at::date - old_flow.docsReceived) as \"Формирование счёта\",
avg((COALESCE(old_flow.send_date, new_flow.send_date))::date - old_flow.created_at::date) as \"Отправление счёта\",
avg((COALESCE(old_flow.delivery_date, new_flow.delivery_date))::date - (COALESCE(old_flow.send_date, new_flow.send_date))::date) as \"Доставка счёта\",
avg(old_flow.payment_date - old_flow.created_at::date) as \"Оплата счёта\",
avg(old_flow.payment_date - old_flow.from_at::date) as \"Оплата заявки\"

     

from (select 
    
    requests.id,
    last_unloading.from_at,
    ((jsonb_path_query_array(transportations.statuses_history, '$[*] ? (@.Status == \"docsReceived\")')->-1->>'IssueDateTime')::timestamp)::date as docsReceived,
    ((jsonb_path_query_array(transportations.statuses_history, '$[*] ? (@.Status == \"docsApproved\")')->-1->>'IssueDateTime')::timestamp )::date as docsApproved,
    juridical_persons.short_title,
    documents_packets.delivery_date,
    documents_packets.send_date,
    bills.status as bills_status,
    bills.created_at,
    bills.payment_date::date

FROM erp_requests.requests

INNER JOIN erp_requests.transportations
ON requests.id = transportations.request_id

left join (
  select distinct on (s.request_id)
  request_id, id, locality, address, \"from\" as from_at
  from erp_requests.request_finish_route_points s
  order by s.request_id desc, s.from desc
) as last_unloading
on requests.id = last_unloading.request_id

 left join (
    select
        juridical_persons.id,
        juridical_persons.short_title
        from erp_juridical_persons.juridical_persons
    left join erp_clients.juridicals on juridical_persons.id = juridicals.juridical_person_id and juridical_persons.version = juridicals.juridical_person_version
    left join erp_clients.clients on juridicals.client_id = clients.id and juridicals.client_version = clients.version
    where juridical_persons.is_actual and clients.is_actual=true
) as juridical_persons
on requests.juridical_person_version_id = juridical_persons.id

 left JOIN (
    select 
        document_packets_requests.id,
         documents_packets.status,
         documents_packets.delivery_date,
    documents_packets.send_date
     from erp_documents_packets.document_packets_requests
     inner join erp_documents_packets.documents_packets
     ON document_packets_requests.documents_packet_id = documents_packets.id 
     AND document_packets_requests.documents_packet_version = documents_packets.version
     and documents_packets.is_actual
 ) as documents_packets
 ON requests.id = documents_packets.id AND requests.is_actual

left join (select bills.status,

        bills.created_at,
        bills.payment_date,

      unnest(bills.request_ids) as request_id 

      from erp_bills.bills 
      
      where bills.is_actual and bills.type = 'Charterer') as bills

on requests.id = bills.request_id

where requests.is_actual = true 

AND requests.status not in ('cancelled', 'transportationCancelled', 'active')

and last_unloading.from_at >= $1 and last_unloading.from_at <= $2

) as old_flow


left join (select bills.status, bills.send_date, bills.delivery_date, unnest(bills.request_ids) as request_id 

      from document_flow.bills 
      
      where bills.is_actual) as new_flow

on old_flow.id = new_flow.request_id

where old_flow.bills_status = 'Paid'

group by Контрагент", params = list(startDate, endDate))
  
}


# getDZ_confirmed_receivables <- function(){
#   
#   requests_all <- dbGetQuery(con, 
#                              "select bills.client_juridical_person_short_title as Контрагент,
# 	   bills.number as \"Номер счёта\",
# 	   bills.amount as Сумма,
# 	   bills.delivery_date::date as \"Дата доставки\",
# 	   clients.grace_of_payments_days as \"Отсрочка платежа\",
# 	   bills.delivery_date::date + make_interval(days => clients.grace_of_payments_days::integer) as \"Дата оплаты расчётная\",
# 	   
# 	   (case
# 	   	when bills.planned_payment_date is not null then bills.planned_payment_date - now()::date
# 	   	else (bills.delivery_date::date + make_interval(days => clients.grace_of_payments_days::integer))::date - now()::date
# 	   	end) as \"Отклонение срока оплаты\"
# 	   	   	   	   
# from document_flow.bills 
# 
# LEFT JOIN (SELECT clients.id, 
#             juridicals.grace_of_payments_days, 
#             juridicals.tsd_settings_doc_verification_days
#             FROM erp_clients.clients
#             INNER JOIN erp_clients.juridicals
#             ON clients.id = juridicals.client_id 
#             AND clients.version = juridicals.client_version 
#             WHERE clients.is_actual = TRUE
#             ) as clients
# ON bills.client_id = clients.id
# 	   
# WHERE bills.is_actual = TRUE and bills.status = 'Delivered'
# ")
#   
# }

getDZ_confirmed_receivables <- function(){
  
  requests_all <- dbGetQuery(con, 
                             "with 
bills as (
        select bills.id,
        
            bills.created_at_original, 
        
            bills.number_doc,
            
            (case
              when bills.status = 'PartialPaid' then bills.invoice_amount - bills.paid_amount
              else bills.invoice_amount
            end) as invoice_amount,
            
            unnest(bills.request_ids) as request_id 
            from erp_bills.bills 
            
            where bills.is_actual and bills.type = 'Charterer' and bills.status != 'Paid' and bills.number_doc is not null and bills.invoice_amount != 0
            
            ),
                    
            
juridical_persons as (
        select juridical_persons.id,
        
        juridical_persons.short_title,
        
        juridicals.grace_of_payment_grace_period_days,
        
        (case
        
            when juridicals.grace_of_payment_hidden_grace_period_days is null then 0
            else juridicals.grace_of_payment_hidden_grace_period_days
            
        end) as grace_of_payment_hidden_grace_period_days
        
    from erp_juridical_persons.juridical_persons
    
    left join erp_clients.juridicals on juridical_persons.id = juridicals.juridical_person_id and juridical_persons.version = juridicals.juridical_person_version
    
    left join erp_clients.clients on juridicals.client_id = clients.id and juridicals.client_version = clients.version
    
    where juridical_persons.is_actual and clients.is_actual=true
    
    ),
    
    
documents_packets as (
     select distinct on (document_packets_requests.id)
     
         document_packets_requests.id,
         
         documents_packets.delivery_date,
         
         documents_packets.status
         
     from erp_documents_packets.document_packets_requests
     
     inner join erp_documents_packets.documents_packets
     
     ON document_packets_requests.documents_packet_id = documents_packets.id
     
     AND document_packets_requests.documents_packet_version = documents_packets.version
     
     and documents_packets.is_actual
     
     order by document_packets_requests.id, documents_packets.created_at desc
),

requests as (
            
            select  requests.id,
            
            requests.short_id,
            
            requests.charterer_rate_amount,
            
            juridical_persons.short_title,
        
            juridical_persons.grace_of_payment_grace_period_days,
            
            juridical_persons.grace_of_payment_hidden_grace_period_days,
            
            documents_packets.status,
        
            documents_packets.delivery_date
            
            from erp_requests.requests
                
            left join juridical_persons
            on requests.juridical_person_version_id = juridical_persons.id
            
            left join documents_packets
            on requests.id = documents_packets.id AND requests.is_actual 
            
            where requests.is_actual = true 
            
),

old_flow as (
        select requests.id as request_id,
        
        requests.short_id,
        
        requests.short_title,
        
        requests.charterer_rate_amount,
        
        requests.grace_of_payment_grace_period_days,
        
        requests.grace_of_payment_hidden_grace_period_days,
        
        bills.id,
        
        bills.number_doc,
        
        bills.invoice_amount,
                
        requests.status,
        
        requests.delivery_date,
        
        (requests.delivery_date AT TIME ZONE 'Europe/Moscow' + (interval '1' day * requests.grace_of_payment_grace_period_days) + (interval '1' day * requests.grace_of_payment_hidden_grace_period_days))::date as planned_payment_date
        
        from bills
        
        left join requests
        
        on bills.request_id = requests.id
        
        order by bills.number_doc, bills.created_at_original desc, request_id
        
),
new_flow as ( 
        select id, delivery_date, status, planned_payment_date 
        
        from document_flow.bills
        
        where bills.is_actual = true 
),
result as (
       select old_flow.short_title as Контрагент,
       old_flow.request_id,
       old_flow.number_doc as \"Номер счёта\",
       old_flow.short_id as \"Номер заявки\",
       old_flow.invoice_amount as Сумма,
       old_flow.charterer_rate_amount as \"Ставка клиента\",
       (COALESCE(old_flow.delivery_date, new_flow.delivery_date) AT TIME ZONE 'Europe/Moscow')::date as \"Дата доставки\",
        old_flow.grace_of_payment_grace_period_days as \"Отсрочка платежа\",
        
        old_flow.grace_of_payment_hidden_grace_period_days as \"Скрытая отсрочка\",
                
    COALESCE(old_flow.planned_payment_date, new_flow.planned_payment_date, (new_flow.delivery_date AT TIME ZONE 'Europe/Moscow' + (interval '1' day * old_flow.grace_of_payment_grace_period_days))::date) as \"Плановая дата оплаты\",
        
    COALESCE(old_flow.planned_payment_date, new_flow.planned_payment_date, (new_flow.delivery_date AT TIME ZONE 'Europe/Moscow' + (interval '1' day * old_flow.grace_of_payment_grace_period_days))::date) - now()::date as \"Отклонение срока оплаты\",
    
    (case
        
            when (COALESCE(old_flow.planned_payment_date, new_flow.planned_payment_date, (new_flow.delivery_date AT TIME ZONE 'Europe/Moscow' + (interval '1' day * old_flow.grace_of_payment_grace_period_days))::date) - now()::date) >= 0 then 'ПДЗ'
            when COALESCE(old_flow.delivery_date, new_flow.delivery_date) is null then 'НДЗ'
            when (COALESCE(old_flow.planned_payment_date, new_flow.planned_payment_date, (new_flow.delivery_date AT TIME ZONE 'Europe/Moscow' + (interval '1' day * old_flow.grace_of_payment_grace_period_days))::date) - now()::date) < 0 then 'ПрДЗ'
            
        end) as Категория
        
        from old_flow
        
        left join new_flow
        
        on old_flow.id = new_flow.id
        
        where COALESCE(old_flow.status, new_flow.status) in ('received', 'approved') or COALESCE(old_flow.status, new_flow.status) in ('Delivered', 'Approved')
)
select * from result;")
}

# getDZ_unconfirmed_receivables <- function(){
#   
#   requests_all <- dbGetQuery(con, 
#                              "SELECT 
# 
# requests.client_juridical_person_short_title AS Контрагент, 
# sum(requests.client_rate) AS Сумма, 
# 
# 	   (case
# 	   	when bills.status = 'Formed' then 'Сформированные'
# 	    when bills.status = 'Sent' then 'Отправленные'
# 	    when bills.status is null and requests.transportation_shipping_documents_info_status = 'Awaiting' then 'В ожидании'
# 	    when bills.status is null and requests.transportation_shipping_documents_info_status = 'Received' then 'Полученные'
# 	    when bills.status is null and requests.transportation_shipping_documents_info_status = 'Approved' then 'Проверенные'
# 	    when bills.status is null and requests.transportation_shipping_documents_info_status = 'Problem' then 'Проблемы'
# 	   	end) as Статус
# 
# FROM document_flow.requests 
# 
# left join (select bills.status, unnest(bills.request_ids) as request_id 
# 
# 			from document_flow.bills 
# 			
# 			where bills.is_actual and bills.status in ('Formed', 'Sent')) as bills
# 
# on requests.id = bills.request_id
# 
# WHERE requests.is_actual = true
# 
# group by Контрагент, Статус")
#   
# }


getDZ_unconfirmed_receivables <- function(startDate, endDate){
  
  sum <- DBI::dbGetQuery(con,
                             "select old_flow.short_title as Контрагент,

	   sum(old_flow.charterer_rate_amount) filter (where COALESCE(old_flow.status, new_flow.status) is null and  old_flow.transportations_status in ('ready', 'onStartPoint', 'loaded', 'onTheWay', 'onFinishPoint', 'unloaded')) as \"В ожидании\",

	   sum(old_flow.charterer_rate_amount) filter (where COALESCE(old_flow.status, new_flow.status) is null and old_flow.transportations_status = 'docsReceived') as Полученные,

	   sum(old_flow.charterer_rate_amount) filter (where COALESCE(old_flow.status, new_flow.status) is null and (old_flow.transportations_status = 'docsApproved' or old_flow.transportations_status = 'paid')) as Проверенные,

	   sum(old_flow.charterer_rate_amount) filter (where COALESCE(old_flow.status, new_flow.status) is null and old_flow.transportations_status = 'docsProblem') as \"Проблемные (пер.)\", 	

	   sum(old_flow.charterer_rate_amount) filter (where COALESCE(old_flow.status, new_flow.status) = 'Formed' or COALESCE(old_flow.status, new_flow.status) = 'formed') as Сформированные,
	   
	   sum(old_flow.charterer_rate_amount) filter (where COALESCE(old_flow.status, new_flow.status) = 'Sent' or COALESCE(old_flow.status, new_flow.status) = 'sent') as Отправленные,
	   
	   sum(old_flow.charterer_rate_amount) filter (where COALESCE(old_flow.status, new_flow.status) = 'Problem' or COALESCE(old_flow.status, new_flow.status) = 'problem') as \"Проблемные (кл.)\"

from (select 
		
		requests.id,
        juridical_persons.short_title,
        requests.charterer_rate_amount,
		documents_packets.status,
		transportations.status as transportations_status,
		bills.status as bills_status

FROM erp_requests.requests

INNER JOIN erp_requests.transportations
ON requests.id = transportations.request_id
and transportations.status in ('ready', 'onStartPoint', 'loaded', 'onTheWay', 'onFinishPoint', 'unloaded', 'docsReceived', 'docsApproved', 'paid', 'docsProblem')

 left join (
    select
        juridical_persons.id,
        juridical_persons.short_title
        from erp_juridical_persons.juridical_persons
    left join erp_clients.juridicals on juridical_persons.id = juridicals.juridical_person_id and juridical_persons.version = juridicals.juridical_person_version
    left join erp_clients.clients on juridicals.client_id = clients.id and juridicals.client_version = clients.version
    where juridical_persons.is_actual and clients.is_actual=true
) as juridical_persons
on requests.juridical_person_version_id = juridical_persons.id

 left JOIN (
 	 select 
 	     document_packets_requests.id,
         documents_packets.status
     from erp_documents_packets.document_packets_requests
     inner join erp_documents_packets.documents_packets
     ON document_packets_requests.documents_packet_id = documents_packets.id 
     AND document_packets_requests.documents_packet_version = documents_packets.version
     and documents_packets.is_actual
 ) as documents_packets
 ON requests.id = documents_packets.id AND requests.is_actual and documents_packets.status in ('formed', 'sent', 'received', 'approved', 'payed', 'problem')

left join (select bills.status,

			unnest(bills.request_ids) as request_id 

			from erp_bills.bills 
			
			where bills.is_actual and bills.type = 'Charterer') as bills

on requests.id = bills.request_id

left join (
  select distinct on (s.request_id)
  request_id, id, locality, address, \"from\" as from_at, fact_arrival_date_time, region, consignee, fact_departure_date_time
  from erp_requests.request_finish_route_points s
  order by s.request_id desc, s.from desc
) as last_unloading
on requests.id = last_unloading.request_id

where requests.is_actual = true 

AND requests.status not in ('cancelled', 'transportationCancelled', 'active', 'paid')

and last_unloading.from_at >= $1 and last_unloading.from_at <= $2

and requests.charterer_rate_amount != 0

and requests.charterer_rate_amount != 1

) as old_flow


left join (select bills.status, unnest(bills.request_ids) as request_id 

			from document_flow.bills 
			
			where bills.is_actual and bills.status in ('Formed', 'Sent', 'Delivered', 'Approved', 'Paid', 'Problem')) as new_flow

on old_flow.id = new_flow.request_id

where old_flow.bills_status != 'Paid' or old_flow.bills_status is null

group by Контрагент", params = list(startDate, endDate))
  
  
  sum[is.na(sum)] <- 0
  
  sum <- sum %>%
    mutate(Всего = `В ожидании` + `Полученные` + `Проверенные` + `Проблемные (пер.)` + `Сформированные` + `Отправленные` + `Проблемные (кл.)`) %>%
    filter(Всего != 0)
  
  
  days <- DBI::dbGetQuery(con,
                              "select old_flow.short_title as Контрагент,

	   avg(now()::date - old_flow.from_at::date) filter (where COALESCE(old_flow.status, new_flow.status) is null and  old_flow.transportations_status in ('ready', 'onStartPoint', 'loaded', 'onTheWay', 'onFinishPoint', 'unloaded')) as \"В ожидании\",

	   avg(now()::date - old_flow.docsReceived) filter (where COALESCE(old_flow.status, new_flow.status) is null and old_flow.transportations_status = 'docsReceived') as Полученные,

	   avg(now()::date - old_flow.docsApproved) filter (where COALESCE(old_flow.status, new_flow.status) is null and (old_flow.transportations_status = 'docsApproved' or old_flow.transportations_status = 'paid')) as Проверенные,

	   avg(now()::date - old_flow.docsProblem) filter (where COALESCE(old_flow.status, new_flow.status) is null and old_flow.transportations_status = 'docsProblem') as \"Проблемные (пер.)\", 	

	   avg(now()::date - old_flow.created_at::date) filter (where COALESCE(old_flow.status, new_flow.status) = 'Formed' or COALESCE(old_flow.status, new_flow.status) = 'formed') as Сформированные,
	   
	   avg(now()::date - (COALESCE(old_flow.send_date, new_flow.send_date))::date) filter (where COALESCE(old_flow.status, new_flow.status) = 'Sent' or COALESCE(old_flow.status, new_flow.status) = 'sent') as Отправленные,
	   
	   avg(now()::date - (COALESCE(old_flow.returned_date, new_flow.return_date))::date) filter (where COALESCE(old_flow.status, new_flow.status) = 'Problem' or COALESCE(old_flow.status, new_flow.status) = 'problem') as \"Проблемные (кл.)\"


from (select 
		
		requests.id,
        juridical_persons.short_title,
        requests.charterer_rate_amount,
		documents_packets.status,
		transportations.status as transportations_status,
		bills.status as bills_status,
		last_unloading.from_at,
		((jsonb_path_query_array(transportations.statuses_history, '$[*] ? (@.Status == \"docsReceived\")')->-1->>'IssueDateTime')::timestamp)::date as docsReceived,
    ((jsonb_path_query_array(transportations.statuses_history, '$[*] ? (@.Status == \"docsApproved\")')->-1->>'IssueDateTime')::timestamp )::date as docsApproved,
		((jsonb_path_query_array(transportations.statuses_history, '$[*] ? (@.Status == \"docsProblem\")')->-1->>'IssueDateTime')::timestamp )::date as docsProblem,
		bills.created_at,
		documents_packets.send_date,
		documents_packets.returned_date
		
FROM erp_requests.requests

INNER JOIN erp_requests.transportations
ON requests.id = transportations.request_id
and transportations.status in ('ready', 'onStartPoint', 'loaded', 'onTheWay', 'onFinishPoint', 'unloaded', 'docsReceived', 'docsApproved', 'paid', 'docsProblem')

 left join (
    select
        juridical_persons.id,
        juridical_persons.short_title
        from erp_juridical_persons.juridical_persons
    left join erp_clients.juridicals on juridical_persons.id = juridicals.juridical_person_id and juridical_persons.version = juridicals.juridical_person_version
    left join erp_clients.clients on juridicals.client_id = clients.id and juridicals.client_version = clients.version
    where juridical_persons.is_actual and clients.is_actual=true
) as juridical_persons
on requests.juridical_person_version_id = juridical_persons.id

 left JOIN (
 	 select 
 	     document_packets_requests.id,
         documents_packets.status,
         documents_packets.send_date,
         documents_packets.returned_date
     from erp_documents_packets.document_packets_requests
     inner join erp_documents_packets.documents_packets
     ON document_packets_requests.documents_packet_id = documents_packets.id 
     AND document_packets_requests.documents_packet_version = documents_packets.version
     and documents_packets.is_actual
 ) as documents_packets
 ON requests.id = documents_packets.id AND requests.is_actual and documents_packets.status in ('formed', 'sent', 'received', 'approved', 'payed', 'problem')

left join (select bills.status,

			bills.created_at,

			unnest(bills.request_ids) as request_id 

			from erp_bills.bills 
			
			where bills.is_actual and bills.type = 'Charterer') as bills

on requests.id = bills.request_id

left join (
  select distinct on (s.request_id)
  request_id, id, locality, address, \"from\" as from_at, fact_arrival_date_time, region, consignee, fact_departure_date_time
  from erp_requests.request_finish_route_points s
  order by s.request_id desc, s.from desc
) as last_unloading
on requests.id = last_unloading.request_id

where requests.is_actual = true 

AND requests.status not in ('cancelled', 'transportationCancelled', 'active')

and last_unloading.from_at >= $1 and last_unloading.from_at <= $2

and requests.charterer_rate_amount != 0

and requests.charterer_rate_amount != 1

) as old_flow


left join (select bills.status, bills.send_date, bills.return_date, unnest(bills.request_ids) as request_id 

			from document_flow.bills 
			
			where bills.is_actual and bills.status in ('Formed', 'Sent', 'Delivered', 'Approved', 'Paid', 'Problem')) as new_flow

on old_flow.id = new_flow.request_id

where old_flow.bills_status != 'Paid' or old_flow.bills_status is null

group by Контрагент", params = list(startDate, endDate))
  
  
  days[is.na(days)] <- 0
  
  days <- days %>%
    mutate(Всего = `В ожидании` + `Полученные` + `Проверенные` + `Проблемные (пер.)` + `Сформированные` + `Отправленные` + `Проблемные (кл.)`) %>%
    filter(Всего != 0) %>%
    select(-Всего)
  
  
  sum %>%
    left_join(days, by = c("Контрагент" = "Контрагент"))
  
}

# НДЗ - экспорт в excel
getDZ_unconfirmed_receivables_detail <- function(startDate, endDate){
  
  requests_all <- DBI::dbGetQuery(con,
                              "select old_flow.short_title as Контрагент,

	   old_flow.short_id as \"Номер заявки\",	
	   
	   (old_flow.from_at AT TIME ZONE 'Europe/Moscow')::date as \"Дата выгрузки\",
	   
	   	   	   (case
	    when COALESCE(old_flow.status, new_flow.status) is null and  old_flow.transportations_status in ('ready', 'onStartPoint', 'loaded', 'onTheWay', 'onFinishPoint', 'unloaded') then now()::date - old_flow.from_at::date
	    else old_flow.docsReceived - old_flow.from_at::date
	   	end) as \"В ожидании, дни\",
	   
	   old_flow.req_status as \"Статус заявки\",
	   
	   old_flow.trans_status as \"Статус перевозки\",
	   
	   old_flow.bills_status as \"Статус счёта\",

	   old_flow.charterer_rate_amount as Сумма,

	   (case
	   	when COALESCE(old_flow.status, new_flow.status) = 'Formed' or COALESCE(old_flow.status, new_flow.status) = 'formed' then 'Сформированные'
	    when COALESCE(old_flow.status, new_flow.status) = 'Sent' or COALESCE(old_flow.status, new_flow.status) = 'sent' then 'Отправленные'
	    when COALESCE(old_flow.status, new_flow.status) is null and  old_flow.transportations_status in ('ready', 'onStartPoint', 'loaded', 'onTheWay', 'onFinishPoint', 'unloaded') then 'В ожидании'
	    when COALESCE(old_flow.status, new_flow.status) is null and old_flow.transportations_status = 'docsReceived' then 'Полученные'
	    when COALESCE(old_flow.status, new_flow.status) is null and (old_flow.transportations_status = 'docsApproved' or old_flow.transportations_status = 'paid') then 'Проверенные'
	    when COALESCE(old_flow.status, new_flow.status) is null and old_flow.transportations_status = 'docsProblem' then 'Проблемы (пер.)'
	    when COALESCE(old_flow.status, new_flow.status) = 'Delivered' or COALESCE(old_flow.status, new_flow.status) = 'received' then 'Доставленные'
	    when COALESCE(old_flow.status, new_flow.status) = 'Approved' or COALESCE(old_flow.status, new_flow.status) = 'approved' then 'Подтверждённые'
	    when COALESCE(old_flow.status, new_flow.status) = 'Problem' or COALESCE(old_flow.status, new_flow.status) = 'problem' then 'Проблемы (кл.)'
	    when COALESCE(old_flow.status, new_flow.status) = 'Paid' or COALESCE(old_flow.status, new_flow.status) = 'payed' then 'Оплаченные'

	   	else 'Статус неизвестен'
	   	end) as \"Статус ДЗ\"	   	

from (select 
		
		requests.id,
		requests.short_id,

		
		(case
 when requests.status = 'carrierFound' then 'Перевозчик найден'
 when requests.status = 'paid' then 'Клиент всё оплатил'
 when requests.status = 'cancelled' then 'Заявка отменена'
 when requests.status = 'transportationCancelled' then 'Перевозка отменена'
 when requests.status = 'docsReceived' then 'Документы получены клиентом'
 when requests.status = 'docsApproved' then 'Документы подтверждены клиентом'
 when requests.status = 'docsGrouped' then 'Документы готовы к отправке'
 when requests.status = 'docsProblem' then 'Есть проблемы с документами'
 when requests.status = 'docsSent' then 'Документы высланы клиенту'

 end) as req_status,

(case
 when transportations.status = 'ready' then 'Перевозка началась'
 when transportations.status = 'onStartPoint' then 'Водитель прибыл на загрузку'
 when transportations.status = 'loaded' then 'Водитель в пути'
 when transportations.status = 'onTheWay' then 'Пройдена половина пути, всё хорошо'
 when transportations.status = 'onFinishPoint' then 'Водитель прибыл на разгрузку'
 when transportations.status = 'unloaded' then 'Водитель разгрузился, ждём документы'
 when transportations.status = 'docsReceived' then 'Получены документы от водителя'
 when transportations.status = 'paid' then 'Перевозка оплачена'
 when transportations.status = 'cancelled' then 'Перевозка отменена'
 when transportations.status = 'docsProblem' then 'У водителя проблемы с документами'
 when transportations.status = 'docsApproved' then 'Документы водителя подтверждены'

 end) as trans_status,
		
		
		last_unloading.from_at,
    juridical_persons.short_title,
    requests.charterer_rate_amount,
		documents_packets.status,
		transportations.status as transportations_status,
		((jsonb_path_query_array(transportations.statuses_history, '$[*] ? (@.Status == \"docsReceived\")')->-1->>'IssueDateTime')::timestamp)::date as docsReceived,
		
				(case
 when bills.status = 'ForPayment' or bills.status = 'NotPaid' then 'Не оплачен'
 when bills.status = 'PartialPaid' then 'Частично оплачен'
 when bills.status = 'Paid' then 'Оплачено'
 when bills.status is null then 'Не выставлен/нет информации'

 end) as bills_status

FROM erp_requests.requests

INNER JOIN erp_requests.transportations
ON requests.id = transportations.request_id
and transportations.status in ('ready', 'onStartPoint', 'loaded', 'onTheWay', 'onFinishPoint', 'unloaded', 'docsReceived', 'docsApproved', 'paid', 'docsProblem')

left join (
  select distinct on (s.request_id)
  request_id, id, locality, address, \"from\" as from_at
  from erp_requests.request_finish_route_points s
  order by s.request_id desc, s.from desc
) as last_unloading
on requests.id = last_unloading.request_id


 left join (
    select
        juridical_persons.id,
        juridical_persons.short_title
        from erp_juridical_persons.juridical_persons
    left join erp_clients.juridicals on juridical_persons.id = juridicals.juridical_person_id and juridical_persons.version = juridicals.juridical_person_version
    left join erp_clients.clients on juridicals.client_id = clients.id and juridicals.client_version = clients.version
    where juridical_persons.is_actual and clients.is_actual=true
) as juridical_persons
on requests.juridical_person_version_id = juridical_persons.id

 left JOIN (
 	 select 
 	     document_packets_requests.id,
         documents_packets.status
     from erp_documents_packets.document_packets_requests
     inner join erp_documents_packets.documents_packets
     ON document_packets_requests.documents_packet_id = documents_packets.id 
     AND document_packets_requests.documents_packet_version = documents_packets.version
     and documents_packets.is_actual
 ) as documents_packets
 ON requests.id = documents_packets.id AND requests.is_actual and documents_packets.status in ('formed', 'sent', 'received', 'approved', 'payed', 'problem')

left join (select bills.status,

			unnest(bills.request_ids) as request_id 

			from erp_bills.bills 
			
			where bills.is_actual and bills.type = 'Charterer') as bills

on requests.id = bills.request_id

where requests.is_actual = true 

AND requests.status not in ('cancelled', 'transportationCancelled', 'active', 'paid')

and last_unloading.from_at >= $1 and last_unloading.from_at <= $2

and requests.charterer_rate_amount != 0

and requests.charterer_rate_amount != 1

) as old_flow


left join (select bills.status, unnest(bills.request_ids) as request_id 

			from document_flow.bills 
			
			where bills.is_actual and bills.status in ('Formed', 'Sent', 'Delivered', 'Approved', 'Paid', 'Problem')) as new_flow

on old_flow.id = new_flow.request_id

where old_flow.bills_status != 'Оплачено'

order by old_flow.from_at", params = list(startDate, endDate))
  

}


# ПДЗ - экспорт в excel
getDZ_confirmed_receivables_detail <- function(){
  
  my_query <- query('{
"bool": {
      "must": [
        {
  "match": {"isActual": "true"}
        }
      ]
}
}')
  
  selected_field_user <- select_fields('{
  "includes": ["data.id", "data.title"]
}')
  
  user = elastic(cfg$db_elastic$url,
                 "user-profile-version-index") %search% (my_query + selected_field_user) %>%
    group_by(data.id) %>%
    summarise(data.title = first(data.title))

  requests_all <- dbGetQuery(con,
                             "with 
bills as (
        select bills.id, bills.created_at_original,
            
        bills.created_at, bills.number_doc,
        
        (case
              when bills.status = 'PartialPaid' then bills.invoice_amount - bills.paid_amount
              else bills.invoice_amount
         end) as invoice_amount, 
        
        unnest(bills.request_ids) as request_id 
            
        from erp_bills.bills 
            
            where bills.is_actual and bills.type = 'Charterer' and bills.status != 'Paid' and bills.number_doc is not null and bills.invoice_amount != 0
            
            ),
                    
            
juridical_persons as (
        select juridical_persons.id,
        
        juridical_persons.short_title,
        
        juridicals.grace_of_payment_grace_period_days,
        
        (case
        
            when juridicals.grace_of_payment_hidden_grace_period_days is null then 0
            else juridicals.grace_of_payment_hidden_grace_period_days
            
        end) as grace_of_payment_hidden_grace_period_days,
        
        juridicals.tsd_settings_doc_verification_days,
        
        clients.signed_by_uid,
        
        juridicals.responsible_employee_processing_accounts_id
        
    from erp_juridical_persons.juridical_persons
    
    left join erp_clients.juridicals on juridical_persons.id = juridicals.juridical_person_id and juridical_persons.version = juridicals.juridical_person_version
    
    left join erp_clients.clients on juridicals.client_id = clients.id and juridicals.client_version = clients.version
    
    where juridical_persons.is_actual and clients.is_actual=true
    
    ),
    
    
documents_packets as (
     select distinct on (document_packets_requests.id)
     
         document_packets_requests.id,
                  
         documents_packets.status,
         
        documents_packets.planned_send_date,
        
		documents_packets.send_date,
		
		documents_packets.planned_delivery_date,
		
		documents_packets.delivery_date,
		
		documents_packets.planned_verification_date,
		
		documents_packets.verification_date,
		
		documents_packets.returned_date
         
     from erp_documents_packets.document_packets_requests
     
     inner join erp_documents_packets.documents_packets
     
     ON document_packets_requests.documents_packet_id = documents_packets.id
     
     AND document_packets_requests.documents_packet_version = documents_packets.version
     
     and documents_packets.is_actual
     
     order by document_packets_requests.id, documents_packets.created_at desc
),

requests as (
            
            select  requests.id,
            
            juridical_persons.short_title,
        
            juridical_persons.grace_of_payment_grace_period_days,
            
            juridical_persons.grace_of_payment_hidden_grace_period_days,
            
            juridical_persons.tsd_settings_doc_verification_days,
            
            juridical_persons.signed_by_uid,
            
            juridical_persons.responsible_employee_processing_accounts_id,
            
            documents_packets.status,
        
            documents_packets.planned_send_date,
            
            documents_packets.send_date,
		
            documents_packets.planned_delivery_date,
		
            documents_packets.delivery_date,
		
            documents_packets.planned_verification_date,
		
            documents_packets.verification_date,
		
            documents_packets.returned_date
            
            from erp_requests.requests
                
            left join juridical_persons
            on requests.juridical_person_version_id = juridical_persons.id
            
            left join documents_packets
            on requests.id = documents_packets.id AND requests.is_actual 
            
            where requests.is_actual = true 
            
),

old_flow as (
        select distinct on (bills.number_doc)
        
        requests.short_title,
        
        requests.grace_of_payment_grace_period_days,
        
        requests.grace_of_payment_hidden_grace_period_days,
        
        requests.tsd_settings_doc_verification_days,
        
        requests.signed_by_uid,
            
        requests.responsible_employee_processing_accounts_id,
        
        bills.id,
        
        bills.created_at_original, 
        
        bills.created_at, 
        
        bills.number_doc,
        
        bills.invoice_amount,
                
        requests.status,
        
        requests.planned_send_date,
        
		requests.send_date,
		
		requests.planned_delivery_date,
		
		requests.delivery_date,
		
		requests.planned_verification_date,
		
		requests.verification_date,
		
		requests.returned_date,
        
        (requests.delivery_date AT TIME ZONE 'Europe/Moscow' + (interval '1' day * requests.grace_of_payment_grace_period_days) + (interval '1' day * requests.grace_of_payment_hidden_grace_period_days))::date as planned_payment_date
        
        from bills
        
        left join requests
        
        on bills.request_id = requests.id
        
        order by bills.number_doc, bills.created_at_original desc, request_id
        
),
new_flow as ( 
        select id, 
        
        planned_send_date, 
        
        send_date, 
        
        planned_delivery_date, 
        
        delivery_date, 
        
        planned_verification_date,
        
        verification_date,
        
        return_date,
               
        status, 
        
        planned_payment_date 
        
        from document_flow.bills
        
        where bills.is_actual = true 
),
result as (
       select old_flow.short_title as Контрагент,
       
       old_flow.number_doc as \"Номер счёта\",
       
       old_flow.invoice_amount as Сумма,
       
       (old_flow.created_at_original AT TIME ZONE 'Europe/Moscow')::date as \"Дата счёта\",
       (old_flow.created_at AT TIME ZONE 'Europe/Moscow')::date as \"Дата выставления счёта\",
	   
	   (COALESCE(old_flow.planned_send_date, new_flow.planned_send_date) AT TIME ZONE 'Europe/Moscow')::date as \"Плановая дата отправки\",
	   (COALESCE(old_flow.send_date, new_flow.send_date) AT TIME ZONE 'Europe/Moscow')::date as \"Дата отправки\",
       
	   (COALESCE(old_flow.planned_delivery_date, new_flow.planned_delivery_date) AT TIME ZONE 'Europe/Moscow')::date as \"Плановая дата доставки\",
       (COALESCE(old_flow.delivery_date, new_flow.delivery_date) AT TIME ZONE 'Europe/Moscow')::date as \"Дата доставки\",
       
       (COALESCE(old_flow.planned_verification_date, new_flow.planned_verification_date) AT TIME ZONE 'Europe/Moscow')::date as \"Плановая дата верификации\",
	   (COALESCE(old_flow.verification_date, new_flow.verification_date) AT TIME ZONE 'Europe/Moscow')::date as \"Дата верификации\",
       
	   (COALESCE(old_flow.returned_date, new_flow.return_date) AT TIME ZONE 'Europe/Moscow')::date as \"Дата возврата\",
	   
        old_flow.grace_of_payment_grace_period_days as \"Отсрочка платежа\",
        old_flow.grace_of_payment_hidden_grace_period_days as \"Скрытая отсрочка\",

        old_flow.tsd_settings_doc_verification_days as \"Сроки верификации\",

                        
    COALESCE(old_flow.planned_payment_date, new_flow.planned_payment_date, (new_flow.delivery_date AT TIME ZONE 'Europe/Moscow' + (interval '1' day * old_flow.grace_of_payment_grace_period_days))::date) as \"Плановая дата оплаты\",
        
    COALESCE(old_flow.planned_payment_date, new_flow.planned_payment_date, (new_flow.delivery_date AT TIME ZONE 'Europe/Moscow' + (interval '1' day * old_flow.grace_of_payment_grace_period_days))::date) - now()::date as \"Отклонение срока оплаты\",
    
    (case
        
            when (COALESCE(old_flow.planned_payment_date, new_flow.planned_payment_date, (new_flow.delivery_date AT TIME ZONE 'Europe/Moscow' + (interval '1' day * old_flow.grace_of_payment_grace_period_days))::date) - now()::date) >= 0 then 'ПДЗ'
            when COALESCE(old_flow.delivery_date, new_flow.delivery_date) is null then 'НДЗ'
            when (COALESCE(old_flow.planned_payment_date, new_flow.planned_payment_date, (new_flow.delivery_date AT TIME ZONE 'Europe/Moscow' + (interval '1' day * old_flow.grace_of_payment_grace_period_days))::date) - now()::date) < 0 then 'ПрДЗ'
            
        end) as Категория,
        
        (case
	   	when COALESCE(old_flow.status, new_flow.status) = 'Formed' or COALESCE(old_flow.status, new_flow.status) = 'formed' then 'Сформированные'
	    when COALESCE(old_flow.status, new_flow.status) = 'Sent' or COALESCE(old_flow.status, new_flow.status) = 'sent' then 'Отправленные'
	    when COALESCE(old_flow.status, new_flow.status) = 'Delivered' or COALESCE(old_flow.status, new_flow.status) = 'received' then 'Доставленные'
	    when COALESCE(old_flow.status, new_flow.status) = 'Approved' or COALESCE(old_flow.status, new_flow.status) = 'approved' then 'Подтверждённые'
	    when COALESCE(old_flow.status, new_flow.status) = 'Problem' or COALESCE(old_flow.status, new_flow.status) = 'problem' then 'Проблемные'

	   	end) as Статус,
	   	
	   	old_flow.signed_by_uid as Менеджер,
	   	
	    old_flow.responsible_employee_processing_accounts_id as Дебитор
        
        from old_flow
        
        left join new_flow
        
        on old_flow.id = new_flow.id
        
        order by old_flow.created_at_original
        
)
select * from result;") %>%
  
    left_join(user, by = c("Дебитор" = "data.id")) %>%
    
    mutate("Дебитор" = data.title) %>% select(-data.title) %>%
    
    left_join(user, by = c("Менеджер" = "data.id")) %>%
    
    mutate("Менеджер" = data.title) %>% select(-data.title) 

}

get_unformed_bills <- function(){
  
  endDate <- as.POSIXct(paste(Sys.Date(), "23:59:59"), format="%Y-%m-%d %H:%M:%S") 
  
  attr(endDate, "tzone") <- "UTC" 
  
  unformed_bills <- DBI::dbGetQuery(con,
                               "select Контрагент, 
  	   Месяц,
  	   sum(charterer_rate_amount) as Сумма,
  	   (case
  	      when Статус = 'В ожидании' then 'Ожидает ТСД'
  	      else 'Получили ТСД'
  	   end) as Статус

from (select old_flow.short_title as Контрагент,

old_flow.Месяц,

old_flow.charterer_rate_amount,

	   	(case
	   	when COALESCE(old_flow.status, new_flow.status) = 'Formed' or COALESCE(old_flow.status, new_flow.status) = 'formed' then 'Сформированные'
	    when COALESCE(old_flow.status, new_flow.status) = 'Sent' or COALESCE(old_flow.status, new_flow.status) = 'sent' then 'Отправленные'
	    when COALESCE(old_flow.status, new_flow.status) is null and  old_flow.transportations_status in ('ready', 'onStartPoint', 'loaded', 'onTheWay', 'onFinishPoint', 'unloaded') then 'В ожидании'
	    when COALESCE(old_flow.status, new_flow.status) is null and old_flow.transportations_status = 'docsReceived' then 'Полученные'
	    when COALESCE(old_flow.status, new_flow.status) is null and (old_flow.transportations_status = 'docsApproved' or old_flow.transportations_status = 'paid') then 'Проверенные'
	    when COALESCE(old_flow.status, new_flow.status) is null and old_flow.transportations_status = 'docsProblem' then 'Проблемы'
	    when COALESCE(old_flow.status, new_flow.status) = 'Delivered' or COALESCE(old_flow.status, new_flow.status) = 'received' then 'Доставленные'
	    when COALESCE(old_flow.status, new_flow.status) = 'Approved' or COALESCE(old_flow.status, new_flow.status) = 'approved' then 'Подтверждённые'
	    when COALESCE(old_flow.status, new_flow.status) = 'Paid' or COALESCE(old_flow.status, new_flow.status) = 'payed' then 'Оплаченные'
	    when COALESCE(old_flow.status, new_flow.status) = 'Problem' or COALESCE(old_flow.status, new_flow.status) = 'problem' then 'Проблемы (кл.)'
	    else 'Статус неизвестен'
	   	end) as Статус 	

from (select 
		
		requests.id,
        juridical_persons.short_title,
        requests.charterer_rate_amount,
		documents_packets.status,
		transportations.status as transportations_status,
		bills.status as bills_status,
		date_trunc('month', last_unloading.from_at AT TIME ZONE 'Europe/Moscow') as Месяц

FROM erp_requests.requests

INNER JOIN erp_requests.transportations
ON requests.id = transportations.request_id
and transportations.status in ('ready', 'onStartPoint', 'loaded', 'onTheWay', 'onFinishPoint', 'unloaded', 'docsReceived', 'docsApproved', 'paid', 'docsProblem')

 left join (
    select
        juridical_persons.id,
        juridical_persons.short_title
        from erp_juridical_persons.juridical_persons
    left join erp_clients.juridicals on juridical_persons.id = juridicals.juridical_person_id and juridical_persons.version = juridicals.juridical_person_version
    left join erp_clients.clients on juridicals.client_id = clients.id and juridicals.client_version = clients.version
    where juridical_persons.is_actual and clients.is_actual=true
) as juridical_persons
on requests.juridical_person_version_id = juridical_persons.id

 left JOIN (
 	 select 
 	     document_packets_requests.id,
         documents_packets.status
     from erp_documents_packets.document_packets_requests
     inner join erp_documents_packets.documents_packets
     ON document_packets_requests.documents_packet_id = documents_packets.id 
     AND document_packets_requests.documents_packet_version = documents_packets.version
     and documents_packets.is_actual
 ) as documents_packets
 ON requests.id = documents_packets.id AND requests.is_actual and documents_packets.status in ('formed', 'sent', 'received', 'approved', 'payed', 'problem')

left join (select bills.status,

			unnest(bills.request_ids) as request_id 

			from erp_bills.bills 
			
			where bills.is_actual and bills.type = 'Charterer') as bills

on requests.id = bills.request_id

left join (
  select distinct on (s.request_id)
  request_id, id, locality, address, \"from\" as from_at, fact_arrival_date_time, region, consignee, fact_departure_date_time
  from erp_requests.request_finish_route_points s
  order by s.request_id desc, s.from desc
) as last_unloading
on requests.id = last_unloading.request_id

where requests.is_actual = true 

AND requests.status not in ('cancelled', 'transportationCancelled', 'active', 'paid')

and last_unloading.from_at <= $1

and requests.charterer_rate_amount != 0

and requests.charterer_rate_amount != 1

) as old_flow


left join (select bills.status, unnest(bills.request_ids) as request_id 

			from document_flow.bills 
			
			where bills.is_actual and bills.status in ('Formed', 'Sent', 'Delivered', 'Approved', 'Paid', 'Problem')) as new_flow

on old_flow.id = new_flow.request_id

where old_flow.bills_status != 'Paid' or old_flow.bills_status is null) as final

where final.Статус in ('В ожидании', 'Полученные', 'Проверенные', 'Проблемы')

group by Контрагент, Месяц, Статус ORDER BY Месяц desc;", params = list(endDate)) %>%
    
    group_by(Контрагент, Месяц, Статус) %>%
    
    summarise(Сумма = sum(Сумма))
  
  # Current and previous calendar months, including December across a January rollover.
  current_month <- as.Date(format(Sys.Date(), "%Y-%m-01"))
  month_columns <- format(seq(current_month, by = "-1 month", length.out = 2L), "%Y.%m")
  current_year <- format(current_month, "%Y")
  period_columns <- c(month_columns, "Текущий год", "Ранее")

  result <- unformed_bills %>%
    mutate(Год = format(Месяц, "%Y"),
           Месяц = format(Месяц, "%Y.%m"),
           Период = case_when(
             Месяц %in% month_columns ~ Месяц,
             Год == current_year ~ "Текущий год",
             TRUE ~ "Ранее"
           )) %>%
    group_by(Контрагент, Статус, Период) %>%
    summarise(Сумма = sum(Сумма), .groups = "drop") %>%
    pivot_wider(names_from = Период, values_from = Сумма, values_fill = 0)

  # Keep both month columns even if one month or document status has no rows.
  for (column in period_columns) {
    if (!column %in% names(result)) result[[column]] <- rep(0, nrow(result))
    result[[column]][is.na(result[[column]])] <- 0
  }
  result <- result %>% select(Контрагент, Статус, all_of(period_columns))
  result$Всего <- rowSums(result[period_columns])

  return(result)
}


get_funnel_NDZ <- function(){
  
  endDate <- as.POSIXct(paste(Sys.Date(), "23:59:59"), format="%Y-%m-%d %H:%M:%S") 
  
  attr(endDate, "tzone") <- "UTC" 
  
  unconfirmed_receivables <- DBI::dbGetQuery(con,
                                         "with
last_unloading as (
    select distinct on (s.request_id)
    request_id, id, locality, address, \"from\" as from_at, fact_arrival_date_time, region, consignee, fact_departure_date_time
    from erp_requests.request_finish_route_points s
    order by s.request_id desc, s.from desc
),
bills as (
    select bills.status,
        bills.created_at,
        unnest(bills.request_ids) as request_id
    from erp_bills.bills
    where bills.is_actual and bills.type = 'Charterer'
),
documents_packets as (
     select
         document_packets_requests.id,
         documents_packets.status,
         documents_packets.send_date,
         documents_packets.delivery_date,
         documents_packets.verification_date,
         documents_packets.returned_date

     from erp_documents_packets.document_packets_requests
     inner join erp_documents_packets.documents_packets
     ON document_packets_requests.documents_packet_id = documents_packets.id
     AND document_packets_requests.documents_packet_version = documents_packets.version
     and documents_packets.is_actual
),
juridical_persons as (
    select
        juridical_persons.id,
        juridical_persons.short_title
        from erp_juridical_persons.juridical_persons
    left join erp_clients.juridicals on juridical_persons.id = juridicals.juridical_person_id and juridical_persons.version = juridicals.juridical_person_version
    left join erp_clients.clients on juridicals.client_id = clients.id and juridicals.client_version = clients.version
    where juridical_persons.is_actual and clients.is_actual=true
),
old_flow as (
    select
        requests.id,
        juridical_persons.short_title,
        requests.charterer_rate_amount,
        documents_packets.status,
        transportations.status as transportations_status,
        bills.status as bills_status,
        last_unloading.from_at,
        ((jsonb_path_query_array(transportations.statuses_history, '$[*] ? (@.Status == \"docsReceived\")')->-1->>'IssueDateTime')::timestamp)::date as docsReceived,
        ((jsonb_path_query_array(transportations.statuses_history, '$[*] ? (@.Status == \"docsApproved\")')->-1->>'IssueDateTime')::timestamp )::date as docsApproved,
        ((jsonb_path_query_array(transportations.statuses_history, '$[*] ? (@.Status == \"docsProblem\")')->-1->>'IssueDateTime')::timestamp )::date as docsProblem,
        bills.created_at,
        documents_packets.send_date,
        documents_packets.delivery_date,
        documents_packets.verification_date,
        documents_packets.returned_date

    from erp_requests.requests
    inner join erp_requests.transportations on requests.id = transportations.request_id and transportations.status in ('ready', 'onStartPoint', 'loaded', 'onTheWay', 'onFinishPoint', 'unloaded', 'docsReceived', 'docsApproved', 'paid', 'docsProblem')
    left join juridical_persons on requests.juridical_person_version_id = juridical_persons.id
    left join documents_packets on requests.id = documents_packets.id and requests.is_actual and documents_packets.status in ('formed', 'sent', 'received', 'approved', 'payed', 'problem')
    left join bills on requests.id = bills.request_id
    left join last_unloading on requests.id = last_unloading.request_id
    where
        requests.is_actual = true and
        requests.status not in ('cancelled', 'transportationCancelled', 'active', 'paid') and
        last_unloading.from_at <= $1 and
        requests.charterer_rate_amount != 0 and
        requests.charterer_rate_amount != 1  
),
new_flow as (
    select bills.status, bills.send_date, bills.delivery_date, bills.verification_date, bills.return_date, unnest(bills.request_ids) as request_id
    from document_flow.bills
        
    where bills.is_actual and bills.status in ('Formed', 'Sent', 'Delivered', 'Approved', 'Paid', 'Problem')
),
flow_statuses as (
    select
        old_flow.short_title as Контрагент,
        from_at,
        docsReceived,
        docsApproved,
        docsProblem,
        created_at,
        (COALESCE(old_flow.send_date, new_flow.send_date))::date as send_date,
        (COALESCE(old_flow.delivery_date, new_flow.delivery_date))::date as delivery_date,
        (COALESCE(old_flow.verification_date, new_flow.verification_date))::date as verification_date,
        (COALESCE(old_flow.returned_date, new_flow.return_date))::date as return_date,

        (
            case
                when COALESCE(old_flow.status, new_flow.status) is null and  old_flow.transportations_status in ('ready', 'onStartPoint', 'loaded', 'onTheWay', 'onFinishPoint', 'unloaded') then 'Ожидают ТСД'
                when COALESCE(old_flow.status, new_flow.status) is null and old_flow.transportations_status = 'docsReceived' then 'Ожидают проверки ТСД'
                when COALESCE(old_flow.status, new_flow.status) is null and (old_flow.transportations_status = 'docsApproved' or old_flow.transportations_status = 'paid') then 'Ожидают формирования счёта'
                when COALESCE(old_flow.status, new_flow.status) is null and old_flow.transportations_status = 'docsProblem' then 'Проблемные (пер.)'
                when COALESCE(old_flow.status, new_flow.status) = 'Formed' or COALESCE(old_flow.status, new_flow.status) = 'formed' then 'Ожидают отправки счёта'
                when COALESCE(old_flow.status, new_flow.status) = 'Sent' or COALESCE(old_flow.status, new_flow.status) = 'sent' then 'Ожидают доставки счёта'
                when COALESCE(old_flow.status, new_flow.status) = 'Delivered' or COALESCE(old_flow.status, new_flow.status) = 'received' then 'Ожидают верификации клиентом'
	              when COALESCE(old_flow.status, new_flow.status) = 'Approved' or COALESCE(old_flow.status, new_flow.status) = 'approved' then 'Ожидают оплаты'
	              when COALESCE(old_flow.status, new_flow.status) = 'Paid' or COALESCE(old_flow.status, new_flow.status) = 'payed' then 'Оплаченные'
	              when COALESCE(old_flow.status, new_flow.status) = 'Problem' or COALESCE(old_flow.status, new_flow.status) = 'problem' then 'Проблемные (кл.)'
                else 'Статус неизвестен'
            end
        ) as Статус,
        old_flow.id,
        old_flow.charterer_rate_amount
    from old_flow
    left join new_flow on old_flow.id = new_flow.request_id
    where old_flow.bills_status != 'Paid' or old_flow.bills_status is null
),
flow_sums as (
    select Статус, sum(charterer_rate_amount) as cs_sum from flow_statuses
    group by Статус
),

general_sums as (
    select sum(charterer_rate_amount) as cs_sum from flow_statuses
    where flow_statuses.Статус not in ('Оплаченные')
),

final as (
    select
        flow_statuses.Статус,
		sum(flow_statuses.charterer_rate_amount) as Сумма,
		sum(flow_statuses.charterer_rate_amount / general_sums.cs_sum) AS Доля,
		count(*) as Рейсы,
		(
            case
                when flow_statuses.Статус = 'Ожидают ТСД' then sum((now()::date - flow_statuses.from_at::date)*(flow_statuses.charterer_rate_amount/flow_sums.cs_sum)) filter (where flow_sums.Статус = 'Ожидают ТСД')
                when flow_statuses.Статус = 'Ожидают проверки ТСД' then sum((now()::date - flow_statuses.docsReceived)*(flow_statuses.charterer_rate_amount/flow_sums.cs_sum)) filter (where flow_sums.Статус = 'Ожидают проверки ТСД')
                when flow_statuses.Статус = 'Ожидают формирования счёта' then sum((now()::date - flow_statuses.docsApproved)*(flow_statuses.charterer_rate_amount/flow_sums.cs_sum)) filter (where flow_sums.Статус = 'Ожидают формирования счёта')
                when flow_statuses.Статус = 'Проблемные (пер.)' then sum((now()::date - flow_statuses.docsProblem)*(flow_statuses.charterer_rate_amount/flow_sums.cs_sum)) filter (where flow_sums.Статус = 'Проблемные (пер.)')
                when flow_statuses.Статус = 'Ожидают отправки счёта' then sum((now()::date - flow_statuses.created_at::date)*(flow_statuses.charterer_rate_amount/flow_sums.cs_sum)) filter (where flow_sums.Статус = 'Ожидают отправки счёта')
                when flow_statuses.Статус = 'Ожидают доставки счёта' then sum((now()::date - flow_statuses.send_date)*(flow_statuses.charterer_rate_amount/flow_sums.cs_sum)) filter (where flow_sums.Статус = 'Ожидают доставки счёта')
                when flow_statuses.Статус = 'Ожидают верификации клиентом' then sum((now()::date - flow_statuses.delivery_date)*(flow_statuses.charterer_rate_amount/flow_sums.cs_sum)) filter (where flow_sums.Статус = 'Ожидают верификации клиентом')
                when flow_statuses.Статус = 'Проблемные (кл.)' then sum((now()::date - flow_statuses.return_date)*(flow_statuses.charterer_rate_amount/flow_sums.cs_sum)) filter (where flow_sums.Статус = 'Проблемные (кл.)')
                when flow_statuses.Статус = 'Ожидают оплаты' then sum((now()::date - flow_statuses.verification_date)*(flow_statuses.charterer_rate_amount/flow_sums.cs_sum)) filter (where flow_sums.Статус = 'Ожидают оплаты')
            end
        ) as Дни

    from flow_statuses
    left join flow_sums on flow_statuses.Статус=flow_sums.Статус
    left join general_sums ON True
    where flow_statuses.Статус not in ('Оплаченные')
    group by flow_statuses.Статус
)
select * from final

    ORDER by case WHEN Статус = 'Ожидают ТСД' THEN 0
    WHEN Статус = 'Ожидают проверки ТСД' THEN 1
    WHEN Статус = 'Ожидают формирования счёта' THEN 2
    WHEN Статус = 'Проблемные (пер.)' THEN 3
    WHEN Статус = 'Ожидают отправки счёта' THEN 4
    WHEN Статус = 'Ожидают доставки счёта' THEN 5
    WHEN Статус = 'Ожидают верификации клиентом' THEN 6
    WHEN Статус = 'Проблемные (кл.)' THEN 7
    WHEN Статус = 'Ожидают оплаты' THEN 8
    END asc;", params = list(endDate))

    result <- data.frame(
      Статус = c('Ожидают ТСД', 'Ожидают проверки ТСД', 'Ожидают формирования счёта', 'Проблемные (пер.)',
                 'Ожидают отправки счёта', 'Ожидают доставки счёта', 'Ожидают верификации клиентом',
                 'Проблемные (кл.)', 'Ожидают оплаты')
    ) %>%
      left_join(unconfirmed_receivables, by = c('Статус' = 'Статус'))

    result[is.na(result)] <- 0

    goals <- data.frame(Цели = c(10, 1, 14, 20, 1, 2, 5, 20, 30))

    cbind(result, goals)

}

get_payment_rate <- function(){
  
  requests_all <- dbGetQuery(con, 
                             "with

last_unloading as (
    select distinct on (s.request_id)
    request_id, id, locality, address, \"from\" as from_at, fact_arrival_date_time, region, consignee, fact_departure_date_time
    from erp_requests.request_finish_route_points s
    order by s.request_id desc, s.from desc
),

bills as (
    select bills.payment_date,

			bills.created_at_original,

			unnest(bills.request_ids) as request_id 

			from erp_bills.bills 
			
			where bills.is_actual and bills.type = 'Charterer' and bills.status = 'Paid'
),

requests as (
	select requests.juridical_person_version_short_title as Контрагент,
	
	requests.charterer_rate_amount,
	
	last_unloading.from_at::date,
	
	bills.created_at_original::date,
	
	bills.payment_date::date 
	
	from erp_requests.requests 
	
	left join last_unloading
	
	on requests.id = last_unloading.request_id
	
	right join bills
	
	on requests.id = bills.request_id
	
	where requests.is_actual = true

and requests.status not in ('cancelled', 'transportationCancelled', 'active')

),

flow_sums as (
    select Контрагент, sum(charterer_rate_amount) as cs_sum from requests
    group by Контрагент
),

final as (
    select
        requests.Контрагент,
        
        sum((requests.payment_date - requests.created_at_original)*(requests.charterer_rate_amount/flow_sums.cs_sum)) as \"Оплата счёта, дни\",
        
        sum((requests.payment_date - requests.from_at)*(requests.charterer_rate_amount/flow_sums.cs_sum)) as \"Оплата заявки, дни\"

    from requests
    left join flow_sums on requests.Контрагент=flow_sums.Контрагент
    group by requests.Контрагент
)


select * from final;")
  
}

get_payment_rate <- function(){
  
  requests_all <- dbGetQuery(con, 
                             "with

last_unloading as (
    select distinct on (s.request_id)
    request_id, id, locality, address, \"from\" as from_at, fact_arrival_date_time, region, consignee, fact_departure_date_time
    from erp_requests.request_finish_route_points s
    order by s.request_id desc, s.from desc
),

bills as (
    select bills.payment_date,

			unnest(bills.request_ids) as request_id 

			from erp_bills.bills 
			
			where bills.is_actual and bills.type = 'Charterer' and bills.status = 'Paid'
),

requests as (
	select avg(bills.payment_date::date - last_unloading.from_at::date) as \"Скорость оплаты от даты выгрузки\"
	
	from erp_requests.requests 
	
	left join last_unloading
	
	on requests.id = last_unloading.request_id
	
	right join bills
	
	on requests.id = bills.request_id
	
	where requests.is_actual = true

and requests.status not in ('cancelled', 'transportationCancelled', 'active')

)

select * from requests") %>%
  
  mutate(Цели = 53)

}

get_healthDZ <- function(){
  
  pdz <- dbGetQuery(con,
                    "with
bills as (
        select bills.id,

            bills.created_at_original,

            bills.number_doc,

            (case
              when bills.status = 'PartialPaid' then bills.invoice_amount - bills.paid_amount
              else bills.invoice_amount
            end) as invoice_amount,
            
            unnest(bills.request_ids) as request_id
            from erp_bills.bills

            where bills.is_actual and bills.type = 'Charterer' and bills.status != 'Paid' and bills.number_doc is not null and bills.invoice_amount != 0

            ),


juridical_persons as (
        select juridical_persons.id,

        juridical_persons.short_title,

        juridicals.grace_of_payment_grace_period_days,

        (case

            when juridicals.grace_of_payment_hidden_grace_period_days is null then 0
            else juridicals.grace_of_payment_hidden_grace_period_days

        end) as grace_of_payment_hidden_grace_period_days

    from erp_juridical_persons.juridical_persons

    left join erp_clients.juridicals on juridical_persons.id = juridicals.juridical_person_id and juridical_persons.version = juridicals.juridical_person_version

    left join erp_clients.clients on juridicals.client_id = clients.id and juridicals.client_version = clients.version

    where juridical_persons.is_actual and clients.is_actual=true

    ),


documents_packets as (
     select distinct on (document_packets_requests.id)

         document_packets_requests.id,

         documents_packets.delivery_date,

         documents_packets.status

     from erp_documents_packets.document_packets_requests

     inner join erp_documents_packets.documents_packets

     ON document_packets_requests.documents_packet_id = documents_packets.id

     AND document_packets_requests.documents_packet_version = documents_packets.version

     and documents_packets.is_actual

     order by document_packets_requests.id, documents_packets.created_at desc
),

requests as (

            select  requests.id,

            juridical_persons.short_title,

            juridical_persons.grace_of_payment_grace_period_days,

            juridical_persons.grace_of_payment_hidden_grace_period_days,

            documents_packets.status,

            documents_packets.delivery_date

            from erp_requests.requests

            left join juridical_persons
            on requests.juridical_person_version_id = juridical_persons.id

            left join documents_packets
            on requests.id = documents_packets.id AND requests.is_actual

            where requests.is_actual = true

),

old_flow as (
        select distinct on (bills.number_doc)

        requests.short_title,

        requests.grace_of_payment_grace_period_days,

        requests.grace_of_payment_hidden_grace_period_days,

        bills.id,

        bills.number_doc,

        bills.invoice_amount,

        requests.status,

        requests.delivery_date,

        (requests.delivery_date AT TIME ZONE 'Europe/Moscow' + (interval '1' day * requests.grace_of_payment_grace_period_days) + (interval '1' day * requests.grace_of_payment_hidden_grace_period_days))::date as planned_payment_date

        from bills

        left join requests

        on bills.request_id = requests.id

        order by bills.number_doc, bills.created_at_original desc, request_id

),
new_flow as (
        select id, delivery_date, status, planned_payment_date

        from document_flow.bills

        where bills.is_actual = true
),
result as (
       select old_flow.short_title as Контрагент,
       sum(old_flow.invoice_amount) as Сумма,

    (case

            when (COALESCE(old_flow.planned_payment_date, new_flow.planned_payment_date, (new_flow.delivery_date AT TIME ZONE 'Europe/Moscow' + (interval '1' day * old_flow.grace_of_payment_grace_period_days))::date) - now()::date) >= 0 then 'ПДЗ'
            when COALESCE(old_flow.delivery_date, new_flow.delivery_date) is null then 'НДЗ'
            when (COALESCE(old_flow.planned_payment_date, new_flow.planned_payment_date, (new_flow.delivery_date AT TIME ZONE 'Europe/Moscow' + (interval '1' day * old_flow.grace_of_payment_grace_period_days))::date) - now()::date) < 0 
            and (COALESCE(old_flow.planned_payment_date, new_flow.planned_payment_date, (new_flow.delivery_date AT TIME ZONE 'Europe/Moscow' + (interval '1' day * old_flow.grace_of_payment_grace_period_days))::date) - now()::date) >= -7 then 'До 7 дней'
            when (COALESCE(old_flow.planned_payment_date, new_flow.planned_payment_date, (new_flow.delivery_date AT TIME ZONE 'Europe/Moscow' + (interval '1' day * old_flow.grace_of_payment_grace_period_days))::date) - now()::date) < -7 
            and (COALESCE(old_flow.planned_payment_date, new_flow.planned_payment_date, (new_flow.delivery_date AT TIME ZONE 'Europe/Moscow' + (interval '1' day * old_flow.grace_of_payment_grace_period_days))::date) - now()::date) >= -15 then 'От 7 до 15 дней'
            when (COALESCE(old_flow.planned_payment_date, new_flow.planned_payment_date, (new_flow.delivery_date AT TIME ZONE 'Europe/Moscow' + (interval '1' day * old_flow.grace_of_payment_grace_period_days))::date) - now()::date) < -15 
            and (COALESCE(old_flow.planned_payment_date, new_flow.planned_payment_date, (new_flow.delivery_date AT TIME ZONE 'Europe/Moscow' + (interval '1' day * old_flow.grace_of_payment_grace_period_days))::date) - now()::date) >= -30 then 'От 16 до 30 дней'
            when (COALESCE(old_flow.planned_payment_date, new_flow.planned_payment_date, (new_flow.delivery_date AT TIME ZONE 'Europe/Moscow' + (interval '1' day * old_flow.grace_of_payment_grace_period_days))::date) - now()::date) < -30 then 'Больше 30 дней' 
        end) as Категория

        from old_flow

        left join new_flow

        on old_flow.id = new_flow.id

        where COALESCE(old_flow.status, new_flow.status) in ('received', 'approved') or COALESCE(old_flow.status, new_flow.status) in ('Delivered', 'Approved')

        group by Контрагент, Категория
)
select * from result;")
  
  
#   create.calendar(name="mycal", weekdays=c('saturday', 'sunday'))
#   
#   get_the_exact_date_of_payment <- function(payment_date){
#     first_tuesday_of_the_current_month <- NULL
#     first_tuesday_of_the_next_month <- NULL
#     
#     if (as.POSIXlt(payment_date)$mon + 1 != 12) {
#       
#       x <- seq(ymd(paste0(as.POSIXlt(payment_date)$year + 1900, "-", as.POSIXlt(payment_date)$mon + 1, "-01")), ymd(paste0(as.POSIXlt(payment_date)$year + 1900, "-", as.POSIXlt(payment_date)$mon + 1, "-07")), by="1 day")
#       first_tuesday_of_the_current_month <- x[wday(x) == 3 & day(x) <= 7]
#       
#       y <- seq(ymd(paste0(as.POSIXlt(payment_date)$year + 1900, "-", as.POSIXlt(payment_date)$mon + 2, "-01")), ymd(paste0(as.POSIXlt(payment_date)$year + 1900, "-", as.POSIXlt(payment_date)$mon + 2, "-07")), by="1 day")
#       first_tuesday_of_the_next_month <- y[wday(y) == 3 & day(y) <= 7]
#       
#     } else {
#       
#       x <- seq(ymd(paste0(as.POSIXlt(payment_date)$year + 1900, "-", as.POSIXlt(payment_date)$mon + 1, "-01")), ymd(paste0(as.POSIXlt(payment_date)$year + 1900, "-", as.POSIXlt(payment_date)$mon + 1, "-07")), by="1 day")
#       first_tuesday_of_the_current_month <- x[wday(x) == 3 & day(x) <= 7]
#       
#       y <- seq(ymd(paste0(as.POSIXlt(payment_date)$year + 1900 + 1, "-", as.POSIXlt(payment_date)$mon - 10, "-01")), ymd(paste0(as.POSIXlt(payment_date)$year + 1900 + 1, "-", as.POSIXlt(payment_date)$mon - 10, "-07")), by="1 day")
#       first_tuesday_of_the_next_month <- y[wday(y) == 3 & day(y) <= 7]
#       
#     }
#     
#     if (payment_date <= first_tuesday_of_the_current_month){
#       first_tuesday_of_the_current_month
#     } else {
#       first_tuesday_of_the_next_month
#     }
#   }
# 
#   
#   pdz <- dbGetQuery(con, 
#                     "with 
# bills as (
#         select bills.id,
#         
#             bills.created_at_original, 
#             bills.created_at,
#             bills.number_doc,
#             bills.invoice_amount,
#             unnest(bills.request_ids) as request_id 
#             from erp_bills.bills 
#             
#             where bills.is_actual and bills.type = 'Charterer' and bills.status != 'Paid' and bills.number_doc is not null and bills.invoice_amount != 0
#             
#             ),
#                     
#             
# juridical_persons as (
#         select juridical_persons.id,
#         
#         juridical_persons.short_title,
#         
#         juridicals.grace_of_payment_grace_period_days,
#         
#         juridicals.grace_of_payment_grace_period_type
#         
#     from erp_juridical_persons.juridical_persons
#     
#     left join erp_clients.juridicals on juridical_persons.id = juridicals.juridical_person_id and juridical_persons.version = juridicals.juridical_person_version
#     
#     left join erp_clients.clients on juridicals.client_id = clients.id and juridicals.client_version = clients.version
#     
#     where juridical_persons.is_actual and clients.is_actual=true
#     
#     ),
#     
#     
# documents_packets as (
#      select distinct on (document_packets_requests.id)
#      
#          document_packets_requests.id,
#          
#          documents_packets.delivery_date,
#          
#          documents_packets.status
#          
#      from erp_documents_packets.document_packets_requests
#      
#      inner join erp_documents_packets.documents_packets
#      
#      ON document_packets_requests.documents_packet_id = documents_packets.id
#      
#      AND document_packets_requests.documents_packet_version = documents_packets.version
#      
#      and documents_packets.is_actual
#      
#      order by document_packets_requests.id, documents_packets.created_at desc
# ),
# 
# requests as (
#             
#             select  requests.id,
#             
#             juridical_persons.short_title,
#         
#             juridical_persons.grace_of_payment_grace_period_days,
#             
#             juridical_persons.grace_of_payment_grace_period_type,
#             
#             documents_packets.status,
#         
#             documents_packets.delivery_date
#             
#             from erp_requests.requests
#                 
#             left join juridical_persons
#             on requests.juridical_person_version_id = juridical_persons.id
#             
#             left join documents_packets
#             on requests.id = documents_packets.id AND requests.is_actual 
#             
#             where requests.is_actual = true 
#             
# ),
# 
# old_flow as (
#         select distinct on (bills.number_doc)
#         
#         requests.short_title,
#         
#         requests.grace_of_payment_grace_period_days,
#         
#         requests.grace_of_payment_grace_period_type,
#         
#         bills.id,
#         
#         bills.created_at,
#         
#         bills.invoice_amount,
#                 
#         requests.status,
#         
#         requests.delivery_date
#         
#         from bills
#         
#         left join requests
#         
#         on bills.request_id = requests.id
#         
#         order by bills.number_doc, bills.created_at_original desc, request_id
#         
# ),
# new_flow as ( 
#         select id, delivery_date, status 
#         
#         from document_flow.bills
#         
#         where bills.is_actual = true 
# ),
# result as (
#        select old_flow.short_title as Контрагент,
#        old_flow.invoice_amount as Сумма,
#         
#         (old_flow.created_at AT TIME ZONE 'Europe/Moscow')::date as \"Дата выставления счёта\",
# 
#         (COALESCE(old_flow.delivery_date, new_flow.delivery_date) AT TIME ZONE 'Europe/Moscow')::date as \"Дата доставки\",
#        
#         old_flow.grace_of_payment_grace_period_days as \"Отсрочка платежа\",
#         
#         (case
#             when old_flow.grace_of_payment_grace_period_type = 'WithoutVacations' then 'БД'
#             when old_flow.grace_of_payment_grace_period_type = 'WithVacations' then 'КД'
#             else 'БД'
#         end) as \"Тип отсрочки\"
#         
#         from old_flow
#         
#         left join new_flow
#         
#         on old_flow.id = new_flow.id
#         
#         where COALESCE(old_flow.status, new_flow.status) in ('received', 'approved', 'problem') or COALESCE(old_flow.status, new_flow.status) in ('Delivered', 'Approved', 'Problem')
#         
# )
# select * from result;") %>% 
#   
#   mutate(`Плановая дата оплаты` = case_when(`Тип отсрочки` == 'БД' ~ bizdays::offset(`Дата доставки`, `Отсрочка платежа`, "mycal"), 
#                                          `Тип отсрочки` == 'КД' ~ `Дата доставки` + `Отсрочка платежа`,
#                                          Контрагент == 'Клиент 14' ~ `Дата выставления счёта` + `Отсрочка платежа`,
#                                          Контрагент =='Клиент 03' ~ bizdays::offset(`Дата выставления счёта`, `Отсрочка платежа`, "mycal")
#   )) %>%
#   
#   rowwise() %>%
#   
#   mutate(`Итоговая дата оплаты` = case_when(
#     Контрагент == 'Клиент 15' | Контрагент == 'Клиент 15' | Контрагент == 'Клиент 15' ~ as.Date(paste0(as.POSIXlt(`Плановая дата оплаты`)$year + 1900, "-0", as.POSIXlt(`Плановая дата оплаты`)$mon + 2, "-28"), format = '%Y-%m-%d'),
#     Контрагент == 'Клиент 01' ~ get_the_exact_date_of_payment(`Плановая дата оплаты`),
#     TRUE ~ `Плановая дата оплаты`
#   )) %>%
#   
#   mutate(Категория = case_when(as.numeric(`Итоговая дата оплаты` - Sys.Date()) >= 0 ~ "ПДЗ",
#                                as.numeric(`Итоговая дата оплаты` - Sys.Date()) < 0 ~ "ПрДЗ"
#                                ))
#   
#   pdz1 <- pdz %>%
#     
#     group_by(Контрагент, Категория) %>%
#   
#   summarise(Сумма = sum(Сумма))

  endDate <- as.POSIXct(paste(Sys.Date(), "23:59:59"), format="%Y-%m-%d %H:%M:%S") 
  
  attr(endDate, "tzone") <- "UTC" 
  
  ndz <- DBI::dbGetQuery(con,
  
  "select old_flow.short_title as Контрагент,

	   sum(old_flow.charterer_rate_amount) filter (where COALESCE(old_flow.status, new_flow.status) is null and  old_flow.transportations_status in ('ready', 'onStartPoint', 'loaded', 'onTheWay', 'onFinishPoint', 'unloaded')) as \"В ожидании\",

	   sum(old_flow.charterer_rate_amount) filter (where COALESCE(old_flow.status, new_flow.status) is null and old_flow.transportations_status = 'docsReceived') as Полученные,

	   sum(old_flow.charterer_rate_amount) filter (where COALESCE(old_flow.status, new_flow.status) is null and (old_flow.transportations_status = 'docsApproved' or old_flow.transportations_status = 'paid')) as Проверенные,

	   sum(old_flow.charterer_rate_amount) filter (where COALESCE(old_flow.status, new_flow.status) is null and old_flow.transportations_status = 'docsProblem') as \"Проблемные (пер.)\", 	

	   sum(old_flow.charterer_rate_amount) filter (where COALESCE(old_flow.status, new_flow.status) = 'Formed' or COALESCE(old_flow.status, new_flow.status) = 'formed') as Сформированные,
	   
	   sum(old_flow.charterer_rate_amount) filter (where COALESCE(old_flow.status, new_flow.status) = 'Sent' or COALESCE(old_flow.status, new_flow.status) = 'sent') as Отправленные,
	   
	   sum(old_flow.charterer_rate_amount) filter (where COALESCE(old_flow.status, new_flow.status) = 'Problem' or COALESCE(old_flow.status, new_flow.status) = 'problem') as \"Проблемные (кл.)\"  	


from (select 
		
		requests.id,
        juridical_persons.short_title,
        requests.charterer_rate_amount,
		documents_packets.status,
		transportations.status as transportations_status,
		bills.status as bills_status

FROM erp_requests.requests

INNER JOIN erp_requests.transportations
ON requests.id = transportations.request_id
and transportations.status in ('ready', 'onStartPoint', 'loaded', 'onTheWay', 'onFinishPoint', 'unloaded', 'docsReceived', 'docsApproved', 'paid', 'docsProblem')

 left join (
    select
        juridical_persons.id,
        juridical_persons.short_title
        from erp_juridical_persons.juridical_persons
    left join erp_clients.juridicals on juridical_persons.id = juridicals.juridical_person_id and juridical_persons.version = juridicals.juridical_person_version
    left join erp_clients.clients on juridicals.client_id = clients.id and juridicals.client_version = clients.version
    where juridical_persons.is_actual and clients.is_actual=true
) as juridical_persons
on requests.juridical_person_version_id = juridical_persons.id

 left JOIN (
 	 select 
 	     document_packets_requests.id,
         documents_packets.status
     from erp_documents_packets.document_packets_requests
     inner join erp_documents_packets.documents_packets
     ON document_packets_requests.documents_packet_id = documents_packets.id 
     AND document_packets_requests.documents_packet_version = documents_packets.version
     and documents_packets.is_actual
 ) as documents_packets
 ON requests.id = documents_packets.id AND requests.is_actual and documents_packets.status in ('formed', 'sent', 'received', 'approved', 'payed', 'problem')

left join (select bills.status,

			unnest(bills.request_ids) as request_id 

			from erp_bills.bills 
			
			where bills.is_actual and bills.type = 'Charterer') as bills

on requests.id = bills.request_id

left join (
  select distinct on (s.request_id)
  request_id, id, locality, address, \"from\" as from_at, fact_arrival_date_time, region, consignee, fact_departure_date_time
  from erp_requests.request_finish_route_points s
  order by s.request_id desc, s.from desc
) as last_unloading
on requests.id = last_unloading.request_id

where requests.is_actual = true 

AND requests.status not in ('cancelled', 'transportationCancelled', 'active', 'paid')

and last_unloading.from_at <= $1

and requests.charterer_rate_amount != 0

and requests.charterer_rate_amount != 1

) as old_flow


left join (select bills.status, unnest(bills.request_ids) as request_id 

			from document_flow.bills 
			
			where bills.is_actual and bills.status in ('Formed', 'Sent', 'Delivered', 'Approved', 'Paid', 'Problem')) as new_flow

on old_flow.id = new_flow.request_id

where old_flow.bills_status != 'Paid' or old_flow.bills_status is null

group by Контрагент", params = list(endDate))
  
  ndz[is.na(ndz)] <- 0
  
  ndz <- ndz %>%
    mutate(НДЗ = `В ожидании` + `Полученные` + `Проверенные` + `Проблемные (пер.)` + `Сформированные` + `Отправленные` + `Проблемные (кл.)`) %>%
    filter(НДЗ != 0) %>%
    select(Контрагент, НДЗ)
  
  confirmed_pdz <- pdz %>%
    group_by(Контрагент) %>%
    summarise(ПДЗ = sum(Сумма)) 
  
  overdue_dz <- pdz %>%
    filter(Категория != "ПДЗ" & Категория != "НДЗ") %>%
    group_by(Контрагент) %>%
    summarise(ПрДЗ = sum(Сумма))
  
  result <- ndz %>%
    full_join(confirmed_pdz, by = c("Контрагент" = "Контрагент")) %>%
    full_join(overdue_dz, by = c("Контрагент" = "Контрагент"))
  
    result[is.na(result)] <- 0
    
    health <- result %>%
      mutate(
        "НДЗ/ПДЗ" = НДЗ / ПДЗ,
        "ПрДЗ/ПДЗ" = ПрДЗ / ПДЗ
        )
    
    health[mapply(is.infinite, health)] <- 0
    
# prdz <- dbGetQuery(con,
#                "with
# bills as (
#         select bills.id,
# 
#             bills.created_at_original,
# 
#             bills.number_doc,
#             bills.invoice_amount,
#             unnest(bills.request_ids) as request_id
#             from erp_bills.bills
# 
#             where bills.is_actual and bills.type = 'Charterer' and bills.status != 'Paid' and bills.number_doc is not null and bills.invoice_amount != 0
# 
#             ),
# 
# 
# juridical_persons as (
#         select juridical_persons.id,
# 
#         juridical_persons.short_title,
# 
#         juridicals.grace_of_payment_grace_period_days,
# 
#         (case
# 
#             when juridicals.grace_of_payment_hidden_grace_period_days is null then 0
#             else juridicals.grace_of_payment_hidden_grace_period_days
# 
#         end) as grace_of_payment_hidden_grace_period_days
# 
#     from erp_juridical_persons.juridical_persons
# 
#     left join erp_clients.juridicals on juridical_persons.id = juridicals.juridical_person_id and juridical_persons.version = juridicals.juridical_person_version
# 
#     left join erp_clients.clients on juridicals.client_id = clients.id and juridicals.client_version = clients.version
# 
#     where juridical_persons.is_actual and clients.is_actual=true
# 
#     ),
# 
# 
# documents_packets as (
#      select distinct on (document_packets_requests.id)
# 
#          document_packets_requests.id,
# 
#          documents_packets.delivery_date,
# 
#          documents_packets.status
# 
#      from erp_documents_packets.document_packets_requests
# 
#      inner join erp_documents_packets.documents_packets
# 
#      ON document_packets_requests.documents_packet_id = documents_packets.id
# 
#      AND document_packets_requests.documents_packet_version = documents_packets.version
# 
#      and documents_packets.is_actual
# 
#      order by document_packets_requests.id, documents_packets.created_at desc
# ),
# 
# requests as (
# 
#             select  requests.id,
# 
#             juridical_persons.short_title,
# 
#             juridical_persons.grace_of_payment_grace_period_days,
# 
#             juridical_persons.grace_of_payment_hidden_grace_period_days,
# 
#             documents_packets.status,
# 
#             documents_packets.delivery_date
# 
#             from erp_requests.requests
# 
#             left join juridical_persons
#             on requests.juridical_person_version_id = juridical_persons.id
# 
#             left join documents_packets
#             on requests.id = documents_packets.id AND requests.is_actual
# 
#             where requests.is_actual = true
# 
# ),
# 
# old_flow as (
#         select distinct on (bills.number_doc)
# 
#         requests.short_title,
# 
#         requests.grace_of_payment_grace_period_days,
# 
#         requests.grace_of_payment_hidden_grace_period_days,
# 
#         bills.id,
# 
#         bills.number_doc,
# 
#         bills.invoice_amount,
# 
#         requests.status,
# 
#         requests.delivery_date,
# 
#         (requests.delivery_date AT TIME ZONE 'Europe/Moscow' + (interval '1' day * requests.grace_of_payment_grace_period_days) + (interval '1' day * requests.grace_of_payment_hidden_grace_period_days))::date as planned_payment_date
# 
#         from bills
# 
#         left join requests
# 
#         on bills.request_id = requests.id
# 
#         order by bills.number_doc, bills.created_at_original desc, request_id
# 
# ),
# new_flow as (
#         select id, delivery_date, status, planned_payment_date
# 
#         from document_flow.bills
# 
#         where bills.is_actual = true
# ),
# result as (
#        select old_flow.short_title as Контрагент,
#        sum(old_flow.invoice_amount) as Сумма,
# 
#     (case
# 
#             when (COALESCE(old_flow.planned_payment_date, new_flow.planned_payment_date, (new_flow.delivery_date AT TIME ZONE 'Europe/Moscow' + (interval '1' day * old_flow.grace_of_payment_grace_period_days))::date) - now()::date) >= 0 then 'ПДЗ'
#             when COALESCE(old_flow.delivery_date, new_flow.delivery_date) is null then 'НДЗ'
#             when (COALESCE(old_flow.planned_payment_date, new_flow.planned_payment_date, (new_flow.delivery_date AT TIME ZONE 'Europe/Moscow' + (interval '1' day * old_flow.grace_of_payment_grace_period_days))::date) - now()::date) < 0 
#             and (COALESCE(old_flow.planned_payment_date, new_flow.planned_payment_date, (new_flow.delivery_date AT TIME ZONE 'Europe/Moscow' + (interval '1' day * old_flow.grace_of_payment_grace_period_days))::date) - now()::date) >= -7 then 'До 7 дней'
#             when (COALESCE(old_flow.planned_payment_date, new_flow.planned_payment_date, (new_flow.delivery_date AT TIME ZONE 'Europe/Moscow' + (interval '1' day * old_flow.grace_of_payment_grace_period_days))::date) - now()::date) < -7 
#             and (COALESCE(old_flow.planned_payment_date, new_flow.planned_payment_date, (new_flow.delivery_date AT TIME ZONE 'Europe/Moscow' + (interval '1' day * old_flow.grace_of_payment_grace_period_days))::date) - now()::date) >= -15 then 'От 7 до 15 дней'
#             when (COALESCE(old_flow.planned_payment_date, new_flow.planned_payment_date, (new_flow.delivery_date AT TIME ZONE 'Europe/Moscow' + (interval '1' day * old_flow.grace_of_payment_grace_period_days))::date) - now()::date) < -15 
#             and (COALESCE(old_flow.planned_payment_date, new_flow.planned_payment_date, (new_flow.delivery_date AT TIME ZONE 'Europe/Moscow' + (interval '1' day * old_flow.grace_of_payment_grace_period_days))::date) - now()::date) >= 30 then 'От 16 до 30 дней'
#             when (COALESCE(old_flow.planned_payment_date, new_flow.planned_payment_date, (new_flow.delivery_date AT TIME ZONE 'Europe/Moscow' + (interval '1' day * old_flow.grace_of_payment_grace_period_days))::date) - now()::date) < -30 then 'Больше 30 дней' 
#         end) as Категория
# 
#         from old_flow
# 
#         left join new_flow
# 
#         on old_flow.id = new_flow.id
# 
#         where COALESCE(old_flow.status, new_flow.status) in ('received', 'approved', 'problem') or COALESCE(old_flow.status, new_flow.status) in ('Delivered', 'Approved', 'Problem')
# 
#         group by Контрагент, Категория
# )
# select * from result;") %>% filter(Категория != "ПДЗ" & Категория != "НДЗ") %>%
      
      # mutate(`Отклонение срока оплаты` = as.numeric(`Итоговая дата оплаты` - Sys.Date())) %>%
      # 
      # mutate(Сроки = case_when(`Отклонение срока оплаты` < 0 & `Отклонение срока оплаты` >= -7 ~ "До 7 дней",
      #                          `Отклонение срока оплаты` < -7 & `Отклонение срока оплаты` >= -15 ~ "От 7 до 15 дней",
      #                          `Отклонение срока оплаты` < -15 & `Отклонение срока оплаты` >= -30 ~ "От 16 до 30 дней",
      #                          `Отклонение срока оплаты` < -30 ~ "Больше 30 дней",
      # )) %>%
      # group_by(Контрагент, Сроки) %>%
      # 
      # summarise(Сумма = sum(Сумма)) %>%
      
    prdz <- pdz %>% 
      filter(Категория != "ПДЗ" & Категория != "НДЗ") %>%
      pivot_wider(names_from = Категория,
                  values_from = Сумма,
                  values_fill = 0) %>%
      
      select(Контрагент, `До 7 дней`, `От 7 до 15 дней`, `От 16 до 30 дней`, `Больше 30 дней`) %>%

      mutate(Всего = `До 7 дней` + `От 7 до 15 дней` + `От 16 до 30 дней` + `Больше 30 дней`)
    
    resultList <- list("health" = health, "prdz" = prdz)
    
    
    return(resultList)
    
}


get_dz_turnover <- function(startDate){
  # endDate <- as.POSIXct(paste(Sys.Date(), "23:59:59"), format="%Y-%m-%d %H:%M:%S") 
  # 
  # attr(endDate, "tzone") <- "UTC" 
  
  turnover <- DBI::dbGetQuery(con,
                          
                          "with 
bills as (
        select bills.id,
        
            bills.created_at_original, 
        
            bills.number_doc,
            
            bills.invoice_amount,
            
            bills.payment_date,
            
            bills.status as bills_status,
            
            unnest(bills.request_ids) as request_id 
            from erp_bills.bills 
            
            where bills.is_actual and bills.type = 'Charterer' and bills.number_doc is not null and bills.invoice_amount != 0
            
            ),
                    
            
juridical_persons as (
        select juridical_persons.id,
        
        juridical_persons.short_title,
        
        juridicals.grace_of_payment_grace_period_days,
        
        (case
        
            when juridicals.grace_of_payment_hidden_grace_period_days is null then 0
            else juridicals.grace_of_payment_hidden_grace_period_days
            
        end) as grace_of_payment_hidden_grace_period_days
        
    from erp_juridical_persons.juridical_persons
    
    left join erp_clients.juridicals on juridical_persons.id = juridicals.juridical_person_id and juridical_persons.version = juridicals.juridical_person_version
    
    left join erp_clients.clients on juridicals.client_id = clients.id and juridicals.client_version = clients.version
    
    where juridical_persons.is_actual and clients.is_actual=true
    
    ),
    
    
documents_packets as (
     select distinct on (document_packets_requests.id)
     
         document_packets_requests.id,
                  
         documents_packets.status
         
     from erp_documents_packets.document_packets_requests
     
     inner join erp_documents_packets.documents_packets
     
     ON document_packets_requests.documents_packet_id = documents_packets.id
     
     AND document_packets_requests.documents_packet_version = documents_packets.version
     
     and documents_packets.is_actual
     
     order by document_packets_requests.id, documents_packets.created_at desc
),

requests as (
            
            select  requests.id,
            
            juridical_persons.short_title,
                              
            documents_packets.status,
            
            requests.charterer_rate_amount
            
            from erp_requests.requests
                
            left join juridical_persons
            on requests.juridical_person_version_id = juridical_persons.id
            
            left join documents_packets
            on requests.id = documents_packets.id AND requests.is_actual 
            
            where requests.is_actual = true 
            
),

old_flow as (
        select distinct on (bills.number_doc)
        
        requests.short_title,                
        
        bills.id,
        
        bills.invoice_amount,
        
        bills.created_at_original,
        
        bills.payment_date,
        
        bills.bills_status,
                
        requests.status        
        
        from bills
        
        left join requests
        
        on bills.request_id = requests.id
        
        order by bills.number_doc, bills.created_at_original desc, request_id
        
),
new_flow as ( 
        select id, status
        
        from document_flow.bills
        
        where bills.is_actual = true 
),
pdz_now as (
       select old_flow.short_title as Контрагент,
       sum(old_flow.invoice_amount) as Сумма            
        
        from old_flow
        
        left join new_flow
        
        on old_flow.id = new_flow.id
        
        where (COALESCE(old_flow.status, new_flow.status) in ('received', 'approved', 'problem') 
        or COALESCE(old_flow.status, new_flow.status) in ('Delivered', 'Approved', 'Problem'))
        and old_flow.bills_status != 'Paid'
        
        group by Контрагент
),
pdz_before as (
       select old_flow.short_title as Контрагент,
       sum(old_flow.invoice_amount) as Сумма            
        
        from old_flow
        
        left join new_flow
        
        on old_flow.id = new_flow.id
        
        where (COALESCE(old_flow.status, new_flow.status) in ('received', 'approved', 'problem') 
        or COALESCE(old_flow.status, new_flow.status) in ('Delivered', 'Approved', 'Problem'))
        and old_flow.created_at_original <= $1
        and (old_flow.payment_date > $1 or old_flow.payment_date is null)
        
        group by Контрагент
),

revenue as (

select juridical_persons.short_title as Контрагент,

		sum(requests.charterer_rate_amount) as Оборот
		
		from erp_requests.requests
		
		 left join (
    select
        juridical_persons.id,
        juridical_persons.short_title
        from erp_juridical_persons.juridical_persons
    left join erp_clients.juridicals on juridical_persons.id = juridicals.juridical_person_id and juridical_persons.version = juridicals.juridical_person_version
    left join erp_clients.clients on juridicals.client_id = clients.id and juridicals.client_version = clients.version
    where juridical_persons.is_actual and clients.is_actual=true
) as juridical_persons
on requests.juridical_person_version_id = juridical_persons.id
		
		left join (
  select distinct on (s.request_id)
  request_id, id, locality, address, \"from\" as from_at, fact_arrival_date_time, region, consignee, fact_departure_date_time
  from erp_requests.request_finish_route_points s
  order by s.request_id desc, s.from desc
) as last_unloading
on requests.id = last_unloading.request_id

where requests.is_actual = true 

AND requests.status not in ('cancelled', 'transportationCancelled', 'active')

and last_unloading.from_at >= $1 and last_unloading.from_at::date <= now()::date

group by Контрагент 

),

result as (
	select pdz_now.Контрагент,
	
	coalesce(pdz_now.Сумма, 0) as ПДЗ_сейчас,
	
    coalesce(pdz_before.Сумма, 0) as ПДЗ_раньше,
    
    coalesce(revenue.Оборот, 0) as Оборот
	
	from pdz_now
	
	left join pdz_before
	on pdz_now.Контрагент = pdz_before.Контрагент
	
	left join revenue
	on  pdz_now.Контрагент = revenue.Контрагент
	
)

select * from result", params = list(startDate))

turnover <- turnover %>%
  mutate(Оборачиваемость =  as.numeric(Sys.Date() - as.Date(startDate)) / (Оборот / ((ПДЗ_сейчас + ПДЗ_раньше) / 2))
                        )

turnover[mapply(is.infinite, turnover)] <- 0

return(turnover)

}


get_client_health <- function(startDate, endDate){
  
  my_query <- query('{
"bool": {
      "must": [
        {
  "match": {"isActual": "true"}
        }
      ]
}
}')
  
  selected_field_user <- select_fields('{
  "includes": ["data.id", "data.title"]
}')
  
  user = elastic(cfg$db_elastic$url,
                 "user-profile-version-index") %search% (my_query + selected_field_user) %>%
    group_by(data.id) %>%
    summarise(data.title = first(data.title))
  
  requests_all <- dbGetQuery(con,
                             paste("SELECT date_trunc('day', last_unloading.from_at AT TIME ZONE 'Europe/Moscow') as Дата,                                  
                     requests.responsible_expeditor_title as Логист,
                     requests.charterer_version_title as Клиент,
                     count(*) AS Рейсы,
                     SUM(requests.charterer_rate_amount) as Оборот,                  
                    (case 
              		when requests.charterer_version_title ilike '%НДС%' then SUM(requests.charterer_rate_amount)
              		else SUM(requests.charterer_rate_amount / 1.2)
              	    end) as Выручка,
              	    SUM(transportations.profit_amount) as \"Маржа, ERP\",
              	    
              	    SUM((case
 when requests.charterer_version_title ilike '%НДС%' then requests.charterer_rate_amount
 else requests.charterer_rate_amount / 1.2
 end) - (case
         when transportations.carrier_rate_info_with_nds = TRUE then transportations.carrier_rate_info_amount / 1.2
         else transportations.carrier_rate_info_amount
         end)) as \"Маржа, расчёт\"

                     FROM erp_requests.requests

                   LEFT JOIN erp_requests.transportations
                   ON requests.id = transportations.request_id

left join (
    select distinct on (s.request_id)
          request_id, id, locality, address, \"from\" as from_at, fact_arrival_date_time, region
    from erp_requests.request_start_route_points s
    order by s.request_id desc, s.from asc
) as first_loading
on requests.id = first_loading.request_id

left join (
    select distinct on (s.request_id)
          request_id, id, locality, address, \"from\" as from_at, fact_arrival_date_time, region, consignee, fact_departure_date_time
    from erp_requests.request_finish_route_points s
    order by s.request_id desc, s.from desc
) as last_unloading
on requests.id = last_unloading.request_id


                   WHERE requests.is_actual=true
                   AND requests.status not in ('cancelled', 'transportationCancelled', 'active')
                   AND last_unloading.from_at >=", paste0("'", startDate, "'::timestamp"), "AND last_unloading.from_at <=", paste0("'",endDate, "'::timestamp"),
                                   
                                   "group by Дата, Клиент, Логист;"))
  
}



get_client_health <- function(startDate, endDate){

  requests_all <- dbGetQuery(con,
                             paste("SELECT date_trunc('month', last_unloading.from_at AT TIME ZONE 'Europe/Moscow') as Дата,
                     requests.responsible_expeditor_title as Логист,
                     requests.charterer_version_title as Клиент,
                     count(*) AS Рейсы,
                     SUM(requests.charterer_rate_amount) as Оборот,
                    (case
              		when requests.charterer_version_title ilike '%НДС%' then SUM(requests.charterer_rate_amount)
              		else SUM(requests.charterer_rate_amount / 1.2)
              	    end) as Выручка,
              	    SUM(transportations.profit_amount) as \"Маржа, ERP\",

              	    SUM((case
 when requests.charterer_version_title ilike '%НДС%' then requests.charterer_rate_amount
 else requests.charterer_rate_amount / 1.2
 end) - (case
         when transportations.carrier_rate_info_with_nds = TRUE then transportations.carrier_rate_info_amount / 1.2
         else transportations.carrier_rate_info_amount
         end)) as \"Маржа, расчёт\"

                     FROM erp_requests.requests

                   LEFT JOIN erp_requests.transportations
                   ON requests.id = transportations.request_id

left join (
    select distinct on (s.request_id)
          request_id, id, locality, address, \"from\" as from_at, fact_arrival_date_time, region, consignee, fact_departure_date_time
    from erp_requests.request_finish_route_points s
    order by s.request_id desc, s.from desc
) as last_unloading
on requests.id = last_unloading.request_id


                   WHERE requests.is_actual=true
                   AND requests.status not in ('cancelled', 'transportationCancelled', 'active')
                   AND date_trunc('month', last_unloading.from_at AT TIME ZONE 'Europe/Moscow') >=", paste0("'", startDate, "'::timestamp"), "AND date_trunc('month', last_unloading.from_at AT TIME ZONE 'Europe/Moscow') <=", paste0("'",endDate, "'::timestamp"),

                                   "group by Дата, Клиент, Логист;"))

}

get_prepayment <- function(startDate, endDate){
  prepayment <- dbGetQuery(con,
                             paste("SELECT date_trunc('day', transportations.created_at AT TIME ZONE 'Europe/Moscow') as Дата,
                     requests.charterer_version_title as Клиент,
                     requests.responsible_expeditor_title as Логист,
                     transportations.carrier_version_title as Перевозчик,
                     count(*) AS Рейсы,
                     SUM(requests.charterer_rate_amount) as Оборот,
              	     SUM(case
 when requests.charterer_rate_with_nds = TRUE then requests.charterer_rate_amount / 1.2
 else requests.charterer_rate_amount
 end) as Выручка,
              	    SUM(transportations.profit_amount) as Маржа,
              	    
              	    SUM(case
 when transportations.carrier_rate_info_with_nds = TRUE then transportations.carrier_rate_info_amount / 1.2
 else transportations.carrier_rate_info_amount
 end) as \"Итоговая ставка\",
 
               	    SUM(case
 when transportations.carrier_rate_info_with_nds = TRUE then transportations.carrier_rate_info_original_amount / 1.2
 else transportations.carrier_rate_info_original_amount
 end) as \"Начальная ставка\",
 
                	    SUM(case
 when transportations.carrier_rate_info_with_nds = TRUE then transportations.prepayment_amount / 1.2
 else transportations.prepayment_amount
 end) as Предоплата
              	    
                     FROM erp_requests.requests

                   LEFT JOIN erp_requests.transportations
                   ON requests.id = transportations.request_id



                   WHERE requests.is_actual=true
                   AND transportations.prepayment_type IN ('Fast', 'FastPostpayment')
                   AND requests.status not in ('cancelled', 'transportationCancelled', 'active')
                   AND date_trunc('day', transportations.created_at AT TIME ZONE 'Europe/Moscow') >=", paste0("'", startDate, "'::timestamp"), "AND date_trunc('day', transportations.created_at AT TIME ZONE 'Europe/Moscow') <=", paste0("'",endDate, "'::timestamp"),
                                   
                                   "group by Дата, Клиент, Логист, Перевозчик;"))
  
  not_prepayment <- dbGetQuery(con,
                             paste("SELECT date_trunc('day', transportations.created_at AT TIME ZONE 'Europe/Moscow') as Дата,
                     requests.charterer_version_title as Клиент,
                     requests.responsible_expeditor_title as Логист,
                     transportations.carrier_version_title as Перевозчик,
                     count(*) AS Рейсы,
                     SUM(requests.charterer_rate_amount) as Оборот,
              	     SUM(case
 when requests.charterer_rate_with_nds = TRUE then requests.charterer_rate_amount / 1.2
 else requests.charterer_rate_amount
 end) as Выручка,
              	    SUM(transportations.profit_amount) as Маржа

                     FROM erp_requests.requests
                     
                    LEFT JOIN (
                   select request_id,
                        created_at,
                        carrier_version_title,
                   		  profit_amount, 
                   		  (case
                   		  	when prepayment_type is null then 'Not'
                   		  	else prepayment_type
                   		  end) as prepayment_type
                   	
                   from erp_requests.transportations 
                   ) as transportations
                   ON requests.id = transportations.request_id
                     
                   WHERE requests.is_actual=true
                   AND transportations.prepayment_type NOT IN ('Fast', 'FastPostpayment')
                   AND requests.status not in ('cancelled', 'transportationCancelled', 'active')
                   AND date_trunc('day', transportations.created_at AT TIME ZONE 'Europe/Moscow') >=", paste0("'", startDate, "'::timestamp"), "AND date_trunc('day', transportations.created_at AT TIME ZONE 'Europe/Moscow') <=", paste0("'",endDate, "'::timestamp"),
                                   
                                   "group by Дата, Клиент, Логист, Перевозчик;")) 
  
  result <- prepayment %>%
    full_join(not_prepayment, by = c('Дата' = 'Дата', 'Клиент' = 'Клиент', 'Логист' = 'Логист', 'Перевозчик' = 'Перевозчик')) %>%
  
    left_join(expeditors_group, by = c("Логист" = "data.id"))
  
  
  result[is.na(result)] <- 0
  
  return(result)
  
}


get_SB_carriers_status <- function(startDate, endDate){
  
  my_query <- query('{
"bool": {
      "must": [
        {
  "match": {"isActual": "true"}
        }
      ]
}
}')
  
  selected_field_user <- select_fields('{
  "includes": ["data.id", "data.title"]
}')
  
  user = elastic(cfg$db_elastic$url,
                 "user-profile-version-index") %search% (my_query + selected_field_user) %>%
    group_by(data.id) %>%
    summarise(data.title = first(data.title))
  
  carriers_change1 =
    dbGetQuery(con,
               paste("SELECT distinct on (security_status_updated_at) (security_status_updated_at AT TIME ZONE 'Europe/Moscow')::date as \"Дата изменения\",
             title as Перевозчик,
             security_status_user_id as \"Сотрудник\",
                                        	(case 
              		when security_status_status_value = 'Waiting' then 'Ожидает проверки'
              		when security_status_status_value = 'Success' then 'Допущен к работе'
              		when security_status_status_value = 'Warning' then 'Требует перепроверки'
              		when security_status_status_value = 'Blacklist' then 'Запрещено работать'
              		when security_status_status_value = 'TemporarilyApproved' then 'Временно допущен'
              	end) as Статус,
             security_status_comment as Комментарий
             
             FROM erp_carriers.carriers
               WHERE security_status_updated_at >=", paste0("'", startDate, "'::timestamp"),
                     "AND security_status_updated_at <=", paste0("'", endDate, "'::timestamp"),
                     "ORDER BY security_status_updated_at, version;")) %>%
    
    inner_join(user, by = c("Сотрудник" = "data.id")) %>%
    
    transmute(`Дата изменения` = `Дата изменения`,
              Перевозчик = Перевозчик,
              Сотрудник = data.title,
              Статус = Статус,
              Комментарий = Комментарий,
              Изменений = 1
              )
  
}


get_SB_drivers_status <- function(startDate, endDate){
  my_query <- query('{
"bool": {
      "must": [
        {
  "match": {"isActual": "true"}
        }
      ]
}
}')
  
  selected_field_user <- select_fields('{
  "includes": ["data.id", "data.title"]
}')
  
  user = elastic(cfg$db_elastic$url,
                 "user-profile-version-index") %search% (my_query + selected_field_user) %>%
    group_by(data.id) %>%
    summarise(data.title = first(data.title))
  
  drivers_change =
    dbGetQuery(con1,
               paste("SELECT distinct on (security_status_modified_at) security_status_modified_at as \"Дата изменения\",
             passport_last_name || ' ' || passport_first_name || ' ' || passport_middle_name as Водитель,
             security_status_modified_by as Сотрудник,
                           	(case 
              		when security_status_value = 0 then 'Ожидает проверки'
              		when security_status_value = 1 then 'Допущен к работе'
              		when security_status_value = 2 then 'Требует перепроверки'
              		when security_status_value = 3 then 'Запрещено работать'
              		when security_status_value = 4 then 'Временно допущен'
              	end) as Статус,
              	security_status_comment as Комментарий
             FROM profiles.archived_drivers
               WHERE security_status_modified_at >=", startDate,
                     "AND security_status_modified_at <=", endDate,
                     "ORDER BY security_status_modified_at, version;")) %>%
    
    mutate(Время = as.Date(as.POSIXct(`Дата изменения` / 1000, origin = "1970-01-01"))) %>%
    
    inner_join(user, by = c("Сотрудник" = "data.id"))
  
  drivers_change1 =
    dbGetQuery(con1,
               paste("SELECT distinct on (security_status_modified_at) security_status_modified_at as \"Дата изменения\",
             passport_last_name || ' ' || passport_first_name || ' ' || passport_middle_name as Водитель,
             security_status_modified_by as Сотрудник,
                           	(case 
              		when security_status_value = 0 then 'Ожидает проверки'
              		when security_status_value = 1 then 'Допущен к работе'
              		when security_status_value = 2 then 'Требует перепроверки'
              		when security_status_value = 3 then 'Запрещено работать'
              		when security_status_value = 4 then 'Временно допущен'
              	end) as Статус,
              	security_status_comment as Комментарий
             FROM profiles.drivers
               WHERE security_status_modified_at >=", startDate,
                     "AND security_status_modified_at <=", endDate,
                     "ORDER BY security_status_modified_at, version;")) %>%
    
    mutate(Время = as.Date(as.POSIXct(`Дата изменения` / 1000, origin = "1970-01-01"))) %>%
    
    inner_join(user, by = c("Сотрудник" = "data.id"))
  
  result = rbind(drivers_change, drivers_change1) %>%
    
    transmute(`Дата изменения` = Время,
              Водитель = Водитель,
              Сотрудник = data.title,
              Статус = Статус,
              Комментарий = Комментарий,
              Изменений = 1
    ) %>% 
    unique()
  
  return(result)
}



get_SB_vehicles_status <- function(startDate, endDate){
  
  my_query <- query('{
"bool": {
      "must": [
        {
  "match": {"isActual": "true"}
        }
      ]
}
}')
  
  selected_field_user <- select_fields('{
  "includes": ["data.id", "data.title"]
}')
  
  user = elastic(cfg$db_elastic$url,
                 "user-profile-version-index") %search% (my_query + selected_field_user) %>%
    group_by(data.id) %>%
    summarise(data.title = first(data.title))
  
  carriers_change1 =
    dbGetQuery(con,
               paste("SELECT (updated_at AT TIME ZONE 'Europe/Moscow')::date as \"Дата изменения\",
             vehicle_passport_brand || ' ' || vehicle_passport_model as ТС,
             updated_by as \"Сотрудник\",
                (case 
              		when ownership_type = 'Own' then 'Собственность'
              		when ownership_type = 'Leasing' then 'Лизинг'
              		when ownership_type = 'Rent' then 'Аренда'
              		when ownership_type = 'Banned' then 'Запрещён'
              		when ownership_type is null then 'Не установлен'
              		else ownership_type
              	end) as Статус
             
             FROM erp_vehicles.vehicles
               WHERE updated_at >=", paste0("'", startDate, "'::timestamp"),
                     "AND updated_at <=", paste0("'", endDate, "'::timestamp"),
                     "ORDER BY updated_at, version;")) %>%
    
    inner_join(user, by = c("Сотрудник" = "data.id")) %>%
    
    transmute(`Дата изменения` = `Дата изменения`,
              ТС = ТС,
              Сотрудник = data.title,
              Статус = Статус,
              Изменений = 1
    ) %>% 
    unique()
  
}
