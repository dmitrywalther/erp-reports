# Генератор синтетических данных для демо-режима.
#
# Создаёт схему (demo/schema.sql) и наполняет её вымышленными, но согласованными
# данными: клиенты, логисты, перевозчики, заявки, перевозки, точки маршрута,
# счета, пакеты документов, заявки с тендерных площадок, справочники для
# заглушки Elasticsearch. Все имена и компании вымышлены.
#
# Запуск:  Rscript -e "source('demo/seed_demo.R', encoding = 'UTF-8')"
# Параметры подключения берутся из переменных окружения REPORTS_ERP_PG_*
# (см. demo/run_demo.ps1).

suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres)
  library(dplyr)
})

set.seed(20240101)
Sys.setenv(TZ = "UTC")

pg_env <- function(name, default) {
  v <- Sys.getenv(name, unset = "")
  if (v == "") default else v
}

con <- dbConnect(RPostgres::Postgres(),
                 host = pg_env("REPORTS_ERP_PG_HOST", "127.0.0.1"),
                 port = as.integer(pg_env("REPORTS_ERP_PG_PORT", "5433")),
                 user = pg_env("REPORTS_ERP_PG_USER", "postgres"),
                 password = pg_env("REPORTS_ERP_PG_PASSWORD", ""),
                 dbname = pg_env("REPORTS_ERP_PG_DB", "erp_demo"),
                 timezone = "UTC")

if (!dbExistsTable(con, Id(schema = "erp_requests", table = "requests"))) {
  stop("Сначала примените demo/schema.sql (psql -f demo/schema.sql), см. demo/run_demo.ps1")
}
if (dbGetQuery(con, "select count(*) as n from erp_requests.requests")$n > 0) {
  stop("База уже наполнена; для повторной генерации примените demo/schema.sql заново")
}

# ------------------------------------------------------------------ helpers
gen_uuid <- function(n) {
  hex <- c(0:9, letters[1:6])
  block <- function(k) replicate(n, paste(sample(hex, k, replace = TRUE), collapse = ""))
  paste(block(8), block(4), paste0("4", block(3)), paste0(sample(c("8", "9", "a", "b"), n, TRUE), block(3)), block(12), sep = "-")
}
jstr <- function(x) ifelse(is.na(x), "null", paste0('"', gsub('"', '\\\\"', x), '"'))
fmt_ts <- function(x) format(x, "%Y-%m-%d %H:%M:%S")       # для JSON: ISO без зоны (UTC)
fmt_iso_z <- function(x) format(x, "%Y-%m-%dT%H:%M:%SZ")
rdays <- function(n, from, to) runif(n, from, to) * 86400
pick <- function(x, n, prob = NULL) sample(x, n, replace = TRUE, prob = prob)
write_tbl <- function(schema, table, df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  dbWriteTable(con, Id(schema = schema, table = table), df, append = TRUE)
  message(sprintf("  %s.%s: %d строк", schema, table, nrow(df)))
}
now_utc <- as.POSIXct(format(Sys.time(), tz = "UTC"), tz = "UTC")
today <- as.Date(now_utc)

# ------------------------------------------------------------------ people
make_names <- function(n, role = "Демо сотрудник") {
  sprintf("%s %03d", role, seq_len(n))
}

group_names <- c("Группа «Север»", "Группа «Юг»", "Группа «Запад»", "Группа «Восток»")
group_ids <- gen_uuid(length(group_names))
n_logists <- 28
logists <- data.frame(
  id = gen_uuid(n_logists),
  title = make_names(n_logists, "Логист"),
  group = rep(group_names, length.out = n_logists),
  stringsAsFactors = FALSE
)
logists$group_id <- group_ids[match(logists$group, group_names)]
logists$position <- "логист"
for (g in group_names) {
  i <- which(logists$group == g)
  logists$position[i] <- c("рук. группы", "ст. логист", rep("логист", max(0, length(i) - 4)), "мл. логист", "ассистент")[seq_along(i)]
}
# «сложность» логиста — влияет на объём и маржу
logists$skill <- runif(n_logists, 0.6, 1.4)

managers <- data.frame(id = gen_uuid(5), title = make_names(5, "Менеджер"), stringsAsFactors = FALSE)
do_staff <- data.frame(id = gen_uuid(6), title = make_names(6, "Специалист ДО"), stringsAsFactors = FALSE)   # документооборот / бухгалтерия
sb_staff <- data.frame(id = gen_uuid(3), title = make_names(3, "Специалист СБ"), stringsAsFactors = FALSE)   # служба безопасности
system_user <- data.frame(id = gen_uuid(1), title = "System", stringsAsFactors = FALSE)
all_users <- rbind(logists[, c("id", "title")], managers, do_staff, sb_staff, system_user)

write_tbl("erp_users", "user_profiles", data.frame(original_id = all_users$id, version = 1L, is_actual = TRUE, title = all_users$title))
write_tbl("search_index", "user-profile-version-index", setNames(all_users[, c("id", "title")], c("data.id", "data.title")))
write_tbl("search_index", "expeditors-group-version-index", data.frame(`data.id` = c(group_ids, gen_uuid(1)), `data.name` = c(group_names, "Коммерческий отдел"), check.names = FALSE))
write_tbl("search_index", "logist_groups", data.frame(`data.id` = c(logists$title, managers$title), `data.name` = c(logists$group, rep("Коммерческий отдел", nrow(managers))), position = c(logists$position, rep("менеджер", nrow(managers))), check.names = FALSE))

role_id <- gen_uuid(1)
write_tbl("public", "Users", data.frame(Id = c(logists$id, managers$id), Description = c(logists$title, managers$title), IsArchived = FALSE, check.names = FALSE))
write_tbl("public", "Roles", data.frame(Id = c(role_id, gen_uuid(1)), Name = c("expeditor", "manager"), check.names = FALSE))
write_tbl("public", "RelationOfUserAndRole", data.frame(UserId = logists$id, RoleId = role_id, check.names = FALSE))

# ------------------------------------------------------------------ glossary
glossary <- data.frame(
  key = c("tent", "refrigerator", "isotherm", "van", "jumbo",
          "cap_20_82", "cap_20_90", "cap_10_50", "cap_5_36", "cap_3_20", "cap_1_5_12",
          "fmcg", "food", "beverages", "building", "household", "chemicals", "furniture"),
  title = c("Тент", "Рефрижератор", "Изотерм", "Фургон", "Джамбо",
            "20 т / 82 м3", "20 т / 90 м3", "10 т / 50 м3", "5 т / 36 м3", "3 т / 20 м3", "1,5 т / 12 м3",
            "FMCG", "Продукты питания", "Напитки", "Стройматериалы", "Бытовая техника", "Бытовая химия", "Мебель"),
  type = c(rep("typesOfBody", 5), rep("carryingCapacityAndVolume", 6), rep("cargoType", 7)),
  stringsAsFactors = FALSE
)
write_tbl("search_index", "glossary-element-version-index", setNames(glossary, c("data.key", "data.title", "data.type")))
body_keys <- glossary$key[glossary$type == "typesOfBody"]
cap_keys <- glossary$key[glossary$type == "carryingCapacityAndVolume"]
cargo_keys <- glossary$key[glossary$type == "cargoType"]
cap_tons <- c(cap_20_82 = 20, cap_20_90 = 20, cap_10_50 = 10, cap_5_36 = 5, cap_3_20 = 3, cap_1_5_12 = 1.5)
cap_volume <- c(cap_20_82 = 82, cap_20_90 = 90, cap_10_50 = 50, cap_5_36 = 36, cap_3_20 = 20, cap_1_5_12 = 12)

# ------------------------------------------------------------------ geography
regions <- data.frame(
  region = c("Московская обл", "Ленинградская обл", "Нижегородская обл", "Свердловская обл", "Респ Татарстан", "Краснодарский край", "Новосибирская обл", "Самарская обл", "Ростовская обл", "Воронежская обл", "Тверская обл", "Ярославская обл", "Калужская обл", "Тульская обл", "Владимирская обл"),
  city = c("Москва", "Санкт-Петербург", "Нижний Новгород", "Екатеринбург", "Казань", "Краснодар", "Новосибирск", "Самара", "Ростов-на-Дону", "Воронеж", "Тверь", "Ярославль", "Калуга", "Тула", "Владимир"),
  lat = c(55.7558, 59.9343, 56.2965, 56.8389, 55.7963, 45.0355, 55.0084, 53.1959, 47.2357, 51.6608, 56.8587, 57.6261, 54.5136, 54.1931, 56.1290),
  lon = c(37.6173, 30.3351, 43.9361, 60.6057, 49.1088, 38.9753, 82.9357, 50.1002, 39.7015, 39.2003, 35.9176, 39.8845, 36.2612, 37.6173, 40.4070),
  weight = c(10, 5, 3, 2, 2, 2, 1.5, 2, 2, 2, 1.5, 1.5, 1.5, 1.5, 1.5),
  stringsAsFactors = FALSE
)
region_dist <- function(i, j) {   # км по прямой * 1.3
  r <- 6371
  la1 <- regions$lat[i] * pi / 180; la2 <- regions$lat[j] * pi / 180
  dlo <- (regions$lon[j] - regions$lon[i]) * pi / 180
  d <- acos(pmin(1, sin(la1) * sin(la2) + cos(la1) * cos(la2) * cos(dlo))) * r
  ifelse(i == j, 40, d * 1.3)
}
streets <- c("ул. Промышленная", "ш. Складское", "пр-т Логистический", "ул. Заводская", "ул. Транспортная", "пер. Складской")
consignees_special <- c("РЦ Демо РЦ 01", "РЦ Демо РЦ 02")
consignees <- c("РЦ «Северный»", "РЦ «Восточный»", "Логопарк «Ока»", "ДЦ «Южные ворота»", "Склад №3", "РЦ «Заречье»", "ТК «Меридиан»", consignees_special)

# ------------------------------------------------------------------ clients
client_names <- sprintf("Клиент %02d", seq_len(18))
n_clients <- length(client_names)
clients <- data.frame(
  id = gen_uuid(n_clients),
  jp_id = gen_uuid(n_clients),
  short_title = client_names,
  stringsAsFactors = FALSE
)
clients$title <- ifelse(seq_len(n_clients) %% 4 == 0, paste(clients$short_title, "НДС"), clients$short_title)   # часть клиентов работает «с НДС»
clients$manager_id <- pick(managers$id, n_clients)
clients$weight <- rlnorm(n_clients, 0, 0.7)
clients$grace_days <- pick(c(30L, 45L, 60L, 14L), n_clients)
clients$hidden_grace <- pick(c(0L, 0L, 5L, 10L), n_clients)
clients$doc_days <- pick(c(5L, 7L, 10L), n_clients)
clients$verif_days <- pick(c(3L, 5L, 7L), n_clients)
clients$accounts_id <- pick(do_staff$id, n_clients)
clients$doc_id <- pick(do_staff$id, n_clients)
clients$assistant_id <- pick(managers$id, n_clients)
clients$inn <- sprintf("DEMO-CLIENT-%03d", seq_len(n_clients))

write_tbl("erp_clients", "clients", data.frame(id = clients$id, version = 1L, is_actual = TRUE, signed_by_uid = clients$manager_id))
write_tbl("erp_clients", "juridicals", data.frame(
  id = gen_uuid(n_clients), client_id = clients$id, client_version = 1L,
  juridical_person_id = clients$jp_id, juridical_person_version = 1L,
  grace_of_payments_days = clients$grace_days, tsd_settings_doc_verification_days = clients$verif_days,
  responsible_employee_processing_doc_id = clients$doc_id, responsible_employee_processing_accounts_id = clients$accounts_id,
  responsible_assistant_id = clients$assistant_id,
  grace_of_payment_grace_period_days = clients$grace_days, grace_of_payment_hidden_grace_period_days = clients$hidden_grace))
write_tbl("search_index", "charterer-version-index", data.frame(`data.id` = clients$id, `data.docVerification.days` = clients$verif_days, check.names = FALSE))

# ------------------------------------------------------------------ carriers, vehicles, drivers
n_carriers <- 140
carrier_titles <- sprintf("Перевозчик %03d", seq_len(n_carriers))
carrier_titles <- make.unique(carrier_titles, sep = " ")
carriers <- data.frame(
  id = gen_uuid(n_carriers), jp_id = gen_uuid(n_carriers), title = carrier_titles,
  created_at = now_utc - rdays(n_carriers, 30, 700),
  agreement = ifelse(runif(n_carriers) < 0.7, sprintf("РД-%04d", sample(9999, n_carriers)), NA),
  weight = rlnorm(n_carriers, 0, 0.8),
  inn = sprintf("DEMO-CARRIER-%03d", seq_len(n_carriers)),
  stringsAsFactors = FALSE
)
carriers$with_nds <- runif(n_carriers) < 0.35

# юр. лица: клиенты + перевозчики
write_tbl("erp_juridical_persons", "juridical_persons", data.frame(
  id = c(clients$jp_id, carriers$jp_id), version = 1L, is_actual = TRUE,
  short_title = c(clients$short_title, carriers$title),
  agreement = c(sprintf('{"Number": "Д-%d/%d"}', sample(100:999, n_clients), pick(2021:2023, n_clients)), rep(NA, n_carriers)),
  inn = c(clients$inn, carriers$inn),
  juridical_address = paste0("г. ", pick(regions$city, n_clients + n_carriers), ", ", pick(streets, n_clients + n_carriers), ", д. ", sample(1:120, n_clients + n_carriers, TRUE))))

# версии перевозчиков (для отчётов СБ): 1–3 версии, статусы безопасности
sb_status_values <- c("Waiting", "Success", "Warning", "Blacklist", "TemporarilyApproved")
carrier_versions <- do.call(rbind, lapply(seq_len(n_carriers), function(i) {
  k <- sample(1:3, 1)
  upd <- sort(carriers$created_at[i] + rdays(k, 0, 300))
  upd[upd > now_utc] <- now_utc - rdays(sum(upd > now_utc), 0, 5)
  data.frame(id = carriers$id[i], version = seq_len(k), is_actual = seq_len(k) == k, title = carriers$title[i],
             created_at = carriers$created_at[i], created_by = pick(sb_staff$id, 1),
             updated_at = upd, updated_by = pick(sb_staff$id, k),
             security_status_status_value = pick(sb_status_values, k, prob = c(2, 6, 1.5, 0.5, 1)),
             security_status_updated_at = upd, security_status_user_id = pick(sb_staff$id, k),
             security_status_comment = pick(c("Проверка пройдена", "Документы в порядке", "Требуется повторная проверка", "Замечания по договору", NA), k),
             stringsAsFactors = FALSE)
}))
write_tbl("erp_carriers", "carriers", carrier_versions)
carrier_actual <- carrier_versions[carrier_versions$is_actual, ]
write_tbl("search_index", "carrier-version-index", data.frame(
  `data.id` = carrier_actual$id, `data.createdAt` = fmt_ts(carrier_actual$created_at), `data.type` = pick(c("Carrier", "Expeditor"), n_carriers, prob = c(8, 2)),
  `data.securityStatus.dateLastUpdated` = fmt_ts(carrier_actual$security_status_updated_at),
  `data.securityStatus.statusValue` = match(carrier_actual$security_status_status_value, sb_status_values) - 1L, check.names = FALSE))

n_vehicles <- 220
brands <- c("КАМАЗ", "MAN", "Volvo", "Scania", "Mercedes-Benz", "DAF", "Iveco")
vehicle_versions <- do.call(rbind, lapply(seq_len(n_vehicles), function(i) {
  k <- sample(1:2, 1)
  cr <- now_utc - rdays(1, 30, 600)
  upd <- sort(cr + rdays(k, 0, 200)); upd[upd > now_utc] <- now_utc - rdays(sum(upd > now_utc), 0, 5)
  data.frame(id = gen_uuid(1), version = seq_len(k), is_actual = seq_len(k) == k,
             vehicle_passport_brand = pick(brands, 1), vehicle_passport_model = pick(c("FH", "R450", "Actros", "XF", "5490", "TGX", "Stralis"), 1),
             ownership_type = pick(c("Own", "Leasing", "Rent", "Banned", NA), k, prob = c(5, 3, 2, 0.3, 1)),
             created_at = cr, created_by = pick(sb_staff$id, 1), updated_at = upd, updated_by = pick(sb_staff$id, k), stringsAsFactors = FALSE)
}))
write_tbl("erp_vehicles", "vehicles", vehicle_versions)
vehicles <- vehicle_versions[vehicle_versions$is_actual, ]
vehicles$plate <- sprintf("DEMO-TRUCK-%03d", seq_len(n_vehicles))
vehicles$body <- pick(body_keys, n_vehicles, prob = c(6, 3, 1, 1, 0.5))
vehicles$cap <- pick(cap_keys, n_vehicles, prob = c(5, 3, 1, 1, 0.5, 0.3))
write_tbl("search_index", "vehicle-version-index", data.frame(id = vehicles$id, `data.typesOfBody` = vehicles$body, `data.payload` = cap_tons[vehicles$cap], `data.cubicCapacity` = cap_volume[vehicles$cap], check.names = FALSE))

n_drivers <- 240
driver_names <- make_names(n_drivers, "Водитель")
drv_created <- now_utc - rdays(n_drivers, 10, 700)
drv_updated <- pmin(drv_created + rdays(n_drivers, 0, 300), now_utc - rdays(n_drivers, 0, 3))
drivers <- data.frame(
  id = gen_uuid(n_drivers), version = 2L, is_deleted = runif(n_drivers) < 0.05,
  passport_last_name = sub(" .*", "", driver_names), passport_first_name = sub(".* ", "", driver_names),
  passport_middle_name = "Демо",
  created_at = sprintf("%.0f", as.numeric(drv_created) * 1000), created_by = pick(sb_staff$id, n_drivers),
  updated_at = sprintf("%.0f", as.numeric(drv_updated) * 1000), last_updated_by = pick(sb_staff$id, n_drivers),
  security_status_value = pick(0:4, n_drivers, prob = c(2, 6, 1.5, 0.5, 1)),
  security_status_modified_at = sprintf("%.0f", as.numeric(drv_updated) * 1000), security_status_modified_by = pick(sb_staff$id, n_drivers),
  security_status_comment = pick(c("Проверен", "Допущен", "Повторная проверка", NA), n_drivers),
  stringsAsFactors = FALSE
)
write_tbl("profiles", "drivers", drivers)
archived <- drivers[sample(n_drivers, 120), ]
archived$version <- 1L
arch_upd <- as.numeric(archived$updated_at) - runif(120, 5, 120) * 86400000
archived$updated_at <- sprintf("%.0f", arch_upd); archived$security_status_modified_at <- archived$updated_at
archived$security_status_value <- pick(0:4, 120)
write_tbl("profiles", "archived_drivers", archived)
drivers$phone <- sprintf("DEMO-PHONE-%03d", seq_len(n_drivers))

# адреса (часть — специальные РЦ)
n_addresses <- 80
addresses <- data.frame(id = gen_uuid(n_addresses), version = 1L, type = ifelse(runif(n_addresses) < 0.12, "dc_special", "regular"))
write_tbl("erp_addresses", "addresses", addresses)

# ------------------------------------------------------------------ requests
start_day <- today - 540
end_day <- today + 14
days <- seq(start_day, end_day, by = "day")
wday <- as.POSIXlt(days)$wday
per_day <- rpois(length(days), ifelse(wday %in% c(0, 6), 6, 24) * (1 + 0.35 * sin(2 * pi * as.numeric(days - start_day) / 365)))
n_req <- sum(per_day)
message("заявок: ", n_req)

req_day <- rep(days, per_day)
created_at <- as.POSIXct(req_day, tz = "UTC") + runif(n_req, 5, 16) * 3600   # 8–19 по Москве
client_idx <- pick(seq_len(n_clients), n_req, prob = clients$weight)
logist_idx <- pick(seq_len(n_logists), n_req, prob = logists$skill)
start_r <- pick(seq_len(nrow(regions)), n_req, prob = regions$weight)
same_region <- runif(n_req) < 0.25
finish_r <- ifelse(same_region, start_r, pick(seq_len(nrow(regions)), n_req, prob = regions$weight))
finish_r[!same_region & finish_r == start_r] <- ((finish_r[!same_region & finish_r == start_r]) %% nrow(regions)) + 1
dist_km <- region_dist(start_r, finish_r)
loading_plan <- created_at + rdays(n_req, 0.5, 5)
travel_h <- ifelse(same_region, runif(n_req, 4, 12), dist_km / 55 + runif(n_req, 8, 30))
unloading_plan <- loading_plan + travel_h * 3600
req_type <- pick(c("Warranty", "Spot"), n_req, prob = c(7, 3))
body <- pick(body_keys, n_req, prob = c(6, 3, 1, 1, 0.5))
cap <- pick(cap_keys, n_req, prob = c(5, 3, 1, 1, 0.5, 0.3))
cargo <- pick(cargo_keys, n_req)
rate <- round((18000 + 38 * dist_km) * (1 + 0.25 * (body == "refrigerator")) * runif(n_req, 0.85, 1.2) * (1 + 0.1 * (req_type == "Spot")), -2)
rate_with_nds <- clients$title[client_idx] != clients$short_title[client_idx]   # клиенты «с НДС»

# статусы жизненного цикла
age_days <- as.numeric(difftime(now_utc, unloading_plan, units = "days"))
u <- runif(n_req)
status <- ifelse(age_days < 0,
                 ifelse(u < 0.22, "active", "carrierFound"),
                 ifelse(u < 0.045, "cancelled",
                 ifelse(u < 0.07, "transportationCancelled",
                 ifelse(u < 0.08 & age_days < 20, "active",
                 ifelse(age_days < 6, "carrierFound",
                 ifelse(age_days < 14, "docsReceived",
                 ifelse(age_days < 28, "docsApproved",
                 ifelse(age_days < 45, pick(c("docsSent", "docsGrouped"), n_req),
                 ifelse(runif(n_req) < 0.85, "paid", "docsApproved")))))))))
has_transport <- !(status %in% c("active", "cancelled")) | (status == "cancelled" & runif(n_req) < 0.3)
has_docs_problem <- has_transport & age_days > 3 & status %in% c("carrierFound", "docsReceived", "docsApproved") & runif(n_req) < 0.08

short_id <- sprintf("%06d", 100000 + seq_len(n_req))
# связанные заявки (пересозданные после отмены)
relative <- rep(NA_character_, n_req)
cancelled_idx <- which(status == "cancelled")
recreated <- sample(cancelled_idx, round(length(cancelled_idx) * 0.3))
relative[pmin(recreated + sample(1:40, length(recreated), TRUE), n_req)] <- short_id[recreated]

cancel_reason <- ifelse(status == "cancelled", pick(c("Отказ клиента", "Не нашли перевозчика", "Отказ клиента: изменились сроки", "Дубль заявки"), n_req, prob = c(4, 3, 1, 0.5)),
                        ifelse(status == "transportationCancelled", "Перевозчик снял машину", NA))
note_text <- ifelse(!is.na(cancel_reason), cancel_reason,
                    ifelse(runif(n_req) < 0.3, pick(c("Обработано", "Груз хрупкий, аккуратная погрузка", "Позвонить за час до подачи", "Пропуск оформлен", "Обработано, документы отправлены"), n_req), NA))
notes_json <- ifelse(is.na(note_text), "[]", paste0('[{"Text": ', jstr(note_text), '}]'))
tsd_json <- ifelse(runif(n_req) < 0.08 & age_days > 5, '[{"Key": "torg_2_discrepancy_act"}]', "[]")

req_hist <- paste0('[{"Status": "active", "IssueDateTime": "', fmt_ts(created_at), '", "UserId": "', logists$id[logist_idx], '", "Reason": null}',
                   ifelse(has_transport, paste0(', {"Status": "carrierFound", "IssueDateTime": "', fmt_ts(created_at + runif(n_req, 0.5, 30) * 3600), '", "UserId": "', logists$id[logist_idx], '", "Reason": null}'), ""),
                   "]")

doc_days <- clients$doc_days[client_idx]
planned_doc_date <- unloading_plan + doc_days * 86400
docs_received_at <- ifelse(status %in% c("docsReceived", "docsApproved", "docsSent", "docsGrouped", "paid") | has_docs_problem, unloading_plan + rdays(n_req, 2, 14), NA)
## Статус обозначает уже случившееся событие. Случайная задержка не должна
## переносить его в будущее (иначе НДЗ показывает отрицательный возраст этапа).
docs_received_at <- pmin(as.POSIXct(docs_received_at, origin = "1970-01-01", tz = "UTC"), now_utc)
docs_approved_at <- ifelse(status %in% c("docsApproved", "docsSent", "docsGrouped", "paid") & !has_docs_problem,
                           docs_received_at + rdays(n_req, 1, 6), NA)
docs_approved_at <- pmin(as.POSIXct(docs_approved_at, origin = "1970-01-01", tz = "UTC"), now_utc)
paid_at <- ifelse(status == "paid", docs_approved_at + rdays(n_req, 20, 75), NA)
paid_at <- pmin(as.POSIXct(paid_at, origin = "1970-01-01", tz = "UTC"), now_utc)

requests <- data.frame(
  id = gen_uuid(n_req), is_actual = TRUE, status = status, created_at = created_at, created_by = logists$id[logist_idx],
  short_id = short_id, relative_request_short_id = relative,
  external_number = ifelse(runif(n_req) < 0.6, sprintf("%s-%05d", pick(c("EXT", "ORD", "CL"), n_req), sample(99999, n_req, TRUE)), NA),
  type = req_type,
  charterer_version_id = clients$id[client_idx], charterer_version_title = clients$title[client_idx],
  charterer_version_signed_by_uid = clients$manager_id[client_idx], charterer_version_doc_received_days = doc_days,
  charterer_description = pick(c("Крупный сетевой клиент", "Региональный дистрибьютор", "Производитель", NA), n_req),
  charterer_rate_amount = rate, charterer_rate_with_nds = rate_with_nds,
  charterer_bill_info_number_doc = NA_character_,
  charterer_certificate_info_number = ifelse(status %in% c("docsSent", "docsGrouped", "paid"), sprintf("АК-%06d", seq_len(n_req)), NA),
  charterer_certificate_info_created_at = ifelse(status %in% c("docsSent", "docsGrouped", "paid"), docs_approved_at + 86400, NA),
  charterer_certificate_info_ready_at = ifelse(status %in% c("docsSent", "docsGrouped", "paid"), docs_approved_at + 2 * 86400, NA),
  juridical_person_version_id = clients$jp_id[client_idx], juridical_person_version_short_title = clients$short_title[client_idx],
  responsible_expeditor_id = logists$id[logist_idx], responsible_expeditor_title = logists$title[logist_idx], responsible_expeditor_division = logists$group_id[logist_idx],
  vehicle_requirements_characteristic_preset = glossary$title[match(body, glossary$key)],
  vehicle_requirements_types_of_body = paste0("{", body, "}"),
  vehicle_requirements_carrying_capacity_and_volume = cap,
  cargo_cargo_type = cargo, cargo_brutto_in_tonns = round(cap_tons[cap] * runif(n_req, 0.5, 1), 1), cargo_volume_in_cubometres = round(cap_volume[cap] * runif(n_req, 0.5, 1)),
  cargo_cost = round(runif(n_req, 3e5, 5e6), -3),
  trip_comment = pick(c(NA, NA, "Паллеты обратно", "Выгрузка по записи", "Ночная подача"), n_req),
  notes = notes_json, tsd = tsd_json, statuses_history = req_hist,
  cancellation_info_has_complaint_work = ifelse(status == "cancelled", runif(n_req) < 0.2, FALSE),
  shipping_documents_info_planned_doc_received_date = ifelse(has_transport, planned_doc_date, NA),
  shipping_documents_info_docs_received_date = docs_received_at,
  stringsAsFactors = FALSE
)
for (col in c("charterer_certificate_info_created_at", "charterer_certificate_info_ready_at", "shipping_documents_info_planned_doc_received_date")) {
  requests[[col]] <- as.POSIXct(requests[[col]], origin = "1970-01-01", tz = "UTC")
}
for (col in c("charterer_certificate_info_created_at", "charterer_certificate_info_ready_at")) {
  requests[[col]] <- pmin(requests[[col]], now_utc)
}

# кругорейсы: зеркальные пары среди будущих заявок
future_idx <- which(age_days < -1 & !same_region & status %in% c("active", "carrierFound"))
pair_a <- sample(future_idx, min(40, length(future_idx) %/% 2))
pair_b <- setdiff(future_idx, pair_a)[seq_along(pair_a)]
start_r[pair_b] <- finish_r[pair_a]; finish_r[pair_b] <- start_r[pair_a]
loading_plan[pair_b] <- unloading_plan[pair_a] + runif(length(pair_b), 2, 20) * 3600
unloading_plan[pair_b] <- loading_plan[pair_b] + (dist_km[pair_a] / 55 + runif(length(pair_b), 8, 30)) * 3600
requests$vehicle_requirements_carrying_capacity_and_volume[pair_b] <- cap[pair_a]
requests$vehicle_requirements_types_of_body[pair_b] <- requests$vehicle_requirements_types_of_body[pair_a]
cap[pair_b] <- cap[pair_a]; body[pair_b] <- body[pair_a]

write_tbl("erp_requests", "requests", requests)

# точки маршрута
addr_idx <- pick(seq_len(n_addresses), n_req)
start_points <- data.frame(
  id = gen_uuid(n_req), request_id = requests$id, locality = regions$city[start_r],
  address = paste0("г. ", regions$city[start_r], ", ", pick(streets, n_req), ", д. ", sample(1:90, n_req, TRUE)),
  region = regions$region[start_r], "from" = loading_plan,
  fact_arrival_date_time = as.POSIXct(ifelse(has_transport & age_days > 0, loading_plan + runif(n_req, -1, 3) * 3600, NA), origin = "1970-01-01", tz = "UTC"),
  latitude = round(regions$lat[start_r] + rnorm(n_req, 0, 0.08), 5), longitude = round(regions$lon[start_r] + rnorm(n_req, 0, 0.12), 5),
  address_id = addresses$id[addr_idx], address_version = 1L, check.names = FALSE, stringsAsFactors = FALSE
)
write_tbl("erp_requests", "request_start_route_points", start_points)
addr_idx2 <- pick(seq_len(n_addresses), n_req)
finish_points <- data.frame(
  id = gen_uuid(n_req), request_id = requests$id, locality = regions$city[finish_r],
  address = paste0("г. ", regions$city[finish_r], ", ", pick(streets, n_req), ", д. ", sample(1:90, n_req, TRUE)),
  region = regions$region[finish_r], consignee = pick(consignees, n_req),
  "from" = unloading_plan,
  fact_arrival_date_time = as.POSIXct(ifelse(has_transport & age_days > 0, unloading_plan + runif(n_req, -2, 6) * 3600, NA), origin = "1970-01-01", tz = "UTC"),
  fact_departure_date_time = as.POSIXct(ifelse(has_transport & age_days > 0, unloading_plan + runif(n_req, 2, 9) * 3600, NA), origin = "1970-01-01", tz = "UTC"),
  latitude = round(regions$lat[finish_r] + rnorm(n_req, 0, 0.08), 5), longitude = round(regions$lon[finish_r] + rnorm(n_req, 0, 0.12), 5),
  address_id = addresses$id[addr_idx2], address_version = 1L, check.names = FALSE, stringsAsFactors = FALSE
)
# у части заявок две точки выгрузки (первая — промежуточная, раньше)
extra <- sample(n_req, round(n_req * 0.1))
extra_points <- finish_points[extra, ]
extra_points$id <- gen_uuid(length(extra))
extra_points$`from` <- extra_points$`from` - runif(length(extra), 3, 10) * 3600
extra_points$consignee <- pick(consignees, length(extra))
write_tbl("erp_requests", "request_finish_route_points", rbind(finish_points, extra_points))

# ------------------------------------------------------------------ transportations
tr_idx <- which(has_transport)
n_tr <- length(tr_idx)
carrier_i <- pick(seq_len(n_carriers), n_tr, prob = carriers$weight)
tr_created <- created_at[tr_idx] + runif(n_tr, 0.5, 30) * 3600
tr_created <- pmin(tr_created, loading_plan[tr_idx] - 3600)
c_with_nds <- carriers$with_nds[carrier_i]
carrier_rate <- round(rate[tr_idx] * runif(n_tr, 0.78, 0.96) * (1 + 0.12 * c_with_nds), -2)
profit <- round(rate[tr_idx] / ifelse(rate_with_nds[tr_idx], 1.2, 1) - carrier_rate / ifelse(c_with_nds, 1.2, 1))
tr_status_req <- status[tr_idx]
tr_age <- age_days[tr_idx]
tr_load_age <- as.numeric(difftime(now_utc, loading_plan[tr_idx], units = "days"))
tr_status <- ifelse(tr_status_req == "transportationCancelled" | tr_status_req == "cancelled", "cancelled",
             ifelse(has_docs_problem[tr_idx], "docsProblem",
             ifelse(tr_status_req == "paid", "paid",
             ifelse(tr_status_req %in% c("docsApproved", "docsSent", "docsGrouped"), "docsApproved",
             ifelse(tr_status_req == "docsReceived", "docsReceived",
             ifelse(tr_age > 0, "unloaded",
             ifelse(tr_load_age > 0, pick(c("onTheWay", "onFinishPoint", "loaded"), n_tr),
             ifelse(tr_load_age > -0.3, "onStartPoint", "ready"))))))))
prepay_type <- pick(c("Fast", "FastPostpayment", "Regular", NA), n_tr, prob = c(2, 1, 4, 3))
prepay_amount <- ifelse(prepay_type %in% c("Fast", "FastPostpayment"), round(carrier_rate * runif(n_tr, 0.3, 0.7), -2), NA)
grace <- pick(c(7L, 14L, 21L, 30L), n_tr)
docs_rec <- docs_received_at[tr_idx]; docs_appr <- docs_approved_at[tr_idx]; pd <- paid_at[tr_idx]
problem_at <- ifelse(tr_status == "docsProblem", pmax(docs_rec, tr_created, na.rm = TRUE) + rdays(n_tr, 0.5, 3), NA)
problem_at <- pmin(as.POSIXct(problem_at, origin = "1970-01-01", tz = "UTC"), now_utc)
do_user <- pick(do_staff$id, n_tr)

ev <- function(status, at, user) ifelse(is.na(at), "", paste0(', {"Status": "', status, '", "IssueDateTime": "', fmt_ts(at), '", "UserId": "', user, '", "Reason": ', ifelse(status == "docsProblem", '"Комментарий проблемы: не хватает ТТН"', "null"), '}'))
tr_hist <- paste0(
  '[{"Status": "ready", "IssueDateTime": "', fmt_ts(tr_created), '", "UserId": "', logists$id[logist_idx[tr_idx]], '", "Reason": null}',
  ev("loaded", as.POSIXct(ifelse(tr_load_age > 0 & tr_status != "cancelled", loading_plan[tr_idx] + 3600, NA), origin = "1970-01-01", tz = "UTC"), logists$id[logist_idx[tr_idx]]),
  ev("unloaded", as.POSIXct(ifelse(tr_age > 0 & tr_status != "cancelled", unloading_plan[tr_idx] + 3600, NA), origin = "1970-01-01", tz = "UTC"), logists$id[logist_idx[tr_idx]]),
  ev("docsReceived", docs_rec, do_user), ev("docsProblem", problem_at, do_user), ev("docsApproved", docs_appr, do_user), ev("paid", pd, do_user),
  ifelse(tr_status == "cancelled", paste0(', {"Status": "cancelled", "IssueDateTime": "', fmt_ts(tr_created + 6 * 3600), '", "UserId": "', logists$id[logist_idx[tr_idx]], '", "Reason": "Перевозчик снял машину"}'), ""),
  "]")
scan_hist <- ifelse(is.na(docs_appr), NA, paste0('[{"Status": "approved", "IssueDateTime": "', fmt_ts(pmax(docs_rec, docs_appr - 3600)), '", "UserId": "', do_user, '"}, {"Status": "docsApproved", "IssueDateTime": "', fmt_ts(docs_appr), '", "UserId": "', do_user, '"}]'))

transportations <- data.frame(
  request_id = requests$id[tr_idx], status = tr_status, created_at = tr_created, created_by = logists$id[logist_idx[tr_idx]],
  statuses_history = tr_hist, docs_scan_status = ifelse(is.na(docs_appr), NA, "approved"), docs_scan_statuses_history = scan_hist,
  carrier_version_id = carriers$id[carrier_i], carrier_version_title = carriers$title[carrier_i],
  carrier_version_framework_agreement_number = carriers$agreement[carrier_i],
  juridical_person_version_id = carriers$jp_id[carrier_i], juridical_person_version_short_title = carriers$title[carrier_i],
  carrier_rate_info_amount = carrier_rate, carrier_rate_info_original_amount = round(carrier_rate * runif(n_tr, 1, 1.08), -2),
  carrier_rate_info_with_nds = c_with_nds, carrier_rate_info_commission_in_percent = ifelse(is.na(prepay_amount), NA, pick(c(2, 3, 5), n_tr)),
  prepayment_amount = prepay_amount, prepayment_type = prepay_type, profit_amount = profit, grace_period = grace,
  carrier_grace_of_payment_finish_grace_period_date = as.POSIXct(ifelse(tr_age > 0, unloading_plan[tr_idx] + grace * 86400, NA), origin = "1970-01-01", tz = "UTC"),
  carrier_certificate_info_number = ifelse(!is.na(docs_appr), sprintf("ПА-%06d", tr_idx), NA),
  carrier_certificate_info_created_at = pmin(docs_appr + 86400, now_utc), carrier_certificate_info_ready_at = pmin(docs_appr + 3 * 86400, now_utc),
  carrier_bill_info_paid_at = pmin(as.POSIXct(ifelse(!is.na(docs_appr) & tr_age > grace + 10 & runif(n_tr) < 0.8, docs_appr + (grace + runif(n_tr, 0, 20)) * 86400, NA), origin = "1970-01-01", tz = "UTC"), now_utc),
  stringsAsFactors = FALSE
)
write_tbl("erp_requests", "transportations", transportations)

# документы, водители, ТС перевозки
docs <- rbind(
  data.frame(transportation_request_id = requests$id[tr_idx][runif(n_tr) < 0.85], category = "inner_application_contract_charterer"),
  data.frame(transportation_request_id = requests$id[tr_idx][runif(n_tr) < 0.75], category = "signed_application_contract_carrier"))
write_tbl("erp_requests", "transportation_documents", docs)
drv_i <- pick(seq_len(n_drivers), n_tr)
write_tbl("erp_requests", "transportation_driver_versions", data.frame(transportation_request_id = requests$id[tr_idx], full_name = paste(driver_names[drv_i], drivers$passport_middle_name[drv_i]), phone_numbers = drivers$phone[drv_i]))
veh_i <- pick(seq_len(n_vehicles), n_tr)
trailer_plates <- sprintf("DEMO-TRAILER-%05d", seq_len(n_tr))
write_tbl("erp_requests", "transportation_vehicle_versions", rbind(
  data.frame(transportation_request_id = requests$id[tr_idx], version_id = vehicles$id[veh_i], type = "tractor", title = paste(vehicles$vehicle_passport_brand[veh_i], vehicles$vehicle_passport_model[veh_i]), number_plate = vehicles$plate[veh_i]),
  data.frame(transportation_request_id = requests$id[tr_idx], version_id = gen_uuid(n_tr), type = "semitrailer", title = pick(c("Schmitz", "Krone", "Kögel", "Тонар"), n_tr), number_plate = trailer_plates)))

# ------------------------------------------------------------------ bills (счета клиентам)
billable <- which(status %in% c("docsApproved", "docsSent", "docsGrouped", "paid") & !has_docs_problem)
bill_groups <- split(billable, paste(client_idx[billable], format(as.Date(docs_approved_at[billable]), "%Y-%W")))
bills <- lapply(seq_along(bill_groups), function(g) {
  idx <- bill_groups[[g]]
  # счёт объединяет 1–6 заявок
  chunks <- split(idx, ceiling(seq_along(idx) / sample(1:6, 1)))
  lapply(chunks, function(ch) {
    created <- min(max(docs_approved_at[ch]) + rdays(1, 0.5, 3), now_utc)
    all_paid <- all(status[ch] == "paid")
    st <- if (all_paid) "Paid" else if (max(age_days[ch]) < 40) pick(c("ForPayment", "NotPaid"), 1) else pick(c("NotPaid", "PartialPaid", "ForPayment"), 1)
    amount <- sum(rate[ch])
    list(id = gen_uuid(1), status = st, invoice_amount = amount,
         paid_amount = if (st == "PartialPaid") round(amount * runif(1, 0.2, 0.8)) else if (st == "Paid") amount else 0,
         created_at = created, payment_date = if (all_paid) max(paid_at[ch]) else as.POSIXct(NA),
         user_id = pick(do_staff$id, 1), request_ids = requests$id[ch], client = client_idx[ch][1], short_ids = short_id[ch])
  })
})
bills <- unlist(bills, recursive = FALSE)
n_bills <- length(bills)
bills_df <- data.frame(
  id = sapply(bills, `[[`, "id"), is_actual = TRUE, type = "Charterer", status = sapply(bills, `[[`, "status"),
  number_doc = sprintf("СЧ-%06d", seq_len(n_bills)),
  invoice_amount = sapply(bills, `[[`, "invoice_amount"), paid_amount = sapply(bills, `[[`, "paid_amount"),
  created_at_original = as.POSIXct(sapply(bills, function(b) as.numeric(b$created_at)), origin = "1970-01-01", tz = "UTC"),
  user_id = sapply(bills, `[[`, "user_id"),
  request_ids = sapply(bills, function(b) paste0("{", paste(b$request_ids, collapse = ","), "}")),
  stringsAsFactors = FALSE
)
bills_df$created_at <- pmin(bills_df$created_at_original + runif(n_bills, 0, 2) * 3600, now_utc)
bills_df$payment_date <- as.POSIXct(sapply(bills, function(b) as.numeric(b$payment_date)), origin = "1970-01-01", tz = "UTC")
# У оплаченного счёта все предшествующие фактические события уже завершены.
paid_bills <- bills_df$status == "Paid"
bills_df$created_at[paid_bills] <- pmin(bills_df$created_at[paid_bills], bills_df$payment_date[paid_bills])
bill_client <- sapply(bills, `[[`, "client")
write_tbl("erp_bills", "bills", bills_df)
# номер счёта в заявке
bill_of_request <- rep(NA_character_, n_req)
for (b in seq_len(n_bills)) bill_of_request[match(bills[[b]]$request_ids, requests$id)] <- bills_df$number_doc[b]
dbWriteTable(con, "tmp_bill_num", data.frame(id = requests$id, number_doc = bill_of_request, stringsAsFactors = FALSE)[!is.na(bill_of_request), ], overwrite = TRUE)
dbExecute(con, "UPDATE erp_requests.requests r SET charterer_bill_info_number_doc = t.number_doc FROM tmp_bill_num t WHERE r.id = t.id")
dbExecute(con, "DROP TABLE tmp_bill_num")

# факторинг ~10% счетов
fact_idx <- which(runif(n_bills) < 0.1)
write_tbl("erp_bills", "factoring_of_receivables", data.frame(id = gen_uuid(length(fact_idx)), date = pmin(bills_df$created_at[fact_idx] + rdays(length(fact_idx), 1, 5), now_utc), bill_ids = paste0("{", bills_df$id[fact_idx], "}")))

# пакеты документов (старый документооборот): один пакет на счёт
grace_b <- clients$grace_days[bill_client]
verif_b <- clients$verif_days[bill_client]
pk_send <- bills_df$created_at + rdays(n_bills, 1, 3)
pk_send[paid_bills] <- pmin(pk_send[paid_bills], bills_df$payment_date[paid_bills])
pk_deliv <- pk_send + rdays(n_bills, 2, 7)
pk_deliv[paid_bills] <- pmin(pk_deliv[paid_bills], bills_df$payment_date[paid_bills])
pk_verif <- pk_deliv + verif_b * 86400 + rdays(n_bills, -1, 3)
pk_verif[paid_bills] <- pmin(pk_verif[paid_bills], bills_df$payment_date[paid_bills])
pk_stage <- ifelse(bills_df$status == "Paid", "payed",
            ifelse(pk_send > now_utc, "formed", ifelse(pk_deliv > now_utc, "sent", ifelse(pk_verif > now_utc, "received", ifelse(runif(n_bills) < 0.12, "problem", "approved")))))
packets <- data.frame(
  id = gen_uuid(n_bills), version = 1L, is_actual = TRUE, status = pk_stage, created_by = bills_df$user_id, created_at = bills_df$created_at,
  planned_send_date = bills_df$created_at + 2 * 86400, send_date = as.POSIXct(ifelse(pk_stage != "formed", pk_send, NA), origin = "1970-01-01", tz = "UTC"),
  planned_delivery_date = bills_df$created_at + 7 * 86400, delivery_date = as.POSIXct(ifelse(!pk_stage %in% c("formed", "sent"), pk_deliv, NA), origin = "1970-01-01", tz = "UTC"),
  planned_verification_date = bills_df$created_at + (7 + verif_b) * 86400, verification_date = as.POSIXct(ifelse(pk_stage %in% c("approved", "payed", "problem"), pk_verif, NA), origin = "1970-01-01", tz = "UTC"),
  returned_date = pmin(as.POSIXct(ifelse(pk_stage == "problem", pk_verif + 86400, NA), origin = "1970-01-01", tz = "UTC"), now_utc),
  stringsAsFactors = FALSE
)
write_tbl("erp_documents_packets", "documents_packets", packets)
write_tbl("erp_documents_packets", "document_packets_requests", do.call(rbind, lapply(seq_len(n_bills), function(b) data.frame(id = bills[[b]]$request_ids, documents_packet_id = packets$id[b], documents_packet_version = 1L))))

# новый документооборот (document_flow): счета за последние 150 дней
nf_idx <- which(bills_df$created_at > now_utc - 150 * 86400)
nf_status <- c(formed = "Formed", sent = "Sent", received = "Delivered", approved = "Approved", payed = "Paid", problem = "Problem")[pk_stage[nf_idx]]
write_tbl("document_flow", "bills", data.frame(
  id = bills_df$id[nf_idx], is_actual = TRUE, is_paid = bills_df$status[nf_idx] == "Paid", status = unname(nf_status),
  number = bills_df$number_doc[nf_idx], amount = bills_df$invoice_amount[nf_idx],
  client_id = clients$id[bill_client[nf_idx]], client_juridical_person_short_title = clients$short_title[bill_client[nf_idx]],
  client_juridical_person_responsible_for_bill_processing_id = clients$accounts_id[bill_client[nf_idx]], client_account_manager_id = clients$manager_id[bill_client[nf_idx]],
  formed_at = bills_df$created_at[nf_idx], planned_send_date = packets$planned_send_date[nf_idx], send_date = packets$send_date[nf_idx],
  planned_delivery_date = packets$planned_delivery_date[nf_idx], delivery_date = packets$delivery_date[nf_idx],
  planned_verification_date = packets$planned_verification_date[nf_idx], verification_date = packets$verification_date[nf_idx],
  return_date = packets$returned_date[nf_idx],
  planned_payment_date = as.Date(packets$delivery_date[nf_idx]) + grace_b[nf_idx],
  request_ids = bills_df$request_ids[nf_idx], stringsAsFactors = FALSE))
write_tbl("document_flow", "requests", do.call(rbind, lapply(nf_idx, function(b) data.frame(bill_id = bills[[b]]$id, short_id = bills[[b]]$short_ids, client_rate = rate[match(bills[[b]]$request_ids, requests$id)], is_actual = TRUE))))

# акты перевозчиков
act_idx <- which(!is.na(transportations$carrier_certificate_info_number))
write_tbl("erp_bills", "certificates", data.frame(
  is_actual = TRUE, type = "Carrier", number = transportations$carrier_certificate_info_number[act_idx],
  created_at_original = transportations$carrier_certificate_info_created_at[act_idx], created_at = pmin(transportations$carrier_certificate_info_created_at[act_idx] + 3600, now_utc),
  ready_at = transportations$carrier_certificate_info_ready_at[act_idx], user_title = pick(do_staff$title, length(act_idx)),
  request_ids = paste0("{", transportations$request_id[act_idx], "}")))

# ------------------------------------------------------------------ tenders (public.orders)
n_orders <- 1500
src <- pick(c("inbev", "transporeon", "uniconf", "pepsico", "baltika", "torgtrans", "mosbrew"), n_orders, prob = c(4, 3, 2, 2, 1.5, 1.5, 1))
charterer_title <- ifelse(src == "inbev", "Клиент 01",
                   ifelse(src == "transporeon", pick(c("Клиент 02", "Клиент 03", "Клиент 04", "Клиент 05"), n_orders, prob = c(3, 2, 2, 1)),
                   ifelse(src == "uniconf", "Клиент 06",
                   ifelse(src == "pepsico", "Клиент 07", ifelse(src == "baltika", "Клиент 08",
                   ifelse(src == "torgtrans", pick(c('Клиент 09', 'Клиент 10', 'Клиент 11', 'Клиент 12'), n_orders), "Клиент 13"))))))
o_created <- now_utc - rdays(n_orders, -3, 400)
o_created[o_created > now_utc] <- now_utc - runif(sum(o_created > now_utc), 1, 48) * 3600
o_start <- pick(seq_len(nrow(regions)), n_orders, prob = regions$weight)
o_finish <- pick(seq_len(nrow(regions)), n_orders, prob = regions$weight)
o_finish[o_finish == o_start] <- (o_finish[o_finish == o_start] %% nrow(regions)) + 1
o_load <- o_created + rdays(n_orders, 1, 6)
o_unload <- o_load + (region_dist(o_start, o_finish) / 55 + runif(n_orders, 8, 30)) * 3600
# зеркальные пары активных заявок для поиска кругорейсов
o_future <- which(o_load > now_utc + 86400)
oa <- sample(o_future, min(25, length(o_future) %/% 2)); ob <- setdiff(o_future, oa)[seq_along(oa)]
o_start[ob] <- o_finish[oa]; o_finish[ob] <- o_start[oa]; o_load[ob] <- o_unload[oa] + runif(length(ob), 2, 20) * 3600; o_unload[ob] <- o_load[ob] + 20 * 3600
o_status <- ifelse(o_load > now_utc, pick(c("ACTIVE", "ACTIVE", "CREATED", "REPLY_MATCHED"), n_orders), pick(c("REPLY_MATCHED", "LOST", "CANCELLED", "UNAVAILABLE", "UNDEFINED"), n_orders, prob = c(4, 4, 1, 0.5, 0.3)))
o_body <- pick(body_keys, n_orders, prob = c(6, 3, 1, 1, 0.5)); o_body[ob] <- o_body[oa]
o_cap <- pick(cap_keys, n_orders, prob = c(5, 3, 1, 1, 0.5, 0.3)); o_cap[ob] <- o_cap[oa]
o_price <- round((17000 + 36 * region_dist(o_start, o_finish)) * runif(n_orders, 0.85, 1.2), -2)
o_matched <- ifelse(o_status == "REPLY_MATCHED", round(o_price * runif(n_orders, 0.9, 1.05), -2), NA)
target <- function(r, when, when2) sprintf('{"address": {"locality": %s, "full_formatted": %s, "dadata": {"region_with_type": %s, "geo_lat": "%.5f", "geo_lon": "%.5f"}}, "visit_windows": [{"from": "%s", "to": "%s"}]}',
                                           jstr(regions$city[r]), jstr(paste0("г. ", regions$city[r], ", ", pick(streets, length(r)), ", д. ", sample(1:90, length(r), TRUE))), jstr(regions$region[r]),
                                           regions$lat[r] + rnorm(length(r), 0, 0.08), regions$lon[r] + rnorm(length(r), 0, 0.12), fmt_iso_z(when), fmt_iso_z(when2))
orders <- data.frame(
  id = gen_uuid(n_orders), source_name = src,
  external_id = sprintf("%s-%06d", toupper(substr(src, 1, 3)), sample(1e6, n_orders)),
  status = o_status, trade_type = pick(c("AUCTION", "FIXED"), n_orders, prob = c(7, 3)),
  created_at = o_created, created_on_source_at = o_created - runif(n_orders, 0, 2) * 3600,
  start_trading_on_source_at = as.POSIXct(ifelse(runif(n_orders) < 0.7, o_created + 600, NA), origin = "1970-01-01", tz = "UTC"),
  charterer = paste0('{"title": ', jstr(charterer_title), '}'),
  base_price_net = o_price, matched_price_net = o_matched, base_price_gross = round(o_price * 1.2), matched_price_gross = round(o_matched * 1.2),
  targets = paste0("[", target(o_start, o_load, o_load + 4 * 3600), ", ", target(o_finish, o_unload, o_unload + 6 * 3600), "]"),
  vehicle_requirements = sprintf('{"types_of_body": ["%s"], "payload_in_tons": %s}', o_body, cap_tons[o_cap]),
  cargos = sprintf('[{"type": "%s"}]', pick(cargo_keys, n_orders)),
  charterer_notes = ifelse(runif(n_orders) < 0.4, paste0('[{"text": ', jstr(pick(c("Обязательна лента", "Загрузка по записи", "Нужен пропуск", "Паллеты обменные"), n_orders)), '}]'), "[]"),
  stringsAsFactors = FALSE
)
orders$external_id_params <- ifelse(src == "inbev", sprintf('{"tnx_cargo_order_name": "%s"}', orders$external_id),
                             ifelse(src == "baltika", sprintf('{"baltika_lot_pickup_list_number": "%s"}', orders$external_id), "{}"))
write_tbl("public", "orders", orders)

dbExecute(con, "ANALYZE")
dbDisconnect(con)
message("готово")
