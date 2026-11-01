-- Демо-схема: реконструкция таблиц ERP по SQL-запросам из helpers.R.
-- В продакшене данные жили в четырёх базах PostgreSQL (prod_erp, public_profiles,
-- prod_tenders, prod_auth) и в Elasticsearch. Для демо всё собрано в одной базе:
--   * схемы erp_* и document_flow  — ERP (con)
--   * схема profiles               — профили водителей (con1)
--   * public.orders                — заявки с тендерных площадок (con2)
--   * public."Users" и др.         — сервис авторизации (con3)
--   * схема search_index           — заглушка индексов Elasticsearch
-- Идентификаторы хранятся как text (uuid-строки): часть запросов сравнивает их
-- без приведения типов со значениями из jsonb (->>), что с типом uuid не работает.

DROP SCHEMA IF EXISTS erp_requests, erp_users, erp_bills, document_flow, erp_clients,
  erp_juridical_persons, erp_documents_packets, erp_carriers, erp_vehicles, erp_addresses,
  profiles, search_index CASCADE;
DROP TABLE IF EXISTS public.orders, public."Users", public."Roles", public."RelationOfUserAndRole";

CREATE SCHEMA erp_requests;
CREATE SCHEMA erp_users;
CREATE SCHEMA erp_bills;
CREATE SCHEMA document_flow;
CREATE SCHEMA erp_clients;
CREATE SCHEMA erp_juridical_persons;
CREATE SCHEMA erp_documents_packets;
CREATE SCHEMA erp_carriers;
CREATE SCHEMA erp_vehicles;
CREATE SCHEMA erp_addresses;
CREATE SCHEMA profiles;
CREATE SCHEMA search_index;

-- ---------------------------------------------------------------- users
CREATE TABLE erp_users.user_profiles (
  original_id text NOT NULL,
  version     integer NOT NULL DEFAULT 1,
  is_actual   boolean NOT NULL DEFAULT true,
  title       text NOT NULL
);

-- ---------------------------------------------------------------- clients
CREATE TABLE erp_clients.clients (
  id            text NOT NULL,
  version       integer NOT NULL,
  is_actual     boolean NOT NULL,
  signed_by_uid text
);

CREATE TABLE erp_clients.juridicals (
  id                                          text NOT NULL,
  client_id                                   text NOT NULL,
  client_version                              integer NOT NULL,
  juridical_person_id                         text NOT NULL,
  juridical_person_version                    integer NOT NULL,
  grace_of_payments_days                      integer,
  tsd_settings_doc_verification_days          integer,
  responsible_employee_processing_doc_id      text,
  responsible_employee_processing_accounts_id text,
  responsible_assistant_id                    text,
  grace_of_payment_grace_period_days          integer,
  grace_of_payment_hidden_grace_period_days   integer
);

CREATE TABLE erp_juridical_persons.juridical_persons (
  id                text NOT NULL,
  version           integer NOT NULL,
  is_actual         boolean NOT NULL,
  short_title       text NOT NULL,
  agreement         jsonb,
  inn               text,
  juridical_address text
);

-- ---------------------------------------------------------------- addresses
CREATE TABLE erp_addresses.addresses (
  id      text NOT NULL,
  version integer NOT NULL,
  type    text
);

-- ---------------------------------------------------------------- requests
CREATE TABLE erp_requests.requests (
  id                                                text NOT NULL,
  is_actual                                         boolean NOT NULL DEFAULT true,
  status                                            text NOT NULL,
  created_at                                        timestamptz NOT NULL,
  created_by                                        text,
  short_id                                          text NOT NULL,
  relative_request_short_id                         text,
  external_number                                   text,
  type                                              text,
  charterer_version_id                              text,
  charterer_version_title                           text,
  charterer_version_signed_by_uid                   text,
  charterer_version_doc_received_days               integer,
  charterer_description                             text,
  charterer_rate_amount                             numeric,
  charterer_rate_with_nds                           boolean,
  charterer_bill_info_number_doc                    text,
  charterer_certificate_info_number                 text,
  charterer_certificate_info_created_at             timestamptz,
  charterer_certificate_info_ready_at               timestamptz,
  juridical_person_version_id                       text,
  juridical_person_version_short_title              text,
  responsible_expeditor_id                          text,
  responsible_expeditor_title                       text,
  responsible_expeditor_division                    text,
  vehicle_requirements_characteristic_preset        text,
  vehicle_requirements_types_of_body                text[],
  vehicle_requirements_carrying_capacity_and_volume text,
  cargo_cargo_type                                  text,
  cargo_brutto_in_tonns                             numeric,
  cargo_volume_in_cubometres                        numeric,
  cargo_cost                                        numeric,
  trip_comment                                      text,
  notes                                             jsonb,
  tsd                                               jsonb,
  statuses_history                                  jsonb,
  cancellation_info_has_complaint_work              boolean,
  shipping_documents_info_planned_doc_received_date timestamptz,
  shipping_documents_info_docs_received_date        timestamptz
);

CREATE TABLE erp_requests.transportations (
  request_id                                         text NOT NULL,
  status                                             text NOT NULL,
  created_at                                         timestamptz NOT NULL,
  created_by                                         text,
  statuses_history                                   jsonb,
  docs_scan_status                                   text,
  docs_scan_statuses_history                         jsonb,
  carrier_version_id                                 text,
  carrier_version_title                              text,
  carrier_version_framework_agreement_number         text,
  juridical_person_version_id                        text,
  juridical_person_version_short_title               text,
  carrier_rate_info_amount                           numeric,
  carrier_rate_info_original_amount                  numeric,
  carrier_rate_info_with_nds                         boolean,
  carrier_rate_info_commission_in_percent            numeric,
  prepayment_amount                                  numeric,
  prepayment_type                                    text,
  profit_amount                                      numeric,
  grace_period                                       integer,
  carrier_grace_of_payment_finish_grace_period_date  timestamptz,
  carrier_certificate_info_number                    text,
  carrier_certificate_info_created_at                timestamptz,
  carrier_certificate_info_ready_at                  timestamptz,
  carrier_bill_info_paid_at                          timestamptz
);

CREATE TABLE erp_requests.transportation_documents (
  transportation_request_id text NOT NULL,
  category                  text NOT NULL
);

CREATE TABLE erp_requests.request_start_route_points (
  id                      text NOT NULL,
  request_id              text NOT NULL,
  locality                text,
  address                 text,
  region                  text,
  "from"                  timestamptz,
  fact_arrival_date_time  timestamptz,
  latitude                numeric,
  longitude               numeric,
  address_id              text,
  address_version         integer
);

CREATE TABLE erp_requests.request_finish_route_points (
  id                        text NOT NULL,
  request_id                text NOT NULL,
  locality                  text,
  address                   text,
  region                    text,
  consignee                 text,
  "from"                    timestamptz,
  fact_arrival_date_time    timestamptz,
  fact_departure_date_time  timestamptz,
  latitude                  numeric,
  longitude                 numeric,
  address_id                text,
  address_version           integer
);

CREATE TABLE erp_requests.transportation_driver_versions (
  transportation_request_id text NOT NULL,
  full_name                 text,
  phone_numbers             text
);

CREATE TABLE erp_requests.transportation_vehicle_versions (
  transportation_request_id text NOT NULL,
  version_id                text,
  type                      text,
  title                     text,
  number_plate              text
);

-- ---------------------------------------------------------------- bills
CREATE TABLE erp_bills.bills (
  id                  text NOT NULL,
  is_actual           boolean NOT NULL DEFAULT true,
  type                text NOT NULL,
  status              text,
  number_doc          text,
  invoice_amount      numeric,
  paid_amount         numeric,
  created_at_original timestamptz,
  created_at          timestamptz,
  payment_date        timestamptz,
  user_id             text,
  request_ids         text[]
);

CREATE TABLE erp_bills.certificates (
  is_actual           boolean NOT NULL DEFAULT true,
  type                text NOT NULL,
  number              text,
  created_at_original timestamptz,
  created_at          timestamptz,
  ready_at            timestamptz,
  user_title          text,
  request_ids         text[]
);

CREATE TABLE erp_bills.factoring_of_receivables (
  id       text NOT NULL,
  date     timestamptz,
  bill_ids uuid[]   -- в запросах сравнивается с bills.id::uuid
);

-- ---------------------------------------------------------------- document packets (old flow)
CREATE TABLE erp_documents_packets.documents_packets (
  id                        text NOT NULL,
  version                   integer NOT NULL,
  is_actual                 boolean NOT NULL,
  status                    text,
  created_by                text,
  created_at                timestamptz,
  planned_send_date         timestamptz,
  send_date                 timestamptz,
  planned_delivery_date     timestamptz,
  delivery_date             timestamptz,
  planned_verification_date timestamptz,
  verification_date         timestamptz,
  returned_date             timestamptz
);

CREATE TABLE erp_documents_packets.document_packets_requests (
  id                       text NOT NULL,   -- = requests.id
  documents_packet_id      text NOT NULL,
  documents_packet_version integer NOT NULL
);

-- ---------------------------------------------------------------- document flow (new flow)
CREATE TABLE document_flow.bills (
  id                                                 text NOT NULL,
  is_actual                                          boolean NOT NULL DEFAULT true,
  is_paid                                            boolean NOT NULL DEFAULT false,
  status                                             text,
  number                                             text,
  amount                                             numeric,
  client_id                                          text,
  client_juridical_person_short_title                text,
  client_juridical_person_responsible_for_bill_processing_id text,
  client_account_manager_id                          text,
  formed_at                                          timestamptz,
  planned_send_date                                  timestamptz,
  send_date                                          timestamptz,
  planned_delivery_date                              timestamptz,
  delivery_date                                      timestamptz,
  planned_verification_date                          timestamptz,
  verification_date                                  timestamptz,
  return_date                                        timestamptz,
  planned_payment_date                               date,
  request_ids                                        text[]
);

CREATE TABLE document_flow.requests (
  bill_id     text,
  short_id    text,
  client_rate numeric,
  is_actual   boolean NOT NULL DEFAULT true
);

-- ---------------------------------------------------------------- carriers / vehicles / drivers
CREATE TABLE erp_carriers.carriers (
  id                           text NOT NULL,
  version                      integer NOT NULL,
  is_actual                    boolean NOT NULL,
  title                        text,
  created_at                   timestamptz,
  created_by                   text,
  updated_at                   timestamptz,
  updated_by                   text,
  security_status_status_value text,
  security_status_updated_at   timestamptz,
  security_status_user_id      text,
  security_status_comment      text
);

CREATE TABLE erp_vehicles.vehicles (
  id                     text NOT NULL,
  version                integer NOT NULL,
  is_actual              boolean NOT NULL,
  vehicle_passport_brand text,
  vehicle_passport_model text,
  ownership_type         text,
  created_at             timestamptz,
  created_by             text,
  updated_at             timestamptz,
  updated_by             text
);

CREATE TABLE profiles.drivers (
  id                          text NOT NULL,
  version                     integer NOT NULL,
  is_deleted                  boolean NOT NULL DEFAULT false,
  passport_last_name          text,
  passport_first_name         text,
  passport_middle_name        text,
  created_at                  bigint,   -- epoch, миллисекунды
  created_by                  text,
  updated_at                  bigint,
  last_updated_by             text,
  security_status_value       integer,
  security_status_modified_at bigint,
  security_status_modified_by text,
  security_status_comment     text
);

CREATE TABLE profiles.archived_drivers (LIKE profiles.drivers);

-- ---------------------------------------------------------------- tenders (con2) and auth (con3)
CREATE TABLE public.orders (
  id                         text NOT NULL,
  source_name                text,
  external_id                text,
  external_id_params         jsonb,
  status                     text,
  trade_type                 text,
  created_at                 timestamptz NOT NULL,
  created_on_source_at       timestamptz,
  start_trading_on_source_at timestamptz,
  charterer                  jsonb,
  base_price_net             numeric,
  matched_price_net          numeric,
  base_price_gross           numeric,
  matched_price_gross        numeric,
  targets                    jsonb,
  vehicle_requirements       jsonb,
  cargos                     jsonb,
  charterer_notes            jsonb
);

CREATE TABLE public."Users" (
  "Id"          text NOT NULL,
  "Description" text,
  "IsArchived"  boolean NOT NULL DEFAULT false
);
CREATE TABLE public."Roles" (
  "Id"   text NOT NULL,
  "Name" text
);
CREATE TABLE public."RelationOfUserAndRole" (
  "UserId" text NOT NULL,
  "RoleId" text NOT NULL
);

-- ---------------------------------------------------------------- Elasticsearch stub
-- Одна таблица на индекс; имена колонок = «сплющенные» поля документов.
CREATE TABLE search_index."user-profile-version-index" (
  "data.id"    text,
  "data.title" text
);
CREATE TABLE search_index."expeditors-group-version-index" (
  "data.id"   text,
  "data.name" text
);
CREATE TABLE search_index."glossary-element-version-index" (
  "data.key"   text,
  "data.title" text,
  "data.type"  text
);
CREATE TABLE search_index."charterer-version-index" (
  "data.id"                   text,
  "data.docVerification.days" integer
);
CREATE TABLE search_index."carrier-version-index" (
  "data.id"                                text,
  "data.createdAt"                         text,
  "data.type"                              text,
  "data.securityStatus.dateLastUpdated"    text,
  "data.securityStatus.statusValue"        integer
);
CREATE TABLE search_index."vehicle-version-index" (
  "id"                 text,
  "data.typesOfBody"   text,
  "data.payload"       numeric,
  "data.cubicCapacity" numeric
);
-- Справочник «логист → группа → должность» (в оригинале был захардкожен в helpers.R)
CREATE TABLE search_index.logist_groups (
  "data.id"   text,   -- ФИО логиста
  "data.name" text,   -- группа
  "position"  text    -- должность
);

-- ---------------------------------------------------------------- indexes (для скорости)
CREATE INDEX ON erp_requests.requests (id);
CREATE INDEX ON erp_requests.requests (short_id);
CREATE INDEX ON erp_requests.requests (created_at);
CREATE INDEX ON erp_requests.transportations (request_id);
CREATE INDEX ON erp_requests.request_start_route_points (request_id);
CREATE INDEX ON erp_requests.request_finish_route_points (request_id);
CREATE INDEX ON erp_requests.transportation_documents (transportation_request_id);
CREATE INDEX ON erp_requests.transportation_vehicle_versions (transportation_request_id);
CREATE INDEX ON erp_requests.transportation_driver_versions (transportation_request_id);
CREATE INDEX ON erp_bills.bills USING gin (request_ids);
CREATE INDEX ON erp_documents_packets.document_packets_requests (id);
CREATE INDEX ON document_flow.bills (id);
