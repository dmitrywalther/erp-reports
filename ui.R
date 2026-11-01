source("ui/revenue_profit_ui.R", encoding="utf-8")
source("ui/active_requests_ui.R", encoding="utf-8")
source("ui/logistic_KPI_ui.R", encoding="utf-8")
source("ui/DZ_ui.R", encoding="utf-8")
source("ui/DZ_dashboard_ui.R", encoding="utf-8")
source("ui/DZ_new_ui.R", encoding="utf-8")
source("ui/DO_ui.R", encoding="utf-8")
source("ui/requests_ui.R", encoding="utf-8")
source("ui/KZ_ui.R", encoding="utf-8")
source("ui/canceledOrders_ui.R", encoding="utf-8")
source("ui/canceledOrders_proportion_ui.R", encoding="utf-8")
source("ui/missingDocs_ui.R", encoding="utf-8")
source("ui/manager_plan_fact_ui.R", encoding="utf-8")
source("ui/logistician_plan_fact_ui.R", encoding="utf-8")
source("ui/problemDocs_ui.R", encoding="utf-8")
source("ui/carriers_ui.R", encoding="utf-8")
source("ui/security_ui.R", encoding="utf-8")
source("ui/security_status_ui.R", encoding="utf-8")
source("ui/spot_ui.R", encoding="utf-8")
source("ui/factoring_ui.R", encoding="utf-8")
source("ui/priceIndicative_ui.R", encoding="utf-8")
source("ui/count_requests_ui.R", encoding="utf-8")
source("ui/averageTime_ui.R", encoding="utf-8")
source("ui/drivers_carriers_vehicles_ui.R", encoding="utf-8")
source("ui/cash_ui.R", encoding="utf-8")
source("ui/specialDC_ui.R", encoding="utf-8")
source("ui/registersForInsurance_ui.R", encoding="utf-8")
source("ui/logisticMetrics_ui.R", encoding="utf-8")
source("ui/potentialDoublets_ui.R", encoding="utf-8")
source("ui/low_profitability_ui.R", encoding="utf-8")
source("ui/client_health_ui.R", encoding="utf-8")
source("ui/carrier_prepayment_ui.R", encoding="utf-8")

dashboardPage(
    title = "ERP Reports",
    skin = "blue",
    preloader = list(html = tagList(spin_1(), "Loading ..."), color = "#3c8dbc"),
    dashboardHeader(title = "ERP Reports",
                    tags$li(
                      class = "dropdown",
                      style = "padding: 8px;",
                      logoutUI("logout")
                    )
                    ),

              dashboardSidebar(

                collapsed = TRUE,

                sidebarMenuOutput("sidebarID")
              

              ),

              dashboardBody(
                  tags$head(
                      tags$link(rel = "stylesheet", type = "text/css", href = "my_style.css"),
                      tags$link(href = "https://fonts.googleapis.com/css?family=Karla:400,700|Fira+Mono&display=fallback", rel = "stylesheet")
                  ),
                  
                  if (DEMO_MODE) tags$div(class = "alert alert-info", role = "status",
                    "Демонстрационные данные: сотрудники, клиенты, рейсы и финансовые показатели вымышлены."),
                  loginUI("login"),

                  tabItems(
                    tabItem_revenue_profit,
                    tabItem_logistic_KPI,
                    carrier_prepayment_tab_item,
                    client_health_tabItem,
                    tabItem_active_requests,
                    DZ_tabName,
                    DZ_dashboard_tabItem,
                    DZ_new_tabItem,
                    DO_tabItem,
                    requests_tabItem,
                    KZ_tabName,
                    canceledOrders_tabItem,
                    canceledOrders_proportion_tabItem,
                    missingDocs_tabItem,
                    manager_plan_fact_tabName,
                    logistician_plan_fact_tabItem,
                    problemDocs_tabItem,
                    carriers_tabItem,
                    security_tabName,
                    security_status_tabName,
                    spot_tabItem,
                    factoring_tabName,
                    priceIndicative_tabItem,
                    count_requests_tabName,
                    averageTime_tabName,
                    drivers_carriers_vehicles_tabName,
                    cash_tabItem,
                    specialDC_tabName,
                    registersForInsurance_tabName,
                    logisticMetrics_tabName,
                    potentialDoublets_tabName,
                    low_profitability_tabName
                  )

              )
)


