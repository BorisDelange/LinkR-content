library(magrittr)

tibble::tribble(
  ~page, ~type, ~category, ~key, ~title, ~markdown_file, ~display_order, ~datetime,
  
  "home", "pivot_item", "overview", "overview", "Présentation", "", 1L, "2023-04-10 08:00:00",
  "home", "pivot_item", "news", "news", "News", "", 2L, "2023-04-10 08:00:00",
  "home", "pivot_item", "versions", "versions", "Versions", "", 3L, "2023-04-10 08:00:00",
  
  "home", "card", "overview", "div1", "LinkR", "2023-04-10_overview.Md", 1L, "2023-04-10 08:00:00",
  
  "home", "card", "news", "news1", "News 1", "", 1L, "2023-04-10 08:00:00",
  "home", "card", "news", "news2", "News 2", "", 2L, "2023-04-10 08:00:00",
  
  "resources", "pivot_item", "learn_r", "learn_r", "Programmer en R", "", 2L, "2023-04-10 08:00:00",
  "resources", "pivot_item", "health_data_warehouses", "health_data_warehouses", "Entrepôts de données de santé", "", 1L, "2023-04-10 08:00:00",
  "resources", "pivot_item", "learn_sql", "learn_sql", "Programmer en SQL", "", 3L, "2023-04-10 08:00:00",
  "resources", "pivot_item", "learn_python", "learn_python", "Programmer en Python", "", 4L, "2023-04-10 08:00:00",
  "resources", "card", "health_data_warehouses", "div1", "Div 1", "", 2L, "2023-04-10 08:00:00",
  "resources", "card", "health_data_warehouses", "div2", "Div 2", "", 1L, "2023-04-10 08:00:00",
  "tutorials", "pivot_item", "health_data_warehouses", "health_data_warehouses", "Entrepôts de données de santé", "", 1L, "2023-04-10 08:00:00",
  "tutorials", "card", "health_data_warehouses", "div1", "Div 1", "", 1L, "2023-04-10 08:00:00",
) %>%
  readr::write_csv("home/fr/tabs_and_cards.csv")
