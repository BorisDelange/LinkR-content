library(magrittr)

tibble::tribble(
  ~page, ~type, ~category, ~key, ~title, ~markdown_file, ~display_order, ~datetime,
  
  # 1) Home
  
  ## Pivot_items
  "home", "pivot_item", "overview", "overview", "Présentation", "", 1L, "2023-04-10 08:00:00",
  "home", "pivot_item", "news", "news", "News", "", 2L, "2023-04-10 08:00:00",
  "home", "pivot_item", "versions", "versions", "Versions", "", 3L, "2023-04-10 08:00:00",
  
  ## Cards
  "home", "card", "overview", "overview", "", "2023-05-29_overview.Md", 1L, "2023-05-29 08:00:00",
  
  "home", "card", "news", "news1", "News 1", "", 1L, "2023-04-10 08:00:00",
  "home", "card", "news", "news2", "News 2", "", 2L, "2023-04-10 08:00:00",
  
  
  # 2) Get started
  
  ## Pivot_items
  
  ## Cards
  
  
  # 3) Tutorials
  
  ## Pivot_items
  "tutorials", "pivot_item", "health_data_warehouses", "health_data_warehouses", "Entrepôts de données de santé", "", 1L, "2023-04-10 08:00:00",
  
  ## Cards
  "tutorials", "card", "health_data_warehouses", "div1", "Div 1", "", 1L, "2023-04-10 08:00:00",
  
  
  # 4) Resources
  
  ## Pivot_items
  "resources", "pivot_item", "learn_r", "learn_r", "Programmer en R", "", 2L, "2023-04-10 08:00:00",
  "resources", "pivot_item", "health_data_warehouses", "health_data_warehouses", "Entrepôts de données de santé", "", 1L, "2023-04-10 08:00:00",
  "resources", "pivot_item", "learn_sql", "learn_sql", "Programmer en SQL", "", 3L, "2023-04-10 08:00:00",
  "resources", "pivot_item", "learn_python", "learn_python", "Programmer en Python", "", 4L, "2023-04-10 08:00:00",
  
  ## Cards
  "resources", "card", "health_data_warehouses", "div1", "Div 1", "", 2L, "2023-04-10 08:00:00",
  "resources", "card", "health_data_warehouses", "div2", "Div 2", "", 1L, "2023-04-10 08:00:00"
  
) %>%
  readr::write_csv("home/en/tabs_and_cards.csv")
