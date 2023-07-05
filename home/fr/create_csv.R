# FR

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
  
  #"home", "card", "news", "news1", "News 1", "", 1L, "2023-04-10 08:00:00",
  #"home", "card", "news", "news2", "News 2", "", 2L, "2023-04-10 08:00:00",
 
  
  # 2) Get started
  
  ## Pivot_items
  "get_started", "pivot_item", "app_structure", "app_structure", "Structure de l'application", "", 1L, "2023-07-05 00:00:00",
  "get_started", "pivot_item", "default_data", "default_data", "Données par défaut", "", 1L, "2023-06-05 00:00:00",
  "get_started", "pivot_item", "first_study", "first_study", "Votre première étude", "", 1L, "2023-06-05 00:00:00",
  # "get_started", "pivot_item", "vocabulary", "vocabulary", "Terminologies & concepts", "", 1L, "2023-06-05 00:00:00",
  
  ## Cards
  "get_started", "card", "app_structure", "div1", "1) Structure globale", "2023-07-05_global_structure.Md", 1L, "2023-07-05 00:00:01",
  "get_started", "card", "app_structure", "div2", "2) Accueil", "", 2L, "2023-07-05 00:00:00",
  "get_started", "card", "app_structure", "div3", "3) Données", "", 3L, "2023-07-05 00:00:00",
  "get_started", "card", "app_structure", "div4", "4) Terminologies", "", 4L, "2023-07-05 00:00:00",
  "get_started", "card", "app_structure", "div5", "5) Messages", "", 5L, "2023-07-05 00:00:00",
  "get_started", "card", "app_structure", "div6", "6) Scripts & plugins", "", 6L, "2023-07-05 00:00:00",
  "get_started", "card", "app_structure", "div7", "7) Paramètres", "", 7L, "2023-07-05 00:00:00",
  
  # 3) Tutorials
  
  ## Pivot_items
  # "tutorials", "pivot_item", "health_data_warehouses", "health_data_warehouses", "Entrepôts de données de santé", "", 1L, "2023-04-10 08:00:00",
  
  ## Cards
  # "tutorials", "card", "health_data_warehouses", "div1", "Div 1", "", 1L, "2023-04-10 08:00:00",
  
  
  # 4) Resources
  
  ## Pivot_items
  # "resources", "pivot_item", "learn_r", "learn_r", "Programmer en R", "", 2L, "2023-04-10 08:00:00",
  # "resources", "pivot_item", "health_data_warehouses", "health_data_warehouses", "Entrepôts de données de santé", "", 1L, "2023-04-10 08:00:00",
  # "resources", "pivot_item", "learn_sql", "learn_sql", "Programmer en SQL", "", 3L, "2023-04-10 08:00:00",
  # "resources", "pivot_item", "learn_python", "learn_python", "Programmer en Python", "", 4L, "2023-04-10 08:00:00",
  
  ## Cards
  # "resources", "card", "health_data_warehouses", "div1", "Div 1", "", 2L, "2023-04-10 08:00:00",
  # "resources", "card", "health_data_warehouses", "div2", "Div 2", "", 1L, "2023-04-10 08:00:00"
  
) %>%
  readr::write_csv("home/fr/tabs_and_cards.csv")