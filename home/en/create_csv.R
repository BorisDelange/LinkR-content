# EN

library(magrittr)

tibble::tribble(
  ~page, ~type, ~category, ~key, ~title, ~markdown_file, ~display_order, ~datetime,
  
  # 1) Home
  
  ## Pivot_items
  "home", "pivot_item", "overview", "overview", "Overview", "", 1L, "2023-04-10 08:00:00",
  "home", "pivot_item", "news", "news", "News", "", 2L, "2023-04-10 08:00:00",
  "home", "pivot_item", "versions", "versions", "Versions", "", 3L, "2023-04-10 08:00:00",
  
  ## Cards
  "home", "card", "overview", "overview", "", "2023-05-29_overview.Md", 1L, "2023-05-29 08:00:00",
  
  
  # 2) Get started
  
  ## Pivot_items
  "get_started", "pivot_item", "default_data", "default_data", "Default data", "", 1L, "2023-06-05 00:00:00",
  "get_started", "pivot_item", "first_study", "first_study", "Your first study with LinkR", "", 1L, "2023-06-05 00:00:00",
  
  # 3) Tutorials
  
  
  # 4) Resources
  
) %>%
  readr::write_csv("home/en/tabs_and_cards.csv")