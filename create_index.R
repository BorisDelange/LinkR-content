tibble::tibble(file = list.files("home/news/")) |> readr::write_csv("home/news/index.csv")
