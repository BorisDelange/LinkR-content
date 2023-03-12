library(magrittr)

plugins_dir <- paste0(getwd(), "/plugins")

# XML file with all plugins

plugins_tibble <- tibble::tibble(app_version = character(), type = character(), version = character(), unique_id = character(),
  author = character(), image = character(), description_fr = character(), description_en = character(),
  name_fr = character(), name_en = character(), category_fr = character(), category_en = character())

# Convert XML to tibble

for (category in c("patient_lvl", "aggregated")){

  dirs <- list.dirs(paste0(plugins_dir, "/", category), full.names = TRUE)
  
  for (dir in dirs){
    if (dir != paste0(plugins_dir, "/", category)){
      plugins_tibble <-
        plugins_tibble %>%
        dplyr::bind_rows(
          xml2::read_xml(paste0(dir, "/plugin.xml")) %>%
          XML::xmlParse() %>%
          XML::xmlToDataFrame(nodes = XML::getNodeSet(., "//plugin")) %>%
          tibble::as_tibble()
        )
    }
  }
}

# Convert tibble to XML

plugins_xml <- XML::newXMLDoc()
plugins_node <- XML::newXMLNode("plugins", doc = plugins_xml)

plugins_nodes <- apply(plugins_tibble, 1, function(x) {
  plugin_node <- XML::newXMLNode("plugin")
  XML::addChildren(plugin_node, lapply(names(x), function(y) XML::newXMLNode(y, x[y])))
})

XML::xmlParent(plugins_nodes) <- plugins_node

XML::saveXML(plugins_xml, file = paste0(plugins_dir, "/plugins.xml"))
