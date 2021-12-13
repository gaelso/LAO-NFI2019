
## !!! Aborted plot data only found ONA !!! 

# ## LAO-NFI2019
# ## Rscripts for calculating national level carbon stock from the NFI 2019 in Lao PDR
# ## Initiated in March 2021
# 
# ## Downloading and preparing data stored in ONA
# ## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# 
# ## Create directory for raw data
# dir.create("data-raw", showWarnings = F)
# 
# ## get NFI table from ONA if not already downloaded
# if (!("ona_nfi.csv" %in% list.files("data-raw"))) {
#   
#   ## ONA files:
#   ## - NFI_2016_Inventory_v5 (636)
#   ## - NFI_20162017_Inventory_v1 (1586)
#   ## - NFI_20162017_QC (220)
#   ## - NFI3 (2018 / 1478 records)
#   ## - NFI3QC (2019 / 192 records)
#   
#   ## Download
#   NFI1 <- ona::onaDownload(
#     formName = 'NFI_2016_Inventory_v5', 
#     account = 'fipdlaos',
#     uname = 'fipdlaos',
#     pass = rstudioapi::askForSecret(name = "Pwd", message = "Password for ONA", title = "Password")
#     )
#   
#   NFI2 <- ona::onaDownload(
#     formName = 'NFI_20162017_Inventory_v1', 
#     account = 'fipdlaos',
#     uname = 'fipdlaos',
#     pass = rstudioapi::askForSecret(name = "Pwd", message = "Password for ONA", title = "Password")
#     )
#   
#   
#   NFI3 <- ona::onaDownload(
#     formName = 'NFI3', 
#     account = 'fipdlaos',
#     uname = 'fipdlaos',
#     pass = rstudioapi::askForSecret(name = "Pwd", message = "Password for ONA", title = "Password")
#     )
#   
#   ## Arrange
#   NFI1a <- data.frame(NFI1) %>% mutate(plot_info.plot_code_nmbr = as.character(plot_info.plot_code_nmbr))
#   NFI2a <- data.frame(NFI2) %>% mutate(plot_info.plot_code_nmbr = as.character(plot_info.plot_code_nmbr))
#   NFI3a <- data.frame(NFI3) %>% mutate(plot_info.plot_code_nmbr = as.character(plot_info.plot_code_nmbr))
#   
#   str(NFI1a)
#   ona_nfi <- dplyr::bind_rows(data.frame(NFI1a), data.frame(NFI2a), data.frame(NFI3a), .id = "file")
#   
#   str(ona_nfi)
#   write.csv(ona_nfi, file = "data-raw/ona_nfi.csv", row.names = FALSE)
#   rm(NFI1, NFI2, NFI3, NFI1a, NFI2a, NFI3a, ona_nfi)
#   
# }
# 
# ## Read tables
# ona_nfi <- read.csv("data-raw/ona_nfi.csv", stringsAsFactors = FALSE) %>% as_tibble()
# 
# ## Check duplicates
# tt <- ona_nfi %>%
#   select(-file) %>%
#   distinct()
# 
# ## Split tables
# names(ona_nfi)
# 
# tt <- as_tibble(names(ona_nfi))
# 
# table_lvl1 <- str_remove(tt, "\\..*") %>% unique()
# unique(ona_nfi$deviceid)
# 
# lc_class <- ona_nfi %>% select(starts_with("lc_data"))
# 
# unique(lc_class)
# 
# plot_gps <- ona_nfi %>% select(starts_with("plot_GPS"))
# plot_gps
# 
# plot_info  <-  ona_nfi %>% select(starts_with("plot_info"))
# names(plot_info) <- names(plot_info) %>% str_remove(".*\\.")
# 
# plot_info2 <- plot_info %>%
#   mutate(
#     file    = ona_nfi$file,
#     plot_id = paste0(file, "_", str_squish(plot_code_nmbr)),
#     subplot_no = str_remove(sub_plot, "sub_plot"),
#     subplot_id = paste0(plot_id, "_", subplot_no)) %>%
#   arrange(plot_id, subplot_id)
# 
# length(unique(plot_info2$subplot_id))
# 
# plot_info2
# 
# 
# plot_info_nfi3 <- plot_info2 %>%
#   filter(file == 3)
# 
# 
# 
# 
# ###
# 
# tree_intro1 <- ona_nfi %>% select(starts_with("tree_intro_nest1"))
# 
# unique(tree_intro1)
# 
# tree_data1 <- ona_nfi %>% select(starts_with("tree_data_nest1"))
# 
# tree_data2 <- ona_nfi %>% select(starts_with("tree_data_nest2"))
# 
# 
