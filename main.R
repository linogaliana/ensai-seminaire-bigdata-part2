library(targets)

tar_make()

library(magick)
library(formattable)

source("./R/functions.R")

tar_load(c(wordcloud_relevanc_start, wordcloud_relevanc_clean))
tar_load(c(wordcloud_openfood_start, wordcloud_openfood_clean))
tm1 = tempfile()
tm2 = tempfile()
tm3 = tempfile()
tm4 = tempfile()
copy_s3(wordcloud_relevanc_start, "slides/img/wordcloud_relevanc_start.png", bucket = "projet-relevanc")
copy_s3(wordcloud_relevanc_clean, "slides/img/wordcloud_relevanc_clean.png", bucket = "projet-relevanc")
copy_s3(wordcloud_openfood_start, "slides/img/wordcloud_openfood_start.png", bucket = "projet-relevanc")
copy_s3(wordcloud_openfood_clean, "slides/img/wordcloud_openfood_clean.png", bucket = "projet-relevanc")


tar_load(table_share_CA)
dir.create("slides/input")
data.table::fwrite(table_share_CA, "slides/input/share_CA.csv")

tar_load(example_prediction_label)
data.table::fwrite(example_prediction_label, "slides/input/example_prediction_label.csv")

tar_load(tab_exemples_elastic)
data.table::fwrite(tab_exemples_elastic, "slides/input/tab_exemples_elastic.csv")



