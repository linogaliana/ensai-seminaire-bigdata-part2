library(ggplot2)
library(data.table)
library(formattable)
library(kableExtra)

set.seed(7839128)


write_s3 <- function(object, key = "temp/targets/test.csv", bucket = Sys.getenv("AWS_BUCKET_NAME")){
  aws.s3::s3write_using(object,
                        FUN = data.table::fwrite, object = key, bucket = bucket,
                        opts = list("region" = ""))
  return(NULL)
}

load_matching <- function(key, bucket = Sys.getenv("AWS_BUCKET_NAME")){
  return(
    aws.s3::s3read_using(FUN = arrow::read_parquet, object = key, bucket = bucket,
                         opts = list("region" = ""))
  )
}

decrypt_load <- function(file, fernet_key){
  dat <- cyphr::decrypt(data.table::fread(file), fernet_key)
  return(dat)
}

read_parquet_dt <- function(path, bucket = "w3crk9", ...){
  return(
    data.table::as.data.table(
      aws.s3:::s3read_using(FUN = arrow::read_parquet,
                            ...,
                            object = path,
                            bucket = bucket,
                            opts = list(region = ""))
    )
  )
  #data.table::as.data.table(arrow::read_parquet(path))
}

write_parquet_dt <- function(x, path, bucket = "w3crk9", ...){
  aws.s3::s3write_using(
    x, FUN = arrow::write_parquet, ..., object = path,
    bucket = bucket, 
    opts = list(region = "")
  )
  return(NULL)
}

copy_s3 <- function(data, bucket = "w3crk9", tmp = NULL) {
  if (is.null(tmp)) tmp = tempfile()
  aws.s3::save_object(object = data, bucket = bucket, region = "", file = tmp)
}

match_output_ca_produits <- function(magasin, data_linkage){
  
  ca_franprix = read_parquet_dt(sprintf(
    "data/agg_from_pg/ca_produits_%s.parquet", magasin)
  )
  
  tempdf <- data.table::copy(data_linkage)
  tempdf[,'code_ean' := sub("^0+", "", as.character(code_ean))]
  
  data.table::setnames(ca_franprix, old = colnames(ca_franprix), new = paste0(colnames(ca_franprix), "_", magasin))
  
  essai = merge(tempdf, ca_franprix, 
                by.x = c('LIBELLE', 'code_ean'),
                by.y = paste0(c("LIBELLE_", 'code_ean_'), magasin),
                all.x = TRUE
  )
  
  data.table::setnames(essai, old = paste0("YEAR_", magasin), new = "YEAR")
  
  cols_magasin = colnames(essai)[grepl(colnames(essai), pattern = paste0("id_", magasin))]
  essai[, num_obs := rowSums(!is.na(.SD)), .SDcols = cols_magasin]
  essai = essai[num_obs>1]
  
  essai[, num_obs := NULL]
  essai[,'YEAR' := as.numeric(YEAR)]
  
  return(essai)
}


wide_to_long_revenue <- function(data_magasin, magasin = "franprix"){
  
  cols <- colnames(data_magasin)[!grepl(colnames(data_magasin), pattern = '(monoprix|franprix|casino)$')]
  
  df = data.table::melt(
    data_magasin[,.SD,.SDcols = c(cols, paste0(c("CA_", "produits_vendus_","RAYON_"), magasin))],
    id.vars = c(cols, paste0("RAYON_", magasin)),
    measure.vars = paste0(c("CA_", "produits_vendus_"), magasin)
  )
  df = na.omit(df, cols = "value")
  df[, 'variable' := gsub(paste0("_", magasin), "", get('variable'))]
  df[,'enseigne' := magasin]
  data.table::setnames(df, old = paste0('RAYON_',magasin), new = "RAYON")
  return(df)
}

restructure_data_linkage <- function(dt){
  caracteristics <- colnames(dt)[grepl(pattern = "_step", x = colnames(dt))]
  caracteristics <- gsub("_step", "", caracteristics)
  
  pivot_cols <- colnames(dt)[!grepl(pattern = paste0("(", paste(caracteristics, collapse = "|"), ")"),
                                    colnames(dt))]
  
  df_simple <- dt[,.SD,.SDcols = c(pivot_cols,
                                   paste0(caracteristics, "_step")#,
                                   # caracteristics
  )]
  
  
  df_long <- data.table::melt(
    df_simple, id.vars = pivot_cols,
    variable.name = "caracteristic", value.name = "step",
  )
  
  return(df_long)
}

create_data_stacked_plot <- function(dt, stat = c("number_products","revenue"), byvar = NULL){
  
  stat <- match.arg(stat)
  dt <- data.table::copy(dt)
  
  caracteristics <- colnames(dt)[grepl(pattern = "_step", x = colnames(dt))]
  caracteristics <- gsub("_step", "", caracteristics)
  
  pivot_cols <- colnames(dt)[!grepl(pattern = paste0("(", paste(caracteristics, collapse = "|"), ")"),
                                    colnames(dt))]
  
  df_long <- restructure_data_linkage(dt)
  
  if (stat == "revenue"){
    fp_long = wide_to_long_revenue(df_long)
    monop_long = wide_to_long_revenue(df_long, "monoprix")
    casino_long = wide_to_long_revenue(df_long, "casino")
    df_long = data.table::rbindlist(
      list(fp_long, monop_long, casino_long)
    )
    
  }
  
  
  df_long[, c('caracteristic') := gsub("_100g_step","", get("caracteristic"))]
  
  
  levels_agg <- c("caracteristic","step")
  if ("YEAR" %in% colnames(df_long)) levels_agg <- c(levels_agg, "YEAR")
  levels_agg <- c(levels_agg, byvar)
  
  if (stat=="number_products"){
    df_share <- df_long[,.(N = as.numeric(.N)), by = levels_agg]
  } else{
    levels_agg <- c(levels_agg, c("enseigne", "variable", "RAYON"))
    df_share <- df_long[,.(N = sum(get("value"))), by = levels_agg]
  }
  
  data.table::setnafill(df_share, cols = "step", fill = 0)
  data.table::setorderv(df_share, levels_agg)
  
  
  df_share[, `:=`(step = factor(get('step'), ordered = TRUE,
                                labels = c("Products not found",
                                           "EAN Matching (step 1)",
                                           "Fuzzy matching with OpenFood, restrictive (step 2)",
                                           "Fuzzy matching with OpenFood, less restrictive (step 3)",
                                           "Fuzzy matching with CIQUAL (step 4)"
                                )),
                  caracteristic = factor(caracteristic, ordered = TRUE))]
  df_share[,p := N/sum(N), by = c(levels_agg[!(levels_agg %in% "caracteristic")])]
  
  df_share[, variable := factor(variable, levels = c("CA","produits_vendus"),
                                labels = c("Revenue", "Products sold"))]
  
  
  return(df_share)
}

stats_CA_coicop <- function(dt){
  
  dt_agg = create_data_stacked_plot(dt, "revenue", byvar = "prediction")
  cols_agg = colnames(dt_agg)[!(colnames(dt_agg) %in% c("RAYON","N","p"))]
  dt_agg = dt_agg[,.(N = sum(N)), by = cols_agg]
  dt_agg[,p := N/sum(N), by = c(cols_agg[!(cols_agg %in% "caracteristic")])]
  
  return(dt_agg)
}


create_data_ca <- function(data_linkage, ca_fp, ca_monop, ca_casino){
  
  pivot_var = c(colnames(data_linkage), "YEAR")
  
  dt = merge(
    merge(ca_fp, ca_monop, by = pivot_var, all = TRUE),
    ca_casino, all = TRUE,
    by = pivot_var)
  
  return(dt)
  
}


plot_loss_train_dev <- function(dt){
  dt <- data.table::copy(dt)
  dt[, 'epoch' := seq(.N)]
  dt <- data.table::melt(dt, id.vars = "epoch")
  dt[, variable := data.table::fifelse(
    variable == "dev_loss", "Loss on validation set", "Loss on training set" 
  )]
  
  p <- ggplot(dt) +
    geom_line(aes(x = epoch, y = value, color = variable)) +
    theme_bw() +
    theme(legend.position = "bottom") +
    labs(y = "Quadruplet loss", x = "Number of epochs", fill = NULL) +
    labs(color = NULL) +
    theme(text = element_text(size = 20)) +
    scale_color_manual(values = c("royalblue","red"))
  
  return(p)
}

plot_stacked_bar <- function(df_share){
  
  p <- ggplot(df_share,
              aes(fill = step, x = caracteristic, y = p)) +
    geom_bar(position = position_stack(reverse = TRUE), stat="identity") +
    scale_fill_viridis_d() +
    theme_bw() +
    theme(legend.position = "bottom") +
    labs(y = "Share of total products", x = "Characteristic", fill = NULL) +
    scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0,1)) +
    coord_flip() +
    guides(fill=guide_legend(nrow=3,byrow=TRUE,
                             title.position = "top", title.hjust = 0.5))
  
  return(p)
}

library(ggplot2)

plot_stacked_bar_pattern <- function(df_share, level = "caracteristic"){
  
  df_share[,yvar := get(level)]
  label_x <- ifelse(level == "caracteristic", "Nutrient", "Group")
  
  p <- ggplot(df_share,
              aes(fill = step, x = stringr::str_wrap(yvar, 15), y = p)) +
    geom_bar(position = position_stack(reverse = TRUE), stat="identity") +
    facet_grid(rows = vars(variable), cols= vars(enseigne)) +
    scale_fill_viridis_d() +
    scale_y_continuous(breaks = seq(0, 1, by = 0.2),
                       labels = scales::percent, expand = c(0, 0),
                       limits = c(0,1)) +
    coord_flip() +
    theme_bw() +
    theme(legend.position = "bottom") +
    labs(y = "Share of total", x = label_x, fill = NULL) +
    guides(fill=guide_legend(nrow=3,byrow=TRUE,
                             title.position = "top", title.hjust = 0.5)) +
    theme(strip.background = element_blank(),
          text = element_text(face="bold"))
  
  p2 <- ggplot(df_share[variable == "Revenue"],
               aes(fill = step, x = stringr::str_wrap(yvar, 20), y = p)) +
    geom_bar(position = position_stack(reverse = TRUE), stat="identity") +
    facet_grid(rows= vars(enseigne)) +
    scale_fill_viridis_d() +
    scale_y_continuous(breaks = seq(0, 1, by = 0.2),
                       labels = scales::percent, expand = c(0, 0),
                       limits = c(0,1)) +
    # coord_flip() +
    theme_bw() +
    theme(legend.position = "bottom") +
    labs(y = "Share of total", x = label_x, fill = NULL) +
    guides(fill=guide_legend(nrow=3,byrow=TRUE,
                             title.position = "top", title.hjust = 0.5)) +
    theme(strip.background = element_blank(),
          text = element_text(face="bold")) +
    theme(axis.text.x = element_text(angle = 45, hjust=1))
  #scale_x_discrete(guide = guide_axis(n.dodge=2))
  
  return(list(p, p2))
}

merge_coicop <- function(df, coicop, prediction_var = "prediction"){
  df[, Code := substr(get(prediction_var), 1,6)]
  essai <- merge(df, coicop, by = 'Code')
  essai <- essai[grepl('(^01)|(^02.1)', Code)]
  return(essai)
}


plot_share_filled_CA <- function(CA_produits_stats, level = "caracteristic", coicop = NULL){
  
  essai <- data.table::copy(CA_produits_stats)
  
  if (level == "caracteristic"){
    essai[, caracteristic := forcats::fct_recode(
      caracteristic,
      !!!correspondance_caracteristiques
    )
    ]
  }
  
  if (!is.null(coicop)){
    essai[, Code := substr(prediction, 1,6)]
    essai <- merge(essai, coicop, by = 'Code')
    essai <- essai[grepl('(^01)|(^02.1)', Code)]
    level <- "LibelEnglish"
  }
  
  essai <- essai[,.(N = sum(N)), by = c("step", "YEAR", "enseigne", "variable", level)]
  essai[, p := N/sum(N), by =  c("YEAR", "enseigne", "variable", level)]
  
  return(
    plot_stacked_bar_pattern(essai[YEAR == 2018], level = level)
  )
  
}

stats_openfood <- function(openfood){
  openfood_stats = openfood[, .(N = .N, n_ean = sum(!is.na(energy_100g)))]
  openfood_fill = na.omit(openfood, cols = c("energy-kj_100g", "energy_100g", "fat_100g", "saturated-fat_100g",
                                             "carbohydrates_100g", "sugars_100g", "fiber_100g", "proteins_100g", "salt_100g")
  )
  stats_open = data.table::data.table(
    stat = c("Number of products", "Filled energy", "All characteristics filled"),
    n = c(as.numeric(openfood_stats), openfood_fill[, (n_fill = .N)])
  )
  return(stats_open)
}

stats_missing_openfood <- function(openfood){
  
  share_missing_stats = openfood[, lapply(.SD, function(x) mean(is.na(x))),
                                 .SDcols = c("energy-kj_100g", "energy_100g", "fat_100g",
                                             "saturated-fat_100g",
                                             "carbohydrates_100g", "sugars_100g",
                                             "fiber_100g", "proteins_100g", "salt_100g")]
  share_missing_stats = data.table::data.table(
    caract = colnames(share_missing_stats),
    x = as.numeric(share_missing_stats[1])
  )
  share_missing_stats[, c('caract') := factor(gsub("_100g","", get("caract")))]
  
  return(share_missing_stats)
  
}

plot_missing_openfood <- function(share_missing_stats, correspondance_caracteristiques){
  
  share_missing_stats[, caract := forcats::fct_recode(
    caract,
    !!!correspondance_caracteristiques
  )
  ]
  
  
  p <- ggplot(share_missing_stats, aes(x = x, y = caract, fill = caract)) +
    geom_bar(position = position_stack(reverse = TRUE), stat="identity") +
    scale_fill_viridis_d() +
    scale_x_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0,1)) +
    theme_bw() +
    theme(legend.position = NULL) +
    labs(x = "Share of values missing for each characteristic", y = "Caracteristic", fill = NULL) +
    guides(fill="none") +
    theme(strip.background = element_blank())
  
  return(p)
  
}


plot_violin <- function(data_levensthein){
  
  data_levensthein <- data_levensthein[!is.na(step)]
  data_levensthein[, step := forcats::fct_recode(factor(step),
                                                 "EAN matching\n(step 1)" = "1",
                                                 "Fuzzy matching with OpenFood,\nrestrictive (step 2)" = "2",
                                                 "Fuzzy matching with OpenFood,\nless restrictive (step 3)" = "3",
                                                 "Fuzzy matching with CIQUAL\n(step 4)" = "4")
  ]
  
  p1 <- ggplot(data_levensthein, aes(x=factor(caracteristic), y=ratio1, fill = factor(caracteristic))) +
    geom_violin()  + 
    scale_fill_viridis_d(guide="none") +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 15, hjust=0.5, vjust = 0.7, face = "bold")) +
    labs(x = "Nutritional characteristic", y = "Similarity score between linked labels")
  
  p2 <- ggplot(data_levensthein[caracteristic == "energy"],
               aes(x=factor(step), y=ratio1, fill = factor(step))) +
    geom_violin()  + 
    scale_fill_viridis_d() +
    theme_bw() +
    theme(legend.position = "none") +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    labs(x = "Matching step", y = "Similarity score between linked labels")
  
  if (!("cos_distance" %in% colnames(data_levensthein))) return(list(p1, p2)) 
  
  data_levensthein[, "cos_distance" := 100*get("cos_distance")]
  
  df_long = data.table::melt(
    data_levensthein[, .SD,
                     .SDcols = c("libel_clean_relevanc", "libel_match", "step", "ratio1", "cos_distance")],
    id.vars = c("libel_clean_relevanc", "step", "libel_match"))
  
  df_long[, 'variable' := data.table::fifelse(variable == "ratio1",
                                              "Levensthein",
                                              "Cosine similarity (x100)")]
  
  df_long2 <- data.table::copy(df_long)
  df_long2[, step := 'Overall']
  df_long2 <- data.table::rbindlist(
    list(
      df_long, df_long2
    )
  )
  
  df_long_synth = df_long2[,.SD[sample(.N, size = 0.0005*.N, replace = TRUE)], by = step]
  
  
  p3 <- ggplot(df_long2,
               aes(x=factor(step), y=value,
                   color = factor(variable),
                   fill = factor(variable))) +
    geom_violin(position = position_dodge(0.7))  +
    geom_rug(data = df_long_synth[step != "Overall"],
             sides = "b", position = "jitter", alpha = 1/2,
             color = "black") +
    # geom_boxplot(width=0.01, color="grey", outlier.shape = NA,
    #              position = position_dodge(0.7), alpha=0.2) +
    scale_color_viridis_d(alpha = 0.9) +
    scale_fill_viridis_d(alpha = 0.6) +
    theme_bw() +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    labs(x = NULL, y = "Similarity score between linked labels",
         fill = "Distance measure", color = "Distance measure") +
    theme(legend.position = "bottom", legend.box="horizontal",
          axis.text = element_text(face="bold"))
  
  return(
    list(p1, p2, p3)
  )
  
}

load_siamese_evaluation_matches <- function(characteristics, prefix = "",  bucket = Sys.getenv("AWS_BUCKET_NAME")){
  db <- aws.s3::s3read_using(FUN = fread, object = paste0(prefix, 'siamese/valid_match_', characteristics, '_100g.csv'), bucket = bucket,
                             opts = list("region" = ""))
  db[, caracteristic:=characteristics]           
  db[, V1:=NULL]
  return(db[,.(caracteristic, step, libel_clean_relevanc,libel_match, cos_distance)])
}

plot_siamese_distance <- function(data_validation){
  p1 <- ggplot(data_validation, aes(x = factor(caracteristic), y = cos_distance, fill = as.factor(caracteristic))) +
    geom_violin()  + 
    scale_fill_viridis_d(guide="none") +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 15, hjust=0.5, vjust = 0.7, face = "bold")) +
    labs(x = "Nutritional characteristic", y = "Siamese similarity between\nlinked labels")
  
  p2 <- ggplot(rbind(data_validation[caracteristic == "energy",.(step, cos_distance)], data_validation[caracteristic == "energy",.(step='Overall',cos_distance)]),
               aes(x=factor(step), y= cos_distance, fill = factor(step))) +
    geom_violin()  + 
    scale_fill_viridis_d() +
    theme_bw() +
    theme(legend.position = "bottom") +
    labs(x = "Matching step", y = "Siamese similarity between\nlinked labels")
  
  return(list(p1,p2))
}
table_siamese_validation <- function(data_validation){
  tab1 <- rbind(data_validation[,.(`Less than 0.3` = formatC(mean(cos_distance<0.3), digits = 2),
                                   `D1` = formatC(quantile(cos_distance,0.1), digits = 2 ),
                                   `Mean` = formatC(mean(cos_distance), digits = 2 ),
                                   `Median` = formatC(median(cos_distance), digits = 2 ),
                                   `D9` = formatC(quantile(cos_distance,0.9), digits = 2 )),by=.(Step = step)][order(Step)],
                data_validation[,.(`Less than 0.3` = formatC(mean(cos_distance<0.3), digits = 2),
                                   `D1` = formatC(quantile(cos_distance,0.1), digits = 2 ),
                                   `Mean` = formatC(mean(cos_distance), digits = 2 ),
                                   `Median` = formatC(median(cos_distance), digits = 2 ),
                                   `D9` = formatC(quantile(cos_distance,0.9), digits = 2) , Step = 'Overall')]
  )
  
  tab2 <- rbind(data_validation[caracteristic=='energy'][cos_distance>0.8][sample(1:.N,3),.(libel_clean_relevanc ,libel_match, cos_distance = formatC(cos_distance, digits = 2))],
                data_validation[caracteristic=='energy'][cos_distance<0.8 & cos_distance>0.5][sample(1:.N,3),.(libel_clean_relevanc ,libel_match,cos_distance =formatC(cos_distance, digits = 2))],
                data_validation[caracteristic=='energy'][cos_distance<0.5 & cos_distance>0.3][sample(1:.N,3),.(libel_clean_relevanc ,libel_match,cos_distance =formatC(cos_distance, digits = 2))],
                data_validation[caracteristic=='energy'][cos_distance<0.3][sample(1:.N,3),.(libel_clean_relevanc ,libel_match,cos_distance =formatC(cos_distance, digits = 2))])[order(-cos_distance)]
  names(tab2) <- c('RelevanC','Open Food Facts or Ciqual','Siamese Similarity')
  
  return(list(tab1, tab2))
}

get_COICOP <- function(bucket = 'projet-relevanc', prefix = "", niveau = 'variete'){
  if(niveau == 'variete'){
    return(aws.s3::s3read_using(FUN = fread,
                                object = paste0(prefix, 'coicop/COICOP_Varietes.csv'), bucket = bucket,
                                opts = list("region" = "")))
  }
  if(niveau == '5-digits'){
    coicop <- aws.s3::s3read_using(FUN = function(x) readxl::read_excel(x, skip = 1),
                                   object = paste0(prefix, 'coicop/coicop2016_liste_n4.xls'),
                                   bucket = bucket,
                                   opts = list("region" = ""))
    setDT(coicop)
    coicop[, Code:=gsub("'","",Code)]
    return(coicop) 
  }
  if(niveau == '4-digits'){
    coicop <- aws.s3::s3read_using(FUN = function(x) readxl::read_excel(x, skip = 1),
                                   object = paste0(prefix, 'coicop/coicop_1998_liste_n3.xls'),
                                   bucket = bucket,
                                   opts = list("region" = ""))
    setDT(coicop)
    coicop[, Code:=gsub("'","",Code)]
    names(coicop) <- c('Code','LibelCoicop')
    coicop[, LibelCoicop:= 
             stringi::stri_trans_general(str = LibelCoicop, 
                                         id =  'latin-ascii')]
    coicop <- merge(coicop, english_class_COICOP(), by = "Code", all.x = TRUE)
    # cut some labels
    coicop[LibelEnglish == "Sugar, jams, chocolate, confectionery and frozen products",
           LibelEnglish := "Confectionery and frozen products"]
    coicop[LibelEnglish == "Salt, spices, sauces and foodstuffs nes",
           LibelEnglish := "Salt, spices and sauces"]
    
    return(coicop) 
  }
  
}
english_class_COICOP <- function(){
  data.table(Code = c("01.1.1", "01.1.2", "01.1.3" ,"01.1.4", "01.1.5", "01.1.6", "01.1.7" ,"01.1.8" ,"01.1.9", "01.2.1", "01.2.2" ,
                      "02.1.1" ,"02.1.2" ,"02.1.3"),
             LibelEnglish = c("Bread and cereals","Meat","Fish and shellfish",
                              "Milk, cheese and eggs","Oils and fats","Fruits","Vegetables",
                              "Sugar, jams, chocolate, confectionery and frozen products",
                              "Salt, spices, sauces and foodstuffs nes","Coffee, tea and cocoa","Other soft drinks",
                              "Alcohols","Wines, ciders and champagne","Beers" ))
}

siamese_distance_by_COICOP <- function(characteristics = 'proteins', prefix = "",
                                       bucket = 'projet-relevanc'){
  
  db <- aws.s3::s3read_using(FUN = fread,
                             object = paste0(prefix, 'siamese/valid_match_', characteristics, '_100g.csv'), bucket = bucket,
                             opts = list("region" = ""))
  
  coicop <- get_COICOP(niveau = '4-digits', prefix = prefix, bucket = bucket)
  
  db[, Code:= substr(prediction, 1,6)]
  db <- merge(db , coicop, by = 'Code')
  
  db <- db[grepl('(^01)|(^02.1)', Code)]
  return(db)
}

plot_siamese_distance_by_COICOP <- function(characteristics = 'proteins',
                                            prefix = "",
                                            bucket = 'projet-relevanc'){
  
  db <- siamese_distance_by_COICOP(characteristics, bucket,
                                   prefix = prefix)
  
  ggplot(db,
         aes(x=factor(LibelEnglish), y= cos_distance, fill = factor(LibelEnglish))) +
    geom_violin()  + 
    scale_fill_viridis_d(guide = 'none') +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 15, hjust=0.5, vjust = 0.7, face = "bold"))+
    labs(x = "COICOP Class", y = "Siamese similarity between\nlinked labels")
}

table_siamese_example <- function(dt, ntop = 4){
  
  cols <- c("product_name",
            'echo',
            'cos_distance',
            'ratio1')
  
  dt <- dt[, .SD, .SDcols = c(cols, 'input_tokenized')]
  dt <- dt[!grepl(
    paste0(
      '(', paste(unique(dt$input_tokenized), collapse = "|"), ')'
    ), echo)]
  #dt <- dt[tolower(input_tokenized) != tolower(echo)]
  
  dt <- dt[order(-cos_distance), head(.SD, ntop), by=c("input_tokenized")]
  
  groups <- c("chips lays","dragibus", "crocodiles haribo","nutella","nacho",'ricard')
  dt <- dt[input_tokenized %in% groups]
  
  dt[,'rn' := seq(.N)]
  table_groups <- dt[, .(start = min(rn), max = max(rn)),
                     by = "input_tokenized"]
  
  list_products <- unique(dt$input_tokenized)
  table_groups[, input_tokenized:= paste0("Queried product: ",
                                          toupper(input_tokenized))]
  
  ft_dt <- dt[,.SD,.SDcols = cols]
  ft_dt$cos_distance <- formattable::color_bar("lightgreen")(100*round(ft_dt$cos_distance, 2))
  ft_dt$ratio1 <- formattable::color_bar("orange")(round(ft_dt$ratio1))
  
  #ft_dt <- ft_dt[c("car", "mpg", "cyl", "disp", "hp")]
  
  
  tab = kbl(ft_dt, escape = FALSE,
            col.names = c("Product name in OpenFood Facts", "Tokenized name",
                          "Cosine similarity", "Levensthein similarity") ,
            caption = "Examples to understand siamese network embedding") |>
    kable_classic_2(full_width = FALSE) |>
    kable_styling(bootstrap_options = c("condensed")) |>
    pack_rows(table_groups[1]$input_tokenized,
              table_groups[1]$start,
              table_groups[1]$max) |>
    pack_rows(table_groups[2]$input_tokenized,
              table_groups[2]$start,
              table_groups[2]$max) |>
    pack_rows(table_groups[3]$input_tokenized,
              table_groups[3]$start,
              table_groups[3]$max) |>
    pack_rows(table_groups[4]$input_tokenized,
              table_groups[4]$start,
              table_groups[4]$max) |>
    pack_rows(table_groups[5]$input_tokenized,
              table_groups[5]$start,
              table_groups[5]$max) |>
    pack_rows(table_groups[6]$input_tokenized,
              table_groups[6]$start,
              table_groups[6]$max) |>
    kableExtra::add_header_above(c("Nearest neighbors in embedding space" = 2,
                                   "Distance measures" = 2)) |>
    kableExtra::footnote(general =
                           c("By construction, when both the query and the echo are identical, both string measures are maximal. These cases have been removed from this table. Best echoes where the queried terms appears have been removed from this table to stress cases where the embedding finds unexpected echoes from an editing distance perspective.",
                             "Cosine similarity is multipled by 100. It has been computed with <code>faiss</code> library.",
                             "Levensthein similarity is based on the normalizing edit distance implemented in <code>rapidfuzz</code>."),
                         escape = FALSE
    )
  
  return(tab)
}


plot_siamese_perf <- function(data_perf_siamese, data_perf_siamese_sample,
                              labels = c("Whole test set (16,000 products)",
                                         "Sample of test products (2,000 products)")){
  
  data_perf_siamese <- data.table::copy(data_perf_siamese)
  data_perf_siamese_sample <- data.table::copy(data_perf_siamese_sample)
  data_perf_siamese[,'label' := labels[1]]
  data_perf_siamese_sample[,'label' := labels[2]]
  df <- data.table::rbindlist(list(data_perf_siamese, data_perf_siamese_sample))
  
  p <- ggplot(df, aes(x = topk, y = accuracy, color = label)) +
    geom_line() + geom_point()
  p <- p +
    scale_x_continuous(breaks = 1:10) +
    scale_y_continuous(labels = scales::percent) +
    labs(x = "Number of nearest neighbors considered (k)", y = "True match belongs to the\nset of nearest neighbors (% of cases)")
  p <- p + theme_bw() +
    theme(text = element_text(face="bold"))
  p <- p + theme(legend.position = "bottom") +
    #scale_color_viridis_d() +
    scale_color_manual(values = c("royalblue","red")) +
    labs(color = "Test set")
  return(p)
}

plot_siamese_perf_coicop <- function(data_siamese_coicop, sample_rug = TRUE){
  
  dt = data_siamese_coicop[, .(
    accuracy_1 = mean(top_1),
    accuracy_3 = mean(top_3),
    accuracy_8 = mean(top_8)
  ), by = "LibelEnglish"]
  
  dt <- data.table::melt(dt, id.vars = "LibelEnglish")
  dt[,variable := gsub("accuracy_","",variable)]
  
  if (sample_rug){
    df_long_synth <- data_siamese_coicop[,.SD[sample(.N, size = 0.1*.N, replace = TRUE)], by = LibelEnglish]
  } else{
    df_long_synth <-  data_siamese_coicop
  }
  
  p <- ggplot2::ggplot(dt, aes(x = factor(stringr::str_wrap(LibelEnglish, 20)), y = value, fill = factor(variable))) +
    geom_bar(stat = "identity", width=0.8, position = position_dodge(0.7)) +
    geom_point(position = position_dodge(0.7)) +
    coord_cartesian(ylim = c(0, .65)) +
    geom_rug(data = df_long_synth,
             inherit.aes = FALSE,
             aes(x = factor(stringr::str_wrap(LibelEnglish, 20)), y = 0.6),
             sides = "b", position = "jitter", alpha = 1/2,
             color = "black") +
    scale_fill_viridis_d() +
    theme_bw() +
    labs(x = "Product family", y = "True siamese belongs to the\nset of nearest neighbors (% of cases)",
         fill = "Number of nearest neighbors considered", point = "Number of nearest neighbors considered"
    ) +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust=1))
  
  return(p)
}


plot_violin_coicop <- function(data_levensthein){
  
  data_levensthein <- data_levensthein[!is.na(libel_match)]
  
  data_levensthein[, "cos_distance" := 100*get("cos_distance")]
  
  df_long = data.table::melt(
    data_levensthein[, .SD,
                     .SDcols = c("libel_clean_relevanc", "libel_match",
                                 "LibelEnglish",
                                 "ratio1", "cos_distance")],
    id.vars = c("libel_clean_relevanc", "LibelEnglish", "libel_match"))
  
  df_long[, 'variable' := data.table::fifelse(variable == "ratio1",
                                              "Levensthein",
                                              "Cosine similarity (x100)")]
  
  
  df_long_synth = df_long[,.SD[sample(.N, size = 0.002*.N, replace = TRUE)],
                          by = "LibelEnglish"]
  
  
  p3 <- ggplot(df_long,
               aes(x=factor(stringr::str_wrap(LibelEnglish, 20)), y=value,
                   color = factor(variable),
                   fill = factor(variable))) +
    geom_violin(position = position_dodge(0.7))  +
    geom_rug(data = df_long_synth,
             sides = "b", position = "jitter", alpha = 1/2,
             color = "black") +
    # geom_boxplot(width=0.01, color="grey", outlier.shape = NA,
    #              position = position_dodge(0.7), alpha=0.2) +
    scale_color_viridis_d(alpha = 0.9) +
    scale_fill_viridis_d(alpha = 0.6) +
    theme_bw() +
    labs(x = NULL, y = "Similarity score between linked labels",
         fill = "Distance measure", color = "Distance measure") +
    theme(legend.position = "bottom", legend.box="horizontal",
          axis.text = element_text(face="bold"),
          axis.text.x = element_text(angle = 45, hjust=1))
  
  return(
    p3
  )
  
}

random_examples_mismatch <- function(data_elastic_mismatch){
  dt <- data_elastic_mismatch[stringr::str_length(elastic_libel_clean_OFF_true)<50]
  dt = dt[sample(.N, size = 10), .SD,
          .SDcols = c(
            "libel_clean_relevanc",
            "elastic_libel_clean_OFF_true",
            "elastic_energy_100g_true",
            "elastic_libel_clean_OFF_top",
            "elastic_energy_100g_top")]
  dt[,`:=`(elastic_energy_100g_true = round(elastic_energy_100g_true),
           elastic_energy_100g_top = round(elastic_energy_100g_top))]
  return(dt)
}


table_share_linkage <- function(figure_share_CA){
  
  tab_out <- figure_share_CA[[1]]$data[caracteristic == "Energy (kcal)"]
  tab_out <- tab_out[order(enseigne, step)][,.SD,.SDcols = c("step","enseigne","variable","p")]
  tab_out <- data.table::dcast(tab_out, step + enseigne ~ ...)
  tab_out <- data.table::dcast(tab_out, step ~ enseigne, value.var=c("Revenue", "Products sold"))
  
  cols = colnames(tab_out)[colnames(tab_out) != "step"]
  tab_out[, c(cols) := round(100*.SD, 1), .SDcols = cols]
  
  data.table::setcolorder(tab_out,
                          c("step",
                            do.call(paste0,
                                    expand.grid(c("Products sold","Revenue"), c("_casino", "_franprix","_monoprix")))
                          )
  )
  
  # tab_out[, c(cols) := lapply(.SD, function(x) formattable::color_bar("ff7f7f")(x)), .SDcols = cols]
  
  
  return(tab_out)
  
}


rotating_text <- function(x, align = "center") {
  glue::glue('
<div style="overflow: hidden; height: 1.2em;">
<ul class="content__container__list {align}" style="text-align: {align}">
<li class="content__container__list__item">{x[1]}</li>
<li class="content__container__list__item">{x[2]}</li>
<li class="content__container__list__item">{x[3]}</li>
</ul>
</div>')
}

