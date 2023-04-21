library(paws)
library(targets)

source("R/functions.R", encoding = "UTF-8")
source("R/dico_paths.R")

# externe --->
bucket = "projet-relevanc"
prefix = ""
endpoint = paste0("https://", Sys.getenv("AWS_S3_ENDPOINT"))
# interne --->
#bucket = "w3crk9"
#prefix = "relevanc/output/"
#endpoint = paste0("http://", Sys.getenv("AWS_S3_ENDPOINT"))

tar_option_set(
  resources = tar_resources(
    aws = tar_resources_aws(bucket = bucket,
                            endpoint = endpoint)
  )
)



correspondance_caracteristiques <- c(
  'Sugar' = 'sugars',
  'Saturated Fat' = 'saturated-fat',
  'Carbohydrates' = 'carbohydrates', 
  'Energy (kcal)' = 'energy',
  'Energy (kJ)' = 'energy-kj',
  'Fat' = 'fat',
  'Fibers' = 'fiber',
  'Proteins' = 'proteins',
  'Salt' = 'salt'
)

list(
  tar_target(
    data_linkage, read_parquet_dt(paste0(prefix, path_linkage),
                                  bucket = bucket),
    format = "fst_dt", # Set format = "aws_qs" in targets <= 0.10.0.
    repository = "aws" # No repository argument in targets <= 0.10.0.
  ),
  
  tar_target(
    data_cosine_distance,
    read_parquet_dt(paste0(prefix, siamese_distance_dataframe),
                    bucket = bucket),
    format = "fst_dt", # Set format = "aws_qs" in targets <= 0.10.0.
    repository = "aws" # No repository argument in targets <= 0.10.0.
  ),
  
  tar_target(
    data_siamese_diff_elastic,
    read_parquet_dt(paste0(prefix, siamese_diff_elastic),
                    bucket = bucket),
    format = "fst_dt", # Set format = "aws_qs" in targets <= 0.10.0.
    repository = "aws" # No repository argument in targets <= 0.10.0.
  ),
  
  tar_target(
    data_perf_siamese,
    read_parquet_dt(paste0(prefix, siamese_test_perf),
                    bucket = bucket),
    format = "fst_dt", # Set format = "aws_qs" in targets <= 0.10.0.
    repository = "aws" # No repository argument in targets <= 0.10.0.
  ),
  
  tar_target(
    data_perf_siamese_sample,
    read_parquet_dt(paste0(prefix, siamese_test_perf_subset),
                    bucket = bucket),
    format = "fst_dt", # Set format = "aws_qs" in targets <= 0.10.0.
    repository = "aws" # No repository argument in targets <= 0.10.0.
  ),
  tar_target(
    data_siamese_perf_decomposed,
    read_parquet_dt(paste0(prefix, siamese_perf_coicop),
                    bucket = bucket),
    format = "fst_dt", # Set format = "aws_qs" in targets <= 0.10.0.
    repository = "aws" # No repository argument in targets <= 0.10.0.
  ),
  
  tar_target(
    data_siamese_perf_decomposed_sample,
    read_parquet_dt(paste0(prefix, siamese_perf_coicop_subset),
                    bucket = bucket),
    format = "fst_dt", # Set format = "aws_qs" in targets <= 0.10.0.
    repository = "aws" # No repository argument in targets <= 0.10.0.
  ),
  
  tar_target(
    data_elastic_mismatch,
    read_parquet_dt(paste0(prefix, path_mismatch_elastic),
                    bucket = bucket),
    format = "fst_dt", # Set format = "aws_qs" in targets <= 0.10.0.
    repository = "aws" # No repository argument in targets <= 0.10.0.
  ),
  
  
  # PARTIE SPECIFIQUE INTERNE -------
  
  # tar_target(
  #   ca_franprix, match_output_ca_produits("franprix", data_linkage)
  # ),
  # tar_target(
  #   ca_monop, match_output_ca_produits("monoprix", data_linkage)
  # ),
  # tar_target(
  #   ca_casino, match_output_ca_produits("casino", data_linkage)
  # ),
  # 
  # tar_target(
  #   CA_produits_linkage,
  #   create_data_ca(data_linkage, ca_franprix, ca_monop, ca_casino),
  #   format = "fst_dt", # Set format = "aws_qs" in targets <= 0.10.0.
  #   repository = "aws" # No repository argument in targets <= 0.10.0.
  # ),
  # 
  # tar_target(
  #   CA_produits_stats,
  #   create_data_stacked_plot(CA_produits_linkage, "revenue"),
  #   format = "fst_dt", # Set format = "aws_qs" in targets <= 0.10.0.
  #   repository = "aws" # No repository argument in targets <= 0.10.0.
  # ),
  # 
  # tar_target(
  #   CA_produits_coicop,
  #   stats_CA_coicop(CA_produits_linkage),
  #   format = "fst_dt", # Set format = "aws_qs" in targets <= 0.10.0.
  #   repository = "aws" # No repository argument in targets <= 0.10.0.
  # ),
  # 
  # tar_target(
  #   write_CA_produits_stats,
  #   write_parquet_dt(
  #     x = CA_produits_stats,
  #     path = paste0(prefix, path_CA_produits_stats),
  #     bucket = bucket
  #   ),
  #   format = "fst_dt", # Set format = "aws_qs" in targets <= 0.10.0.
  #   repository = "aws" # No repository argument in targets <= 0.10.0.
  # ),
  # 
  # tar_target(
  #   write_CA_produits_coicop,
  #   write_parquet_dt(
  #     x = CA_produits_coicop,
  #     path = paste0(prefix, path_CA_coicop),
  #     bucket = bucket
  #   ),
  #   format = "fst_dt", # Set format = "aws_qs" in targets <= 0.10.0.
  #   repository = "aws" # No repository argument in targets <= 0.10.0.
  # ),
  
  
  # RETOUR AU PIPELINE GENERAL --------------------------
  
  tar_target(
    CA_produits_stats,
    read_parquet_dt(paste0(prefix, path_CA_produits_stats),
                    bucket = bucket),
    format = "fst_dt", # Set format = "aws_qs" in targets <= 0.10.0.
    repository = "aws" # No repository argument in targets <= 0.10.0.
  ),
  tar_target(
    CA_produits_coicop,
    read_parquet_dt(paste0(prefix, path_CA_coicop),
                    bucket = bucket),
    format = "fst_dt", # Set format = "aws_qs" in targets <= 0.10.0.
    repository = "aws" # No repository argument in targets <= 0.10.0.
  ),
  
  tar_target(
    examples_siamese,
    read_parquet_dt(paste0(prefix, path_examples_siamese),
                    bucket = bucket),
    format = "fst_dt", # Set format = "aws_qs" in targets <= 0.10.0.
    repository = "aws" # No repository argument in targets <= 0.10.0.
  ),
  tar_target(
    siamese_perf,
    read_parquet_dt(paste0(prefix, siamese_model_history),
                    bucket = bucket),
    format = "fst_dt", # Set format = "aws_qs" in targets <= 0.10.0.
    repository = "aws" # No repository argument in targets <= 0.10.0.
  ),  
  tar_target(
    figure_share_CA,
    plot_share_filled_CA(CA_produits_stats),
    format = "qs", # Set format = "aws_qs" in targets <= 0.10.0.
    repository = "aws"
  ),
  
  tar_target(
    table_share_CA,
    table_share_linkage(figure_share_CA)
  ),
  
  tar_target(
    coicop, 
    get_COICOP(bucket = bucket, niveau = '4-digits', prefix = prefix)
  ),
  
  tar_target(
    figure_share_coicop,
    plot_share_filled_CA(CA_produits_coicop, level = "prediction", coicop),
    format = "qs", # Set format = "aws_qs" in targets <= 0.10.0.
    repository = "aws"
  ),
  
  tar_target(
    non_matched_products,
    figure_share_CA[[1]]$data[step == "Products not found" & variable == "Revenue" & caracteristic == "Energy (kcal)"],
    format = "fst_dt",
    repository = "aws"
  ),
  tar_target(
    stats_appariement_EAN,
    data_linkage[, .(n = .N), by = "energy_100g_step"],
    format = "fst_dt", # Set format = "aws_qs" in targets <= 0.10.0.
    repository = "aws" # No repository argument in targets <= 0.10.0.
  ),
  tar_target(
    wordcloud_relevanc_start,
    paste0(prefix, path_wordcloud_relevanc_start),
    format = "qs"
  ),
  
  tar_target(
    wordcloud_relevanc_clean,
    paste0(prefix, path_wordcloud_relevanc_clean),
    # copy_s3("relevanc/output/relevanc_clean.png"),
    format = "qs"
    # repository = "aws"
  ),
  
  tar_target(
    wordcloud_openfood_start,
    paste0(prefix, path_wordcloud_openfood_start),
    format = "qs"
  ),
  
  tar_target(
    wordcloud_openfood_clean,
    paste0(prefix, path_wordcloud_openfood_clean),
    # copy_s3("relevanc/output/relevanc_clean.png"),
    format = "qs"
    # repository = "aws"
  ),
  
  tar_target(
    example_prediction, read_parquet_dt(paste0(prefix, "output/example_prediction.parquet"),
                                        bucket = bucket),
    format = "fst_dt", # Set format = "aws_qs" in targets <= 0.10.0.
    repository = "aws" # No repository argument in targets <= 0.10.0.
  ),
  
  tar_target(
    example_prediction_label, merge_coicop(tab_exemples_elastic, coicop, prediction_var = "prediction"),
    format = "fst_dt", # Set format = "aws_qs" in targets <= 0.10.0.
    repository = "aws" # No repository argument in targets <= 0.10.0.
  ),
  
  
  
  tar_target(
    openfood, read_parquet_dt(paste0(prefix, path_openfood_clean),
                              bucket = bucket),
    format = "fst_dt", # Set format = "aws_qs" in targets <= 0.10.0.
    repository = "aws" # No repository argument in targets <= 0.10.0.
  ),
  tar_target(
    summary_openfood, stats_openfood(openfood),
    format = "fst_dt", # Set format = "aws_qs" in targets <= 0.10.0.
    repository = "aws" # No repository argument in targets <= 0.10.0.
  ),
  
  
  
  tar_target(
    df_missing_openfood, stats_missing_openfood(openfood),
    format = "fst_dt", # Set format = "aws_qs" in targets <= 0.10.0.
    repository = "aws" # No repository argument in targets <= 0.10.0.
  ),
  
  tar_target(
    figure_missing_openfood, plot_missing_openfood(df_missing_openfood, correspondance_caracteristiques),
    format = "qs", # Set format = "aws_qs" in targets <= 0.10.0.
    repository = "aws" # No repository argument in targets <= 0.10.0.
  ),
  
  tar_target(
    data_levensthein, read_parquet_dt(paste0(prefix, path_levensthein),
                                      bucket = bucket),
    format = "fst_dt", # Set format = "aws_qs" in targets <= 0.10.0.
    repository = "aws" # No repository argument in targets <= 0.10.0.
  ),
  
  tar_target(
    figure_violin, plot_violin(data_cosine_distance),
    format = "qs", # Set format = "aws_qs" in targets <= 0.10.0.
    repository = "aws" # No repository argument in targets <= 0.10.0.
  ),
  
  tar_target(
    figure_violin_coicop,
    plot_violin_coicop(
      merge_coicop(data_cosine_distance, coicop, prediction_var = "prediction")
    ),
    format = "qs", # Set format = "aws_qs" in targets <= 0.10.0.
    repository = "aws" # No repository argument in targets <= 0.10.0.
  ),
  
  
  
  tar_target(
    data_validation, 
    rbindlist(
      lapply(c('sugars','sodium','saturated-fat','proteins','fiber','fat','energy','energy-kj','carbohydrates'),
             FUN = function(car) load_siamese_evaluation_matches(car, bucket = bucket, prefix = prefix))),
    format = "fst_dt",
    repository = "aws"),
  
  tar_target(
    figure_validation,
    plot_siamese_distance(data_validation),
    format = "qs",
    repository = "aws"
  ),
  
  tar_target(
    tab_siamese_validation,
    table_siamese_validation(data_validation),
    format = "qs",
    repository = "aws"
  ),
  
  tar_target(
    tab_exemples_elastic,
    data_linkage[energy_100g_step == 2L][sample(1:.N,10), .SD,
                                         .SDcols = c("LIBELLE", 'libel_clean_relevanc', "prediction",
                                                     "energy_100g_libel_clean_OFF","energy_100g")]
  ),
  
  tar_target(
    plot_siamese_coicop,
    plot_siamese_distance_by_COICOP(characteristics = 'proteins',
                                    bucket = bucket,
                                    prefix = prefix),
    format = "qs",
    repository = "aws"
  ),
  
  tar_target(
    sample_cos_pourri,
    data_siamese_diff_elastic[cos_distance<0.2][sample(5), .SD,
                                                .SDcols = c("libel_clean_relevanc", "energy_100g_libel_match", "cos_distance")],
    format = "fst_dt",
    repository = "aws"
  ),
  
  tar_target(
    plot_topK_siamese,
    plot_siamese_perf(data_perf_siamese, data_perf_siamese_sample)
  ),
  
  tar_target(
    data_siamese_coicop,
    merge_coicop(data_siamese_perf_decomposed, coicop),
    format = "fst_dt",
    repository = "aws"
  ),
  
  tar_target(
    data_siamese_coicop_sample,
    merge_coicop(data_siamese_perf_decomposed_sample, coicop),
    format = "fst_dt",
    repository = "aws"
  ),
  
  
  tar_target(
    barplot_siamese_coicop,
    plot_siamese_perf_coicop(data_siamese_coicop)
  ),
  
  tar_target(
    exemples_mismatch_elastic,
    random_examples_mismatch(data_elastic_mismatch)    
  )
  
  # tar_target(
  #   stacked_data_caracteristics, create_data_stacked_plot(data_linkage),
  #   format = "fst_dt", # Set format = "aws_qs" in targets <= 0.10.0.
  #   repository = "aws" # No repository argument in targets <= 0.10.0.
  # ),
  # tar_target(
  #   stacked_chart_share_filled, plot_stacked_bar(stacked_data_caracteristics),
  #   format = "qs", # Set format = "aws_qs" in targets <= 0.10.0.
  #   repository = "aws" # No repository argument in targets <= 0.10.0.
  # )
)



# list(
#   tar_target(raw_data_linkage, "temp/relevanc_post_linkage.parquet",
#              format = "file", repository = "aws"),
#   tar_target(data_linkage, data.table::as.data.table(read_parquet(raw_data_linkage)),
#              format = "fst_dt", repository = "aws")
# )
# list(
#     tar_target(name = raw_data_linkage,
#                command = "temp/relevanc_post_linkage.parquet",
#                format = "file", repository = "aws")
# )


