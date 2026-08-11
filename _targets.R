library(targets)

# Load the nine numbered analytical modules in their documented order.
module_files <- sort(list.files(
  "R",
  pattern = "^[0-9]{2}_.*\\.R$",
  full.names = TRUE
))
invisible(lapply(module_files, sys.source, envir = .GlobalEnv))

tar_option_set(
  packages = c(
    "collapse", "data.table", "haven", "readxl", "arrow",
    "tidyselect", "yaml", "dplyr"
  ),
  error = "stop",
  memory = "transient",
  garbage_collection = TRUE
)

list(
  # Configuration and source-data tracking ------------------------------------
  tar_target(config_path, "config/config.yml", format = "file"),
  tar_target(config, {
    config_path
    read_project_config()
  }),
  tar_target(
    input_status,
    check_required_inputs(config),
    cue = tar_cue(mode = "always")
  ),
  tar_target(cfg, {
    input_status
    config
  }),

  tar_target(
    population_inputs,
    manifest_files(input_status, c("population_current", "population_workbook")),
    format = "file"
  ),
  tar_target(
    cod_inputs,
    manifest_files(input_status, c("cod_raw", "icd_to_nbd_lookup")),
    format = "file"
  ),
  tar_target(
    injury_inputs,
    manifest_files(input_status, c("nims_2000", "ims_2009", "famhis_2017")),
    format = "file"
  ),
  tar_target(
    completeness_inputs,
    manifest_files(
      input_status,
      c("completeness_child", "completeness_province", "npr", "investigation_parameters")
    ),
    format = "file"
  ),
  tar_target(
    prevalence_input,
    manifest_files(input_status, "prevalence_raw"),
    format = "file"
  ),
  tar_target(
    yll_inputs,
    manifest_files(input_status, c("analysis_to_za_lookup", "yll")),
    format = "file"
  ),
  tar_target(
    database_input,
    manifest_files(input_status, "asr_factors"),
    format = "file"
  ),

  # Stage 01: population -------------------------------------------------------
  tar_target(population_file, {
    population_inputs
    stage_prepare_population(cfg)
  }, format = "file"),

  # Stage 02: registered cause-of-death data ----------------------------------
  # The expensive record-level ICD verification is cached separately from the
  # demographic redistribution steps so interrupted runs resume efficiently.
  tar_target(cod_verified_group_file, {
    cod_inputs
    stage_group_cod_records(cfg)
  }, format = "file"),
  tar_target(
    cod_annual_file,
    stage_prepare_cod_annual(cfg, cod_verified_group_file),
    format = "file"
  ),
  tar_target(
    cod_sex_file,
    stage_redistribute_cod_sex(cfg, cod_annual_file),
    format = "file"
  ),
  tar_target(
    cod_age_file,
    stage_redistribute_cod_age(cfg, cod_sex_file),
    format = "file"
  ),
  tar_target(
    cod_file,
    stage_redistribute_cod_population(cfg, cod_age_file),
    format = "file"
  ),

  # Stage 03: injury cause fractions ------------------------------------------
  tar_target(injury_survey_file, {
    injury_inputs
    stage_prepare_injury_surveys(cfg)
  }, format = "file"),
  tar_target(
    injury_outputs,
    stage_estimate_injuries(cfg, cod_file, injury_survey_file),
    format = "file"
  ),

  # Stage 04: completeness and NPR adjustment ---------------------------------
  tar_target(
    investigation_file,
    {
      completeness_inputs
      stage_adjust_completeness(
        cfg,
        cod_file,
        select_stage_file(injury_outputs, "03_final_injuries.parquet")
      )
    },
    format = "file"
  ),

  # Stage 05: HIV/AIDS reallocation -------------------------------------------
  tar_target(
    prevalence_population_file,
    {
      prevalence_input
      stage_prepare_prevalence_population(cfg, population_file)
    },
    format = "file"
  ),
  tar_target(
    hiv_outputs,
    stage_reallocate_hiv(cfg, investigation_file, prevalence_population_file),
    format = "file"
  ),

  # Stage 06: ill-defined and garbage-code redistribution ---------------------
  tar_target(
    redistributed_file,
    stage_redistribute_garbage(
      cfg,
      select_stage_file(hiv_outputs, "05_hiv_reallocated_wide.parquet")
    ),
    format = "file"
  ),

  # Stage 07: years of life lost -----------------------------------------------
  tar_target(
    deaths_yll_file,
    {
      yll_inputs
      stage_calculate_yll(cfg, redistributed_file)
    },
    format = "file"
  ),

  # Stage 08: final Version 1 database ----------------------------------------
  tar_target(
    database_file_output,
    {
      database_input
      stage_build_database(cfg, deaths_yll_file, population_file)
    },
    format = "file"
  )
)
