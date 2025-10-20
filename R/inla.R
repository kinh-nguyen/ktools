#' Submit an INLA model to SLURM
#'
#' Prepare inputs and write an R script and SLURM submission script to run an INLA model
#' on a SLURM cluster. This function saves model inputs (and optional extra objects)
#' to an .RData file, writes an R script that loads those inputs and runs INLA, and
#' writes a SLURM submission script to run that R script.
#'
#' @param stk_path Path to a saved INLA stack (.RData) that contains an object named `stk`.
#' @param inla_formula A formula to pass to INLA.
#' @param inla_family Family name passed to INLA (default: "nbinomial").
#' @param inla_control_predictor List passed to control.predictor (default: list(compute = TRUE)).
#' @param inla_control_compute List passed to control.compute (default: list(dic = TRUE, waic = TRUE, cpo = TRUE)).
#' @param job_name,partition,time,ntasks,nodes,conda_env,rscript,work_dir,rsave_path,submit,submit_cmd See function arguments for SLURM and script control.
#' @param partition SLURM partition to use (default: "fuchs").
#' @param time Walltime for the job (default: "8:00:00").
#' @param ntasks Number of tasks (default: 1).
#' @param nodes Number of nodes (default: 1).
#' @param conda_env Name of the conda environment to activate (default: "kinh").
#' @param rscript Name of the R script file to write (default: "run.r").
#' @param work_dir Working directory where scripts and .RData will be saved (default: current directory).
#' @param rsave_path Path to save the .RData file containing model inputs (default: "inla_input.RData").
#' @param submit set to TRUE to submit the job immediately after writing the scripts.
#' @param submit_cmd Command to use for submission (default: "sbatch").
#' @param extra_objects Named list of additional R objects to save into the .RData file (e.g. list(pc_prec = pc_prec)).
#' @param ... Additional unused arguments kept for extensibility.
#'
#' @return Invisibly returns a list with paths to the generated files and submission info:
#'   list(submitted = logical, rscript = <path>, slurm = <path>, rdata = <path>, output = <submission output if any>).
#' @export
run_inla_on_slurm <- function(
  stk_path,
  inla_formula,
  inla_family = "nbinomial",
  inla_control_predictor = list(compute = TRUE),
  inla_control_compute    = list(dic = TRUE, waic = TRUE, cpo = TRUE),
  job_name    = "INLAjob",
  partition   = "fuchs",
  time        = "8:00:00",
  ntasks      = 1,
  nodes       = 1,
  conda_env   = "kinh",
  rscript     = "run.r",
  work_dir    = ".",
  rsave_path  = "inla_input.RData",
  submit      = FALSE,
  submit_cmd  = "sbatch",
  extra_objects = NULL,   # Named list of additional objects (e.g. list(pc_prec=pc_prec))
  ...
) {
  libs <- c("INLA", "Matrix")
  missing_libs <- libs[!sapply(libs, function(x) requireNamespace(x, quietly = TRUE))]
  if(length(missing_libs) > 0) stop(sprintf("Missing libraries: %s", paste(missing_libs, collapse = ",")))
  if(!file.exists(stk_path)) stop(sprintf("Stack object file not found: %s", stk_path))
  
  # Identify and save model inputs and any extra named objects
  model_inputs <- list(
    stk_path    = stk_path,
    formula     = inla_formula,
    family      = inla_family,
    control_predictor = inla_control_predictor,
    control_compute   = inla_control_compute
  )
  # Save the stack for reproducibility, and extra user-supplied objects (e.g. pc_prec)
  saveobj <- model_inputs
  if(!is.null(extra_objects)) {
    for(nm in names(extra_objects)) {
      saveobj[[nm]] <- extra_objects[[nm]]
    }
  }
  save(list = names(saveobj), file = rsave_path, envir = list2env(saveobj, parent = .GlobalEnv))
  
  # Write the R script to disk for running on SLURM
  rscript_path <- file.path(work_dir, rscript)
  object_load_lines <- paste(sprintf("if (file.exists('%s')) load('%s')", rsave_path, rsave_path), collapse = "\n")
  # Optionally, attach all named elements from model_inputs, extra_objects in .GlobalEnv after loading
  attach_lines <- paste(sprintf("attach(%s, name = 'model_inputs')", deparse(substitute(model_inputs))), collapse = "\n")
  cat(
    sprintf(
      "library(INLA)
library(Matrix)
load('%s')

load(stk_path)

r <- inla(
  formula,
  data = inla.stack.data(stk),
  family = family,
  control.predictor = c(control_predictor, list(A = inla.stack.A(stk))),
  control.compute   = control_compute
)
save(r, file = 'inla_result.RData')
", rsave_path
    ),
    file = rscript_path
  )
  
  # SLURM script
  slurm_path <- file.path(work_dir, "submit_inla.sh")
  cat(
    sprintf(
      "#!/bin/bash
#SBATCH --job-name=%s
#SBATCH --partition=%s
#SBATCH --ntasks=%d
#SBATCH --nodes=%d
#SBATCH --time=%s

source /home/fuchs/fias/knguyen/.bashrc
conda activate %s

cd %s
srun R CMD BATCH --no-save --no-restore ./run.r
",
      job_name, partition, ntasks, nodes, time,
      conda_env,
      work_dir
    ),
    file = slurm_path
  )
  
  message(
    "Files written: ", rscript_path, ", ", slurm_path, ", ", rsave_path
  )
  
  if (submit) {
    msg <- tryCatch(
      system2(submit_cmd, slurm_path, stdout = TRUE, stderr = TRUE),
      error = function(e) paste("Error submitting SLURM job:", e$message)
    )
    message("Submission output:\n", paste(msg, collapse = "\n"))
    return(invisible(list(submitted = TRUE, output = msg,
                         rscript = rscript_path, slurm = slurm_path, rdata = rsave_path)))
  } else {
    message("To submit, run: ", submit_cmd, " ", slurm_path)
    return(invisible(list(submitted = FALSE,
                         rscript = rscript_path, slurm = slurm_path, rdata = rsave_path)))
  }
}

#' Check SLURM job status
#'
#' Inspect SLURM job status using multiple heuristics:
#' - read a saved job id from a file (.last_sbatch_jobid),
#' - guess job id from recent slurm-*.out logs,
#' - or extract job name from a SLURM submission script (#SBATCH --job-name=).
#' The function then calls squeue to report whether the job is queued/running or not found.
#'
#' @param slurm_file Path to the SLURM submission script to inspect (default: "submit_inla.sh").
#' @param submit_cmd Submission command used previously (kept for compatibility; default: "sbatch").
#' @return Invisibly returns a list with job_id or job_name and status/output details, or NULL if nothing found.
#' @export
#' @examples
#' \dontrun{
#' check_slurm_job_status("submit_inla.sh")
#' }
check_slurm_job_status <- function(slurm_file = "submit_inla.sh", submit_cmd = "sbatch") {
  job_name <- NULL
  job_id   <- NULL
  
  # Try to find job name in submit_inla.sh
  if (file.exists(slurm_file)) {
    lines <- readLines(slurm_file, warn = FALSE)
    name_line <- grep("^#SBATCH --job-name=", lines, value = TRUE)
    if (length(name_line)) {
      job_name <- sub("^#SBATCH --job-name=", "", name_line[1])
      job_name <- trimws(job_name)
    }
  }
  
  # Try to extract job ID from recent sbatch output, or check job files
  jobid_file <- ".last_sbatch_jobid"
  if (file.exists(jobid_file)) {
    job_id <- readLines(jobid_file, n = 1, warn = FALSE)
    job_id <- gsub("[^0-9]", "", job_id)
    if (!nzchar(job_id)) job_id <- NULL
  }
  
  # Fallback: try to find the most recent .slurm* file
  if (is.null(job_id)) {
    slurm_logs <- Sys.glob("slurm-*.out")
    if (length(slurm_logs)) {
      newest <- slurm_logs[which.max(file.mtime(slurm_logs))]
      idguess <- sub("slurm-(\\d+)\\.out.*", "\\1", newest)
      if (grepl("^[0-9]+$", idguess)) job_id <- idguess
    }
  }
  
  # Build squeue command for status
  if (!is.null(job_id)) {
    # squeue -j JOBID gives a line if the job is queued/running
    stat_txt <- try(system2("squeue", c("-j", job_id), stdout = TRUE, stderr = TRUE), silent = TRUE)
    if (inherits(stat_txt, "try-error")) stat_txt <- "Unable to check job status (squeue failed?)"
    # Remove header line, just show line with jobID if it exists
    lines <- stat_txt
    show <- if (length(lines) > 1) lines[-1] else character(0)
    status <- if (length(show)) "QUEUED or RUNNING" else "NOT FOUND (may be finished or deleted from squeue)"
    message(sprintf("Job ID: %s  Status: %s\nDetails:\n%s", job_id, status, paste(show, collapse = "\n")))
    return(invisible(list(job_id = job_id, status = status, output = show)))
  }
  # If no job ID, try checking by name
  if (!is.null(job_name)) {
    stat_txt <- try(system2("squeue", c("-n", shQuote(job_name)), stdout = TRUE, stderr = TRUE), silent = TRUE)
    if (inherits(stat_txt, "try-error")) stat_txt <- "Unable to check job status (squeue failed?)"
    message(sprintf("Job name: %s\nsqueue output:\n%s", job_name, paste(stat_txt, collapse = "\n")))
    return(invisible(list(job_name = job_name, output = stat_txt)))
  }
  message("Unable to determine job status: no job ID or name found.")
  invisible(NULL)
}
