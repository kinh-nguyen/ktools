#' Unload loaded TMB dynamics 
#' 
#' when using `devtools` multiple times, multiple vesions of the same package
#' exist. Tried to unload with several others methods but none work for me
#' except this way.
#' 
#' @param name Name of the package, exact.
#' @export 
tmb_unload <- function(name) {
    ldll <- getLoadedDLLs() 
    idx  <- grep(name, names(ldll))
    for (i in seq_along(idx)) dyn.unload(unlist(ldll[[idx[i]]])$path)
    cat('Unload ', length(idx), "loaded versions.\n")
}

#' Generate model matrix for random effect, such as AR model
#' 
#' the original value is saved a attributes
#' 
#' @param x the covariate that we wish to smooth
#' @details the original values is made unique and sorted
#' @export 
#' @examples 
#' make_re_matrix(sample(1:10, 7))
make_re_matrix <- function(x, lower, upper, space = 1) {
  require(Matrix)
  if (missing(lower)) lower <- min(x)
  if (missing(upper)) upper <- max(x)
  mm <- seq(lower, upper, space)
  id <- match(x, mm)
  ma <- matrix(0, length(x), length(mm))
  ma[cbind(1:length(x), id)] <- 1
  re <- as(ma, "sparseMatrix")
  attr(re, "value") <- min(x):max(x)
  re
}

#' Basic loading of TMB model with basic name handling, unloading
#' 
#' @param x character name of the TMB model, with or without extension is OK
#' @inheritParams TMB::config
tmb_load <- function(x, ...) {
    if (tools::file_ext(x) == 'cpp') x <- tools::file_path_sans_ext(x)
    tmb_unload(x)
    TMB::compile(paste0(x, '.cpp'))
    base::dyn.load(TMB::dynlib(x))
    TMB::config(tape.parallel = 0, DLL = x, ...)
}

#' Autoregressive order 2 - AR(2) precision matrix generator
#' 
#' @param n length 
#' @param sd standard deviation 
#' @param rho two AR2 coefficients 
#' @examples 
#' QQ <- AR2_Q(10)
#' x <- INLA::inla.qsample(1, Q, 
#'   constr = list(A = matrix(1, ncol = nrow(Q)), e = 0))
#' plot(x, type = 'l')
#' @export
AR2_Q <- function(n = 10, sd = 1, rho = c(0.9, 0.05)) {
    R <- Matrix::Diagonal(n, 1)
    for (i in 2:n) {
        if (i == 2) {
            R[i, i - 1] <- -rho[1]
            next
        }
        R[i, i - 1] <- -rho[1]
        R[i, i - 2] <- -rho[2]
    }
    R <- t(R) %*% (R) # Kai
    # precision
    Q <- (1 / sd^2) * R
    Q
}

#' Null-space penalty of the precision matrix
#' 
#' This add e.g., constraints zero intercept and slope for second-order random
#' walk model.
#' 
#' @details 
#' Using eigen decomposition to find the zero eigenvalues, which is then added
#' back to the original penalized matrix. Note that this leads to loss of
#' sparseness.
#' 
#' @param x a precision matrix
#' @export 
nullspace_penalty <- function(x) {
    eg <- eigen(x, TRUE)
    ind <- eg$values < max(eg$values) * .Machine$double.eps^.66
    U <- eg$vectors[, ind, drop = FALSE]
    x + U %*% t(U)
}

#' MakeADFun safely terminated if there is a bound error
#' 
#' to prevent TMB crashing an R session. This should not be done with TMB in parallel mode
#' 
#' @param ... MakeADFun argments
#' 
#' @export
MakeADFunSafe <- function(...) {
  if (is_windows()) stop('not able to do parallel jobs on Windows')
  .n <- TMB::openmp()
  on.exit(TMB::openmp(.n))
  TMB::openmp(1)
  o <- parallel::mcparallel(TMB::MakeADFun(...))
  parallel::mccollect(o)[[1]]
}

#' Crash testing an expression
#' 
#' Only on Unix
#' 
#' @param x this will be quoted with rlang so anything
#' 
#' @export
crash_test <- function(x) {
  if (is_windows()) stop('not able to do parallel jobs on Windows')
  t = rlang::enquo(x)
  t = rlang::quo(parallel::mcparallel(!!t))
  t = rlang::eval_tidy(t)
  parallel::mccollect(t)
}

#' Compile TMB with ktools header
#' 
#' Add ktools.hpp to compile flags
#' 
#' @param user_flags extra flag to compile
#' @param verbose print 
#' @inheritParams TMB::compile
#' @export
kompile <- function(..., user_flags, verbose=FALSE) {
  kfile <- system.file('include', 'ktools.hpp', package='ktools')
  kheader <- paste0("-I", dirname(kfile))
  if (!verbose)
    kheader <- paste(kheader, '-Wno-macro-redefined -Wno-unused-variable -Wno-unused-function -Wno-unused-local-typedefs -Wno-unknown-pragmas -Wno-c++11-inline-namespace')
  if (!missing(user_flags))
    kheader <- paste(kheader, user_flags)
  TMB::compile(..., flags=kheader)
}

#' precision to sd
#' 
#' standard deviation to precision
#' @param x precision
#' @export
prec2sd <- function(x) {
    x^-.5
}

#' sd to precision
#' 
#' standard deviation to precision
#' @param x standard deviation
#' @export
sd2prec <- function(x) {
    1/x^2
}

#' Calculate Information criteria for TMB model
#'
#' Note that the model need to report *point-wise density* of the likelihood to
#' calculate the ICs
#'
#' @param post_sample posterior samples (results from ktools::sample_tmb)
#' @param pointwise character name of pointwise predictive density reported from your model
#' @param islog Is the pointwise density or log density?
#' @param looic Report leave one out IC from `loo` package?
#' @param fix_nan replace NaN density with minimum density - pls check the likelihood yourself
#' @return WAIC-1 and WAIC-2, DIC, and LOO when requested
#' @export
tmb_ICs <- function(post_sample, pointwise = 'pwdens', islog = TRUE, looic=FALSE, fix_nan = TRUE) {
  # pointwise_predictive_density
  ppd = apply(post_sample, 1, function(x) obj$report(x)[[pointwise]])
  if (islog) ppd <- exp(ppd)
  if (any(is.na(ppd))) warnings("There are NaN in individual density.")
  if (fix_nan) ppd[is.na(ppd)] <- min(ppd, na.rm = TRUE)
  log_ppd = sum(log(rowMeans(ppd)))

  # DIC - dum logic here but lazy to improve
  post_est = obj$report(obj$env$last.par.best)[[pointwise]]
  if (islog) post_est <- exp(post_est)
  log_post = sum(log(post_est))
  mean_log = mean(colSums(log(ppd)))

  # WAIC
  log_mean_post = log(rowMeans(ppd))
  mean_log_post = rowMeans(log(ppd))

  p_DIC   = 2 * (log_post - mean_log)
  p_WAIC1 = 2 * sum(log_mean_post - mean_log_post)
  p_WAIC2 = sum(apply(log(ppd), 1, var))

  # LOO-PSIS # n_post x N
  LOOIC = NULL
  if (looic) {
    LOOIC = loo::loo(t(log(ppd)))$looic
  }

  c(DIC   = -2 * log_post + 2 * p_DIC,
    WAIC1 = -2 * (log_ppd - p_WAIC1),
    WAIC2 = -2 * (log_ppd - p_WAIC2),
    LOOIC = LOOIC
  )
}

#' Generate random walk precision structure matrix
#' 
#' @export
genR <- function(n=10, order=2, scale=1) {
    D <- diff(diag(n), diff = order)
    Q <- t(D) %*% D # == crossprod
    if (!scale) return(Q)
    Q_pert = Q + Matrix::Diagonal(n) * max(diag(Q)) * sqrt(.Machine$double.eps)
    INLA::inla.scale.model(Q_pert)
}

#' @export
gen_inla_rw <- function(n=10, order=1, sd=1, seed=123) {
    Q = INLA:::inla.rw(n, order=order, scale.model=TRUE)
    constr = list(A = rbind(rep(1, n), c(scale(1:n))), e = rep(0, 2))
    INLA::inla.qsample(1, sd^-2 * (Q+Matrix::Diagonal(n, 1e-9)), constr=constr, seed=seed)
}

#' Simulated random walk
#' 
#' @export
gen_rw <- function(n, order=2, sig=0.1) {
  D <- diff(diag(n), diff = order) # differences matrix 
  x_i = rnorm(n - order, sd = sig)
  c(t(D) %*% solve( D %*% t(D) ) %*% x_i)
}

#' @export
NullSpace <- function (A) {
  m <- dim(A)[1]; n <- dim(A)[2]
  ## QR factorization and rank detection
  QR <- base::qr.default(A)
  r <- QR$rank
  ## cases 2 to 4
  if ((r < min(m, n)) || (m < n)) {
    R <- QR$qr[1:r, , drop = FALSE]
    P <- QR$pivot
    F <- R[, (r + 1):n, drop = FALSE]
    I <- base::diag(1, n - r)
    B <- -1.0 * base::backsolve(R, F, r)
    Y <- base::rbind(B, I)
    X <- Y[base::order(P), , drop = FALSE]
    return(X)
    }
  ## case 1
  return(base::matrix(0, n, 1))
}

#' TMB fix parameters
#' @param par initial set of parameter
#' @param fix_names names of parameters to be fixed (not fitting)
#' @return TMB's format for map argument
#' @seealso \code{\link[ktools]{get_report}}.
#' @export
tmb_fixit <- function(par, fix_names) {
  for (i in names(par)) {
    if (i %in% fix_names)
      par[[i]][] <- NA
    else
      par[[i]] <- NULL
  }
  par %>% lapply(as.factor)
}

#' Get values of summary(\code{\link[TMB]{sd_report}}) by name
#' 
#' Get values of summary(\code{\link[TMB]{sd_report}}) by name
#' 
#' @param x object
#' @param name regex of parameter's name
#' @param se get the standard error as well
#' @export
get_report <- function(x, name='.*', se = TRUE) {
  pick_rows <- row2id(name, x)
  cols <- ifelse(se, c(1, 2), 1)
  x[row2id(name, x), cols]
}

#' Sample multivariates with precision matrix
#' 
#' @param n number of samples 
#' @param mean scalar/vector of the mean(s) 
#' @param prec precision matrix
#' @return n samples 
#' @export 
rmvnorm_sparseprec <- function(n, mean = rep(0, nrow(prec)), prec = diag(length(mean))) {
  z <- matrix(rnorm(n * length(mean)), ncol = n)
  L_inv <- Matrix::Cholesky(prec)
  v <- mean +
    Matrix::solve(
      as(L_inv, "pMatrix"),
      Matrix::solve(
        Matrix::t(as(L_inv, "Matrix")), z
      )
    )
  as.matrix(Matrix::t(v))
}

# y=z
# y = backsolve(chol(prec), z)
# L_inv = Matrix::Cholesky(prec, perm=FALSE, LDL=FALSE)

#' Sample TMB fit
#'
#' @param fit The TMB fit
#' @param nsample Number of samples
#' @param random_only Random only
#' @param verbose If TRUE prints additional information.
#'
#' @return Sampled fit.
#' @export
sample_tmb <- function(fit, obj, nsample = 1000, random_only = TRUE, verbose = TRUE) {
  to_tape <- TMB:::isNullPointer(obj$env$ADFun$ptr)
  if (to_tape) {
    message("Retaping...")
    obj <- with(
      obj$env,
      TMB::MakeADFun(
        data,
        parameters,
        map = map,
        random = random,
        silent = silent,
        DLL = DLL
      )
    )
  } else {
    message("No taping done.")
  }
  par.full <- obj$env$last.par.best
  if (!random_only) {
    if (verbose) message("Calculating joint precision")
    rp <- TMB::sdreport(obj, fit$par, getJointPrecision = TRUE)
    if (verbose) message("Drawing sample")
    smp <- rmvnorm_sparseprec(nsample, par.full, rp$jointPrecision)
  } else {
    r_id <- obj$env$random
    par_f <- par.full[-r_id]
    par_r <- par.full[r_id]
    hess_r <- obj$env$spHess(par.full, random = TRUE)
    smp_r <- rmvnorm_sparseprec(nsample, par_r, hess_r)
    smp <- matrix(0, nsample, length(par.full))
    smp[, r_id] <- smp_r
    smp[, -r_id] <- matrix(par_f, nsample, length(par_f), byrow = TRUE)
    colnames(smp) <- names(par.full)
  }
  smp
}

#' Create a TMB 'map' list from parameter templates
#'
#' Convert a named list of parameter values and a corresponding
#' `map_words` specification into a list of factor maps suitable for TMB's
#' `map` argument. Supports fixing all elements, tying elements, leaving
#' parameters free, or providing explicit numeric/factor patterns.
#'
#' @param params Named list of model parameters (vectors or matrices) as used
#'   when building a TMB model.
#' @param map_words Named list specifying mapping rules for parameters. Supported
#'   values include:
#'   - "all" or "fixed": fix all elements of the parameter
#'   - "tied", "shared", or "constant": tie all elements to a single level
#'   - "none" or "free": leave parameter free (no map entry created)
#'   - numeric/factor vector or matrix: explicit pattern defining groups/levels
#' @return A named list of factor objects (one per parameter) representing the
#'   mapping to be passed to TMB's `map` argument. Parameters with "none"/"free"
#'   are omitted (left free).
#' @export
#' @examples
#' params <- list(beta = rep(0, 3), Sigma = matrix(0, 2, 2))
#' make_tmb_map(params, list(beta = "shared", Sigma = "none"))
make_tmb_map <- function(params, map_words) {
  map <- list()
  for (nm in names(map_words)) {
    val <- params[[nm]]
    key <- map_words[[nm]]
    len <- length(val)
    if (is.matrix(val)) shape <- dim(val)

    # Fixed/All
    if (is.character(key) && key %in% c("all", "fixed")) {
      map_matrix <- factor(rep(NA, len), levels = NA)
      if (is.matrix(val)) dim(map_matrix) <- shape
      map[[nm]] <- map_matrix; next
    }
    # Shared/Tied
    if (is.character(key) && key %in% c("tied", "shared", "constant")) {
      map_matrix <- factor(rep(1, len))
      if (is.matrix(val)) dim(map_matrix) <- shape
      map[[nm]] <- map_matrix; next
    }
    if (is.character(key) && key %in% c("none", "free")) {
      next
    }
    # Explicit pattern
    if (is.numeric(key) || is.factor(key)) {
      if (!is.matrix(val)) {
        if (length(key) != len) stop(sprintf(
          "Map for '%s': vector pattern length (%d) does not match parameter length (%d)", nm, length(key), len))
        map_matrix <- factor(key); map[[nm]] <- map_matrix; next
      }
      if (is.matrix(key)) {
        if (!identical(dim(key), shape)) stop(sprintf(
          "Map for '%s': matrix pattern shape (%s) does not match parameter shape (%s)",
            nm, paste(dim(key), collapse=","), paste(shape, collapse=",")))
        map_matrix <- factor(as.vector(key)); dim(map_matrix) <- shape; map[[nm]] <- map_matrix; next
      }
      if (length(key) == len) {
        map_matrix <- factor(key); dim(map_matrix) <- shape; map[[nm]] <- map_matrix; next
      } else {
        stop(sprintf(
          "Map for '%s': numeric/factor pattern length (%d) does not match parameter length (%d)", nm, length(key), len))
      }
    }
    stop(sprintf("Unrecognized map specification for '%s': %s", nm, key))
  }
  return(map)
}

#' Run TMB Model on SLURM Cluster
#'
#' Creates and optionally submits a SLURM job to run a TMB model on a computing cluster.
#'
#' @param data Data list for TMB model
#' @param params Parameter list for TMB model
#' @param random Character vector of random effect parameter names
#' @param map_words Named list of parameter mapping specifications
#' @param dll Character name of compiled TMB model (without extension)
#' @param n_cores Number of cores for OpenMP parallelization
#' @param job_name Character name for SLURM job
#' @param partition Character name of SLURM partition
#' @param time Character time limit for job ("HH:MM:SS")
#' @param ntasks Number of tasks for SLURM
#' @param nodes Number of nodes for SLURM
#' @param conda_env Character name of conda environment
#' @param rscript Character name for generated R script
#' @param work_dir Character path to working directory
#' @param submit Logical whether to submit job immediately
#' @param submit_cmd Character command to submit SLURM job
#' @param data_path Character path to save data
#' @param param_path Character path to save parameters
#' @param map_path Character path to save parameter map
#' @param bashrc_path Character path to bashrc file
#'
#' @return Invisibly returns a list with:
#'   \item{submitted}{Logical indicating if job was submitted}
#'   \item{output}{System output from job submission if applicable}
#'   \item{rscript}{Path to generated R script}
#'   \item{slurm}{Path to generated SLURM script}
#'   \item{data}{Path to saved data}
#'   \item{param}{Path to saved parameters}
#'   \item{map}{Path to saved parameter map if applicable}
#'
#' @details
#' This function generates necessary R and SLURM scripts to run a TMB model
#' on a SLURM-based computing cluster. It saves model data and parameters
#' to files, creates an R script to fit the model, and a SLURM submission
#' script. The job can be submitted immediately or the scripts can be
#' saved for later submission.
#'
#' @export
run_tmb_on_slurm <- function(
  data,
  params,
  random = NULL,
  map_words = NULL,           
  dll = "model",
  n_cores     = NULL,        
  job_name    = "TMBjob",
  partition   = "fuchs",
  time        = "8:00:00",
  ntasks      = 1,
  nodes       = 1,
  conda_env   = "kinh",
  rscript     = "run_tmb.r",
  work_dir    = ".",
  submit      = FALSE,
  submit_cmd  = "sbatch",
  data_path   = "tmb_data.RData",
  param_path  = "tmb_params.RData",
  map_path    = "tmb_param_map.RData",
  bashrc_path = "/home/fuchs/fias/knguyen/.bashrc"
) {
  # Save supplied objects
  save(data, file = data_path)
  save(params, file = param_path)

  # Construct and save TMB map object if requested
  map <- list()
  if (!is.null(map_words)) {
    map <- make_tmb_map(params, map_words)
    save(map, file = map_path)
  }

  rscript_path <- file.path(work_dir, rscript)
  # R code to load objects and run TMB, use map if present
  rlines <- c(
    "library(TMB)",
    sprintf('load("%s")\nload("%s")', data_path, param_path),
    if (!is.null(map_words)) sprintf('load("%s")', map_path) else "",
    sprintf('compile("%s.cpp")', dll),
    sprintf('dyn.load(dynlib("%s"))', dll),
    if (!is.null(n_cores)) sprintf('openmp(n = %d)', n_cores) else "",
    sprintf(
      'obj <- MakeADFun(
  data = data,
  parameters = params%s%s,
  DLL = "%s"
)',
      if (!is.null(random)) sprintf(',\n  random = c(%s)', paste(sprintf('"%s"', random), collapse = ",")) else "",
      if (!is.null(map_words)) ',\n  map = map' else "",
      dll
    ),
    "opt <- nlminb(obj$par, obj$fn, obj$gr)",
    "sdrep <- sdreport(obj)",
    'save(opt, sdrep, obj, file="tmb_result.RData")'
  )
  writeLines(rlines, con = rscript_path)

  slurm_path <- file.path(work_dir, "submit_tmb.sh")
  cat(
    sprintf(
      "#!/bin/bash
#SBATCH --job-name=%s
#SBATCH --partition=%s
#SBATCH --ntasks=%d
#SBATCH --nodes=%d
#SBATCH --time=%s

source %s
conda activate %s

cd %s
srun R CMD BATCH --no-save --no-restore ./run_tmb.r
",
      job_name, partition, ntasks, nodes, time,
      bashrc_path,
      conda_env,
      work_dir
    ),
    file = slurm_path
  )

  message("Files written: ", rscript_path, ", ", slurm_path, ", ", data_path, ", ", param_path, if (!is.null(map_words)) paste(",", map_path) else "")
  
  if (submit) {
    msg <- tryCatch(
      system2(submit_cmd, slurm_path, stdout = TRUE, stderr = TRUE),
      error = function(e) paste("Error submitting SLURM job:", e$message)
    )
    message("Submission output:\n", paste(msg, collapse = "\n"))
    return(invisible(list(submitted = TRUE, output = msg,
                         rscript = rscript_path, slurm = slurm_path, data = data_path, param = param_path, map = if (!is.null(map_words)) map_path else NULL)))
  } else {
    message("To submit, run: ", submit_cmd, " ", slurm_path)
    return(invisible(list(submitted = FALSE,
                         rscript = rscript_path, slurm = slurm_path, data = data_path, param = param_path, map = if (!is.null(map_words)) map_path else NULL)))
  }
}

#' Check Status of TMB SLURM Job
#' 
#' Checks the status of a TMB model running as a SLURM job by examining the job queue
#' and output files. This function helps monitor TMB jobs submitted to a SLURM cluster.
#'
#' @param slurm_script Character. Path to the SLURM submission script. Default is "submit_tmb.sh".
#' @param r_batch_out Character. Path to the R batch output file. Default is "run_tmb.r.Rout".
#'
#' @return Character. Returns the job status as one of:
#'   - Full SLURM queue details if job is running/queued
#'   - "COMPLETED" if job finished successfully
#'   - "FAILED" if job ended with error
#'   - "UNKNOWN" if status cannot be determined
#'
#' @details
#' The function works by:
#' 1. Extracting job name from SLURM script
#' 2. Checking if job is in SLURM queue
#' 3. If not in queue, examining R output log for completion/error
#'
#' @export
check_tmb_slurm_status <- function(
  slurm_script = "submit_tmb.sh",
  r_batch_out = "run_tmb.r.Rout"
) {
  # Extract job name from the SLURM script
  job_name <- NULL
  if (file.exists(slurm_script)) {
    lines <- readLines(slurm_script, warn = FALSE)
    name_line <- grep("^#SBATCH --job-name=", lines, value = TRUE)
    if (length(name_line)) {
      job_name <- sub("^#SBATCH --job-name=", "", name_line[1])
      job_name <- trimws(job_name)
    }
  }
  # Query squeue for this job name
  if (!is.null(job_name)) {
    cmd <- sprintf("squeue --name=%s", shQuote(job_name))
    status <- tryCatch(system(cmd, intern = TRUE), error = function(e) NULL)
    if (length(status) > 1) { # header plus job(s)
      message("Job is currently QUEUED or RUNNING:")
      cat(paste(status, collapse = "\n"), "\n")
      return(invisible(status))
    }
  }
  # If not in squeue, check for completion via .Rout or result file
  if (file.exists(r_batch_out)) {
    message("SLURM job is not in queue. Checking output log: ", r_batch_out)
    tail_log <- tail(readLines(r_batch_out, warn = FALSE), 20)
    cat(paste(tail_log, collapse = "\n"))
    if (any(grepl("Execution halted", tail_log))) {
      warning("The job ended with an error (Execution halted).")
      return("FAILED (see log)")
    }
    if (any(grepl("save\\(opt, sdrep, obj", tail_log))) {
      message("TMB job appears to have finished successfully (result file likely written).")
      return("COMPLETED")
    }
  }
  message("Job not in queue and no log file found; it may be finished, failed, or never submitted.")
  return("UNKNOWN")
}
