
local({

  # the requested version of renv
  version <- "1.0.7"
  attr(version, "sha") <- NULL

  # the project directory
  project <- getwd()

  # use start-up diagnostics if enabled
  diagnostics <- Sys.getenv("RENV_STARTUP_DIAGNOSTICS", unset = "FALSE")
  if (diagnostics) {
    start <- Sys.time()
    profile <- FALSE
    on.exit({
      if (profile) {
        end <- Sys.time()
        elapsed <- difftime(end, start, units = "auto")
        prefix <- "[renv startup diagnostics]"
        message(sprintf("%s ~ Desktop: %s", prefix, .Platform$GUI))
        message(sprintf("%s ~ Elapsed: %s", prefix, format(elapsed)))
      }
    }, add = TRUE)
  }

  # signal that we're loading renv during R startup
  Sys.setenv("RENV_R_INITIALIZING" = "true")
  on.exit(Sys.unsetenv("RENV_R_INITIALIZING"), add = TRUE)

  # signal that we've consented to use renv
  options(renv.consent = TRUE)

  # load the 'utils' package eagerly -- this ensures that renv shims, which
  # mask 'utils' packages, parsing uses 'Rcmd tools/dependencies.R' will work
  library(utils, quietly = TRUE)

  # unload renv if it's already been loaded
  if ("renv" %in% loadedNamespaces())
    unloadNamespace("renv")

  # load bootstrap tools
  `%||%` <- function(x, y) {
    if (is.null(x)) y else x
  }

  catf <- function(fmt, ..., appendLF = TRUE) {

    quiet <- getOption("renv.bootstrap.quiet", default = FALSE)
    if (quiet)
      return(invisible())

    msg <- sprintf(fmt, ...)
    cat(msg, file = stdout(), sep = if (appendLF) "\n" else "")

    invisible(msg)

  }

  header <- function(label,
                     ...,
                     prefix = "#",
                     suffix = "-",
                     n = min(getOption("width"), 78))
  {
    label <- sprintf(label, ...)
    n <- max(n - nchar(label) - nchar(prefix) - 2L, 8L)
    if (n <= 0)
      return(paste(prefix, label))

    tail <- paste(rep.int(suffix, n), collapse = "")
    paste0(prefix, " ", label, " ", tail)

  }

  startswith <- function(string, prefix) {
    substring(string, 1, nchar(prefix)) == prefix
  }

  bootstrap <- function(version, library) {

    # attempt to download renv
    catf(header("Downloading renv %s", version))
    tarball <- tryCatch(renv_bootstrap_download(version), error = identity)
    if (inherits(tarball, "error"))
      stop("failed to download renv ", version)

    # now attempt to install
    catf(header("Installing renv %s", version))
    status <- tryCatch(renv_bootstrap_install(version, tarball, library), error = identity)
    if (inherits(status, "error"))
      stop("failed to install renv ", version)

  }

  renv_bootstrap_tests_running <- function() {
    getOption("renv.tests.running", default = FALSE)
  }

  renv_bootstrap_repos <- function() {

    # get CRAN repository
    cran <- getOption("renv.repos.cran", "https://cloud.r-project.org")

    # check for repos override
    repos <- Sys.getenv("RENV_CONFIG_REPOS_OVERRIDE", unset = NA)
    if (!is.na(repos)) {

      # check for NULL string
      if (identical(repos, "NULL"))
        return(character())

      return(repos)

    }

    # check for lockfile
    repos <- tryCatch(renv_bootstrap_repos_lockfile(), error = identity)
    if (!inherits(repos, "error") && length(repos))
      return(repos)

    # retrieve current repos
    repos <- getOption("repos")

    # ensure @CRAN@ entries are resolved
    repos[repos == "@CRAN@"] <- cran

    # add in renv.repos.* options if set
    envvars <- Sys.getenv()
    idx <- which(startswith(names(envvars), "RENV_CONFIG_REPOS_"))
    if (length(idx)) {
      repos <- as.list(repos)
      for (i in idx) {
        name <- substring(names(envvars)[[i]], nchar("RENV_CONFIG_REPOS_") + 1L)
        repos[[name]] <- envvars[[i]]
      }
    }

    # remove duplicates and return repos
    repos <- unlist(repos)
    dupes <- duplicated(repos) | duplicated(names(repos))
    repos[!dupes]

  }

  renv_bootstrap_repos_lockfile <- function() {

    lockfile <- Sys.getenv("RENV_PATHS_LOCKFILE", unset = "renv.lock")
    if (!file.exists(lockfile))
      return(NULL)

    lockfile_json <- renv_json_read(lockfile)
    repos <- lockfile_json$R$Repositories
    if (length(repos) == 0)
      return(NULL)

    keys <- vapply(repos, `[[`, "Name", FUN.VALUE = character(1))
    vals <- vapply(repos, `[[`, "URL", FUN.VALUE = character(1))
    names(vals) <- keys

    return(vals)

  }

  renv_bootstrap_download <- function(version) {

    sha <- attr(version, "sha", exact = TRUE)

    methods <- if (!is.null(sha)) {
      list(
        renv_bootstrap_download_github
      )
    } else {
      list(
        renv_bootstrap_download_cran_latest,
        renv_bootstrap_download_cran_archive
      )
    }

    for (method in methods) {
      path <- tryCatch(method(version), error = identity)
      if (is.character(path) && file.exists(path))
        return(path)
    }

    stop("All download methods failed")

  }

  renv_bootstrap_download_cran_latest <- function(version) {

    spec <- renv_bootstrap_download_cran_specification(version)
    repos <- spec$repos
    name <- spec$name
    type <- spec$type

    if (is.null(repos))
      stop("CRAN repositories not available")

    urls <- contrib.url(repos, type)
    if (type == "source") {
      ext <- ".tar.gz"
    } else if (Sys.info()[["sysname"]] == "Windows") {
      ext <- ".zip"
    } else {
      ext <- ".tgz"
    }

    for (url in urls) {

      candidate <- file.path(url, paste0(name, ext))
      destfile <- file.path(tempdir(), basename(candidate))

      status <- tryCatch(
        renv_bootstrap_download_impl(candidate, destfile),
        error = identity
      )

      if (!inherits(status, "error"))
        return(destfile)

    }

    stop("All download methods failed")

  }

  renv_bootstrap_download_cran_archive <- function(version) {

    spec <- renv_bootstrap_download_cran_specification(version)
    repos <- spec$repos
    name <- spec$name

    if (is.null(repos))
      stop("CRAN repositories not available")

    urls <- file.path(repos, "src/contrib/Archive/renv", name)

    for (url in urls) {

      destfile <- file.path(tempdir(), basename(url))

      status <- tryCatch(
        renv_bootstrap_download_impl(url, destfile),
        error = identity
      )

      if (!inherits(status, "error"))
        return(destfile)

    }

    stop("All download methods failed")

  }

  renv_bootstrap_download_cran_specification <- function(version) {

    name <- sprintf("renv_%s", version)
    repos <- renv_bootstrap_repos()

    # try binary types first on appropriate platforms
    sysos <- Sys.info()[["sysname"]]
    types <- c("source")

    if (sysos == "Darwin") {
      if (getRversion() >= "4.0.0") {
        types <- c("mac.binary.big-sur-arm64", "mac.binary.big-sur-x86_64", types)
      } else {
        types <- c("mac.binary", types)
      }
    } else if (sysos == "Windows") {
      types <- c("win.binary", types)
    }

    for (type in types) {

      version_fmt <- if (type == "source")
        version
      else
        gsub("-", ".", version)

      name <- sprintf("renv_%s", version_fmt)
      urls <- contrib.url(repos, type)

      for (url in urls) {

        ok <- renv_bootstrap_download_cran_available(url, name)
        if (ok)
          return(list(name = name, type = type, repos = repos))

      }

    }

    return(list(name = name, type = "source", repos = repos))

  }

  renv_bootstrap_download_cran_available <- function(url, name) {

    pdb <- tryCatch(
      available.packages(contriburl = url),
      error = identity,
      warning = identity
    )

    if (inherits(pdb, "condition"))
      return(FALSE)

    name %in% rownames(pdb)

  }

  renv_bootstrap_download_github <- function(version) {

    enabled <- Sys.getenv("RENV_BOOTSTRAP_FROM_GITHUB", unset = "TRUE")
    if (!identical(enabled, "TRUE"))
      return(FALSE)

    sha <- attr(version, "sha", exact = TRUE)

    url <- if (is.null(sha)) {
      file.path("https://api.github.com/repos/rstudio/renv/tarball", version)
    } else {
      file.path("https://api.github.com/repos/rstudio/renv/tarball", sha)
    }

    destfile <- file.path(tempdir(), sprintf("renv-%s.tar.gz", version))
    status <- tryCatch(
      renv_bootstrap_download_impl(url, destfile),
      error = identity
    )

    if (inherits(status, "error"))
      return(FALSE)

    destfile

  }

  renv_bootstrap_download_impl <- function(url, destfile) {

    mode <- "wb"

    args <- list(
      url      = url,
      destfile = destfile,
      mode     = mode,
      quiet    = TRUE
    )

    if ("headers" %in% names(formals(utils::download.file)))
      args$headers <- renv_bootstrap_download_custom_headers(url)

    do.call(utils::download.file, args)

  }

  renv_bootstrap_download_custom_headers <- function(url) {

    headers <- getOption("renv.download.headers")
    if (is.null(headers))
      return(character())

    if (!is.function(headers))
      stopf("'renv.download.headers' is not a function")

    headers <- headers(url)
    if (length(headers) == 0L)
      return(character())

    if (is.list(headers))
      headers <- unlist(headers, recursive = FALSE, use.names = TRUE)

    if (!is.character(headers))
      stopf("'renv.download.headers' did not return a character vector")

    headers

  }

  renv_bootstrap_install <- function(version, tarball, library) {

    # attempt to install it into project library
    dir.create(library, showWarnings = FALSE, recursive = TRUE)

    # invoke using system2 so we can capture and report output
    bin <- R.home("bin")
    exe <- if (Sys.info()[["sysname"]] == "Windows") "R.exe" else "R"
    R <- file.path(bin, exe)

    args <- c(
      "--vanilla", "CMD", "INSTALL", "--no-multiarch",
      "-l", shQuote(path.expand(library)),
      shQuote(path.expand(tarball))
    )

    output <- system2(R, args, stdout = TRUE, stderr = TRUE)
    message(paste(output, collapse = "\n"))

    # check for successful install
    status <- attr(output, "status") %||% 0L
    if (!identical(status, 0L)) {
      header <- "Installation of renv failed"
      lines <- paste(rep.int("=", nchar(header)), collapse = "")
      text <- paste(c(header, lines, output), collapse = "\n")
      stop(text)
    }

    # return the path to the installed renv
    invisible(file.path(library, "renv"))

  }

  renv_bootstrap_platform_prefix <- function() {

    # construct version prefix
    version <- paste(R.version$major, R.version$minor, sep = ".")
    prefix <- paste("R", numeric_version(version)[1, 1:2], sep = "-")

    # include SVN revision for development versions of R
    # (to avoid sharing platform-specific artefacts with released versions of R)
    devel <-
      identical(R.version[["status"]],   "Under development (unstable)") ||
      identical(R.version[["nickname"]], "Unsuffered Consequences")

    if (devel)
      prefix <- paste(prefix, R.version[["svn rev"]], sep = "-r")

    # build list of path components
    components <- c(prefix, R.version$platform)

    # include prefix if provided by user
    prefix <- renv_bootstrap_platform_prefix_impl()
    if (!is.na(prefix) && nzchar(prefix))
      components <- c(prefix, components)

    # build prefix
    paste(components, collapse = "/")

  }

  renv_bootstrap_platform_prefix_impl <- function() {

    # if an explicit prefix has been supplied, use it
    prefix <- Sys.getenv("RENV_PATHS_PREFIX", unset = NA)
    if (!is.na(prefix))
      return(prefix)

    # if the user has requested
    auto <- Sys.getenv("RENV_PATHS_PREFIX_AUTO", unset = NA)
    if (is.na(auto) && getRversion() >= "4.4.0")
      auto <- "TRUE"

    if (identical(auto, "TRUE"))
      return(renv_bootstrap_platform_prefix_auto())

    # empty prefix by default
    ""

  }

  renv_bootstrap_platform_prefix_auto <- function() {

    prefix <- tryCatch(renv_bootstrap_platform_prefix_auto_impl(), error = identity)
    if (inherits(prefix, "error") || prefix == "unknown") {
      msg <- paste(
        "failed to infer current operating system",
        "please file a bug report at https://github.com/rstudio/renv/issues",
        sep = "; "
      )
      warning(msg)
    }

    prefix

  }

  renv_bootstrap_platform_prefix_auto_impl <- function() {

    # on non-Linux systems, just use sysname
    sysinfo <- Sys.info()
    sysname <- sysinfo[["sysname"]]
    if (sysname != "Linux")
      return(tolower(sysname))

    # read /etc/os-release if it exists
    release <- if (file.exists("/etc/os-release"))
      readLines("/etc/os-release", warn = FALSE)

    # pattern to use for matching lines
    pattern <- "^([[:alnum:]_]+)=(.*)$"

    # skip non-matching lines
    ok <- grepl(pattern, release)
    release <- release[ok]

    # if no releases matching, fallback to unknown
    if (length(release) == 0)
      return("unknown")

    # extract keys + values
    keys <- sub(pattern, "\\1", release)
    vals <- sub(pattern, "\\2", release)
    vals <- gsub("\"", "", vals)
    names(vals) <- keys

    # try to guess the distro type
    distro_id <- tolower(vals[["ID"]])
    if (is.na(distro_id))
      return("unknown")

    # compute version
    version <-
      vals[["VERSION_ID"]] %||%
      vals[["BUILD_ID"]] %||%
      vals[["DISTRIB_RELEASE"]] %||%
      ""

    # for version, just use first component
    if (nzchar(version)) {
      version <- regmatches(version, regexpr("^[[:alnum:]]+", version))
      distro_id <- paste(distro_id, version, sep = "-")
    }

    distro_id

  }

  renv_bootstrap_library_root_name <- function(project) {

    # use project name if set
    name <- basename(project)

    # use filesystem root if project is root
    if (name == "")
      name <- paste0("root", file.path(project, ""))

    name

  }

  renv_bootstrap_library_root_impl <- function(project) {

    path <- Sys.getenv("RENV_PATHS_LIBRARY_ROOT", unset = NA)
    if (!is.na(path))
      return(path)

    path <- renv_bootstrap_library_root_impl_compute(project)
    Sys.setenv("RENV_PATHS_LIBRARY_ROOT" = path)
    path

  }

  renv_bootstrap_library_root_impl_compute <- function(project) {

    name <- renv_bootstrap_library_root_name(project)

    for (path in renv_bootstrap_library_root_impl_candidates()) {

      # make sure it's a directory
      if (!file.exists(path))
        next

      info <- file.info(path)
      if (is.na(info$isdir) || !info$isdir)
        next

      # check for write access
      if (file.access(path, 2) != 0L)
        next

      # if we got here, we found the root
      # see: https://github.com/rstudio/renv/issues/1711
      return(file.path(path, "renv/library", name))

    }

    file.path(project, "renv/library")

  }

  renv_bootstrap_library_root_impl_candidates <- function() {

    # try some default directories
    candidates <- c(
      Sys.getenv("XDG_CACHE_HOME", unset = NA),
      Sys.getenv("XDG_DATA_HOME", unset = NA)
    )

    # NOTE: LOCALAPPDATA may not be available on Windows 7
    if (Sys.info()[["sysname"]] == "Windows")
      candidates <- c(candidates, Sys.getenv("LOCALAPPDATA", unset = NA))

    candidates <- candidates[!is.na(candidates)]

    # if nothing available, try some fallback default locations
    if (length(candidates) == 0) {
      candidates <- c(
        if (Sys.info()[["sysname"]] == "Darwin")
          "~/Library/Application Support",
        "~/.local/share"
      )
    }

    candidates

  }

  renv_bootstrap_library_root <- function(project) {

    path <- renv_bootstrap_library_root_impl(project)

    prefix <- renv_bootstrap_platform_prefix()
    if (nzchar(prefix))
      path <- file.path(path, prefix)

    path

  }

  renv_bootstrap_library_path_impl <- function(project) {

    root <- renv_bootstrap_library_root(project)

    path <- Sys.getenv("RENV_PATHS_LIBRARY", unset = NA)
    if (!is.na(path))
      return(path)

    root

  }

  renv_bootstrap_library_path <- function(project) {

    path <- renv_bootstrap_library_path_impl(project)

    if (!file.exists(path)) {
      ok <- dir.create(path, showWarnings = FALSE, recursive = TRUE)
      if (!ok)
        stop("failed to create library at path '", path, "'")
    }

    path

  }

  renv_bootstrap_validate_version <- function(version, description = NULL) {

    # resolve description file
    description <- description %||% {

      # first, try current project
      libroot <- renv_bootstrap_library_root(project)
      descpath <- file.path(libroot, "renv/DESCRIPTION")
      if (file.exists(descpath))
        return(descpath)

      # next, try usual library path
      for (libpath in .libPaths()) {
        descpath <- file.path(libpath, "renv/DESCRIPTION")
        if (file.exists(descpath))
          return(descpath)
      }

    }

    # no description file found; just report nothing
    if (is.null(description) || !file.exists(description))
      return(NULL)

    # read DESCRIPTION file
    contents <- readLines(description, warn = FALSE)
    idx <- grep("^Version:", contents)
    if (length(idx) != 1)
      return(NULL)

    installed <- sub("^Version:[[:space:]]*", "", contents[[idx]])

    # check our requirements
    if (version == installed)
      return(installed)

    # not an exact match; is the installed version compatible?
    # see: https://github.com/rstudio/renv/issues/1758
    compatible <- Sys.getenv("RENV_BOOTSTRAP_VERSION_CHECK_COMPATIBLE", unset = "TRUE")
    if (!identical(compatible, "TRUE"))
      return(NULL)

    # if the requested version is not stable, but the
    # installed version is, then don't proceed
    check <- tryCatch(
      utils::compareVersion(installed, version) >= 0,
      error = identity,
      warning = identity
    )

    if (identical(check, TRUE))
      return(installed)

    # version mismatch
    return(NULL)

  }

  renv_bootstrap_hash_text <- function(text) {

    hashfile <- tempfile("renv-hash-")
    on.exit(unlink(hashfile), add = TRUE)

    writeLines(text, con = hashfile)
    tools::md5sum(hashfile)

  }

  renv_bootstrap_load <- function(project, libpath, version) {

    # try to load renv from one of the library paths
    for (path in c(libpath, .libPaths())) {
      descpath <- file.path(path, "renv/DESCRIPTION")
      if (renv_bootstrap_validate_version(version, descpath)) {
        library("renv", lib.loc = path, quietly = TRUE)
        return(TRUE)
      }
    }

    FALSE

  }

  renv_json_read <- function(file = NULL, text = NULL) {

    jlerr <- NULL

    # if jsonlite is loaded, use that
    if ("jsonlite" %in% loadedNamespaces()) {
      json <- tryCatch(renv_json_read_jsonlite(file, text), error = identity)
      if (!inherits(json, "error"))
        return(json)
      jlerr <- json
    }

    # otherwise, try using our default JSON implementation
    json <- tryCatch(renv_json_read_default(file, text), error = identity)
    if (!inherits(json, "error"))
      return(json)

    # report an error
    if (!is.null(jlerr))
      stop(jlerr)

    stop(json)

  }

  renv_json_read_jsonlite <- function(file = NULL, text = NULL) {
    text <- text %||% readLines(file, warn = FALSE)
    jsonlite::fromJSON(txt = text, simplifyVector = FALSE)
  }

  renv_json_read_default <- function(file = NULL, text = NULL) {

    text <- text %||% paste(readLines(file, warn = FALSE), collapse = "\n")

    # find strings in the JSON
    pattern <- '["](?:(?:\\\\.)|(?:[^"\\\\]))*?["]'
    locs <- gregexpr(pattern, text, perl = TRUE)[[1]]

    # if any are found, replace with placeholders
    replaced <- text
    strings <- character()
    replacements <- character()

    if (!identical(c(locs), -1L)) {

      # extract strings
      starts <- locs
      ends <- locs + attr(locs, "match.length") - 1L
      strings <- substring(text, starts, ends)

      # generate placeholders
      replacements <- sprintf('"\032Column_%i\032"', seq_along(strings))

      # replace using iteration
      for (i in seq_along(strings))
        replaced <- sub(strings[[i]], replacements[[i]], replaced, fixed = TRUE)

    }

    # transform the JSON into something the R parser can understand
    transformed <- replaced
    transformed <- gsub("{}", "emptyenv()", transformed, fixed = TRUE)
    transformed <- gsub("[[{]", "list(", transformed)
    transformed <- gsub("[]}]", ")", transformed)
    transformed <- gsub(":", "=", transformed, fixed = TRUE)

    # parse it
    json <- parse(text = transformed, keep.source = FALSE, srcfile = NULL)[[1]]

    # construct map between placeholder and string
    names(strings) <- replacements

    # convert to list
    map <- function(json) {

      if (is.language(json) || is.symbol(json)) {
        return(map(eval(json, baseenv())))
      }

      if (is.character(json)) {
        idx <- match(json, names(strings))
        if (is.na(idx))
          return(json)

        return(renv_json_read_default_unescape(strings[[idx]]))
      }

      if (is.list(json)) {
        return(lapply(json, map))
      }

      json

    }

    map(json)

  }

  renv_json_read_default_unescape <- function(string) {

    # remove leading / trailing double-quotes
    string <- substring(string, 2L, nchar(string) - 1L)

    # transform unicode escapes
    string <- gsub("\\\\u([0-9a-fA-F]{4})", "\\\\U\\1", string, perl = TRUE)

    # parse the string
    eval(parse(text = paste0('"', string, '"'), keep.source = FALSE))

  }

  renv_bootstrap_exec <- function(project, libpath, version) {

    if (!renv_bootstrap_load(project, libpath, version))
      renv_bootstrap_run(version, libpath)

    # construct arguments for renv::load()
    args <- list(project = project)

    # renv < 1.0.0 used 'quiet' instead of 'verbose'
    info <- as.list(utils::packageVersion("renv"))
    if (info[[1]] < 1)
      args$quiet <- TRUE

    # call renv::load()
    do.call("load", args, envir = asNamespace("renv"))

  }

  renv_bootstrap_run <- function(version, libpath) {

    # perform bootstrap
    bootstrap(version, libpath)

    # try loading again
    if (requireNamespace("renv", lib.loc = libpath, quietly = TRUE)) {
      return(TRUE)
    }

    # failed
    FALSE

  }

  # determine path to library
  libpath <- renv_bootstrap_library_path(project)

  if (diagnostics) {
    profile <- TRUE
    message(sprintf("# renv activation diagnostics"))
    message(sprintf("- renv version: %s", version))
    message(sprintf("- libpath: %s", libpath))
    message(sprintf("- .Library: %s", .Library))
    message(sprintf("- .libPaths(): %s", paste(.libPaths(), collapse = ", ")))
  }

  # run bootstrap code
  renv_bootstrap_exec(project, libpath, version)

  invisible()

})
