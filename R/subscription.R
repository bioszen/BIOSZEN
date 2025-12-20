#' Supabase authentication and subscription helpers
#'
#' These utilities provide a thin wrapper around the Supabase REST API
#' to authenticate a user and verify device entitlements. They are kept
#' internal to the package and rely on two environment variables:
#'
#' - `SUPABASE_URL`
#' - `SUPABASE_ANON_KEY`
#'
#' Functions here use `httr2` for HTTP requests and return parsed JSON
#' objects via `jsonlite`.
#'
#' @keywords internal

# base request with project URL and anon key
.sb_request <- function(path) {
  httr2::request(paste0(Sys.getenv("SUPABASE_URL"), path)) |>
    httr2::req_headers(
      apikey = Sys.getenv("SUPABASE_ANON_KEY"),
      "Content-Type" = "application/json"
    )
}

# login with email and password, returning access token and user id
sb_login <- function(email, password) {
  if (Sys.getenv("SUPABASE_URL") == "" || Sys.getenv("SUPABASE_ANON_KEY") == "") {
    stop("Faltan variables SUPABASE_URL o SUPABASE_ANON_KEY", call. = FALSE)
  }
  req <- .sb_request("/auth/v1/token?grant_type=password") |>
    httr2::req_body_json(list(email = email, password = password))
  resp <- httr2::req_perform(req)
  if (httr2::resp_status(resp) >= 400) {
    err <- try(httr2::resp_body_json(resp)$error_description, silent = TRUE)
    stop(if (inherits(err, "try-error")) "Credenciales inválidas" else err, call. = FALSE)
  }

  out <- httr2::resp_body_json(resp)
  list(access_token = out$access_token, user_id = out$user$id)
}

# login via Google OAuth in browser
sb_login_google <- function() {
  if (Sys.getenv("SUPABASE_URL") == "" || Sys.getenv("SUPABASE_ANON_KEY") == "") {
    stop("Faltan variables SUPABASE_URL o SUPABASE_ANON_KEY", call. = FALSE)
  }
  redirect <- "http://127.0.0.1:4321/auth/callback"
  auth_url <- paste0(Sys.getenv("SUPABASE_URL"),
                     "/auth/v1/authorize?provider=google&redirect_to=",
                     utils::URLencode(redirect, reserved = TRUE))
  token_env <- new.env(parent = emptyenv())
  handler <- list(
    call = function(req) {
      path <- req$PATH_INFO
      if (identical(path, "/auth/callback")) {
        html <- "<html><body>Autenticación completada. Puede cerrar esta ventana.<script>fetch('/auth/token?' + window.location.hash.substring(1)).then(()=>window.close());</script></body></html>"
        list(status = 200, headers = list(`Content-Type` = "text/html"), body = html)
      } else if (identical(path, "/auth/token")) {
        q <- httr2::url_parse(req$REQUEST_URI)$query
        token_env$access <- q[["access_token"]]
        list(status = 200, headers = list(`Content-Type` = "text/plain"), body = "OK")
      } else {
        list(status = 404, headers = list(), body = "")
      }
    }
  )
  srv <- httpuv::startServer("127.0.0.1", 4321, handler)
  utils::browseURL(auth_url)
  while (is.null(token_env$access)) Sys.sleep(0.1)
  httpuv::stopServer(srv)
  req <- .sb_request("/auth/v1/user") |>
    httr2::req_headers(Authorization = paste("Bearer", token_env$access))
  resp <- httr2::req_perform(req)
  out <- httr2::resp_body_json(resp)
  list(access_token = token_env$access, user_id = out$id)
}

# generic RPC call with authorization
.sb_rpc <- function(fn, token, body = list()) {
  req <- .sb_request(paste0("/rest/v1/rpc/", fn)) |>
    httr2::req_headers(Authorization = paste("Bearer", token)) |>
    httr2::req_body_json(body)
  resp <- httr2::req_perform(req)
  httr2::resp_body_json(resp)
}

# query v_entitlement view for current user
sb_get_entitlement <- function(user_id, token) {
  url <- paste0("/rest/v1/v_entitlement?user_id=eq.", user_id, "&select=*")
  req <- .sb_request(url) |>
    httr2::req_headers(Authorization = paste("Bearer", token))
  resp <- httr2::req_perform(req)
  out <- httr2::resp_body_json(resp)
  if (length(out) == 0) NULL else out[[1]]
}

# register device and start trial if needed
check_or_start_trial <- function(device_id, token, user_id) {
  ent <- sb_get_entitlement(user_id, token)
  now <- Sys.time()
  if (!is.null(ent) && ent$status %in% c("active", "trialing") &&
      now < as.POSIXct(ent$valid_until, tz = "UTC")) {
    return(TRUE)
  }
  res <- .sb_rpc("register_device", token, list(p_device_id = device_id))
  if (!isTRUE(res$ok)) return(FALSE)
  res2 <- .sb_rpc("start_trial_if_eligible", token, list(p_device_id = device_id))
  isTRUE(res2$ok)
}

# obtain a simple device identifier, falling back to host/machine pair
get_device_id <- function() {
  id <- Sys.getenv("DEVICE_ID")
  if (id == "") {
    info <- Sys.info()
    id <- paste(info[["nodename"]], info[["machine"]], sep = "-")
  }
  id
}

# ---- Local caching of auth tokens ---------------------------------------
.cache_dir <- function() {
  dir <- file.path(Sys.getenv("LOCALAPPDATA"), "BIOSZEN")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}

.cache_file <- function() file.path(.cache_dir(), "license.json")

cache_auth_save <- function(token, user_id, days = 28) {
  jsonlite::write_json(
    list(
      token = token,
      user_id = user_id,
      valid_until = Sys.time() + as.difftime(days, units = "days")
    ),
    .cache_file(),
    auto_unbox = TRUE
  )
}

cache_auth_load <- function() {
  path <- .cache_file()
  if (!file.exists(path)) return(NULL)
  out <- tryCatch(jsonlite::read_json(path), error = function(e) NULL)
  if (is.null(out)) return(NULL)
  if (is.null(out$valid_until) ||
      Sys.time() > as.POSIXct(out$valid_until, tz = "UTC")) {
    return(NULL)
  }
  out
}

