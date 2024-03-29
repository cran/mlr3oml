download_parquet = function(data_id, server, desc = download_desc_data(data_id, server)) {
  get_parquet(desc$minio_url, api_key = get_api_key(server))
}

get_parquet = function(url, ..., server, api_key = get_api_key(server), retries = 3L) {
  path = tempfile(fileext = ".parquet")
  url = sprintf(url, ...)

  lg$info("Retrieving parquet.", url = url, authenticated = !is.na(api_key))

  for (retry in seq_len(retries)) {
    response = download_file(url, path, api_key = api_key)

    if (response$ok) {
      lg$debug("Downloaded parquet file.", path = path)
      return(path)
    } else if (retry < retries && response$http_code >= 500L) {
      delay = max(rnorm(1L, mean = 10), 0)
      lg$debug("Server busy, retrying in %.2f seconds", delay, try = retry)
      Sys.sleep(delay)
    }
  }

  download_error(response)
}
