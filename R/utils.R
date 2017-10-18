random.seed.gen <- function(n = 1,
                            min = 0,
                            max = 10 ^ 5) {
  seed <- sample(min:max, n, replace = TRUE)

  return(seed)
}

set.dataset <- function(dataset, type) {
  if (is.list(dataset) &&
      length(dataset) == 2 &&
      as.logical(min(c("train", "test") %in% names(dataset)))) {
    dataset <- dataset[type]
  }

  return(dataset)
}


open.conn.db <- function(host, port, user, password, dbname) {
  conn <- RPostgreSQL::dbConnect(
    drv = RPostgreSQL::PostgreSQL(),
    host = host,
    port = port,
    user = user,
    password = password,
    dbname = dbname
  )

  return(conn)
}

get.data.db <- function(conn, table, ts_gte, ts_lt) {
  if (!RPostgreSQL::dbExistsTable(conn, table))
    stop(sprintf("Table '%s' does not exist!", table))

  sql <- paste(
    "SELECT * FROM",
    table,
    "WHERE ts >= to_timestamp($1) AND ts < to_timestamp($2)",
    "ORDER BY ts ASC;"
  )

  dataset <- RPostgreSQL::dbGetQuery(conn, sql,
                                     list(ts_gte, ts_lt))

  return(dataset)
}


close.conn.db <- function(conn) {
  RPostgreSQL::postgresqlCloseConnection(conn)
  invisible()
}
