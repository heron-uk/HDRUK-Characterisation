getTime <- function(seconds) {
  s <- round(seconds %% 60)
  m <- seconds %/% 60 %% 60
  h <- seconds %/% (60*60) %% 24
  d <- seconds %/% (60*60*24)
  sprintf('%id %02ih %02im %02is', d, h, m, s)
}
