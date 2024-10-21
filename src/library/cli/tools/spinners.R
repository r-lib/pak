
json <- "https://raw.githubusercontent.com/sindresorhus/cli-spinners/45cef9dff64ac5e36b46a194c68bccba448899ac/spinners.json"
parsed <- jsonlite::fromJSON(json, simplifyVector = TRUE)
pasis <- lapply(parsed, function(x) { x$frames <- I(x$frames); x })
pdt <- as.data.frame(do.call(rbind, pasis))
pdt$name <- rownames(pdt)
rownames(pdt) <- NULL
spinners <- pdt[, c("name", "interval", "frames")]
usethis::use_data(spinners, internal = TRUE)
spinners <- rbind(
  spinners,
  list(name = "growVeriticalDotsLR", interval = 80, frames = strsplit("⠀⡀⣀⣄⣤⣦⣶⣷⣿⣾⣶⣴⣤⣠⣀⢀", "", fixed = TRUE)),
  list(name = "growVeriticalDotsRL", interval = 80, frames = strsplit("⠀⢀⣀⣠⣤⣴⣶⣾⣿⣷⣶⣦⣤⣄⣀⡀", "", fixed = TRUE)),
  list(name = "growVeriticalDotsLL", interval = 80, frames = strsplit("⠀⡀⣀⣄⣤⣦⣶⣷⣿⣷⣶⣦⣤⣄⣀⡀", "", fixed = TRUE)),
  list(name = "growVeriticalDotsRR", interval = 80, frames = strsplit("⠀⡀⣀⣠⣤⣴⣶⣾⣿⣾⣶⣴⣤⣠⣀⢀", "", fixed = TRUE))
)
