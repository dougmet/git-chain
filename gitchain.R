library(git2r)

if(dir.exists("chain")) unlink("chain", force = TRUE, recursive = TRUE)
dir.create("chain")
chain <- init("chain")

get_sha <- function(repo) {
  if(is.null(git2r::head(chain))) "INIT" else git2r::branch_target(git2r::head(chain))
}

header <- get_sha(chain)
proof_of_work <- paste(openssl::rand_bytes(20), collapse = "") 
tally <- "hello tally 1"
writeLines(text = c(header, tally), con = "chain/tally.txt")

git2r::add(chain, "chain/tally.txt")
git2r::commit(chain, message = proof_of_work)

sha <- branch_target(head(chain))

i <- 1
while(substr(sha, 1,3) != "aaa") {
  proof_of_work <- paste(openssl::rand_bytes(20), collapse = "") 
  git2r::reset(commits(chain)[[2]], reset_type = "soft")
  git2r::commit(chain, message = proof_of_work)
  sha <- get_sha(chain)
  i <- i + 1
}
cat(sha, "in", i, fill = TRUE)

