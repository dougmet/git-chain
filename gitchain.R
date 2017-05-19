library(git2r)


get_sha <- function(repo) {
  if(is.null(git2r::head(chain))) "INIT" else git2r::branch_target(git2r::head(chain))
}

del_repo <- function(path) {
  unlink(file.path(path,".git"), force = TRUE, recursive = TRUE)
  init(path)
}

add_block <- function(chain, new_tally) {
  header <- get_sha(chain)
  tally <- new_tally
  writeLines(text = c(header, tally), con = "chain/tally.txt")
  git2r::add(chain, "chain/tally.txt")
  git2r::commit(chain, message = "default")
  get_sha(chain)
}

prove_block <- function(chain, prefix = "aa") {
  i <- 1
  sha <- get_sha(chain)
  while(substr(sha, 1,nchar(prefix)) != prefix) {
    proof_of_work <- paste(openssl::rand_bytes(20), collapse = "") 
    git2r::reset(commits(chain)[[2]], reset_type = "soft")
    git2r::commit(chain, message = proof_of_work)
    sha <- get_sha(chain)
    i <- i + 1
  }
  cat(sha, "in", i, fill = TRUE)
  sha
}

prove_first_block <- function(chain, prefix = "aa") {
  i <- 1
  sha <- get_sha(chain)
  while(substr(sha, 1,nchar(prefix)) != prefix) {
    proof_of_work <- paste(openssl::rand_bytes(20), collapse = "") 
    chain <- del_repo("chain")
    git2r::add(chain, "chain/tally.txt")
    git2r::commit(chain, message = proof_of_work)
    sha <- get_sha(chain)
    i <- i + 1
  }
  cat(sha, "in", i, fill = TRUE)
  chain
}


# Create repo -------------------------------------------------------------
if(dir.exists("chain")) unlink("chain", force = TRUE, recursive = TRUE)
dir.create("chain")
chain <- init("chain")

# Write the first block ---------------------------------------------------


sha <- add_block(chain, "First tally")
chain <- prove_first_block(chain)

# Redo until you have a nice commit id ------------------------------------

sha <- get_sha(chain)
i <- 1
while(substr(sha, 1,2) != "aa") {
  proof_of_work <- paste(openssl::rand_bytes(20), collapse = "") 
  chain <- del_repo("chain")
  git2r::add(chain, "chain/tally.txt")
  git2r::commit(chain, message = proof_of_work)
  sha <- get_sha(chain)
  i <- i + 1
}
cat(sha, i, fill = TRUE)


# Make some changes -------------------------------------------------------

sha <- add_block(chain, "second tally")
prove_block(chain)

# Make some changes -------------------------------------------------------

sha <- add_block(chain, "third tally")
prove_block(chain)
