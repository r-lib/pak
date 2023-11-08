
process__exists <- function(pid) {
  chain_call(c_processx__process_exists, pid)
}
