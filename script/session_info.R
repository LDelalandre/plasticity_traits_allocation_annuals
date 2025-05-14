# Provide information about the dependencies (for reproducibility)
write_session_info <- F
if (write_session_info == T){
  sink("session_info.txt")
  sessionInfo()
  sink()
}
