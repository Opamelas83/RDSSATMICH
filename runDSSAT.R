options(DSSAT.CSM = 'dscsm048')

write_dssbatch("UYAG1701.CSX", trtno = 1:67)




run_dssat(suppress_output = TRUE)
