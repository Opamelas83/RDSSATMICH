options(DSSAT.CSM = 'dscsm048')

write_dssbatch("UYAB1901.CSX", trtno = 1:67)




run_dssat(suppress_output = TRUE)
