# economic functions

medcost_func <- function(c_hosp, num_inpat, c_out, num_outpat){
  (c_hosp * num_inpat) + (c_out * num_outpat)
}

