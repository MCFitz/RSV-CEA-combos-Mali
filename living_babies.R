# calculate how many infants are alive at each age

mort_mat
num_infants

living_babies <- matrix(NA, nrow = 12, ncol = 47)
for (i in 1:length(num_infants)) {
  living_babies[i,i] <- num_infants[i]
  for (j in 1:35) {
    living_babies [i,i+j] <- living_babies[i,i+j-1]*(1-mort_mat[i,i+j])
  }
}

living_babies
rownames(living_babies) = month.abb
write.csv(living_babies, "babypop.csv", row.names = month.abb)

simple_babies <- rep(NA, 36)
simple_babies[1] <- mean(num_infants)*(1-mort_vec[1])
for (k in 2:length(mort_vec)) {
  simple_babies[k] <- simple_babies[k-1]*(1-mort_vec[k])
}

save_babies <- as.data.frame(simple_babies)
write_csv(save_babies, file = "simplepop.csv")
 
 