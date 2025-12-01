

# find a good crop
plot(lcm, main = "Honefoss",ext = c(4325000,4340000,4110000,4125000))

# take a look at sdam
plot(sdam_econ, main = "Honefoss",ext = c(4325000,4340000,4110000,4125000))
plot(sdam_pop, main = "Honefoss",ext = c(4325000,4340000,4110000,4125000))

# now take a look at potm
plot(potm, main = "Honefoss",ext = c(4325000,4340000,4110000,4125000))

# and a look at spam
plot(spam, main = "Honefoss",ext = c(4325000,4340000,4110000,4125000))

# look at total flow accum
plot(flow_acc_total, main = "Honefoss",ext = c(4325000,4340000,4110000,4125000))

# make a tmph map that gives weights for the flow accumulation function
tmph<- 1-(potm/100)

# the weight map cannot have NAs. So I turn all NA cells into 1, implying that water
# just flows across them (zero imperviousness)
tmph[is.na(tmph)]<-1

# plot it
plot(tmph, main = "Honefoss",ext = c(4325000,4340000,4110000,4125000))

# plot flowdir
plot(flowdir_total, main = "Honefoss",ext = c(4325000,4340000,4110000,4125000))

flow_acc_tmph <- terra::flowAccumulation(flowdir_total,tmph)
# take a look at the resulting map
plot(flow_acc_tmph, main = "Honefoss",ext = c(4325000,4340000,4110000,4125000))

# compare to total accumulation
plot(flow_acc_total, main = "Honefoss",ext = c(4325000,4340000,4110000,4125000))

# make the ratio
ratioh <- 1 - flow_acc_tmph/flow_acc_total 

# look at the ratio
plot(ratioh, main = "Honefoss",ext = c(4325000,4340000,4110000,4125000))

# and then look at who benefits
flowdeconh<-sdam_econ*ratioh
flowdpoph<-sdam_pop*ratioh

# and then look at the result
plot(flowdeconh, main = "Honefoss",ext = c(4325000,4340000,4110000,4125000))
plot(flowdpoph, main = "Honefoss",ext = c(4325000,4340000,4110000,4125000))

# and then compare with the binary based results
plot(flow_d_econ, main = "Honefoss",ext = c(4325000,4340000,4110000,4125000))
plot(flow_d_pop, main = "Honefoss",ext = c(4325000,4340000,4110000,4125000))

plot(sdam_econ)


plot(flow_d_pop_c)

