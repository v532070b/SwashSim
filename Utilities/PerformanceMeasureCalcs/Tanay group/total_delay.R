#Input detector data #note:change directory to yours
dataset <- read.csv("M:/Documents/R/PM Project/TSD_1_0_1_2223.csv")

#create matrix for data process
data_mat <- as.matrix(dataset)

#create matrix for output
n <- matrix(nrow=0, ncol=5)

#manual setting
link_length = 280  #link length

#calculate individual link delay
r = nrow(data_mat)
travel_time = 0

for(i in 1:r) {
  if (i != r) {
    z = as.numeric(data_mat[i,2])   #id 
    y = as.numeric(data_mat[i+1,2]) #id+1
    x = as.numeric(data_mat[i,21])  #desired_speed
    if (z==y){
      time_step1 = as.numeric(data_mat[i,1])
      time_step2 = as.numeric(data_mat[i+1,1])
      delta_t = time_step2 - time_step1
      travel_time = travel_time + delta_t
    } else{
      des_TT = link_length / x #desired travel time
      Ind_delay = travel_time-des_TT  #Individual vehicle delay
      d = c(z,travel_time,x,des_TT,Ind_delay)
      n = rbind(n,d)
      travel_time = 0
    }
  } else{
    z1 = as.numeric(data_mat[i,2]) #id
    x1 = as.numeric(data_mat[i,21]) #desired_speed
    des_TT1 = link_length / x1  #desired travel time
    Ind_delay1 = travel_time - des_TT1  #delay 
    f = c(z1,travel_time,x1,des_TT1,Ind_delay1)
    n = rbind(n,f)
  }
}

colnames(n) = c("ID","travel_time","desired speed","desired travel time","Individual vehicle delay")
write.csv(n,"M:/Documents/R/PM Project/totaldelay.csv")

