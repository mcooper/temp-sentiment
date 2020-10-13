library(data.table)

all <- fread('~/tweets/all.csv')

all[ , list(hedono=mean(hedono)), list(dow)]

#          dow   hedono
# 1:    Sunday 5.512028
# 2:  Saturday 5.514164
# 3:    Friday 5.508499
# 4:  Thursday 5.506288
# 5: Wednesday 5.503873
# 6:   Tuesday 5.501525
# 7:    Monday 5.501273

r <- all[ , list(hedono=mean(hedono)), list(doy)]

# > r[r$hedono == max(r$hedono), ]
#    doy   hedono
# 1: 359 5.639907
# > r[r$hedono == min(r$hedono), ]
#    doy   hedono
# 1:   7 5.493566
