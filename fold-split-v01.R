##  Eunji Kim 2015-05-08

generate <- function(nshared=2938) {
        nstudy <- 3
        data <- matrix(0,nstudy,2)
	#T1D
 	#1st col=case 2nd col=cont
	data[1,1] = 1963
	data[1,2]= 0
        #CD
	data[2,1] = 1748
	data[2,2] = 0
	#RA
	data[3,1]= 1860
	data[3,2]= 0

	ntable <- matrix(0,nstudy,4)
        #initial effective number
	for(i in 1:nstudy) {
		ntable[i,1]= case = data[i,1]
		ntable[i,2]= cont = data[i,2]
		ntable[i,3] = (4*(ntable[i,1])*(ntable[i,2]))/((ntable[i,1])+(ntable[i,2]))
	}

	#ntable contains 1st col : case info  2nd col :cont 3rd col: effective number 4th will include updated controls(added) from the shared one.
        #update 
	ncheck <- rep(0,nstudy)
	oldcheck <- rep(0,nstudy)
        # done case part 
	print(c("initial table", ntable))

        for (j in 1:nshared){
				tempcont= 1
				ncheck <- rep(0,nstudy)

				for (k in 1:nstudy){
						ncheck[k] = (4*(ntable[k,1])*(ntable[k,2]+tempcont))/((ntable[k,1])+(ntable[k,2]+tempcont))
		                                ncheck[k] = ncheck[k]-oldcheck[k]
				}
				maxn <- (which(ncheck==max(ncheck)))[1]
				ntable[maxn,2] = as.numeric(ntable[maxn,2]) + 1
				ntable[maxn,3] = (4*(ntable[maxn,1])*(ntable[maxn,2]))/((ntable[maxn,1])+(ntable[maxn,2]))	
				ntable[maxn,4] = ntable[maxn,4]  + 1
				oldcheck <- ntable[,3]
	}

        #print(c("final table",ntable))
        print(c("Final shared control","T1D",ntable[1,4], "CD",ntable[2,4],"RA",ntable[3,4]))
 	#T1D: 1035 CD:922 RA: 981
	print("All done")
}
