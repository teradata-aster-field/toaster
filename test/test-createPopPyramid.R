
data = data.frame(x=rep(1:10,2), y=rnorm(20, mean=100, sd=10), sex=rep(c('M','F'), each=10))
createPopPyramid(data=data, bin='x', count='y', divideBy='sex', values=c('M','F'))