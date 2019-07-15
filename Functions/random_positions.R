random_positions <- function(lat1, long1, lat2, long2, n)
{

  min(long1,long2)
  
  ps_x <- seq(min(long1,long2),max(long1,long2), 0.000001) %>% 
          sample(size = n, replace = TRUE)
  
  ps_y <- seq(min(lat1,lat2), max(lat1,lat2), 0.000001) %>% 
          sample(size = n, replace = TRUE)
  
  ps <- data.frame(ID=eval(666000+1:n),LAT=ps_y,LONG=ps_x)
  
  return(ps)
}