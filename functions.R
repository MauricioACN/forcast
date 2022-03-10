library(tictoc)

reduction_size = function(df){
  tictoc::tic()
  deptos2 <- df %>% st_transform(3116)
  deptos3  <- sf::st_simplify(deptos2, preserveTopology = TRUE, dTolerance = 1000)
  deptos4 <- deptos3 %>% st_transform(4326)
  tictoc::toc()
  peso_in = object.size(df)
  peso_fin = object.size(deptos4)
  messa = paste("peso inicial de ",peso_in," , peso final de ",peso_fin, "reduccion del ",round(peso_fin/peso_in*100,2))
  message(messa)
  return(deptos4)
}
