

#' make_artificial_society
#'
#' Creates a homophilous social influence network
#'
#' @param society tibble containing society
#' @param homophily tibble with homophily parameters for society
#' @param nu gower distance exponent
#'
#' @return a tidygraph object
#' @export
#' @importFrom magrittr %>%
#' @examples
make_artificial_society <- function(society=society,homophily=homophily,nu=5){
  #create a random homophilous social network
  #social distance measure=gower distance
  #nu gives the social distance decay exponentlarger mu higher assortativity
  #agents with degree zero remain degree zero but there may be additional nodes with degree zero
  society_factor <- unclass(society %>% dplyr::mutate_if(is.character,as.factor)) %>% as.data.frame()
  society_factor1 <- dplyr::filter(society_factor,degree != 0)
  society1 <- dplyr::filter(society, degree != 0)

  N_society1 <- dim(society1)[1]


  zeronodes <- dplyr::filter(society,degree==0)$ID #nodes with no influencers

  dist_mat <- cluster::daisy(society_factor1[,seq(2,dim(society)[2])], metric ="gower", weights=homophily$weights) %>% as.matrix()
  prob_mat <- (1-dist_mat)^nu
  prob_mat1 <- 1.1*prob_mat %*% diag(society_factor1$degree/apply(prob_mat,2,sum)) #adjust this parameter

  nodes <- tidyr::tibble(ID=society1$ID)
  edges <- tidyr::expand_grid(from=1:N_society1,to=1:N_society1)
  edges <- dplyr::filter(edges, from < to) #avoid loops
  #edges <- filter(edges, !(from %in% zeronodes)) #no edges frsociom zeronodes
  edges <- edges %>% dplyr::rowwise() %>% dplyr::mutate(p=prob_mat1[from,to])
  edges <- edges %>% dplyr::rowwise() %>% dplyr::mutate(keep=ifelse(stats::runif(1)< p,T,F))
  edges <- edges %>% dplyr::filter(keep)

  edges <- edges[,1:2]
  edges$from <- nodes$ID[edges$from] #relable to orginal ids
  edges$to <- nodes$ID[edges$to]
  #restore zero nodes
  nodes <- dplyr::bind_rows(nodes,tidyr::tibble(ID=zeronodes)) %>% dplyr::arrange(ID)
  #restore
  g <- tidygraph::tbl_graph(nodes=nodes,edges=edges,directed=F) %>% dplyr::inner_join(society,by="ID")
  return(g)

}

#g <- make_artificial_society(pv_society_oo,homophily,4.5)
#cor(igraph::degree(g),pv_society$degree) #correlation is 73%
#plot(igraph::degree(g), pv_society$degree)


#' get_network_characteristics
#'
#' @param society society used to construct homophilous network
#' @param g homophilous social network
#'
#' @return table
#' @export
#'
#' @examples
get_network_characteristics <- function(society,g){
  homophily1 <- homophily %>% dplyr::rowwise() %>% dplyr::mutate( assortativity= igraph::assortativity_nominal(g,as.integer(factor(dplyr::pull(society,variable)))))
  knitr::kable(homophily1 %>% dplyr::filter(variable != "degree")) %>% kableExtra::kable_styling()
  }



#igraph::transitivity(g,type="global") #low transitivity ... need clique models

#social influence network compatible with

#df <- tibble()
#for(nu in seq(0.1,12,by=0.1)){
  #
#   g <- make_artificial_society(pv_society_oo,homophily,nu=nu)
#   df <- bind_rows(df, tibble(nu=nu, deg = mean(igraph::degree(g))))#, simil_areatype = assortativity(g,factor(society$area_type)),simil_education = assortativity(g,factor(society$education))))
#}

#df_0 %>% ggplot( aes(deg,nu)) + geom_point() + geom_vline(xintercept=mean(pv_society_oo$degree)) + geom_smooth()+scale_x_continuous(breaks=0:12)
# observed degrees fixes nu=4.5
#df %>% select(-deg) %>% pivot_longer(-nu) %>% ggplot( aes(nu,value,colour=name)) + geom_point()

