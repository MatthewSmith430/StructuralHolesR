#' @title structural_holes_matrix
#'
#' @description This function calulates structural holes measures for all nodes in the network.Input network is a matrix.
#' @param matrix matrix
#' @export
#' @return Dataframe of strucutral hole measures
#'
structural_holes_matrix<-function(matrix){
  egomat<-matrix
  B<-list()
  for (i in 1:ncol(egomat)){
    EFFSIZE<-egonet::index.egonet(egomat,ego.name = rownames(egomat)[i],
                            index = "effsize")
    CONSTRAINT<-egonet::index.egonet(egomat,ego.name = rownames(egomat)[i],
                             index = "constraint")
    EFF<-egonet::index.egonet(egomat,ego.name = rownames(egomat)[i],
                                     index = "efficiency")

    HIER<-egonet::index.egonet(egomat,ego.name = rownames(egomat)[i],
                              index = "hierarchy")

    GDEN1<-egonet::index.egonet(egomat,ego.name = rownames(egomat)[i],
                                index = "gden")

    EGDEN<-egonet::index.egonet(egomat,ego.name = rownames(egomat)[i],
                                index = "ego.gden")

    OUT1<-egonet::index.egonet(egomat,ego.name = rownames(egomat)[i],
                                index = "outdegree")

    IN1<-egonet::index.egonet(egomat,ego.name = rownames(egomat)[i],
                                index = "indegree")



    DF<-data.frame(id=rownames(egomat)[i],
                   effsize=as.numeric(EFFSIZE),
                   constraint=CONSTRAINT,
                   efficiency=EFF,
                   hierarchy=HIER,
                   gden=GDEN1,
                   ego.gden=EGDEN,
                   outdegree=OUT1,
                   indegree=IN1)
    B[[i]]<-DF
  }
  structural_holes_DF<-plyr::ldply(B, data.frame)

  return(structural_holes_DF)
}


