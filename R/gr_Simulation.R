
#' Creates an object of class \code{Simulation}
#'
#' It is used to generate plots by using method \code{plot} on an object of that class.
#'
#' @param T a matrix with four dimensions where the first dimension is the number of parameters,
#' the second the number of k used in the simulation, the third the number of estimators and the
#' fourth the number of repetitions in the simulation.
#' @param k a vector of k values used in the simulation
#' @param estimator_names a vector of strings indicating the names of the estimators used in
#' the simulation
#' @param params a vector of numeric values of the true parameters
#' @param ends_to_params a matrix of dimensions #parameters*2 containing the incident vertices to
#' the edges.
#' it is assumed that the order of the parameters in T, trueParameters and endsToParameters
# is the same.
Simulation<- function(T, k, params, ends_to_params, estimator_names=NULL)
{
  # it is assumed that the order of the parameters in T, trueParameters and endsToParameters
  # is the same. This is not verified but it should be put as an explanation in the documentation
  # of the function.


  # debug


  #---------------


  obj<- list(estimates = T,
             kValues = k,
             estimators = estimator_names,
             trueParameters = params,
             endsToParameters = ends_to_params)
  class(obj)<- append(class(obj), "Simulation")
  validate(obj)

  return(obj)
}















#' Creates an object of class \code{Simulation1}
#'
#' It is used to generate plots by using method \code{plot} on an object of that class.
#'
#' @param T a matrix with four dimensions where the first dimension is the number of parameters,
#' the second the number of k used in the simulation, the third the number of estimators and the
#' fourth the number of repetitions in the simulation.
#' @param k a vector of k values used in the simulation
#' @param estimator_names a vector of strings indicating the names of the estimators used in
#' the simulation
#' @param params a vector of numeric values of the true parameters
#' @param ends_to_params a matrix of dimensions #parameters*2 containing the incident vertices to
#' the edges.
#' it is assumed that the order of the parameters in T, trueParameters and endsToParameters
# is the same.
Simulation1<- function(T, k, params, ends_to_params, estimator_names=NULL)
{
  # it is assumed that the order of the parameters in T, trueParameters and endsToParameters
  # is the same. This is not verified but it should be put as an explanation in the documentation
  # of the function.


  # debug


  #---------------


  obj<- list(estimates = T,
             kValues = k,
             estimators = estimator_names,
             trueParameters = params,
             endsToParameters = ends_to_params)
  class(obj)<- append(class(obj), "Simulation1")
  validate(obj)

  return(obj)
}









