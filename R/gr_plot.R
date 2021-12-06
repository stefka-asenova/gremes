
# plotmy<- function(obj, k, ylabstr, true_val, ends_to_params, names_estimators,...)
# {
#
#   a<-
#   matplot(k, t(obj),
#           type = "l",
#           lty=c(1:nrow(obj)),
#           ylab=ylabstr,
#           ylim=c(min(0,min(obj)), max(0,max(obj))),
#           main = substitute(paste(theta[e]," = ",t),
#                             list(e=paste(ends_to_params[1],ends_to_params[2], sep=""),
#                                  t= true_val )),
#           col=1,
#           ...)
#   abline(a = 0, b = 0)
#   if (!is.null(names_estimators))
#   {
#     legend(x="topright",names_estimators,
#            lty = c(1:nrow(obj)),
#            col=1,
#            ...)
#   }
# }


#' Plot method for Simulation results.
#' @param x an object of class Simulation
#' @param ind_params a numeric vector of indices which indicates for which parameters the plots
#' are to be produced. For instance: the object that contains the estimates has four dimensions,
#' the first one equal to the number of parameters in the model. If ind_params=c(1,3) then plots
#' will be produced for the parameters in the first and third places of the first dimension
#' of the object with estimates.
#' @param path_to_file the path to the place where the file will be stored
#' @param name_file the name of the file
#' @param ... additional arguments
#' @export
#' @importFrom grDevices pdf dev.off
#' @importFrom graphics abline curve legend matplot par title
plot.Simulation<- function(x, ind_params, path_to_file=NULL, name_file= "plot", ...)
{

  # #debug
 # obj<- simobj
#  ind_params<- 4
#  path_to_file=NULL
  # #-------------


  # calculates the bias, variance and rmse for all numbers in the ind_params and then plots all
  # that is in the matrices bias, var_, and rmse. 'ind_params' should contain the indices of the
  # parameters for which plots are requested. For instance if a plot is requested for parameters 1   # and 3 only then ind_params should be passed as c(1,3).
  # returns the bias, the variance and the rmse
obj<- x

  dim_est<- dim(obj$estimates)
M<- obj$estimates
if (is.null(obj$estimators) & length(dim(obj$estimates))==3)
{
  M<- array(0,dim=c(dim_est[1],dim_est[2],1, dim_est[3]))
  M[,,1,]<- obj$estimates
  obj$estimates<- M

}
  a<- dim(M)[3] #number of estimators
  np<- length(ind_params)
  bias<- array(0, dim=c(np, length(obj$kValues), a))
  var_<- array(0, dim=c(np, length(obj$kValues), a))
  rmse<- array(0, dim=c(np, length(obj$kValues), a))
  ii<- 1
  for (i in ind_params)
  {

    for (j in 1:length(obj$kValues))
    {
      for (l in 1:a)
      {
        index<- which((!is.nan(obj$estimates[i,j,l,]))&(obj$estimates[i,j,l, ]>0))
        aa<- obj$estimates[i,j, l, index]
        bias[ii,j,l]<- mean(aa) - obj$trueParameters[i]
        var_[ii,j,l]<- stats::var(aa)
        rmse[ii,j,l]<- sqrt(bias[ii,j,l]^2 + var_[ii,j,l])

      }
    }
    ii<- ii+1
  }

k<- obj$kValues

  # plots using latex symbols
  #lgd<- obj$estimators

  for (ii in 1:np)
  {
    if (!is.null(path_to_file))
    {
      grDevices::pdf(file = paste0(path_to_file, name_file, ii, ".pdf"),
          width=7.50, height=3.80, pointsize = 8)
    }

    par(mfrow=c(1,3))
    matplot(k, bias[ii,,],
            type = "l",
            lty = c(1:a),
            col = c(1:a),
            lwd = 2,
            ylab = "bias",
            cex.lab = 1.5,
            cex.axis = 1.3,
            ylim = c(min(bias[ii,,],0), max(bias[ii,,],0)), ...)
    abline(a = 0, b = 0)
    matplot(k, sqrt(var_[ii,,]),
            type = "l",
            lty = c(1:a),
            col = c(1:a),
            lwd = 2,
            ylab = "st.deviation",
            cex.lab = 1.5,
            cex.axis = 1.3,
            ylim = c(0, max(sqrt(var_[ii,,]))),
            main = substitute(paste(theta[e]," = ",t),
                              list(e=paste(obj$endsToParameters[ind_params[ii],1],
                                           obj$endsToParameters[ind_params[ii],2], sep=""),
                                   t= obj$trueParameters[ind_params[ii]] )),
            cex.main = 1.5, ...)
    abline(a = 0, b = 0)

    if (!is.null(obj$estimators))
    {
      legend(x="top",obj$estimators,
           lty = c(1:a),
           col = c(1:a),
           lwd = 2,
           cex = 1.5)

    }

    matplot(k, rmse[ii,,],
            type = "l",
            lty = c(1:a),
            col = c(1:a),
            lwd = 2,
            ylab = "rmse",
            cex.lab = 1.5,
            cex.axis = 1.3,
            ylim = c(0, max(rmse[ii,,])),
            ...)
    abline(a = 0, b = 0)
    if (!is.null(path_to_file)) {dev.off()}
  }
return(list(bias, var_, rmse))
}


#######################################################
##### NOT FULLY FINISHED

# plot.Simulation1<- function(obj, ind_params, path_to_file=NULL, name_file= "plot", ...)
# {
#
#
#   # debug
#   # obj<- sim1obj
#   #  ind_params<- c(1:9)
#   #####################
#
#   M<- obj$estimates
#
#   # if obj contains more than one estimator then the first estimator is taken
#   if (length(dim(obj$estimates))>3)
#   {
#     M<- array(0,dim=c(dim_est[1],dim_est[2], dim_est[4]))
#     M<- obj$estimates[,,1,]
#     obj$estimates<- M
#
#   }
#
#   # calculation part
#   np<- length(ind_params)
#   bias<- array(0, dim=c(np, length(obj$kValues)))
#   var_<- array(0, dim=c(np, length(obj$kValues)))
#   rmse<- array(0, dim=c(np, length(obj$kValues)))
#   ii<- 1
#   for (i in ind_params)
#   {
#
#     for (j in 1:length(obj$kValues))
#     {
#       index<- which((!is.nan(M[i,j,]))&(M[i,j,]>0))
#       aa<- M[i,j, index]
#       bias[ii,j]<- mean(aa) - obj$trueParameters[i]
#       var_[ii,j]<- stats::var(aa)
#       rmse[ii,j]<- sqrt(bias[ii,j]^2 + var_[ii,j])
#
#     }
#     ii<- ii+1
#   }
#
#
#   # plotting part
#   par(mar=c(4,5.3,4,1)+.1, mfrow=c(1,3))
#   matplot(k, t(bias),
#           type = "l",
#           lty = c(1:nrow(bias)),
#           col = c(1:nrow(bias)),
#           lwd = 2,
#           ylab = "bias",
#           cex.lab = 1.5,
#           cex.axis = 1.3,
#           ylim= c(min(bias,0), max(bias,0)))
#   abline(0,0)
#   stds<- sqrt(var_)
#   matplot(k, t(stds),
#           type = "l",
#           lty = c(1:nrow(stds)),
#           col = c(1:nrow(stds)),
#           lwd = 2,
#           ylab = "st.deviation",
#           cex.lab = 1.5,
#           cex.axis = 1.3,
#           ylim = c(0, max(stds)))
#   #main = substitute(paste(theta[e]," = ",t),
#   #                 list(e=paste(obj$endsToParameters[ind_params[ii],1],
#   #                              obj$endsToParameters[ind_params[ii],2], sep=""),
#   #                       t= obj$trueParameters[ind_params[ii]] )))
#   le<- list(e=paste0(obj$endsToParameters[,1], obj$endsToParameters[,2]))
#   my_leg<- substitute(delta[e], le)
#   legend( "topleft", my_leg,
#           lty = c(1:nrow(stds)),
#           col = c(1:nrow(stds)),
#           lwd = 2,
#           cex = 1.5)
#   abline(0,0)
#   matplot(k, t(rmse),
#           type = "l",
#           lty = c(1:nrow(rmse)),
#           col = c(1:nrow(rmse)),
#           lwd = 2,
#           ylab = "rmse",
#           cex.lab = 1.5,
#           cex.axis = 1.3,
#           ylim= c(0, max(rmse)))
#   abline(0,0)
#
#   if (!is.null(path_to_file)) {dev.off()}
#   return(list(bias, var_, rmse))
# }
#
#
#
