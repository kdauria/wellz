#include <Rcpp.h>
using namespace Rcpp;

//' A very fast rolling mean with "INTERVAL"
//'
//' zoo::rollmean finds the "neighborhood" for the local mean by
//' finding the "k" closest points. This function finds all the
//' points that are distance "width" away from the point of interest
// [[Rcpp::export]]
NumericVector rollmean_fc( NumericVector x, NumericVector y, 
                           NumericVector xout, double width) {
  double total=0;
  unsigned int n=x.size(), nout=xout.size(), i, ledge=0, redge=0;
  NumericVector out(nout);
  
  for( i=0; i<nout; i++ ) {
    while( x[ redge ] - xout[i] <= width && redge<n ) 
      total += y[redge++];
    while( xout[i] - x[ ledge ] > width && ledge<n ) 
      total -= y[ledge++];
    if( ledge==redge ) { out[i]=NAN; total=0; continue; }
    out[i] = total / (redge-ledge);
  }
  return out;
}




