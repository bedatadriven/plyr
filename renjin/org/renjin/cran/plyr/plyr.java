package org.renjin.cran.plyr;

import com.google.common.collect.Lists;
import org.renjin.eval.Context;
import org.renjin.eval.EvalException;
import org.renjin.invoke.annotations.Current;
import org.renjin.sexp.*;

import java.util.List;

/**
 * Replacements for the native functions coded in C/C++.
 * 
 * <p>Renjin </p>
 */
public class plyr {

  //' Split indices.
//'
//' An optimised version of split for the special case of splitting row
//' indices into groups, as used by \code{\link{splitter_d}}.
//'
//' @param index integer indices
//' @param n largest integer (may not appear in index). This is hint: if
//'   the largest value of \code{group} is bigger than \code{n}, the output
//'   will silently expand.
//' @useDynLib plyr
//' @keywords internal manip
//' @export
//' @examples
//' split_indices(sample(10, 100, rep = TRUE))
//' split_indices(sample(10, 100, rep = TRUE), 10)
// [[Rcpp::export]]
  public static ListVector plyr_split_indices(IntVector group, int n) {
    if (n < 0) throw new EvalException("n must be a positive integer");

    List<IntArrayVector.Builder> ids = Lists.newArrayListWithCapacity(n);

    for (int i = 0; i < group.length(); ++i) {
      // group is 1-indexed
      int groupIndex = group.getElementAsInt(i) - 1;
      
      while(ids.size() <= groupIndex) {
        ids.add(new IntArrayVector.Builder());
      }
      ids.get(groupIndex).add(i + 1);
    }

    ListVector.Builder list = new ListVector.Builder(0, ids.size());
    for (int i = 0; i < ids.size(); ++i) {
      list.add(ids.get(i).build());
    }
    
    return list.build();
  }

  public static SEXP loop_apply_(@Current Context context, int n, Function f, Environment rho) {
    ListVector.Builder results = new ListVector.Builder(0, n);

    for(int i = 0; i < n; ++i) {
      results.add(context.evaluate(FunctionCall.newCall(f, IntVector.valueOf(i)), rho));
    }
    
    return results.build();
  }
}
