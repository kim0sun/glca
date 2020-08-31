#include <Rcpp.h>
using namespace Rcpp;

// Combining Measurement probability
// [[Rcpp::export]]
NumericMatrix MeasProd(IntegerMatrix y,
                       List Meas,
                       int N,
                       int C,
                       int M,
                       IntegerVector R,
                       NumericMatrix Prod)
{
   int i, c, m;

   for (m = 0; m < M; m ++)
   {
      NumericMatrix tmpP = Meas[m];

      for (i = 0; i < N; i ++)
         for (c = 0; c < C; c ++)
            if (y(i, m) > 0) // MAR missing handling
               Prod(i, c) *= tmpP(c, y(i, m) - 1);
   }

   return Prod;
}


// [[Rcpp::export]]
NumericMatrix MeasProd1(IntegerMatrix y,
                        List Meas,
                        int N,
                        int C,
                        int M,
                        IntegerVector R)
{
   int i, c, m;
   NumericMatrix Prod(N, C);
   Prod.fill(std::numeric_limits<double>::max());

   for (m = 0; m < M; m ++)
   {
      NumericMatrix tmpP = Meas[m];

      for (i = 0; i < N; i ++)
         for (c = 0; c < C; c ++)
            if (y(i, m) > 0) // MAR missing handling
               Prod(i, c) *= tmpP(c, y(i, m) - 1);
   }

   return Prod;
}


// Calculate MGLCA posterior
// [[Rcpp::export]]
List GetPost(List y,
             List gamma,
             List rho,
             IntegerVector Ng,
             int G,
             int C,
             int M,
             IntegerVector R)
{
   int i, g, c;
   List Post(G);

   for (g = 0; g < G; g ++)
   {
      NumericMatrix gamma_g = gamma[g];

      // Avoid Underflow
      NumericMatrix Prod =
         std::numeric_limits<double>::max() * clone(gamma_g);
      NumericMatrix clike_g = MeasProd(y[g], rho[g], Ng[g], C, M, R, Prod);

      NumericVector mlike_g = rowSums(clike_g);
      NumericMatrix Post_g(Ng[g], C);

      for (i = 0; i < Ng[g]; i ++)
         for (c = 0; c < C; c ++)
            Post_g(i, c) = clike_g(i, c) / mlike_g[i];

      Post[g] = Post_g;
   }

   return Post;
}


// Get posterior using UD algorithm
// [[Rcpp::export]]
List GetUDPost(List y,
               NumericVector delta,
               NumericMatrix gamma,
               List rho,
               IntegerVector Ng,
               int G,
               int W,
               int C,
               int M,
               IntegerVector R)
{
   int i, g, w, c;
   List ret;
   NumericMatrix PostW(G, W);
   NumericMatrix PostWC(W, C);
   List PostC(G);

   for (g = 0; g < G; g ++)
   {
      NumericMatrix PostC_g(Ng[g], C);
      NumericVector Uprob = log(delta);
      List Dprob(W);

      NumericMatrix MeasP = MeasProd1(y[g], rho, Ng[g], C, M, R);

      for (w = 0; w < W; w ++)
      {
         NumericMatrix Dprob_w(Ng[g], C);

         for (i = 0; i < Ng[g]; i ++)
         {
            NumericVector like_cw(C);

            for (c = 0; c < C; c ++)
            {
               like_cw[c] = log(gamma(w, c)) + log(MeasP(i, c));
            }

            Uprob[w] += log(sum(exp(like_cw - max(like_cw)))) +
               max(like_cw);

            Dprob_w.row(i) = exp(like_cw - max(like_cw)) /
               sum(exp(like_cw - max(like_cw)));
         }

         Dprob[w] = Dprob_w;
      }

      Uprob = exp(Uprob - max(Uprob)) / sum(exp(Uprob - max(Uprob)));

      for (w = 0; w < W; w ++)
      {
         for (i = 0; i < Ng[g]; i ++)
         {
            for (c = 0; c < C; c ++)
            {
               NumericMatrix Dprob_w = Dprob[w];
               double value = Uprob[w] * Dprob_w(i, c);

               PostW(g, w)   += value;
               PostWC(w, c)  += value;
               PostC_g(i, c) += value;
            }
         }
      }

      PostC[g] = PostC_g;
   }

   ret["PostW"] = PostW;
   ret["PostWC"] = PostWC;
   ret["PostC"] = PostC;

   return ret;
}

// Get posterior using UD algorithm
// [[Rcpp::export]]
List GetMLLike(List y,
               NumericVector delta,
               NumericMatrix gamma,
               List rho,
               IntegerVector Ng,
               int G,
               int W,
               int C,
               int M,
               IntegerVector R)
{
   int i, g, w, c;
   List mlike(G);

   for (g = 0; g < G; g ++)
   {
      NumericVector mlike_g(Ng[g]);
      NumericMatrix MeasP = MeasProd1(y[g], rho, Ng[g], C, M, R);

      for (w = 0; w < W; w ++)
      {
         for (i = 0; i < Ng[g]; i ++)
         {
            for (c = 0; c < C; c ++)
               mlike_g[i] += delta[w] * gamma(w, c) * MeasP(i, c);
         }
      }

      mlike[g] = mlike_g / std::numeric_limits<double>::max();
   }

   return mlike;
}

// Get posterior using UD algorithm (with covariates)
// [[Rcpp::export]]
List GetUDPostX(List y,
                List x,
                List z,
                NumericVector delta,
                List gamma,
                List rho,
                IntegerVector Ng,
                int G,
                int W,
                int C,
                int P,
                int Q,
                int M,
                IntegerVector R)
{
   int i, g, w, c, c1, c2, p1, p2, q1, q2;
   double ind;
   List ret;
   NumericMatrix PostW(G, W);
   NumericMatrix PostWC(W, C);
   NumericVector grad(W * (C - 1) * P + (C - 1) * Q);
   NumericMatrix hess(W * (C - 1) * P + (C - 1) * Q,
                      W * (C - 1) * P + (C - 1) * Q);
   List PostC(G);

   for (g = 0; g < G; g ++)
   {
      IntegerMatrix y_g = y[g];
      NumericMatrix x_g = x[g];
      NumericMatrix z_g = z[g];
      List gamma_g = gamma[g];

      NumericMatrix PostC_g(Ng[g], C);
      NumericVector Uprob = log(clone(delta));
      List Dprob(W);

      NumericMatrix MeasP = MeasProd1(y_g, rho, Ng[g], C, M, R);

      for (w = 0; w < W; w ++)
      {
         NumericMatrix gamma_w = gamma_g[w];
         NumericMatrix Dprob_w(Ng[g], C);

         for (i = 0; i < Ng[g]; i ++)
         {
            NumericVector like_cw(C);

            for (c = 0; c < C; c ++)
            {
               like_cw[c] = log(gamma_w(i, c)) + log(MeasP(i, c));
            }

            Uprob[w] += log(sum(exp(like_cw - max(like_cw)))) +
               max(like_cw);

            Dprob_w.row(i) = exp(like_cw - max(like_cw)) /
               sum(exp(like_cw - max(like_cw)));
         }

         Dprob[w] = Dprob_w;
      }

      Uprob = exp(Uprob - max(Uprob))/ sum(exp(Uprob - max(Uprob)));

      for (i = 0; i < Ng[g]; i ++)
      {
         for (w = 0; w < W; w ++)
         {
            NumericMatrix Dprob_w = Dprob[w];
            NumericMatrix gamma_w = gamma_g[w];
            double postw = 0;

            for (c = 0; c < C; c ++)
            {
               double value = Uprob[w] * Dprob_w(i, c);
               postw += value;
               PostW(g, w)   += value;
               PostWC(w, c)  += value;
               PostC_g(i, c) += value;
            }

            for (c1 = 0; c1 < C - 1; c1 ++)
            {
               double postcw = Uprob[w] * Dprob_w(i, c1);

               for (p1 = 0; p1 < P; p1 ++)
               {
                  grad[w * (C - 1) * P + c1 * P + p1] +=
                     (postcw - gamma_w(i, c1) * postw) * x_g(i, p1);
                  for (c2 = 0; c2 < C - 1; c2 ++)
                  {
                     for (p2 = 0; p2 < P; p2 ++)
                     {
                        if (c1 == c2) ind = 1.0;
                        else ind = 0.0;
                        hess(w * (C - 1) * P + c1 * P + p1,
                             w * (C - 1) * P + c2 * P + p2) +=
                                postw * gamma_w(i, c1) * (gamma_w(i, c2) - ind) *
                                x_g(i, p1) * x_g(i, p2);
                     }
                  }
               }

               for (q1 = 0; q1 < Q; q1 ++)
               {
                  grad[W * (C - 1) * P + c1 * Q + q1] +=
                     (postcw - gamma_w(i, c1) * postw) * z_g(i, q1);

                  for (c2 = 0; c2 < C - 1; c2 ++)
                  {
                     for (p1 = 0; p1 < P; p1 ++)
                     {
                        if (c1 == c2) ind = 1.0;
                        else ind = 0.0;
                        double value = postw * gamma_w(i, c1) *
                           (gamma_w(i, c2) - ind) * x_g(i, p1) * z_g(i, q1);
                        hess(w * (C - 1) * P + c2 * P + p1,
                             W * (C - 1) * P + c1 * Q + q1) += value;
                        hess(W * (C - 1) * P + c1 * Q + q1,
                             w * (C - 1) * P + c2 * P + p1) += value;
                     }

                     for (q2 = 0; q2 < Q; q2 ++)
                     {
                        if (c1 == c2) ind = 1.0;
                        else ind = 0.0;
                        hess(W * (C - 1) * P + c1 * Q + q1,
                             W * (C - 1) * P + c2 * Q + q2) +=
                                postw * gamma_w(i, c1) * (gamma_w(i, c2) - ind) *
                                z_g(i, q1) * z_g(i, q2);
                     }
                  }
               }
            }
         }
      }

      PostC[g] = PostC_g;
   }

   ret["PostW"] = PostW;
   ret["PostWC"] = PostWC;
   ret["grad"] = grad;
   ret["hess"] = hess;
   ret["PostC"] = PostC;

   return ret;
}

// Get score in MGLCA (without covariates)
// [[Rcpp::export]]
List GetScore(List y,
              List post,
              List gamma,
              List rho,
              IntegerVector Ng,
              int G,
              int C,
              int M,
              IntegerVector R)
{
   int i, g, c, m, k, pos, ind;

   List ret;
   List gscore(G);
   List rscore(G);

   for (g = 0; g < G; g ++)
   {
      NumericMatrix gscore_g(Ng[g], G * (C - 1));
      NumericMatrix rscore_g(Ng[g], C * (sum(R) - M));

      NumericMatrix y_g = y[g];
      NumericMatrix post_g  = post[g];
      NumericMatrix gamma_g = gamma[g];
      List rho_g = rho[g];

      for (i = 0; i < Ng[g]; i ++)
         for (c = 0; c < C - 1; c ++)
            gscore_g(i, g * (C - 1) + c) = post_g(i, c) - gamma_g(i, c);

      for (m = 0; m < M; m ++)
      {
         NumericMatrix tmp_rho = rho_g[m];

         for (i = 0; i < Ng[g]; i ++)
         {
            for (c = 0; c < C; c ++)
            {
               pos = C * (sum(R) - sum(R[Range(m, M - 1)]) - m) +
                  c * (R[m] - 1);

               for (k = 0; k < R[m] - 1; k ++)
               {
                  if (y_g(i, m) == k + 1) ind = 1;
                  else ind = 0;

                  rscore_g(i, pos + k) =
                     post_g(i, c) * (ind - tmp_rho(c, k));
               }
            }
         }
      }

      gscore[g] = gscore_g;
      rscore[g] = rscore_g;
   }

   ret["g"] = gscore;
   ret["r"] = rscore;

   return ret;
}


// Get score in MGLCA (with covariates)
// [[Rcpp::export]]
List GetScoreX(List y,
               List x,
               List post,
               List gamma,
               List rho,
               IntegerVector Ng,
               int G,
               int C,
               int M,
               IntegerVector R,
               int P,
               bool coeff_inv)
{
   int i, g, g2, c, m, k, p, pos, ind;

   List ret;
   List bscore(G);
   List rscore(G);

   for (g = 0; g < G; g ++)
   {
      NumericMatrix bscore_g(Ng[g], G * P * (C - 1));
      NumericMatrix rscore_g(Ng[g], C * (sum(R) - M));

      NumericMatrix y_g = y[g];
      NumericMatrix x_g = x[g];
      NumericMatrix post_g  = post[g];
      NumericMatrix gamma_g = gamma[g];
      List rho_g = rho[g];

      for (i = 0; i < Ng[g]; i ++)
      {
         for (c = 0; c < C - 1; c ++)
         {
            bscore_g(i, g * P * (C - 1) + c * P) =
               post_g(i, c) - gamma_g(i, c);

            for (p = 1; p < P; p ++)
            {
               if (coeff_inv)
                  for (g2 = 0; g2 < G; g2 ++)
                     bscore_g(i, g2 * P * (C - 1) + c * P + p) =
                        x_g(i, p) * (post_g(i, c) - gamma_g(i, c));
               else
                  bscore_g(i, g * P * (C - 1) + c * P + p) =
                     x_g(i, p) * (post_g(i, c) - gamma_g(i, c));
            }
         }
      }


      for (m = 0; m < M; m ++)
      {
         NumericMatrix tmp_rho = rho_g[m];

         for (i = 0; i < Ng[g]; i ++)
         {
            for (c = 0; c < C; c ++)
            {
               pos = C * (sum(R) - sum(R[Range(m, M - 1)]) - m) +
                  c * (R[m] - 1);

               for (k = 0; k < R[m] - 1; k ++)
               {
                  if (y_g(i, m) == k + 1) ind = 1;
                  else ind = 0;

                  rscore_g(i, pos + k) =
                     post_g(i, c) * (ind - tmp_rho(c, k));
               }
            }
         }
      }

      bscore[g] = bscore_g;
      rscore[g] = rscore_g;
   }

   ret["b"] = bscore;
   ret["r"] = rscore;

   return ret;
}


// Get score in MLCA (without covariates)
// [[Rcpp::export]]
NumericMatrix GetUDScore(List y,
                         NumericVector delta,
                         NumericMatrix gamma,
                         List rho,
                         IntegerVector Ng,
                         int G,
                         int W,
                         int C,
                         int M,
                         IntegerVector R)
{
   int i, g, w, c, m, k, pos, ind;

   NumericMatrix score(G, W * C - 1 + C * (sum(R) - M));

   for (g = 0; g < G; g ++)
   {
      IntegerMatrix y_g = y[g];
      NumericVector Uprob = log(clone(delta));
      List Dprob(W);

      NumericMatrix MeasP = MeasProd1(y_g, rho, Ng[g], C, M, R);

      for (w = 0; w < W; w ++)
      {
         NumericMatrix Dprob_w(Ng[g], C);

         for (i = 0; i < Ng[g]; i ++)
         {
            NumericVector like_cw(C);

            for (c = 0; c < C; c ++)
            {
               like_cw[c] = log(gamma(w, c)) + log(MeasP(i, c));
            }

            Uprob[w] += log(sum(exp(like_cw - max(like_cw)))) +
               max(like_cw);

            Dprob_w.row(i) = exp(like_cw - max(like_cw)) /
               sum(exp(like_cw - max(like_cw)));
         }

         Dprob[w] = Dprob_w;
      }

      Uprob = exp(Uprob - max(Uprob)) / sum(exp(Uprob - max(Uprob)));

      for (w = 0; w < W; w ++)
      {
         for (i = 0; i < Ng[g]; i ++)
         {
            NumericMatrix Dprob_w = Dprob[w];
            NumericMatrix Dprob_W = Dprob[W - 1];
            double value1 = 0;

            for (c = 0; c < C; c ++)
               value1 += Uprob[w] * Dprob_w(i, c);

            if (w < W - 1 && i == 0)
               score(g, w) += value1 - delta[w];

            for (c = 0; c < C; c ++)
            {
               double value = Uprob[w] * Dprob_w(i, c);

               if (c < C - 1)
                  score(g, W - 1 + w * (C - 1) + c) +=
                     value - gamma(w, c) * value1;

               for (m = 0; m < M; m ++)
               {
                  NumericMatrix rho_m = rho[m];
                  pos = W - 1 + W * (C - 1) +
                     C * (sum(R) - sum(R[Range(m, M - 1)]) - m) +
                     c * (R[m] - 1);

                  for (k = 0; k < R[m] - 1; k ++)
                  {
                     if (y_g(i, m) == k + 1) ind = 1;
                     else ind = 0;

                     score(g, pos + k) -=
                        value * (ind - rho_m(c, k));
                  }
               }
            }
         }
      }
   }

   return score;
}

// Get score in MLCA (with covariates)
// [[Rcpp::export]]
NumericMatrix GetUDScoreX(List y,
                          List x,
                          List z,
                          NumericVector delta,
                          List gamma,
                          List rho,
                          IntegerVector Ng,
                          int G,
                          int W,
                          int P,
                          int Q,
                          int C,
                          int M,
                          IntegerVector R,
                          bool coeff_inv)
{
   int i, g, w, w2, c, p, q, m, k, pos, ind;
   NumericMatrix score(G, W - 1 + (W * P + Q) * (C - 1) + C * (sum(R) - M));

   for (g = 0; g < G; g ++)
   {
      List gamma_g = gamma[g];
      IntegerMatrix y_g = y[g];
      NumericMatrix x_g = x[g];
      NumericMatrix z_g = z[g];
      NumericVector Uprob = log(clone(delta));
      List Dprob(W);

      NumericMatrix MeasP = MeasProd1(y_g, rho, Ng[g], C, M, R);

      for (w = 0; w < W; w ++)
      {
         NumericMatrix gamma_w = gamma_g[w];
         NumericMatrix Dprob_w(Ng[g], C);

         for (i = 0; i < Ng[g]; i ++)
         {
            NumericVector like_cw(C);

            for (c = 0; c < C; c ++)
            {
               like_cw[c] = log(gamma_w(i, c)) + log(MeasP(i, c));
            }

            Uprob[w] += log(sum(exp(like_cw - max(like_cw)))) +
               max(like_cw);

            Dprob_w.row(i) = exp(like_cw - max(like_cw)) /
               sum(exp(like_cw - max(like_cw)));
         }

         Dprob[w] = Dprob_w;
      }

      Uprob = exp(Uprob - max(Uprob)) / sum(exp(Uprob - max(Uprob)));

      for (w = 0; w < W; w ++)
      {
         for (i = 0; i < Ng[g]; i ++)
         {
            NumericMatrix gamma_w = gamma_g[w];
            NumericMatrix Dprob_w = Dprob[w];
            NumericMatrix Dprob_W = Dprob[W - 1];
            double value1 = 0;

            for (c = 0; c < C; c ++)
               value1 += Uprob[w] * Dprob_w(i, c);

            for (c = 0; c < C; c ++)
            {
               double value = Uprob[w] * Dprob_w(i, c);

               if (w < W - 1)
                  score(g, w) += value;

               if (c < C - 1)
               {
                  score(g, W - 1 + w * (C - 1) * P + c * P) +=
                     value - gamma_w(i, c) * value1;

                  for (p = 1; p < P; p ++)
                  {
                     if (coeff_inv)
                     {
                        for (w2 = 0; w2 < W; w2 ++)
                           score(g, W - 1 + w2 * (C - 1) * P + c * P + p) +=
                              (value - gamma_w(i, c) * value1) * x_g(i, p);
                     }
                     else
                        score(g, W - 1 + w * (C - 1) * P + c * P + p) +=
                           (value - gamma_w(i, c) * value1) * x_g(i, p);
                  }

                  for (q = 0; q < Q; q ++)
                  {
                     score(g, W - 1 + W * (C - 1) * P + c * Q + q) +=
                        (value - gamma_w(i, c) * value1) * z_g(i, q);
                  }
               }

               for (m = 0; m < M; m ++)
               {
                  NumericMatrix rho_m = rho[m];
                  pos = W - 1 + W * (C - 1) * P + (C - 1) * Q +
                     C * (sum(R) - sum(R[Range(m, M - 1)]) - m) +
                     c * (R[m] - 1);

                  for (k = 0; k < R[m] - 1; k ++)
                  {
                     if (y_g(i, m) == k + 1) ind = 1;
                     else ind = 0;

                     score(g, pos + k) -=
                        value * (ind - rho_m(c, k));
                  }
               }
            }

            if (w < W - 1)
               score(g, w) -= delta[w];
         }
      }
   }

   return score;
}

// Marginal likelihood for MGLCA
// [[Rcpp::export]]
double GetLik(List y,
              List gamma,
              List rho,
              IntegerVector Ng,
              int G,
              int C,
              int M,
              IntegerVector R)
{
   int i, g;
   double mlike = 0;

   for (g = 0; g < G; g ++)
   {
      NumericVector mlike_g(Ng[g]);
      NumericMatrix gamma_g = gamma[g];

      // Avoid numerical underflow
      NumericMatrix clike_g =
         std::numeric_limits<double>::max() * clone(gamma_g);

      clike_g = MeasProd(y[g], rho[g], Ng[g], C, M, R, clone(clike_g));
      mlike_g = rowSums(clike_g);

      for (i = 0; i < Ng[g]; i ++)
         mlike += log(mlike_g[i]) - log(std::numeric_limits<double>::max());
   }

   return mlike;
}

// Marginal likelihood for MGLCA
// [[Rcpp::export]]
NumericVector GetFitted(IntegerMatrix pattern,
                        NumericMatrix gamma,
                        List rho,
                        int N,
                        int C,
                        int M,
                        IntegerVector R)
{
   NumericVector fitted(pattern.nrow());

   // Avoid numerical underflow
   NumericMatrix clike =
      std::numeric_limits<double>::max() * clone(gamma);

   clike = MeasProd(pattern, rho, pattern.nrow(),
                    C, M, R, clone(clike));
   fitted = exp(log(1.0 * N) + log(rowSums(clike)) -
      log(std::numeric_limits<double>::max()));

   return fitted;
}


// [[Rcpp::export]]
double GetUDlik(List y,
                NumericVector delta,
                NumericMatrix gamma,
                List rho,
                IntegerVector Ng,
                int G,
                int W,
                int C,
                int M,
                IntegerVector R)
{
   int i, g, w, c;
   double mlike = 0;

   for (g = 0; g < G; g ++)
   {
      IntegerMatrix y_g = y[g];
      NumericVector clike = log(clone(delta));
      NumericMatrix MeasP = MeasProd1(y_g, rho, Ng[g], C, M, R);

      for (w = 0; w < W; w ++)
      {
         for (i = 0; i < Ng[g]; i ++)
         {
            NumericVector like_cw(C);

            for (c = 0; c < C; c ++)
               like_cw[c] = log(gamma(w, c)) + log(MeasP(i, c));

            clike[w] += log(sum(exp(like_cw - max(like_cw)))) +
               max(like_cw) - log(std::numeric_limits<double>::max());
         }
      }

      mlike += log(sum(exp(clike - max(clike)))) + max(clike);
   }

   return mlike;
}



// Marginal likelihood for MLCA (without covariates)
// [[Rcpp::export]]
NumericVector GetUDfit(IntegerMatrix pattern,
                       NumericVector delta,
                       NumericVector gamma_m,
                       List rho,
                       int N,
                       int W,
                       int C,
                       int M,
                       IntegerVector R)
{
   int i, w, c;
   NumericVector fitted(pattern.nrow());

   NumericMatrix MeasP =
      MeasProd1(pattern, rho, pattern.nrow(), C, M, R);

   for (i = 0; i < pattern.nrow(); i ++)
      for (w = 0; w < W; w ++)
         for (c = 0; c < C; c ++)
            fitted[i] +=
               delta[w] * gamma_m(w, c) * MeasP(i, c) /
               std::numeric_limits<double>::max() * N;

   return fitted;
}


// Marginal likelihood for standard LCA (with covariates)
// [[Rcpp::export]]
double GetUDlikX(List y,
                 NumericVector delta,
                 List gamma,
                 List rho,
                 IntegerVector Ng,
                 int G,
                 int W,
                 int C,
                 int M,
                 IntegerVector R)
{
   int i, g, w, c;
   double mlike = 0;

   for (g = 0; g < G; g ++)
   {
      IntegerMatrix y_g = y[g];
      List gamma_g = gamma[g];
      NumericVector clike = log(clone(delta));
      NumericMatrix MeasP = MeasProd1(y_g, rho, Ng[g], C, M, R);

      for (w = 0; w < W; w ++)
      {
         NumericMatrix gamma_w = gamma_g[w];

         for (i = 0; i < Ng[g]; i ++)
         {
            NumericVector Like_cw(C);
            NumericVector like_cw(C);

            for (c = 0; c < C; c ++)
            {
               Like_cw[c] = gamma_w(i, c) * MeasP(i, c);
               like_cw[c] = log(gamma_w(i, c)) + log(MeasP(i, c));
            }

            clike[w] += log(sum(exp(like_cw - max(like_cw)))) +
               max(like_cw) - log(std::numeric_limits<double>::max());
         }
      }

      mlike += log(sum(exp(clike - max(clike)))) + max(clike);
   }

   return mlike;
}

// Gradient / Hessian for prevalence in MGLCA
// [[Rcpp::export]]
List GetDeriv(NumericMatrix post,
              NumericMatrix x,
              NumericMatrix gamma,
              int N,
              int C,
              int P)
{
   int i, c1, c2, p, q;
   double ind;
   NumericVector grad(P * (C - 1));
   NumericMatrix hess(P * (C - 1), P * (C - 1));
   List ret;

   for (i = 0; i < N; i ++)
   {
      for (c1 = 0; c1 < C - 1; c1 ++)
      {
         for (p = 0; p < P; p ++) // gradient
         {
            grad[c1 * P + p] +=
               x(i, p) * (post(i, c1) - gamma(i, c1));
            for (c2 = 0; c2 < C - 1; c2 ++)
            {
               if (c1 == c2) ind = 1.0;
               else ind = 0.0;

               for (q = 0; q < P; q ++) // hessian
               {
                  hess(c1 * P + p, c2 * P + q) +=
                     x(i, p) * x(i, q) *
                        - gamma(i, c1) * (ind - gamma(i, c2));
               }
            }
         }
      }
   }

   ret["grad"] = grad;
   ret["hess"] = hess;
   return ret;
}

// Gradient / Hessian for prevalence in MGLCA (coeff.inv)
// [[Rcpp::export]]
List GetDeriv2(List post,
               List x,
               List gamma,
               IntegerVector Ng,
               int G,
               int C,
               int P)
{
   int i, g, c1, c2, p, q;
   double ind;
   NumericVector grad((G + P - 1) * (C - 1));
   NumericMatrix hess((G + P - 1) * (C - 1), (G + P - 1) * (C - 1));
   List ret;

   for (g = 0; g < G; g ++)
   {
      NumericMatrix gamma_g = gamma[g];
      NumericMatrix post_g = post[g];
      NumericMatrix x_g = x[g];

      for (i = 0; i < Ng[g]; i ++)
      {
         for (c1 = 0; c1 < C - 1; c1 ++)
         {
            grad[g * (C - 1) + c1] += (post_g(i, c1) - gamma_g(i, c1));

            for (c2 = 0; c2 < C - 1; c2 ++)
            {
               if (c1 == c2) ind = 1.0;
               else ind = 0.0;
               hess(g * (C - 1) + c1, g * (C - 1) + c2) +=
                  - gamma_g(i, c1) * (ind - gamma_g(i, c2));

               for (p = 1; p < P; p ++)
               {
                  hess(g * (C - 1) + c1, G * (C - 1) + c2 * (P - 1) + p - 1) +=
                     - x_g(i, p) * gamma_g(i, c1) * (ind - gamma_g(i, c2));
                  hess(G * (C - 1) + c2 * (P - 1) + p - 1, g * (C - 1) + c1) +=
                     - x_g(i, p) * gamma_g(i, c1) * (ind - gamma_g(i, c2));
               }
            }

            for (p = 1; p < P; p ++) // gradient
            {
               grad[G * (C - 1) + c1 * (P - 1) + p - 1] +=
                  x_g(i, p) * (post_g(i, c1) - gamma_g(i, c1));
               for (c2 = 0; c2 < C - 1; c2 ++)
               {
                  if (c1 == c2) ind = 1.0;
                  else ind = 0.0;

                  for (q = 1; q < P; q ++) // hessian
                  {
                     hess(G * (C - 1) + c1 * (P - 1) + p - 1,
                          G * (C - 1) + c2 * (P - 1) + q - 1) +=
                        x_g(i, p) * x_g(i, q) *
                        - gamma_g(i, c1) * (ind - gamma_g(i, c2));
                  }
               }
            }
         }
      }
   }

   ret["grad"] = grad;
   ret["hess"] = hess;
   return ret;
}

// Delta update
// [[Rcpp::export]]
NumericVector UpDelta(NumericMatrix PostW)
{
   return colSums(PostW) / sum(PostW);
}

// Gamma update in MLCA
// [[Rcpp::export]]
NumericMatrix UpGammaML(NumericMatrix PostW,
                        int W,
                        int C)
{
   int w, c;

   NumericMatrix gamma(W, C);
   NumericVector rowsum = rowSums(PostW);

   for (w = 0; w < W; w ++)
      for (c = 0; c < C; c ++)
         gamma(w, c) = PostW(w, c) / rowsum[w];

   return gamma;
}

// Gamma update in MGLCA
// [[Rcpp::export]]
List UpGamma(List Post,
             IntegerVector Ng,
             int G,
             int C)
{
   int i, g, c;
   List n_gamma(G);

   for (g = 0; g < G; g ++)
   {
      NumericMatrix Post_g = Post[g];
      NumericMatrix n_gamma_g(Ng[g], C);

      NumericVector colsum = colSums(Post_g);
      double denom = sum(colsum);

      for (i = 0; i < Ng[g]; i ++)
         for (c = 0; c < C; c ++)
            n_gamma_g(i, c) = colsum[c] / denom;

      n_gamma[g] = n_gamma_g;
   }

   return n_gamma;
}


// update Measurement probability unrestricted (no Measurment invariance)
// [[Rcpp::export]]
List UpRhoU(List y,
            List Post,
            List rho,
            IntegerVector Ng,
            int G,
            int C,
            int M,
            IntegerVector R)
{
   int i, g, c, m, k;
   List n_Meas(G);

   for (g = 0; g < G; g ++)
   {
      List n_Meas_g(M);
      List rho_g = rho[g];
      IntegerMatrix y_g = y[g];
      NumericMatrix Post_g = Post[g];

      for (m = 0; m < M; m ++)
      {
         NumericMatrix tmp_rho(C, R[m]);
         NumericMatrix rho_gm = rho_g[m];

         for (c = 0; c < C; c ++)
         {
            for (i = 0; i < Ng[g]; i ++)
            {
               if (y_g(i, m) > 0)
               {
                  tmp_rho(c, y_g(i, m) - 1) += Post_g(i, c);
               }
               else
               {
                  for (k = 0; k < R[m]; k ++)
                     tmp_rho(c, k) += Post_g(i, c) * rho_gm(c, k);
               }
            }
         }

         NumericVector denom = rowSums(tmp_rho);

         for (c = 0; c < C; c ++)
            for (k = 0; k < R[m]; k ++)
               tmp_rho(c, k) /= denom[c];

         n_Meas_g[m] = tmp_rho;
      }

      n_Meas[g] = n_Meas_g;
   }

   return n_Meas;
}


// update Measurement probability restricted (with Measurment invariance)
// [[Rcpp::export]]
List UpRhoR(List y,
            List Post,
            List rho,
            IntegerVector Ng,
            int G,
            int C,
            int M,
            IntegerVector R)
{
   int i, g, c, m, k;
   List n_Meas(G);
   List n_Meas_g(M);
   List rho_1 = rho[0];

   for (m = 0; m < M; m ++)
   {
      NumericMatrix rho_1m = rho_1[m];
      NumericMatrix tmp_rho(C, R[m]);

      for (g = 0; g < G; g ++)
      {
         IntegerMatrix y_g = y[g];
         NumericMatrix Post_g = Post[g];

         for (i = 0; i < Ng[g]; i ++)
         {
            for (c = 0; c < C; c ++)
            {
               if (y_g(i, m) > 0)
               {
                  tmp_rho(c, y_g(i, m) - 1) += Post_g(i, c);
               }
               else
               {
                  for (k = 0; k < R[m]; k ++)
                     tmp_rho(c, k) += Post_g(i, c) * rho_1m(c, k);
               }
            }
         }
      }

      NumericVector denom = rowSums(tmp_rho);

      for (c = 0; c < C; c ++)
         for (k = 0; k < R[m]; k ++)
            tmp_rho(c, k) /= denom[c];

      n_Meas_g[m] = tmp_rho;
   }

   for (g = 0; g < G; g ++)
   {
      n_Meas[g] = n_Meas_g;
   }

   return n_Meas;
}


// update Measurement probability in MLCA
// [[Rcpp::export]]
List UpRhoML(List y,
             List PostC,
             List rho,
             IntegerVector Ng,
             int G,
             int C,
             int M,
             IntegerVector R)
{
   int i, g, c, m, k;
   List n_Meas(M);

   for (m = 0; m < M; m ++)
   {
      NumericMatrix tmp_rho(C, R[m]);
      NumericMatrix rho_m = rho[m];

      for (g = 0; g < G; g ++)
      {
         IntegerMatrix y_g = y[g];
         NumericMatrix PostCg = PostC[g];

         for (c = 0; c < C; c ++)
         {
            for (i = 0; i < Ng[g]; i ++)
            {
               if (y_g(i, m) > 0)
                  tmp_rho(c, y_g(i, m) - 1) += PostCg(i, c);
               else
               {
                  for (k = 0; k < R[m]; k ++)
                     tmp_rho(c, k) += PostCg(i, c) * rho_m(c, k);
               }
            }
         }
      }

      NumericVector denom = rowSums(tmp_rho);

      for (c = 0; c < C; c ++)
         for (k = 0; k < R[m]; k ++)
            tmp_rho(c, k) /= denom[c];

      n_Meas[m] = tmp_rho;
   }

   return n_Meas;
}


// [[Rcpp::export]]
NumericVector ObsCell(IntegerMatrix y,
                      int N,
                      int M,
                      IntegerVector R,
                      int maxiter,
                      double eps)
{
   int i, m, p, s, iter;
   int npatt = 1, nmisp, tmpnr;
   double beta = 0;
   IntegerVector misp(M);
   NumericVector pind(M);

   for (m = 0; m < M; m ++)
   {
      npatt  *= R[m];
      pind[m] = npatt / R[m];
   }

   NumericVector theta(npatt);
   NumericVector n_theta(npatt);
   NumericVector diff(npatt);

   NumericVector x(npatt);

   for (p = 0; p < npatt; p ++)
   {
      theta[p] = (double)1 / (double)npatt;
   }

   for (iter = 0; iter < maxiter; iter ++)
   {
      for (p = 0; p < npatt; p ++) x[p] = 0;
      for (i = 0; i < N; i ++)
      {
         nmisp = 1;
         for (m = 0; m < M; m ++)
         {
            if (y(i, m) == 0) misp[m] = R[m];
            else misp[m] = 1;
            nmisp *= misp[m];
         }

         NumericVector tmpp(nmisp);
         tmpnr = 1;

         for (m = 0; m < M; m ++)
         {
            if (misp[m] == 1)
            {
               for (s = 0; s < nmisp; s ++)
               {
                  tmpp[s] += pind[m] * (y(i, m) - 1);
               }
            }
            else
            {
               for (s = 0; s < nmisp; s ++)
               {
                  tmpp[s] += pind[m] * ((s / tmpnr) % misp[m]);
               }
               tmpnr *= R[m];
            }
         }

         beta = 0;
         for (s = 0; s < nmisp; s ++)
            beta += theta[tmpp[s]];
         for (s = 0; s < nmisp; s ++)
            x[tmpp[s]] += theta[tmpp[s]] / beta;
      }

      for (p = 0; p < npatt; p ++)
      {
         n_theta[p] = x[p] / N;
         diff[p] = n_theta[p] - theta[p];
         if (diff[p] < 0) diff[p] = -diff[p];
      }

      theta = clone(n_theta);

      if (max(diff) < eps)
         break;
   }

   return x;
}

// [[Rcpp::export]]
double ObsLik(IntegerMatrix y,
              int N,
              int M,
              IntegerVector R,
              int maxiter,
              double eps)
{
   int i, m, p, s, iter;
   int npatt = 1, nmisp, tmpnr;
   double beta = 0;
   IntegerVector misp(M);
   NumericVector pind(M);
   double loglike = 0;

   for (m = 0; m < M; m ++)
   {
      npatt  *= R[m];
      pind[m] = npatt / R[m];
   }

   NumericVector theta(npatt);
   NumericVector n_theta(npatt);
   NumericVector diff(npatt);
   NumericVector x(npatt);

   for (p = 0; p < npatt; p ++)
   {
      theta[p] = (double)N / (double)npatt;
   }

   for (iter = 0; iter < maxiter; iter ++)
   {
      loglike = 0;

      for (p = 0; p < npatt; p ++) x[p] = 0;
      for (i = 0; i < N; i ++)
      {
         nmisp = 1;
         for (m = 0; m < M; m ++)
         {
            if (y(i, m) == 0) misp[m] = R[m];
            else misp[m] = 1;
            nmisp *= misp[m];
         }

         NumericVector tmpp(nmisp);
         tmpnr = 1;
         for (m = 0; m < M; m ++)
         {
            if (misp[m] == 1)
            {
               for (s = 0; s < nmisp; s ++)
               {
                  tmpp[s] += pind[m] * (y(i, m) - 1);
               }
            }
            else
            {
               for (s = 0; s < nmisp; s ++)
               {
                  tmpp[s] += pind[m] * ((s / tmpnr) % misp[m]);
               }
               tmpnr = R[m];
            }
         }

         beta = 0;
         for (s = 0; s < nmisp; s ++)
            beta += theta[tmpp[s]];
         for (s = 0; s < nmisp; s ++)
            x[tmpp[s]] += theta[tmpp[s]] / beta;

         if (beta != 0)
            loglike += log(beta);
      }

      for (p = 0; p < npatt; p ++)
      {
         n_theta[p] = x[p] / N;
         diff[p] = n_theta[p] - theta[p];
         if (diff[p] < 0) diff[p] = -diff[p];
      }
      theta = clone(n_theta);

      if (max(diff) < eps)
         break;
   }

   return loglike;
}

// [[Rcpp::export]]
IntegerVector ObsCell2(IntegerMatrix sy,
                       IntegerMatrix u,
                       int N,
                       int U)
{
   int i, j = 0;
   IntegerVector x(U);

   for (i = 0; i < N; i ++)
   {
      if (is_true(all(sy.row(i) == u.row(j))))
         x[j] += 1;
      else
      {
         j += 1;
         x[j] += 1;
      }
   }

   return x;
}
