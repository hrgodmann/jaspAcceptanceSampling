functions {
  real normal_mixture_transport(real z, real sigma, data real q, data real extra);
}
data {
  int<lower=1> T;
  array[T] int<lower=0> s;
  array[T] int<lower=0> n;
  array[T] int<lower=0> y;
  real<lower=0> prior_theta1_a;
  real<lower=0> prior_theta1_b;
  real<lower=0> prior_sigma_sd;
  real<lower=0> prior_beta_scale;
  int<lower=1> K;
  matrix[T, K] X;
}
transformed data {
  real jump_probability = 0.05;
  real jump_extra_sd = 1.5;
  if (prior_theta1_a <= 0 || prior_theta1_b <= 0 ||
      is_inf(prior_theta1_a) || is_inf(prior_theta1_b) ||
      is_nan(prior_theta1_a) || is_nan(prior_theta1_b))
    reject("Initial Beta prior shapes must be finite and strictly positive");
  for (t in 1:T)
    if (y[t] > n[t] || n[t] > s[t]) reject("Require y <= n <= s");
}
parameters {
  real mu1; // initial baseline log odds
  real<lower=0> sigma_eta;
  vector[T-1] z_raw;
  vector[K] beta_predictor;
}
transformed parameters {
  real<lower=0, upper=1> theta1 = inv_logit(mu1);
  vector[T-1] eta;
  vector[T] nu;
  vector[T] mu;
  vector[T] theta;

  if (T > 1)
    for (t in 1:(T-1))
      eta[t] = normal_mixture_transport(z_raw[t], sigma_eta,
                                        jump_probability, jump_extra_sd);
  nu[1] = mu1;
  if (T > 1) nu[2:T] = nu[1] + cumulative_sum(eta);
  mu = nu + X * beta_predictor;
  theta = inv_logit(mu);
}
model {
  // Beta prior on inv_logit(mu1), including its transformation Jacobian.
  // Combining the log terms avoids rounding theta1 to 0/1 inside the target.
  // The Beta normalizing constant depends only on the supplied prior shapes.
  target += prior_theta1_a * log_inv_logit(mu1)
            + prior_theta1_b * log1m_inv_logit(mu1);
  sigma_eta ~ normal(0, prior_sigma_sd);
  z_raw ~ std_normal();
  beta_predictor ~ cauchy(0, prior_beta_scale);
  // Binomial log likelihood, omitting only the data-dependent coefficients.
  // Stable log probabilities also cover logits whose inverse rounds to 0/1.
  for (t in 1:T)
    target += y[t] * log_inv_logit(mu[t])
              + (n[t] - y[t]) * log1m_inv_logit(mu[t]);
}
generated quantities {
  int y_pred = y[T] + binomial_rng(s[T] - n[T], theta[T]);
  vector[T-1] jump_responsibility;

  if (T > 1)
    for (t in 1:(T-1)) {
      real broad = log(jump_probability)
                   + normal_lpdf(eta[t] | 0, sqrt(square(sigma_eta) + square(jump_extra_sd)));
      real regular = log1m(jump_probability) + normal_lpdf(eta[t] | 0, sigma_eta);
      jump_responsibility[t] = exp(broad - log_sum_exp(broad, regular));
    }
}
