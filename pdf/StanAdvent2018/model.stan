data {
  int<lower=0> J;         // ŠwZ‚Ì”
  real y[J];              // „’è‚³‚ê‚Ä‚¢‚é‹³ˆç‚ÌŒø‰Ê
  real<lower=0> sigma[J]; // ‹³ˆç‚ÌŒø‰Ê‚Ì•W€Œë·
}

parameters {
  real mu;
  real<lower=0> tau;
  real eta[J];
}

transformed parameters {
  real theta[J];
  for (j in 1:J)
    theta[j] = mu + tau * eta[j];
}

model {
  target += normal_lpdf(eta | 0, 1);
  target += normal_lpdf(y | theta, sigma);
}
