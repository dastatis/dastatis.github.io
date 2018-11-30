data {
  int<lower=0> J;         // �w�Z�̐�
  real y[J];              // ���肳��Ă��鋳��̌���
  real<lower=0> sigma[J]; // ����̌��ʂ̕W���덷
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
