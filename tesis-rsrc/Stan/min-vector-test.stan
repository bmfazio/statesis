data {
  int<lower=1> N; // sample size (data.frame rows)

  int<lower=1> n[N]; // # of attempts (binomial parameter)
  int<lower=0> y[N]; // # of successes (outcome)
}

transformed data{
  real ymin[N];
  real ymax[N];
  
  for (i in 1:N) {
    print(
      min([1, 0])
    );
    print(
      [y[i], 1, n[i]-y[i]]
    );
    print(
      1 - min([ 1, y[i] ])
    );
    print(
      1 - min([ 1, n[i]-y[i] ])
    );
    print(
      "---"
    )
    ymin[i] = 1 - min([ 1, y[i] ]);
    ymax[i] = 1 - min([ 1, n[i]-y[i] ]);
  }

  print(ymin);
  print(ymax);
}
