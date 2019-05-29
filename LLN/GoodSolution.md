Based on the Law of Large Numbers (LLN) you can obtain a good estimate by averaging the height of a subset of the pines. Let $x_1,\dots,x_n$ be your sample of observed pine heights. Than you can compute their mean and standard deviation as:

$$\bar{x} = \frac{1}{n}\sum_{i=1}n x_i \qquad s = \frac{1}{n-1}\sum_{i=1}n (x_i - \bar{x})^2$$

Denote the population mean and standard deviation as $\mu$ and $\sigma$, respectively. The LLN tells you that as $n$ increases, you'll see that your sample estimates gets very close the entire population values: $\bar{x} \approx \mu$ and $s \approx \sigma$

