# Generalities

The operational number $n_{op}$ in reality is different for each link, as it depends on the characteristics of both the nodes and of the repeater's type. The simplification is that we neglect **this** DOF by assuming that all links have the same target $n_{op}$, while **the number of (already purified) entangled pairs changes in between different links**.

Also note that since $g(n)$ is an **exponential** density of entangled pairs and the lower bound of integration is $n^{op} > 0$ in nontrivial cases, $p_{op}$ is **always less than $1$**, thus restricting the physical range of $p = p_{ext}p_{op}$ to exclude $1$.

It feels like both $p_{ext}$ and $p_{op}$ are sometimes used as external parameters and sometimes as independent variables.

In order to make sense of the lines of Fig.2 representing $l^{op}(p_{op})$, which are not straight, I assume that $p_{ext}$ is the external parameter and $p_{op}$ is a variable in $[0,1]$. In order to fix the values of $p_{ext}$, we need first to compute the critical probabilities either analitically for $N \to +\infty$ or computationally from graph $e$ of Fig.2.

## E-R networks
We have $p_{ext}$ which is "fixed" in a range of values. Fixing it to some value leads us to the graphs $a,c$ of Fig.2, as the $D$ part of the graph is just standard percolation. The lines corresponding to $l_{op}$ are **analytic functions of $p_{op}$** and as such they can be computed afterwards.

Graph $e$ instead is a bit more complex: we need to rerun the percolation a lot of times and with different $N$ values to get the behavior of the backbone (LCC) as a function of $p_{ext}$. We then could extrapolate the two critical probabilities straight from the simulation to then reuse these into graphs $a,c$. The other approach would require computing them from the ideal ($N \to +\infty$) model, which is a PITA.

### Percolation done, now we go to criticality:
How tho?
Seems like one has to simulate over and over the size of the backbone (GCC) for various values of the $p_{ext}$ only, keeping $p_{op}$ fixed (to what?)

OKOKOKOKOK:
Since we want to study the subcritical / supercritical regimes, **we need our system to be functionally connected before applying the percolation**. This means that $p_{op}$ is (at least/at maximum) $p_{op}^0$, thus we obtain it by intersecting the $l_{op}(p_{op})$ curve at some $p_{op} = p / p_{ext}$ with the given diameter curve. This gives us the pairs $(p_{ext},p_{op})$ over which we compute the percolation in $p$.

So:
- The curve for $l_{op}(p_{op}) = n_{op}^{1/\alpha}$ and $p_{op} = \int_{n_{op}}^{+\infty} A\exp\left(-n/\langle n\rangle\right)dn$.
- We solve for all $p_{ext}$ the condition $l_{op}(p_{op}) = D(p)$ such that we get the value of $p_{op}$ and we can compute the GCC for the pair $(p_{op},p_{ext})$

In particular:
$$\begin{align}
p_{op} &= \int_{n_{op}}^{+\infty} dn A\exp\left(-n/\langle n\rangle\right)dn  \\
&= -A\langle n \rangle \left[0 - \exp\left(-n_{op}/\langle n\rangle\right)\right] \\
l_{op} &= n_{op}^{1/\alpha} \\
&= \left[-\langle n\rangle\log\left(\frac{p_{op}}{A\langle n \rangle}\right)\right]^{1/\alpha}  \\
A:\quad & \int_{0}^{+\infty} dn A\exp\left(-n/\langle n\rangle\right)dn = 1 \\
A &= \frac{1}{\langle n \rangle}  \\
\end{align}$$
So since we look for $l_{op} = D$:
$$\left[\langle n\rangle\log\left(\frac{1}{p_{op}}\right)\right]^{1/\alpha} = D(p_{op}p_{ext})$$
$$p_{op}^0 = \exp\left(-\frac{D(p_{op}p_{ext})^\alpha}{\langle n\rangle}\right)$$