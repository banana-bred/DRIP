# DRIP
## Dissociative Recombination and Photo $\leftrightarrow$ Ionization

This is me rewriting most of the code that I wrote during my PhD because the original version has become nearly immutable and could be much easier to use.
DRIP is being rebuilt as an [FPM](https://fpm.fortran-lang.org/) package so that it is easier to install and use.
It is very much still a work in progress, and can currently do nothing.
The basic idea is that it could be used alongside the [UKRmol+](https://amosgateway.org/ukrmol/) electron scattering codes to study [dissociative recombination (DR)](https://en.wikipedia.org/wiki/Dissociative_recombination),
$$\mathrm{AB}^+ + \mathrm{e}^- \to \mathrm{A} + \mathrm{B},$$
[photoionization (PI)](https://en.wikipedia.org/wiki/Photoionization),
$$\mathrm{AB} + \mathrm{photon} \to \mathrm{AB}^+,$$
and electron-impact excitation of the example target ion's electronic, vibrational, or rotational degrees of freedom — "or" being inclusive.
The latter is known as rovibronic exctiation (RVE).

DRIP is primarily a code used to study dissociative recombination, but the framework upon which it's based allows for the calculation of RVE/PI cross sections and reaction rate coefficients.
The DR probability is essentially calculated as the probability that no RVE occurs, so it is natural that RVE probabilities can be calculated.
For PI, the final wavefunction, $\mathrm{AB}^+ + \mathrm{e}^+$, is the time reversal of the initial wavefunction for RVE and DR calculations (hence the "photo $\leftrightarrow$ ionization" in the title).


Details on the DR and RVE methods in references [[1]](#1) and [[2]](#2), respectively.

## Dependencies
- The [Fortran Standard Library (stdlib)](https://github.com/fortran-lang/stdlib)
- A working installation of `BLAS` and `LAPACK`
- A working installation of the [CERN Program Library (cernlib)](https://cernlib.web.cern.ch/index.html)

## References
<a id="1">[1]</a>
J. Forer, Dávid Hvizdoš, Xianwu Jiang, Mehdi Ayouz, Chris H. Greene, and Viatcheslav Kokoouline
*Unified treatment of resonant and nonresonant mechanisms in dissociative recombination: Benchamark study of $CH^+$*,
Physical Review A 107, 042801, April 4 2023.

DOI : https://doi.org/10.1103/PhysRevA.107.042801

<a id="2">[2]</a>
J. Forer, Dávid Hvizdoš, Mehdi Ayouz, Chris H. Greene, and Viatcheslav Kokoouline
*Kinetic rate coefficients for electron-driven collisions with $CH^+$: dissociative recombination and rovibronic excitation*,
Monthly Notices of the Royal Astronomical Society, Volume 527, Issue 3, January 2024, Pages 5238 – 5243.

DOI : https://doi.org/10.1093/mnras/stad3577
