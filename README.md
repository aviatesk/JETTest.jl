# JETTest.jl

:warning: _**Attention**_ :warning::
From Julia v1.7, JETTest.jl is deprecated and its functionalities have been moved to [JET.jl](https://github.com/aviatesk/JET.jl).
In short, you use `JET.@report_opt`/`JET.report_opt`/`JET.@test_opt`/`JET.test_opt` instead of `JETTest.@report_dispatch`/`JETTest.@report_dispatch`/`JETTest.@test_nodispatch`/`JETTest.@test_dispatch`.
More detailed documentation can be found at <https://aviatesk.github.io/JET.jl/dev/optanalysis/>.
JETTest.jl is still supported on Julia v1.6, but there won't be any more development.

[![](https://img.shields.io/badge/docs-dev-blue.svg)](https://aviatesk.github.io/JETTest.jl/dev/)
![CI](https://github.com/aviatesk/JETTest.jl/workflows/CI/badge.svg)
[![codecov](https://codecov.io/gh/aviatesk/JETTest.jl/branch/master/graph/badge.svg)](https://codecov.io/gh/aviatesk/JETTest.jl)

JETTest.jl is an advanced testing toolset for the Julia programming language.
It automatically detects otherwise-overlooked problems, and helps you keep your code to be faster and robust.
