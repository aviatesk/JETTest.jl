```@meta
CurrentModule = JETTest
```

# Dispatch Analysis

When Julia compiles your code but type inference was not so successful, the compiler is
likely to be unable to resolve which method should be called at each generic function call-site,
and then it will be looked up at runtime.
That is called "runtime dispatch", which is known as a common source of performance problem —
since the compiler can't do various kinds of optimizations including inlining when it doesn't
know matching methods, and method lookup itself can also be a bottleneck if it happens many times.

In order to avoid this problem, we usually use [`code_typed`](https://docs.julialang.org/en/v1/base/base/#Base.code_typed)
or its family, inspect their output, and check if there is anywhere type is not well inferred
(i.e. where is "type-instable") and optimization was not successful.
But the problem is that they can only present the "final" output of inference or
optimization, and we can't inspect an entire call graph and may not be able to find where
a problem happened and how the "type instability" has been propagated.

There is a nice package called [Cthulhu.jl](https://github.com/JuliaDebug/Cthulhu.jl),
which allows us to inspect the output of `code_typed` by _descending_ into a call tree,
recursively and interactively.
The workflow with Cthulhu is much more powerful, but still, it's tedious.

So, why not automate it ?
JETTest.jl implements such an analyzer that investigates optimized IRs of your code and
automatically detects anywhere the compiler failed to do optimizations, or couldn't
resolve matching methods and thus dispatch will happen at runtime.

## [Quick Start](@id dispatch-analysis-quick-start)

[`@report_dispatch`](@ref) analyzes the entire call graph of a given generic function call,
and then reports detected optimization failures and runtime dispatch points:
```@repl quickstart
using JETTest

function kernel(f, vals)
    s = zero(eltype(vals))
    for v in vals
        s += f(v)
    end
    return s
end

vals = rand(10);
f() = kernel(sin, vals) # this function uses the non-constant global variable and thus is very type-unstable

@report_dispatch f() # runtime dispatches will be reported

f(vals) = kernel(sin, vals); # we can pass parameters as a function argument, and then everything is type-stable
@report_dispatch f(vals) # now runtime dispatch free !
```

With the [`frame_filter`](@ref dispatch-analysis-configurations) configuration, we can focus on type
instabilities within specific modules of our interest:
```@repl quickstart
# problem: when ∑1/n exceeds 30 ?
function compute(x)
    r = 1
    s = 0.0
    n = 1
    @time while r < x
        s += 1/n
        if s ≥ r
            # `println` call is full of runtime dispatches for good reasons
            # and we're not interested in type-instabilities within this call
            # since we know it's only called few times
            println("round $r/$x has been finished")
            r += 1
        end
        n += 1
    end
    return n, s
end

@report_dispatch compute(30) # bunch of reports will be reported from the `println` call

this_module_filter(sv) = sv.mod === @__MODULE__;
@report_dispatch frame_filter=this_module_filter compute(30) # focus on what we wrote, and no error should be reported
```

[`@test_nodispatch`](@ref) can be used to assert that a given function call is free from type instabilities
under [`Test` standard library's unit-testing infrastructure](https://docs.julialang.org/en/v1/stdlib/Test/):
```@repl quickstart
@test_nodispatch f()

@test_nodispatch frame_filter=this_module_filter compute(30)

using Test

@testset "check type-stabilities" begin
    @test_nodispatch f() # should fail

    vals = rand(10)
    @test_nodispatch f(vals) # should pass

    @test_nodispatch frame_filter=this_module_filter compute(30) # should pass

    @test_nodispatch broken=true compute(30) # should pass with the "broken" annotation
end
```

## [Entry Points](@id dispatch-analysis-entry-points)

These macros/functions are the entries of dispatch analysis:
```@docs
@report_dispatch
report_dispatch
@analyze_dispatch
analyze_dispatch
@test_nodispatch
```

## [Configurations](@id dispatch-analysis-configurations)

```@docs
DispatchAnalyzer
```
