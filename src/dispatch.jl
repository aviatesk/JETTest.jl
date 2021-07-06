# DispatchAnalyzer
# ================

"""
Every [entry point of dispatch analysis](@ref dispatch-analysis-entry-points) can accept
any of [JET configurations](https://aviatesk.github.io/JET.jl/dev/config/) as well as
the following additional configurations that are specific to dispatch analysis.

---
- `frame_filter = x::$State->true`:\\
  A predicate which takes `InfernceState` or `OptimizationState` and returns `false` to skip analysis on the frame.
  ```julia
  # only checks code within the current module:
  julia> mymodule_filter(x) = x.mod === @__MODULE__;
  julia> @test_nodispatch frame_filter=mymodule_filter f(args...)
  ...
  ```

---
- `function_filter = @nospecialize(ft)->true`:\\
  A predicate which takes a function type and returns `false` to skip analysis on the call.
  ```julia
  # ignores `Core.Compiler.widenconst` calls (since it's designed to be runtime-dispatched):
  julia> myfunction_filter(@nospecialize(ft)) = ft !== typeof(Core.Compiler.widenconst)
  julia> @test_nodispatch function_filter=myfunction_filter f(args...)
  ...
  ```

---
- `skip_nonconcrete_calls::Bool = true`:\\
  Julia's runtime dispatch is "powerful" because it can always compile code with concrete
  runtime arguments so that [a "kernel" function](https://docs.julialang.org/en/v1/manual/performance-tips/#kernel-functions)
  runs very effectively even if it's called from a type-instable call site.
  This means, we (really) often accept that some parts of our code are not inferred statically,
  and rather we want to just rely on information that is only available at runtime.
  To model this programming style, dispatch analyzer does NOT report any optimization failures
  or runtime dispatches detected within non-concrete calls under the default configuration.
  We can turn off this `skip_nonconcrete_calls` configuration to get type-instabilities
  within non-concrete calls.
  ```julia
  # the following examples are adapted from https://docs.julialang.org/en/v1/manual/performance-tips/#kernel-functions
  julia> function fill_twos!(a)
             for i = eachindex(a)
                 a[i] = 2
             end
         end;

  julia> function strange_twos(n)
             a = Vector{rand(Bool) ? Int64 : Float64}(undef, n)
             fill_twos!(a)
             return a
         end;

  # by default, only type-instabilities within concrete call (i.e. `strange_twos(3)`) are reported
  # and those within non-concrete calls (`fill_twos!(a)`) are not reported
  julia> @report_dispatch strange_twos(3)
  ═════ 2 possible errors found ═════
  ┌ @ REPL[2]:2 %45(Main.undef, n)
  │ runtime dispatch detected: %45::Type{Vector{_A}} where _A(Main.undef, n::Int64)
  └─────────────
  ┌ @ REPL[2]:3 Main.fill_twos!(%46)
  │ runtime dispatch detected: Main.fill_twos!(%46::Vector)
  └─────────────
  Vector (alias for Array{_A, 1} where _A)

  # we can get reports from non-concrete calls with `skip_nonconcrete_calls=false`
  julia> @report_dispatch skip_nonconcrete_calls=false strange_twos(3)
  ═════ 4 possible errors found ═════
  ┌ @ REPL[2]:3 Main.fill_twos!(a)
  │┌ @ REPL[1]:3 Base.setindex!(a, 2, %14)
  ││ runtime dispatch detected: Base.setindex!(a::Vector, 2, %14::Int64)
  │└─────────────
  │┌ @ REPL[1]:3 Base.setindex!(a, 2, i)
  ││┌ @ array.jl:877 Base.convert(_, x)
  │││ runtime dispatch detected: Base.convert(_::Any, x::Int64)
  ││└────────────────
  ┌ @ REPL[2]:2 %45(Main.undef, n)
  │ runtime dispatch detected: %45::Type{Vector{_A}} where _A(Main.undef, n::Int64)
  └─────────────
  ┌ @ REPL[2]:3 Main.fill_twos!(%46)
  │ runtime dispatch detected: Main.fill_twos!(%46::Vector)
  └─────────────
  Vector (alias for Array{_A, 1} where _A)
  ```

---
- `skip_unoptimized_throw_blocks::Bool = true`:\\
  By default, Julia's native compilation pipeline intentionally disables inference (and so
  succeeding optimizations too) on "throw blocks", which are code blocks that will eventually
  lead to `throw` calls, in order to ease [the compilation latency problem, a.k.a. "first-time-to-plot"](https://julialang.org/blog/2020/08/invalidations/).
  Accordingly, the dispatch analyzer also ignores runtime dispatches detected within those blocks
  since we _usually_ don't mind if code involved with error handling isn't optimized.
  If `skip_unoptimized_throw_blocks` is set to `false`, it doesn't ignore them and will
  report type instabilities detected within "throw blocks".

  See also <https://github.com/JuliaLang/julia/pull/35982>.

  ```julia
  # by default, unoptimized "throw blocks" are not analyzed
  julia> @test_nodispatch sin(10)
  Test Passed
    Expression: #= none:1 =# JETTest.@test_nodispatch sin(10)

  # we can turn on the analysis on unoptimized "throw blocks" with `skip_unoptimized_throw_blocks=false`
  julia> @test_nodispatch skip_unoptimized_throw_blocks=false sin(10)
  Dispatch Test Failed at none:1
    Expression: #= none:1 =# JETTest.@test_nodispatch skip_unoptimized_throw_blocks = false sin(10)
    ═════ 1 possible error found ═════
    ┌ @ math.jl:1221 Base.Math.sin(xf)
    │┌ @ special/trig.jl:39 Base.Math.sin_domain_error(x)
    ││┌ @ special/trig.jl:28 Base.Math.DomainError(x, "sin(x) is only defined for finite x.")
    │││ runtime dispatch detected: Base.Math.DomainError(x::Float64, "sin(x) is only defined for finite x.")
    ││└──────────────────────

  ERROR: There was an error during testing

  # we can also turns off the heuristic itself
  julia> @test_nodispatch unoptimize_throw_blocks=false skip_unoptimized_throw_blocks=false sin(10)
  Test Passed
    Expression: #= none:1 =# JETTest.@test_nodispatch unoptimize_throw_blocks = false skip_unoptimized_throw_blocks = false sin(10)
  ```

---
"""
mutable struct DispatchAnalyzer{S,T} <: AbstractAnalyzer
    state::AnalyzerState
    opts::BitVector
    frame_filter::S
    function_filter::T
    concrete_frame::Union{Nothing,Bool}
    skip_unoptimized_throw_blocks::Bool
end
function DispatchAnalyzer(;
    frame_filter = x::State->true,
    function_filter = @nospecialize(ft)->true,
    skip_nonconcrete_calls::Bool = true,
    skip_unoptimized_throw_blocks::Bool = true,
    jetconfigs...)
    state = AnalyzerState(; jetconfigs...)
    concrete_frame = skip_nonconcrete_calls ? true : nothing
    return DispatchAnalyzer(state, BitVector(), frame_filter, function_filter, concrete_frame, skip_unoptimized_throw_blocks)
end

# AbstractAnalyzer API requirements
JETInterfaces.AnalyzerState(analyzer::DispatchAnalyzer) = analyzer.state
JETInterfaces.AbstractAnalyzer(analyzer::DispatchAnalyzer, state::AnalyzerState) =
    DispatchAnalyzer(state, analyzer.opts, analyzer.frame_filter, analyzer.function_filter, analyzer.concrete_frame, analyzer.skip_unoptimized_throw_blocks)
JETInterfaces.ReportPass(analyzer::DispatchAnalyzer) = DispatchAnalysisPass()

# we want to run different analysis with a different filter, so include its hash into the cache key
function JET.get_cache_key(analyzer::DispatchAnalyzer)
    h = @invoke get_cache_key(analyzer::AbstractAnalyzer)
    h = @invoke hash(analyzer.frame_filter::Any, h::UInt)    # HACK avoid dynamic dispatch
    h = @invoke hash(analyzer.function_filter::Any, h::UInt) # HACK avoid dynamic dispatch
    h = hash(isnothing(analyzer.concrete_frame), h)
    h = hash(analyzer.skip_unoptimized_throw_blocks, h)
    return h
end

struct DispatchAnalysisPass <: ReportPass end

# ignore most of reports defined by JET.jl
(::DispatchAnalysisPass)(T::Type{<:InferenceErrorReport}, @nospecialize(_...)) = return
(::DispatchAnalysisPass)(T::Type{GeneratorErrorReport}, @nospecialize(args...)) = SoundPass()(T, args...)

@reportdef struct OptimizationFailureReport <: InferenceErrorReport end
JETInterfaces.get_msg(::Type{OptimizationFailureReport}, args...) =
    return "failed to optimize" #: signature of this MethodInstance

function (::DispatchAnalysisPass)(::Type{OptimizationFailureReport}, analyzer::DispatchAnalyzer, frame::InferenceState)
    report!(OptimizationFailureReport, analyzer, frame.linfo)
end

@reportdef struct RuntimeDispatchReport <: InferenceErrorReport end
JETInterfaces.get_msg(::Type{RuntimeDispatchReport}, analyzer, s) =
    return "runtime dispatch detected" #: call signature

function (::DispatchAnalysisPass)(::Type{RuntimeDispatchReport}, analyzer::DispatchAnalyzer, opt::OptimizationState)
    throw_blocks =
        analyzer.skip_unoptimized_throw_blocks && opt.inlining.params.unoptimize_throw_blocks ?
        find_throw_blocks(opt.src.code) : nothing

    sptypes, slottypes = opt.sptypes, opt.slottypes
    for (pc, x) in enumerate(opt.src.code)
        if !isnothing(throw_blocks)
            # optimization is intentionally turned off for this block, let's ignore anything here
            CC.in(pc, throw_blocks) && continue
        end
        if @isexpr(x, :call)
            ft = widenconst(argextype(first(x.args), opt.src, sptypes, slottypes))
            ft <: Builtin && continue # ignore `:call`s of language intrinsics
            if analyzer.function_filter(ft)
                report!(RuntimeDispatchReport, analyzer, (opt, pc))
            end
        end
    end
end

# branch on https://github.com/JuliaLang/julia/pull/39885
@static if isdefined(CC, :finish!)

import .CC:
    _typeinf,
    finish,
    finish!

function CC._typeinf(analyzer::DispatchAnalyzer, frame::InferenceState)
    @assert isempty(analyzer.opts)

    (; concrete_frame) = analyzer
    skip_nonconcrete_calls = !isnothing(concrete_frame)
    if skip_nonconcrete_calls
        analyzer.concrete_frame = isconcretedispatch(frame.linfo.specTypes)
    end

    ret = @invoke _typeinf(analyzer::AbstractAnalyzer, frame::InferenceState)

    if skip_nonconcrete_calls
        analyzer.concrete_frame = concrete_frame
    end

    @assert isempty(analyzer.opts)

    return ret
end

function CC.finish(frame::InferenceState, analyzer::DispatchAnalyzer)
    ret = @invoke finish(frame::InferenceState, analyzer::AbstractAnalyzer)

    (; concrete_frame, opts) = analyzer
    if !(isnothing(concrete_frame) || concrete_frame) || !analyzer.frame_filter(frame)
        push!(opts, false)
    else
        if isa(frame.result.src, OptimizationState)
            push!(opts, true)
        else
            report_pass!(OptimizationFailureReport, analyzer, frame)
            push!(opts, false)
        end
    end

    return ret
end

function CC.finish!(analyzer::DispatchAnalyzer, caller::InferenceResult)
    opt = caller.src

    ret = @invoke finish!(analyzer::AbstractInterpreter, caller::InferenceResult)

    if popfirst!(analyzer.opts) # optimization happened
        if isa(ret, Const) # the optimization was very successful, nothing to report
        elseif isa(ret, OptimizationState) # compiler cached the optimized IR, just analyze it
            report_pass!(RuntimeDispatchReport, analyzer, ret)
        elseif isa(ret, CodeInfo) # compiler didn't cache the optimized IR, but `finish!(::AbstractInterpreter, ::InferenceResult)` transformed it to `opt.src`, so we can analyze it
            @assert isa(opt, OptimizationState) && opt.src === ret
            report_pass!(RuntimeDispatchReport, analyzer, opt)
        else
            Core.eval(@__MODULE__, :(ret = $ret))
            throw("unexpected state happened, inspect $(@__MODULE__).ret") # this pass should never happen
        end
    end

    return ret
end

else # @static if isdefined(CC, :finish!)

include("legacy/dispatch")

end # @static if isdefined(CC, :finish!)

# entries
# =======

"""
    analyze_dispatch(f, types = Tuple{}; jetconfigs...) -> (analyzer::DispatchAnalyzer, frame::Union{InferenceFrame,Nothing})
    analyze_dispatch(tt::Type{<:Tuple}; jetconfigs...) -> (analyzer::DispatchAnalyzer, frame::Union{InferenceFrame,Nothing})

Analyzes the generic function call with the given type signature, and returns:
- `analyzer::DispatchAnalyzer`: contains analyzed optimization failures and runtime dispatch points
- `frame::Union{InferenceFrame,Nothing}`: the final state of the abstract interpretation,
  or `nothing` if `f` is a generator and the code generation has been failed
"""
function analyze_dispatch(@nospecialize(args...);
                          analyzer = DispatchAnalyzer,
                          jetconfigs...)
    @assert analyzer === DispatchAnalyzer "analyzer is fixed to $DispatchAnalyzer"
    return analyze_call(args...; analyzer, jetconfigs...)
end

"""
    report_dispatch(f, types = Tuple{}; jetconfigs...) -> result_type::Any
    report_dispatch(tt::Type{<:Tuple}; jetconfigs...) -> result_type::Any

Analyzes the generic function call with the given type signature, and then prints detected
optimization failures and runtime dispatch points to `stdout`, and finally returns the result
type of the call.
"""
function report_dispatch(@nospecialize(args...);
                         analyzer = DispatchAnalyzer,
                         jetconfigs...)
    @assert analyzer === DispatchAnalyzer "analyzer is fixed to $DispatchAnalyzer"
    return report_call(args...; analyzer, jetconfigs...)
end

"""
    @analyze_dispatch [jetconfigs...] f(args...)

Evaluates the arguments to the function call, determines its types, and then calls
[`analyze_dispatch`](@ref) on the resulting expression.
As with `@code_typed` and its family, any of [JET configurations](https://aviatesk.github.io/JET.jl/dev/config/)
or [dispatch analysis specific configurations](@ref dispatch-analysis-configurations) can be given as the optional arguments like this:
```julia
# reports `rand(::Type{Bool})` with `unoptimize_throw_blocks` configuration turned on
julia> @analyze_dispatch unoptimize_throw_blocks=true rand(Bool)
```
"""
macro analyze_dispatch(ex0...)
    return gen_call_with_extracted_types_and_kwargs(__module__, :analyze_dispatch, ex0)
end

"""
    @report_dispatch [jetconfigs...] f(args...)

Evaluates the arguments to the function call, determines its types, and then calls
[`report_dispatch`](@ref) on the resulting expression.
As with `@code_typed` and its family, any of [JET configurations](https://aviatesk.github.io/JET.jl/dev/config/)
or [dispatch analysis specific configurations](@ref dispatch-analysis-configurations) can be given as the optional arguments like this:
```julia
# reports `rand(::Type{Bool})` with `unoptimize_throw_blocks` configuration turned on
julia> @report_call unoptimize_throw_blocks=true rand(Bool)
```
"""
macro report_dispatch(ex0...)
    return gen_call_with_extracted_types_and_kwargs(__module__, :report_dispatch, ex0)
end

# Test integration
# ================

"""
    @test_nodispatch [jetconfigs...] [broken=false] [skip=false] f(args...)

Tests the generic function call `f(args...)` is free from runtime dispatch.
Returns a `Pass` result if it is, a `Fail` result if if contains any location where runtime
dispatch or optimization failure happens, or an `Error` result if this macro encounters an
unexpected error. When the test `Fail`s, abstract call stack to each problem location will
also be printed to `stdout`.

```julia
julia> @test_nodispatch sincos(10)
Test Passed
  Expression: #= none:1 =# JETTest.@test_nodispatch sincos(10)
```

As with [`@report_dispatch`](@ref) or [`@analyze_dispatch`](@ref), any of [JET configurations](https://aviatesk.github.io/JET.jl/dev/config/)
or [dispatch analysis specific configurations](@ref dispatch-analysis-configurations) can be given as the optional arguments like this:
```julia
julia> function f(n)
            r = sincos(n)
            println(r) # `println` is full of runtime dispatches, but we can ignore the corresponding reports from `Base` by explicit frame filter
            return r
       end;
julia> this_module_filter(x) = x.mod === @__MODULE__;

julia> @test_nodispatch frame_filter=this_module_filter f(10)
Test Passed
  Expression: #= none:1 =# JETTest.@test_nodispatch frame_filter = this_module_filter f(10)
```

`@test_nodispatch` is fully integrated with [`Test` standard library's unit-testing infrastructure](https://docs.julialang.org/en/v1/stdlib/Test/).
It means, the result of `@test_nodispatch` will be included in the final `@testset` summary,
it supports `skip` and `broken` annotations as `@test` macro does, etc.
```julia
julia> using JETTest, Test

julia> f(params) = sin(params.value); # type-stable
julia> params = (; value = 10);       # non-constant global variable
julia> g() = sin(params.value);       # very type-instable

julia> @testset "check optimizations" begin
           @test_nodispatch f((; value = 10)) # pass
           @test_nodispatch g()               # fail
           @test_nodispatch broken=true g()   # annotated as broken, thus still "pass"
       end
check optimizations: Dispatch Test Failed at none:3
  Expression: #= none:3 =# JETTest.@test_nodispatch g()
  ═════ 2 possible errors found ═════
  ┌ @ none:1 Base.getproperty(%1, :value)
  │ runtime dispatch detected: Base.getproperty(%1::Any, :value::Symbol)
  └──────────
  ┌ @ none:1 Main.sin(%2)
  │ runtime dispatch detected: Main.sin(%2::Any)
  └──────────

Test Summary:       | Pass  Fail  Broken  Total
check optimizations |    1     1       1      3
ERROR: Some tests did not pass: 1 passed, 1 failed, 0 errored, 1 broken.
```
"""
macro test_nodispatch(ex0...)
    ex0 = collect(ex0)

    local broken = nothing
    local skip   = nothing
    idx = Int[]
    for (i,x) in enumerate(ex0)
        if iskwarg(x)
            key, val = x.args
            if key === :broken
                if !isnothing(broken)
                    error("invalid test macro call: cannot set `broken` keyword multiple times")
                end
                broken = esc(val)
                push!(idx, i)
            elseif key === :skip
                if !isnothing(skip)
                    error("invalid test macro call: cannot set `skip` keyword multiple times")
                end
                skip = esc(val)
                push!(idx, i)
            end
        end
    end
    if !isnothing(broken) && !isnothing(skip)
        error("invalid test macro call: cannot set both `skip` and `broken` keywords")
    end
    deleteat!(ex0, idx)

    testres, orig_expr = test_dispatch_exs(ex0, __module__, __source__)

    return quote
        if $(!isnothing(skip) && skip)
            $record($get_testset(), $Broken(:skipped, $orig_expr))
        else
            testres = $testres
            if $(!isnothing(broken) && broken)
                if isa(testres, $DispatchTestFailure)
                    testres = $Broken(:test_nodispatch, $orig_expr)
                elseif isa(testres, $Pass)
                    testres = $Error(:test_unbroken, $orig_expr, nothing, nothing, $(QuoteNode(__source__)))
                end
            else
                isa(testres, $Pass) || ccall(:jl_breakpoint, $Cvoid, ($Any,), testres)
            end
            $record($get_testset(), testres)
        end
    end
end

iskwarg(@nospecialize(x)) = @isexpr(x, :(=))

get_exceptions() = @static if isdefined(Base, :current_exceptions)
    Base.current_exceptions()
else
    Base.catch_stack()
end
@static if !hasfield(Pass, :source)
    Pass(test_type::Symbol, orig_expr, data, thrown, source) = Pass(test_type, orig_expr, data, thrown)
end

function test_dispatch_exs(ex0, m, source)
    analyzer_call = gen_call_with_extracted_types_and_kwargs(m, :analyze_dispatch, ex0)
    orig_expr = QuoteNode(
        Expr(:macrocall, GlobalRef(JETTest, Symbol("@test_nodispatch")), source, ex0...))
    source = QuoteNode(source)
    testres = :(try
        analyzer, frame = $analyzer_call
        reports = $get_reports(analyzer)
        if $length(reports) == 0
            $Pass(:test_nodispatch, $orig_expr, nothing, nothing, $source)
        else
            $DispatchTestFailure($orig_expr, $source, reports)
        end
    catch err
        isa(err, $InterruptException) && rethrow()
        $Error(:test_error, $orig_expr, err, $get_exceptions(), $source)
    end) |> Base.remove_linenums!
    return testres, orig_expr
end

"""
    test_nodispatch(f, types = Tuple{}; broken::Bool = false, skip::Bool = false, jetconfigs...)
    test_nodispatch(tt::Type{<:Tuple}; broken::Bool = false, skip::Bool = false, jetconfigs...)

Tests the generic function call with the given type signature is free from runtime dispatch.
Except that it takes a type signature rather than a call expression, this function works
in the same way as [`@test_nodispatch`](@ref).
"""
function test_nodispatch(@nospecialize(args...);
                         broken::Bool = false, skip::Bool = false,
                         jetconfigs...)
    source = LineNumberNode(@__LINE__, @__FILE__)
    kwargs = map(((k,v),)->Expr(:kw, k, v), collect(jetconfigs))
    orig_expr = :(JETTest.test_nodispatch($(args...); $(kwargs...)))

    if skip
        record(get_testset(), Broken(:skipped, orig_expr))
    else
        testres = try
            analyzer, frame = analyze_dispatch(args...; jetconfigs...)
            reports = get_reports(analyzer)
            if length(reports) == 0
                Pass(:test_nodispatch, orig_expr, nothing, nothing, source)
            else
                DispatchTestFailure(orig_expr, source, reports)
            end
        catch err
            isa(err, InterruptException) && rethrow()
            Error(:test_error, orig_expr, err, get_exceptions(), source)
        end

        if broken
            if isa(testres, DispatchTestFailure)
                testres = Broken(:test_nodispatch, orig_expr)
            elseif isa(testres, Pass)
                testres = Error(:test_unbroken, orig_expr, nothing, nothing, source)
            end
        else
            isa(testres, Pass) || ccall(:jl_breakpoint, Cvoid, (Any,), testres)
        end
        record(get_testset(), testres)
    end
end

# NOTE we will just show abstract call strack, and won't show backtrace of actual test executions

struct DispatchTestFailure <: Test.Result
    orig_expr::Expr
    source::LineNumberNode
    reports::Vector{InferenceErrorReport}
end

const TEST_INDENTS = "  "

function Base.show(io::IO, t::DispatchTestFailure)
    printstyled(io, "Dispatch Test Failed"; bold=true, color=Base.error_color())
    print(io, " at ")
    printstyled(io, something(t.source.file, :none), ":", t.source.line, "\n"; bold=true, color=:default)
    println(io, TEST_INDENTS, "Expression: ", t.orig_expr)
    # print abstract call stack, with appropriate indents
    _, ctx = Base.unwrapcontext(io)
    buf = IOBuffer()
    ioctx = IOContext(buf, ctx)
    print_reports(ioctx, t.reports) # TODO kwargs support
    lines = replace(String(take!(buf)), '\n'=>string('\n',TEST_INDENTS))
    print(io, TEST_INDENTS, lines)
end

Base.show(io::IO, ::MIME"application/prs.juno.inline", t::DispatchTestFailure) =
    return t

function Test.record(::Test.FallbackTestSet, t::DispatchTestFailure)
    println(t)
    throw(FallbackTestSetException("There was an error during testing"))
end

function Test.record(ts::Test.DefaultTestSet, t::DispatchTestFailure)
    if Test.TESTSET_PRINT_ENABLE[]
        printstyled(ts.description, ": ", color=:white)
        print(t)
        println()
    end
    # HACK convert to `Fail` so that test summarization works correctly
    push!(ts.results, Fail(:test_nodispatch, t.orig_expr, nothing, nothing, t.source))
    return t
end
