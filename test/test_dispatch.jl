module TestDispatch

using Test, JETTest, MacroTools
import JETTest: RuntimeDispatchReport, module_filter
import JET: get_reports

# DispatchAnalyzer
# ================

# simple cases
# ------------

# `f(::Int)` a concrete call and just type stable and anything shouldn't be reported
f(a) = a
f(a::Number) = a
let
    analyzer, = @analyze_dispatch f(10)  # should be ok
    @test isempty(get_reports(analyzer))
end

# if the argument type isn't well typed, compiler can't determine which method to call,
# and it will lead to runtime dispatch
let
    analyzer, = analyze_dispatch((Vector{Any},)) do ary
        f(ary[1]) # runtime dispatch !
    end
    @test length(get_reports(analyzer)) == 1
    r = first(get_reports(analyzer))
    @test isa(r, RuntimeDispatchReport)
end

# if we annotate `@noinline` to a function, then its call won't be inlined and will be
# dispatched runtime
@inline   g1(a) = return a
@noinline g2(a) = return a
let
    analyzer, = analyze_dispatch((Vector{Any},)) do ary
        a = ary[1]
        g1(a) # this call should be statically resolved and inlined
        g2(a) # this call should be statically resolved but not inlined, and will be dispatched
    end

    # NOTE the following test is line-sensitive !
    @test length(get_reports(analyzer)) == 1
    r = first(get_reports(analyzer))
    @test isa(r, RuntimeDispatchReport)
    @test any(r.vst) do vf
        vf.file === Symbol(@__FILE__) &&
        vf.line == (@__LINE__) - 9
    end
end

# real-world targets
# ------------------

# the `unoptimize_throw_blocks` configuration disables optimizations on "throw blocks" by default,
# but `DispatchAnalyzer` ignores problems from them, so we don't get error reports here
let
    analyzer, = @analyze_dispatch sin(10)
    @test isempty(get_reports(analyzer))
end

# filter
# ------

# problem: when ∑1/n exceeds 30 ?
function compute(x)
    r = 1
    s = 0.0
    n = 1
    @time while r < x
        s += 1/n
        if s ≥ r
            println("round $r/$x has been finished") # we're not interested type-instabilities within this call
            r += 1
        end
        n += 1
    end
    return n, s
end

# we will get bunch of reports from the `println` call
let
    analyzer, = @analyze_dispatch compute(30)
    @test !isempty(get_reports(analyzer))
end
# if we use different `frame_filter`, a fresh analysis should run
let
    analyzer, = @analyze_dispatch frame_filter=module_filter(@__MODULE__) compute(30)
    @test isempty(get_reports(analyzer))
end

# skip_nonconcrete_calls
# ----------------------

const F1_DEFINITION_LINE = @__LINE__
f1(a) = sin(a)
f1(a::Number) = cos(a)
let
    # by default, we'd ignore error reports from `f1` calls
    analyzer, = analyze_dispatch((Vector{Any},)) do ary
        f1(ary[1]) # runtime dispatch !
    end
    @test length(get_reports(analyzer)) == 1
    r = first(get_reports(analyzer))
    @test isa(r, RuntimeDispatchReport)
end
let
    # when the `skip_nonconcrete_calls` configuration is turned off, we will get error reports
    # from those non-concrete calls of `f1`
    analyzer, = analyze_dispatch((Vector{Any},); skip_nonconcrete_calls=false) do ary
        f1(ary[1]) # runtime dispatch !
    end
    @test length(get_reports(analyzer)) ≥ 3
    @test any(get_reports(analyzer)) do r
        isa(r, RuntimeDispatchReport) &&
        last(r.vst).file === Symbol(@__FILE__) && last(r.vst).line == (@__LINE__) - 5 # report for `f1(ary[1])`
    end
    @test any(get_reports(analyzer)) do r
        isa(r, RuntimeDispatchReport) &&
        last(r.vst).file === Symbol(@__FILE__) && last(r.vst).line == F1_DEFINITION_LINE+1 # report for `sin(a)` within `f1(a)`
    end
    @test any(get_reports(analyzer)) do r
        isa(r, RuntimeDispatchReport) &&
        last(r.vst).file === Symbol(@__FILE__) && last(r.vst).line == F1_DEFINITION_LINE+2 # report for `cos(a)` within `f1(a::Number)`
    end
end

# Test integration
# ================

# runs `f()` in an isolated testset, so that test functions don't influence the current test suite
function with_isolated_testset(f)
    ts = Test.DefaultTestSet("isolated")
    Test.push_testset(ts)
    try
        mktemp() do path, io
            redirect_stdout(io) do
                f()
            end
        end
    catch err
        rethrow(err)
    finally
        Test.pop_testset()
    end
    return ts
end

# do a test that is equivalent to `macroex` but with `test_nodispatch`
macro addfntest(macroex)
    funcex = MacroTools.postwalk(macroex) do x
        pat = :(@test_nodispatch(args__))
        if @capture(x, $pat)
            if isexpr(x, :macrocall)
                kwargs = map(x->Expr(:kw, x.args...), x.args[findall(x->isexpr(x,:(=)), x.args)])
                testex = x.args[findfirst(x->isexpr(x,:call), x.args)::Int]
                return :(test_nodispatch(Base.typesof($(testex.args...)); $(kwargs...)))
            end
        end
        return x
    end

    return quote
        $macroex
        $funcex
    end
end

# positive case
@addfntest let
    ts = with_isolated_testset() do
        @test_nodispatch f(10) # ok
    end
    @test ts.n_passed == 1
end

# negative case
# NOTE the following test is line-sensitive !
g = 10
ff(a) = f(g) # untyped, thus runtime dispatch
let
    ts = with_isolated_testset() do
        @test_nodispatch ff(10)
    end
    @test ts.n_passed == 0
    @test length(ts.results) == 1
    r = ts.results[1]
    @test isa(r, Test.Fail)
    @test r.source === LineNumberNode((@__LINE__)-6, @__FILE__)
end

# actual error within `@test_nodispatch`
# NOTE the following test is line-sensitive !
let
    ts = with_isolated_testset() do
        @test_nodispatch undefvar(10)
    end
    @test ts.n_passed == 0
    @test length(ts.results) == 1
    r = ts.results[1]
    @test isa(r, Test.Error)
    @test r.source === LineNumberNode((@__LINE__)-6, @__FILE__)
    @test occursin("UndefVarError", r.value)
end

# supports `skip` keyword
@addfntest let
    ts = with_isolated_testset() do
        @test_nodispatch skip=true ff(10)
    end
    @test ts.n_passed == 0
    @test length(ts.results) == 1
    r = ts.results[1]
    @test isa(r, Test.Broken)
    @test r.test_type === :skipped

    ts = with_isolated_testset() do
        skip = true
        @test_nodispatch skip=skip ff(10)
    end
    @test ts.n_passed == 0
    @test length(ts.results) == 1
    r = ts.results[1]
    @test isa(r, Test.Broken)
    @test r.test_type === :skipped
end

# supports `broken` keyword
@addfntest let
    ts = with_isolated_testset() do
        @test_nodispatch broken=true ff(10)
    end
    @test ts.n_passed == 0
    @test length(ts.results) == 1
    r = ts.results[1]
    @test isa(r, Test.Broken)
    @test r.test_type === :test_nodispatch

    ts = with_isolated_testset() do
        broken = true
        @test_nodispatch broken=broken ff(10)
    end
    @test ts.n_passed == 0
    @test length(ts.results) == 1
    r = ts.results[1]
    @test isa(r, Test.Broken)
    @test r.test_type === :test_nodispatch

    ts = with_isolated_testset() do
        @test_nodispatch broken=true f(10)
    end
    @test ts.n_passed == 0
    @test length(ts.results) == 1
    r = ts.results[1]
    @test isa(r, Test.Error)
    @test r.test_type === :test_unbroken
end

# supports JET's configurations
@addfntest let
    ts = with_isolated_testset() do
        @test_nodispatch sin(10) # ok
    end
    @test ts.n_passed == 1

    ts = with_isolated_testset() do
        @test_nodispatch sin(10) # still be ok
    end
    @test ts.n_passed == 1

    ts = with_isolated_testset() do
        @test_nodispatch skip_unoptimized_throw_blocks=false sin(10)
    end
    @test ts.n_passed == 0
    @test length(ts.results) == 1
    r = ts.results[1]
    @test isa(r, Test.Fail)

    ts = with_isolated_testset() do
        @test_nodispatch skip_unoptimized_throw_blocks=false broken=true sin(10)
    end
    @test ts.n_passed == 0
    @test length(ts.results) == 1
    r = ts.results[1]
    @test isa(r, Test.Broken)
end

end # module TestDispatch
