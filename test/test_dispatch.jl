module TestDispatch

using Test, JETTest
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
    analyzer, = analyze_dispatch((Any,)) do a
        f(a) # runtime dispatch !
    end
    @test length(get_reports(analyzer)) == 1
    r = first(get_reports(analyzer))
    @test isa(r, RuntimeDispatchReport)
    get_reports(analyzer)
end

# if we annotate `@noinline` to a function, then its call won't be inlined and will be
# dispatched runtime
@inline   g1(a) = return a
@noinline g2(a) = return a
let
    analyzer, = analyze_dispatch((Any,)) do a
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

# by default, DispatchAnalyzer turns off the `unoptimize_throw_blocks` configuration, so that
# we don't get reports from throw blocks
let
    analyzer, = @analyze_dispatch sin(10)
    @test isempty(get_reports(analyzer))
end

# we can test the native interpration with an explicit configuration
let
    analyzer, = @analyze_dispatch unoptimize_throw_blocks=true sin(10)
    @test length(get_reports(analyzer)) == 1
    r = first(get_reports(analyzer))
    @test isa(r, RuntimeDispatchReport)
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
    analyzer, = @analyze_dispatch compute(30);
    @test !isempty(get_reports(analyzer))
end
# if we use different `frame_filter`, a fresh analysis should run
let
    analyzer, = @analyze_dispatch frame_filter=module_filter(@__MODULE__) compute(30);
    @test isempty(get_reports(analyzer))
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

# positive case
let
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
let
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
let
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

# supports JET's configurations at the same time
let
    ts = with_isolated_testset() do
        @test_nodispatch sin(10) # ok
    end
    @test ts.n_passed == 1

    ts = with_isolated_testset() do
        @test_nodispatch unoptimize_throw_blocks=true sin(10)
    end
    @test ts.n_passed == 0
    @test length(ts.results) == 1
    r = ts.results[1]
    @test isa(r, Test.Fail)

    ts = with_isolated_testset() do
        @test_nodispatch unoptimize_throw_blocks=true broken=true sin(10)
    end
    @test ts.n_passed == 0
    @test length(ts.results) == 1
    r = ts.results[1]
    @test isa(r, Test.Broken)
end

end # module TestDispatch
