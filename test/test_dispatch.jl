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
    analyzer, = @report_dispatch f(10)  # should be ok
    @test isempty(get_reports(analyzer))
end

# if the argument type isn't well typed, compiler can't determine which method to call,
# and it will lead to runtime dispatch
let
    analyzer, = report_dispatch((Vector{Any},)) do ary
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
    analyzer, = report_dispatch((Vector{Any},)) do ary
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
    analyzer, = @report_dispatch sin(10)
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
    analyzer, = @report_dispatch compute(30)
    @test !isempty(get_reports(analyzer))
end
# if we use different `frame_filter`, a fresh analysis should run
let
    analyzer, = @report_dispatch frame_filter=module_filter(@__MODULE__) compute(30)
    @test isempty(get_reports(analyzer))
end

# skip_nonconcrete_calls
# ----------------------

const F1_DEFINITION_LINE = @__LINE__
f1(a) = sin(a)
f1(a::Number) = cos(a)
let
    # by default, we'd ignore error reports from `f1` calls
    analyzer, = report_dispatch((Vector{Any},)) do ary
        f1(ary[1]) # runtime dispatch !
    end
    @test length(get_reports(analyzer)) == 1
    r = first(get_reports(analyzer))
    @test isa(r, RuntimeDispatchReport)
end
let
    # when the `skip_nonconcrete_calls` configuration is turned off, we will get error reports
    # from those non-concrete calls of `f1`
    analyzer, = report_dispatch((Vector{Any},); skip_nonconcrete_calls=false) do ary
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

end # module TestDispatch
