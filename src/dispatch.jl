# DispatchAnalyzer
# ================

struct DispatchAnalyzer{S,T} <: AbstractAnalyzer
    state::AnalyzerState
    opts::BitVector
    frame_filter::S
    function_filter::T
    analyze_unoptimized_throw_blocks::Bool
end
function DispatchAnalyzer(;
    frame_filter = x::State->true,
    function_filter = @nospecialize(ft)->true,
    analyze_unoptimized_throw_blocks::Bool = false,
    jetconfigs...)
    state = AnalyzerState(; jetconfigs...)
    return DispatchAnalyzer(state, BitVector(), frame_filter, function_filter, analyze_unoptimized_throw_blocks)
end

# AbstractAnalyzer API requirements
JETInterfaces.AnalyzerState(analyzer::DispatchAnalyzer) = analyzer.state
JETInterfaces.AbstractAnalyzer(analyzer::DispatchAnalyzer, state::AnalyzerState) =
    DispatchAnalyzer(state, analyzer.opts, analyzer.frame_filter, analyzer.function_filter, analyzer.analyze_unoptimized_throw_blocks)
JETInterfaces.ReportPass(analyzer::DispatchAnalyzer) = DispatchAnalysisPass()

# we want to run different analysis with a different filter, so include its hash into the cache key
function JET.get_cache_key(analyzer::DispatchAnalyzer)
    h = @invoke get_cache_key(analyzer::AbstractAnalyzer)
    h = @invoke hash(analyzer.frame_filter::Any, h::UInt)    # HACK avoid dynamic dispatch
    h = @invoke hash(analyzer.function_filter::Any, h::UInt) # HACK avoid dynamic dispatch
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
        !analyzer.analyze_unoptimized_throw_blocks && opt.inlining.params.unoptimize_throw_blocks ?
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
    ret = @invoke _typeinf(analyzer::AbstractAnalyzer, frame::InferenceState)
    @assert isempty(analyzer.opts)
    return ret
end

function CC.finish(frame::InferenceState, analyzer::DispatchAnalyzer)
    ret = @invoke finish(frame::InferenceState, analyzer::AbstractAnalyzer)

    if !analyzer.frame_filter(frame)
        push!(analyzer.opts, false)
    else
        if isa(frame.result.src, OptimizationState)
            push!(analyzer.opts, true)
        else
            report_pass!(OptimizationFailureReport, analyzer, frame)
            push!(analyzer.opts, false)
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

function analyze_dispatch(@nospecialize(f), @nospecialize(types = Tuple{});
                          analyzer = DispatchAnalyzer,
                          jetconfigs...)
    @assert analyzer === DispatchAnalyzer "analyzer is fixed to $DispatchAnalyzer"
    analyze_call(f, types; analyzer, jetconfigs...)
end

function report_dispatch(@nospecialize(f), @nospecialize(types = Tuple{});
                         analyzer = DispatchAnalyzer,
                         jetconfigs...)
    @assert analyzer === DispatchAnalyzer "analyzer is fixed to $DispatchAnalyzer"
    report_call(f, types; analyzer, jetconfigs...)
end

macro analyze_dispatch(ex0...)
    return gen_call_with_extracted_types_and_kwargs(__module__, :analyze_dispatch, ex0)
end

macro report_dispatch(ex0...)
    return gen_call_with_extracted_types_and_kwargs(__module__, :report_dispatch, ex0)
end

# Test integration
# ================

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
