module JETTest

let
    README = normpath(dirname(@__DIR__), "README.md")
    include_dependency(README)
    s = """$(read(README, String))

    The JETTest.jl toolset includes:
    - [Dispatch Analysis](@ref): automatically detects possible performance pitfalls, where the optimization was failed and/or runtime dispatch will happen
    """
    @doc s JETTest
end

const CC = Core.Compiler

# imports
# =======

using JET.JETInterfaces

import JET:
    get_cache_key

import Test:
    record

# usings
# ======

using JET

using JET:
    State,
    @invoke,
    @isexpr,
    gen_call_with_extracted_types_and_kwargs,
    get_reports,
    print_reports

using .CC:
    AbstractInterpreter,
    InferenceState,
    InferenceResult,
    OptimizationState,
    OptimizationParams,
    IRCode,
    find_throw_blocks,
    widenconst,
    argextype

using Core:
    Builtin,
    Const,
    CodeInfo,
    MethodInstance

using Base:
    isconcretedispatch

using Test:
    Test,
    Pass, Fail, Broken, Error,
    Threw,
    get_testset,
    TESTSET_PRINT_ENABLE,
    AbstractTestSet,
    DefaultTestSet,
    FallbackTestSetException

# filters
# =======

function module_filter(m)
    return function (frame::State)
        frame.mod === m
    end
end

# includes
# ========

include("dispatch.jl")

export
    analyze_dispatch,
    @analyze_dispatch,
    report_dispatch,
    @report_dispatch,
    @test_nodispatch,
    module_filter

end
