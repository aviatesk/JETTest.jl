module JETTest

let
    README = normpath(dirname(@__DIR__), "README.md")
    include_dependency(README)
    @doc read(README, String) JET
end

const CC = Core.Compiler

# imports
# =======

using JET.JETInterfaces

import JET:
    get_cache_key

import .CC:
    _typeinf,
    finish,
    finish!,
    optimize,
    collect_argtypes,
    abstract_call_gf_by_type

import Test:
    record

# usings
# ======
# TODO: really use `using` instead

using JET

import JET:
    State,
    @invoke,
    @isexpr,
    gen_call_with_extracted_types_and_kwargs,
    get_reports,
    print_reports

import .CC:
    AbstractInterpreter,
    InferenceState,
    InferenceResult,
    OptimizationState,
    OptimizationParams,
    IRCode,
    widenconst,
    argextype

import Core:
    Builtin,
    Const,
    CodeInfo,
    MethodInstance

import Test:
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
