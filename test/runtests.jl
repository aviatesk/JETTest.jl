using JETTest, Test

@testset "JETTest.jl" begin
    @testset "dispatch.jl" begin
        include("test_dispatch.jl")
    end

    @testset "self-testing" begin
        frame_filter = module_filter(JETTest)
        function function_filter(@nospecialize(ft))
            return ft !== typeof(Core.Compiler.widenconst) # `widenconst` is very untyped, ignore
        end

        @test_nodispatch frame_filter=frame_filter function_filter=function_filter skip_nonconcrete_calls=false analyze_dispatch(sin, (Int,))
    end
end
