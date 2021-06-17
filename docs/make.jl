using JETTest, Documenter

const DOC_SRC_DIR    = normpath(@__DIR__, "src")
const INDEX_FILENAME = normpath(DOC_SRC_DIR, "index.md")

function generate_index!()
    isfile(INDEX_FILENAME) && rm(INDEX_FILENAME)
    open(INDEX_FILENAME, write=true) do io
        s = string(@doc JETTest)
        write(io, s)
    end

    return relpath(INDEX_FILENAME, DOC_SRC_DIR)
end

makedocs(; modules=[JETTest],
           pages=[
               "README" => generate_index!(),
               "Toolset" => let
                    files = readdir(normpath(DOC_SRC_DIR, "toolset"), join=true)
                    relpath.(files, DOC_SRC_DIR)
               end,
           ],
           sitename="JETTest.jl",
           repo="https://github.com/aviatesk/JETTest.jl/blob/{commit}{path}#{line}",
           format=Documenter.HTML(;
               prettyurls=get(ENV, "CI", "false") == "true",
               canonical="https://aviatesk.github.io/JETTest.jl",
               assets=String[],
           ),
)

deploydocs(; repo="github.com/aviatesk/JETTest.jl",
             push_preview = true,
             )
