using LanguageServer
using LanguageServer.SymbolServer

let env = Base.load_path(),
    dir = pwd(),
    depot_path = get(ENV, "JULIA_DEPOT_PATH", ""),
    project_path = dirname(something(Base.current_project(pwd()), Base.load_path_expand(LOAD_PATH[2])))
    using DrWatson: srcdir
    if isdir(srcdir())
        push!(LOAD_PATH, srcdir())
    end

    @info env dir project_path depot_path

    server = LanguageServer.LanguageServerInstance(stdin, stdout, project_path, depot_path)
    server.runlinter = true
    run(server)
end
