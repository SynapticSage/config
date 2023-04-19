using LanguageServer
using LanguageServer.SymbolServer
using DrWatson: projectdir, srcdir, scriptsdir
tmp=true
if isdir(srcdir())
    push!(LOAD_PATH, srcdir())
end

if tmp
    let env=Base.load_path(),
        dir=pwd(),
        project_path=dirname(something(Base.current_project(pwd()),
                                       Base.load_path_expand(LOAD_PATH[2])))

        depot_path=get(ENV, "JULIA_DEPOT_PATH", "$(projectdir()):/home/ryoung/Projects/goal-code/src:/home/ryoung/Projects/goal-code/:$(srcdir())")

        push!(LOAD_PATH, pwd())
        push!(LOAD_PATH, joinpath(pwd(),"src"))
        if isdir(srcdir())
            push!(LOAD_PATH, srcdir())
        end
        if isdir(scriptsdir())
            push!(LOAD_PATH, scriptsdir())
        end

        @info "LANGUAGE SERVER SETTINGS"
        @info env dir project_path depot_path
        @info "srcdir()=$(srcdir())"

        server = LanguageServer.LanguageServerInstance(stdin, stdout, project_path, depot_path)
        server.runlinter=true
        run(server)
    end
else
    env=Base.load_path()
    dir=pwd()
    depot_path=get(ENV, "JULIA_DEPOT_PATH", "$(projectdir()):/home/ryoung/Projects/goal-code/:/home/ryoung/Projects/goal-code/src")
    project_path=dirname(something(Base.current_project(pwd()),
                                   Base.load_path_expand(LOAD_PATH[2])))

    @info env dir project_path depot_path

    server = LanguageServer.LanguageServerInstance(stdin, stdout, project_path, depot_path)
    server.runlinter=true
    run(server)
end
