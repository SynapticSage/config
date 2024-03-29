using Revise
using DrWatson
import OhMyREPL

#import REPL
#REPL.Options(confirm_exit=true)  
OhMyREPL.enable_autocomplete_brackets(false)

#Deactivating ctrl+d
#-------------------
import REPL
mykeys = Dict{Any,Any}("^D" => (s, o...) -> nothing)
function customize_keys(repl)
    repl.interface = REPL.setup_interface(repl; extra_repl_keymap = mykeys)
end
atreplinit(customize_keys)


import OhMyREPL: Passes.SyntaxHighlighter
using Crayons

penumbra_dark = SyntaxHighlighter.ColorScheme()
SyntaxHighlighter.symbol!(penumbra_dark, crayon"#00A0BE")
SyntaxHighlighter.comment!(penumbra_dark, crayon"#636363")
SyntaxHighlighter.string!(penumbra_dark, crayon"#46A473")
SyntaxHighlighter.call!(penumbra_dark, crayon"#7E87D6")
SyntaxHighlighter.op!(penumbra_dark, crayon"#CB7459")
SyntaxHighlighter.keyword!(penumbra_dark, crayon"#BD72A8")
SyntaxHighlighter.function_def!(penumbra_dark, crayon"#7E87D6")
SyntaxHighlighter.error!(penumbra_dark, crayon"#CB7459")
SyntaxHighlighter.argdef!(penumbra_dark, crayon"#00A0BE")
SyntaxHighlighter.macro!(penumbra_dark, crayon"#7E87D6")
SyntaxHighlighter.number!(penumbra_dark, crayon"#A38F2D")
SyntaxHighlighter.text!(penumbra_dark, crayon"#8F8F8F")
SyntaxHighlighter.add!("Penumbra Dark", penumbra_dark)

OhMyREPL.colorscheme!("Penumbra Dark")

ENV["PYTHON"] = "/home/ryoung/miniconda3/envs/conda_jl/bin/python"
ENV["CONDA_JL_HOME"] = "/home/ryoung/miniconda3/envs/conda_jl/"
#ENV["CONDA_JL_CONDA_EXE"] = "/home/ryoung/miniconda3/bin/conda"
ENV["CONDA_JL_CONDA_EXE"] = "/home/ryoung/miniconda3/condabin/mamba"


# Run this the first time!
# ------------------------
import Term: install_term_logger
install_term_logger()
#import Term: install_term_stacktrace
#install_term_stacktrace()  #
using InteractiveUtils
using TruncatedStacktraces


# using REPL
# using REPL: LineEdit
# using FuzzyCompletions
#
# struct FuzzyCompletionProvider <: REPL.CompletionProvider
#     mod::Module
# end
#
# function LineEdit.complete_line(c::FuzzyCompletionProvider, s)
#   partial = REPL.beforecursor(s.input_buffer)
#   full = LineEdit.input_string(s)
#
#   # module-aware repl backend completions
#   comps, range, should_complete = completions(full, lastindex(partial), c.mod)
#   filter!(c->score(c)≥0, comps)
#   return unique!(FuzzyCompletions.completion_text.(comps)), partial[range], should_complete
# end
#
# Base.active_repl.interface.modes[1].complete = FuzzyCompletionProvider(Main) 
# # or whatever module where you want to get completes from
