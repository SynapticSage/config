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
