# CoffeeScript

require(['interact', 'debug'], (interact, debug) ->
   window.ui = {}
   window.ui.interact = interact
   if not window.iron
    window.iron = debug.iron

   window.iron.ui = window.ui
)
