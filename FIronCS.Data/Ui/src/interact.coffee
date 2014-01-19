# CoffeeScript
define([], ->
    el = -> $('#interact')
    blit = (html) ->
        el().html html
    clear = ->
        el().html ''

    html = (model) ->
        if model.actions.length > 0 then blit Handlebars.templates.interact model else clear()

    model = (x, y, actions) ->
        x: x
        y: y
        actions: actions

    show = (x, y) -> 
        pt = [x, y]
        actions = iron.interactAt [x, y] 
        [xt, yt] = iron.screenPos [x * 32, y * 32]
        html model xt, yt, actions 
    
    show: show
)