# Unbind some standard qutebrowser bindings
# c.bindings.default = {}
# If you don't want to unbind everything it might be necessary to
# unbind at least the conflicting keybindings like this:
config.unbind('<Ctrl-x>')

# config.set("colors.webpage.darkmode.enabled", True)
config.set("colors.webpage.darkmode.enabled", False)
# config.source('themes/qute-city-lights/city-lights-theme.py')

config.load_autoconfig(False)

config.set("auto_save.session", True)
c.fonts.default_family = 'Iosevka Medium'
c.fonts.default_size = '11.25pt'
config.bind('<Alt-x>', 'cmd-set-text :')
config.bind('<Ctrl-x>c', 'config-source')
config.bind('<Alt-w>', 'yank selection')
config.bind('<Ctrl-E>', 'open-editor', mode='insert')
config.bind('<Ctrl-x><Ctrl-f>', 'cmd-set-text -s :open -t')
config.bind('<Ctrl--><Ctrl-x><Ctrl-f>', 'cmd-set-text -s :open')
config.bind('<Ctrl-x>l', 'reload')
config.bind('<Ctrl-g>', 'mode-leave')

config.bind('<Ctrl-x>e','cmd-set-text :open {url:pretty}')

config.bind('<ctrl-v>', 'scroll-page 0 0.5')
config.bind('<alt-v>', 'scroll-page 0 -0.5')
config.bind('<alt-s>', 'hint all')
config.bind('<ctrl-x>k', 'tab-close')

config.bind('<Alt-Shift-.', 'move-to-end-of-document')
c.bindings.commands['command'] = {
    '<Ctrl-s>': 'search-next',
    '<Ctrl-r>': 'search-prev',

    '<Ctrl-p>': 'completion-item-focus prev',
    '<Ctrl-n>': 'completion-item-focus next',

    '<Alt-p>': 'command-history-prev',
    '<Alt-n>': 'command-history-next',

	# escape hatch
    '<Ctrl-g>': 'mode-leave',
}




c.editor.command = ['emacsclient', '{}']
# c.editor.command: ['urxvt', '-e', 'vim', '{}']

# https://github.com/qutebrowser/qutebrowser/blob/main/doc/help/configuring.asciidoc
# https://www.ii.com/qutebrowser-tips-fragments/
