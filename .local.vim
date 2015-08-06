
augroup project_settings
    au!
    autocmd BufEnter * let b:start='clisp -i %'
    autocmd BufEnter * let b:dispatch='clisp -i %'
augroup END
