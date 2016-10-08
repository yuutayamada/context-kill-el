# context-kill.el

This package provides a single function that similar to
`paredit-kill`, but it work with not Lisp languages.

## Usage

``` lisp
(add-hook 'prog-mode-hook
  '(lambda () (define-key global-map [remap kill-line] 'context-kill)))
```

## license
GPLv3
