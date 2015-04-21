# helm-fasd

helm-fasd provides an async helm source of fasd search results in emacs.

## Highlights
 - instant results, you don't have to enter an initial query
 - everything works exactly like it does on the CLI

## Installation

Add the containing directory to your load path, load and bind to a key:
(add-to-list 'load-path "~/path/to/helm-fasd/")
(require 'helm-fasd)
(global-set-key (kbd "<menu> o") 'helm-fasd)

## Future improvements
 - support for different search modes of fasd
