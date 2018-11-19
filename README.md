# Termux notification style for alert.el

## Installation

1. Clone this repository and add it to your `load-path`
2. set `alert-default-style` to `'termux`

### With `use-package`

```
(use-package alert
  :config
  (setq alert-default-style 'termux))
```

## Contributing

If you have a feature idea or find a bug, feel free to issue a pull request. If you need any help
with the code, find me on Matrix as
[@gergely.polonkai.eu](https://riot.im/app/#/user/@gergely:polonkai.eu)

## Requirements

This package (obviously) needs the [alert](https://github.com/jwiegley/alert) package to function.

You will also need to install the `termux-api` package within Termux, and install the Termux:API
([F-Droid](https://f-droid.org/en/packages/com.termux.api/), [Google
Play](https://play.google.com/store/apps/details?id=com.termux.api)) addon app to be installed
alongside with Termux.

## License

This package is released under the terms of version 3 of the GPL.
