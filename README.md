# init.el

## Setup

### Idris2

Install [idris2-pack](https://github.com/stefan-hoeck/idris2-pack):

```sh
$ bash -c "$(curl -fsSL https://raw.githubusercontent.com/stefan-hoeck/idris2-pack/main/install.bash)"
$ echo 'export PATH="$HOME/.pack/bin:$PATH"' >> ~/.zshrc
$ source ~/.zshrc
```

Install [idris2-lsp](https://github.com/idris-community/idris2-lsp):

```sh
$ pack install-app idris2-lsp
```

### Haskell

```sh
$ brew install haskell-language-server hlint
```

### Typescript
```sh
$ npm install -g typescript-language-server
```

### Vue

```sh
$ npm install -g @vue/language-server
```

### Svelte

```sh
$ npm install -g svelte-language-server
```

### Ag

```sh
$ brew install the_silver_searcher
```
