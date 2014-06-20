## Instalação

* clone o repositorio para a pasta `~/.dotfiles`:

```
cd ~
git clone https://github.com/pedrowss/dotfiles.git .dotfiles
```

* crie links simbólicos para os arquivos de configuração

```
ln -s .dotfiles/vimrc .vimrc
ln -s .dotfiles/zshrc-oh-my-zsh .zshrc
ln -s .dotfiles/emacs .emacs.d
ln -s .dotfiles/gitconfig .gitconfig
ln -s .dotfiles/gitignore_global .gitignore_global
```

## Emacs

* instalar o cask (http://cask.github.io/installation/):

```
curl -fsSkL https://raw.github.com/cask/cask/master/go | python
```

* executar o comando cask dentro da pasta `.emacs.d`

```
cd ~/.emacs.d
cask
```
