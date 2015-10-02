# etc

A directory to store my own configuration files

## How it works

This repo is an image of my ~/etc directory which contains all my
personal configuration files.

When I install a new box, I clone this repo in my home directory,
and I create links to the configuration files I want to use.

For instance, to get my vim configuration:

```
cd ~
ln -s ~/etc/.vim .
```
