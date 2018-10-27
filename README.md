# EnvStatus

[![Build Status](https://travis-ci.org/gbataille/enstatus.svg?branch=master)](https://travis-ci.org/enstatus/gitHUD)
[![Release](https://img.shields.io/github/release/gbataille/envstatus.svg)](https://github.com/gbataille/envstatus/releases)
[![Hackage](https://img.shields.io/hackage/v/envstatus.svg)](https://hackage.haskell.org/package/envstatus)
[![Hackage](https://img.shields.io/hackage-deps/v/envstatus.svg)](https://hackage.haskell.org/package/envstatus)

## Why???

* Ok so I needed to get back on **Haskell** a bit. I had mostly not touched the language since I had
  done [GitHud](https://github.com/gbataille/githud) a couple of years before
* I like to know where I am (it's part of my productivity), and as such my prompt looks like that:
  ```
  [Terraform workspace: default] [nvm: v8.10.0] [rvm: ruby-2.5.1] [virtualenv: p4d_cloud_352]
   𝘮 ← 1 [PCL-1292-robots_txt]
  gbataille ~/Doc…rog/Pix4d/cloud $                                                     17:01
  ```
  (Yes, I like context. I have a couple more possible info not displayed here ;) )
* I like things to go fast (part of my search for productivity) and with the number of things I want
  to display, my prompt took almost 1s to appear. Way too much to be acceptable (to me)

All of this means that I wanted a **tool** that could **accept a list of commands, organized in a
nice little template**, and that would **render** the template **efficiently**. Efficiently means
basically that all commands are run in parallel, meaning that the time it takes to render the
template is basically the time it takes to execute the longest command.

This is what **envstatus** is.


## Install

Whichever way you install envstatus, don't forget to complete the [Setup](#setup)

### Mac OSX with brew (soon)

* link my tap

```
brew tap gbataille/homebrew-gba
```

* install envstatus

```
brew install envstatus
```

### With Cabal

envstatus is available on hackage. Therefore just get it as usual

```
cabal install envstatus
```

## Setup

The first thing to do is build a configuration file. There is one [example](.envstatusrc) in this
repo.
envstatus searches for a `.envstatusrc` file in your home directory

This file is in "key: value" format and should contain:
* an `output_template` key that is the template to render.
** It can be multiline (see [example](.envstatusrc))
** It can contain references to commands, inside double curly braces
* Several key/command associations. Those command are executed and their output used to fill in the
  placeholders in the `output_template`

## Example

The following config
```
# For multiline strings, just go to the line and indent the content. If there are several lines,
# make sure the indentation is consintent. It will be stripped in the produced output.
# TEMPLATE
output_template: venv: {{virtualenv}}
   python: {{python}}
   rvm: {{rvm}}
   nvm: {{nvm}}
   Terraform: {{tf}}
# COMMANDS
virtualenv: bash -c "echo $VIRTUAL_ENV"
rvm: bash -c "echo $RUBY_VERSION"
nvm: node --version
tf: terraform workspace show
python: python --version
```

outputs
```
venv:
python:  Python 3.5.4
rvm:  ruby-2.5.1
nvm:  v8.10.0
Terraform:  default
```

in about 200 ms (on my old macbook)

## Contributing

### Running the tests

Simply call `./scripts/test.sh`
