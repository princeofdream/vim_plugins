#!/bin/sh

TOP=$(pwd)

WHOAMI=`uname |grep -i Linux`
SYSTEM_TYPE="Unkonw"

if [ ! -z "$WHOAMI" ]
then
	echo "Using Linux"
	SYSTEM_TYPE="Linux"
fi

cd $TOP/../

if [ "$SYSTEM_TYPE" == "Linux" ]
then
	ln -s $TOP/vim_plugins/AfterColors ./
	ln -s $TOP/vim_plugins/Align ./
	ln -s $TOP/vim_plugins/calendar-vim ./
	ln -s $TOP/vim_plugins/CountJump ./
	ln -s $TOP/vim_plugins/cvim ./
	ln -s $TOP/vim_plugins/fencview ./
	ln -s $TOP/vim_plugins/FindMate ./
	ln -s $TOP/vim_plugins/mark.vim ./
	ln -s $TOP/vim_plugins/matchit ./
	# ln -s $TOP/vim_plugins/omnicppcomplete ./
	ln -s $TOP/vim_plugins/project ./
	ln -s $TOP/vim_plugins/python_match ./
	ln -s $TOP/vim_plugins/repeat ./
	ln -s $TOP/vim_plugins/showmarks ./
	ln -s $TOP/vim_plugins/surround ./
	ln -s $TOP/vim_plugins/taglist ./
	ln -s $TOP/vim_plugins/vim-autocomplpop ./
	ln -s $TOP/vim_plugins/vim-fuzzyfinder ./
	ln -s $TOP/vim_plugins/vim-l9 ./
	ln -s $TOP/vim_plugins/ZoomWin ./
	ln -s $TOP/awk-support ./
	ln -s $TOP/bash-support ./
	ln -s $TOP/c-support ./
	ln -s $TOP/git-support ./
	ln -s $TOP/latex-support ./
	ln -s $TOP/lua-support ./
	ln -s $TOP/perl-support ./
	ln -s $TOP/vim-support ./
else
	cp -r $TOP/vim_plugins/AfterColors ./
	cp -r $TOP/vim_plugins/Align ./
	cp -r $TOP/vim_plugins/calendar-vim ./
	cp -r $TOP/vim_plugins/CountJump ./
	cp -r $TOP/vim_plugins/cvim ./
	cp -r $TOP/vim_plugins/fencview ./
	cp -r $TOP/vim_plugins/FindMate ./
	cp -r $TOP/vim_plugins/mark.vim ./
	cp -r $TOP/vim_plugins/matchit ./
	# cp -r $TOP/vim_plugins/omnicppcomplete ./
	cp -r $TOP/vim_plugins/project ./
	cp -r $TOP/vim_plugins/python_match ./
	cp -r $TOP/vim_plugins/repeat ./
	cp -r $TOP/vim_plugins/showmarks ./
	cp -r $TOP/vim_plugins/surround ./
	cp -r $TOP/vim_plugins/taglist ./
	cp -r $TOP/vim_plugins/vim-autocomplpop ./
	cp -r $TOP/vim_plugins/vim-fuzzyfinder ./
	cp -r $TOP/vim_plugins/vim-l9 ./
	cp -r $TOP/vim_plugins/ZoomWin ./
	cp -r $TOP/awk-support ./
	cp -r $TOP/bash-support ./
	cp -r $TOP/c-support ./
	cp -r $TOP/git-support ./
	cp -r $TOP/latex-support ./
	cp -r $TOP/lua-support ./
	cp -r $TOP/perl-support ./
	cp -r $TOP/vim-support ./
fi


