#
# Executes commands at the start of an interactive session.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#


# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# Loads rvm
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

# Changes terminal color with daylight and so
if [[ -d $HOME/.dynamic-colors ]]; then
    # set the $DAYLIGHT based on hour
    source $HOME/.bin/daylight

    # dynamic colors
    export PATH="$PATH:$HOME/.dynamic-colors/bin"
    source $HOME/.dynamic-colors/completions/dynamic-colors.zsh
    export DYNAMIC_COLORS_ROOT="$HOME/.dynamic-colors/"

    if [[ $DAYLIGHT == "false" ]]; then
        dynamic-colors switch solarized-dark-desaturated
    else
        dynamic-colors switch solarized-light
    fi
fi

PATH="${PATH}${PATH+:}/home/sggutier/perl5/bin"; export PATH;
PERL5LIB="/home/sggutier/perl5/lib/perl5${PERL5LIB+:}${PERL5LIB}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="/home/sggutier/perl5${PERL_LOCAL_LIB_ROOT+:}${PERL_LOCAL_LIB_ROOT}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"/home/sggutier/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/home/sggutier/perl5"; export PERL_MM_OPT;
