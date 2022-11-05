#!/bin/bash

#########################
# SETUP SCRIPTS
# ######################

COLOR_NONE="\033[0m"
COLOR_RED="\033[0;31m"
COLOR_GREEN="\033[0;32m"
COLOR_YELLOW="\033[0;33m"
COLOR_WHITE="\033[1;37m"

send_text() {
    number=$1
    shift
    message="$@"
    curl http://textbelt.com/text -d number=$number -d "$message"

}


_version_check() {
    curver="$1"; targetver="$2";
    [ "$targetver" = "$(echo -e "$curver\n$targetver" | sort -V | head -n1)" ]
}

function install_ffmpeg()
{
    sudo apt-get install yasm nasm \
            build-essential automake autoconf \
            libtool pkg-config libcurl4-openssl-dev \
            intltool libxml2-dev libgtk2.0-dev \
            libnotify-dev libglib2.0-dev libevent-dev \
            checkinstall

    git clone git://git.videolan.org/ffmpeg.git

    cd ffmpeg
    ./configure --enable-gpl --prefix=/usr
    time make -j 8
    sudo mkdir /usr/share/ffmpeg
    sudo checkinstall


}

install_trodes_packages() 
{ 
    return 0
}

install_essential_packages() {
    local -a packages; packages=( \
        build-essential \
        vim zsh curl \
        python-software-properties software-properties-common \
        cmake cmake-data ctags autoconf pkg-config \
        terminator htop iotop iftop \
        silversearcher-ag \
        openssh-server mosh rdate \
        )

    sudo apt-get install -y ${packages[@]}
}

install_python_packages() {
    sudo apt-get install -y python-dev virtualenv virtualenvwrapper
    sudo apt-get install -y python-pip python3-pip

    # install recent versions (9+) of pip at /usr/local/bin
    sudo /usr/bin/pip install --upgrade pip         # pip
    sudo /usr/bin/pip3 install --upgrade pip        # pip3
}

install_ppa_git() {
    # https://launchpad.net/~git-core/+archive/ubuntu/ppa
    sudo add-apt-repository -y ppa:git-core/ppa
    sudo apt-get update
    sudo apt-get install -y git-all git-extras
}

install_ppa_vim8() {
    # For Ubuntu 14.04 and 16.04
    # https://launchpad.net/~jonathonf/+archive/ubuntu/vim
    sudo add-apt-repository -y ppa:jonathonf/vim
    sudo apt-get update
    sudo apt-get install -y vim vim-doc vim-nox
    #sudo apt-get install -y vim-gnome vim-gtk
}

install_neovim() {
    # https://launchpad.net/~neovim-ppa/+archive/ubuntu/unstable
    sudo add-apt-repository -y ppa:neovim-ppa/unstable
    sudo apt-get update
    sudo apt-get install -y neovim

    command -v /usr/bin/pip 2>&1 > /dev/null || sudo apt-get install -y python-pip
    command -v /usr/bin/pip3 2>&1 > /dev/null || sudo apt-get install -y python3-pip
    sudo /usr/bin/pip install --upgrade neovim
    sudo /usr/bin/pip3 install --upgrade neovim
}

install_latest_tmux() {
    # tmux 2.5 will be installed from source compilation,
    # since there is no tmux 2.3+ package that is compatible with ubuntu 14.04.
    # For {libncurses,libevent >= 6} (e.g. ubuntu 16.04+), we may use
    # https://launchpad.net/ubuntu/+archive/primary/+files/tmux_2.5-4_${archi}.deb
    # archi=$(dpkg --print-architecture)  # e.g. amd64
    set -e

    if _version_check "$(tmux -V | cut -d' ' -f2)" "2.6"; then
        echo "$(tmux -V) : $(which tmux)"
        echo "  Already installed, skipping installation"; return
    fi
    if [[ $(hostname) == "Linux"  ]]
    then
      sudo apt-get install -y libevent-dev libncurses5-dev libutempter-dev || exit 1;
    else 
      brew install libevent ncurses || exit 1;
    fi
    TMP_TMUX_DIR="/tmp/.tmux-src/"

    TMUX_TGZ_FILE="tmux-2.6.tar.gz"
    TMUX_DOWNLOAD_URL="https://github.com/tmux/tmux/releases/download/2.6/${TMUX_TGZ_FILE}"

    wget -nc ${TMUX_DOWNLOAD_URL} -P ${TMP_TMUX_DIR} || exit 1;
    cd ${TMP_TMUX_DIR} && tar -xvzf ${TMUX_TGZ_FILE} || exit 1;
    cd "tmux-2.6" && ./configure || exit 1;
    make clean && make -j2 || exit 1;

    sudo make install || exit 1;
    tmux -V
}

install_ppa_nginx() {
    sudo service apache2 stop || true;

    # https://launchpad.net/~nginx/+archive/ubuntu/stable
    sudo add-apt-repository -y ppa:nginx/stable
    sudo apt-get update
    sudo apt-get install -y nginx-full
}

install_node() {
    # https://github.com/nodesource/distributions/tree/master/deb
    curl -sL https://deb.nodesource.com/setup_6.x | sudo -E bash -
    sudo apt-get install -y nodejs

    # some default global packages
    sudo npm install -g http-server
}

install_exa() {
    # https://github.com/ogham/exa/releases
    if _version_check "$(exa --version | cut -d' ' -f2)" "0.4.0"; then
        echo "$(exa --version) : $(which exa)"
        echo "  Already installed, skipping installation"; return
    fi

    echo -e "${COLOR_WHITE}Downloading exa...${COLOR_NONE}"
    EXA_DOWNLOAD_URL="https://github.com/ogham/exa/releases/download/v0.4.0/exa-linux-x86_64.zip"
    EXA_BINARY_SHA1SUM="822ea64b390071298866ce84546811f57eb8503c"  # exa-linux-x86_64 v0.4.0
    TMP_EXA_DIR="/tmp/exa/"

    wget -nc ${EXA_DOWNLOAD_URL} -P ${TMP_EXA_DIR} || exit 1;
    cd ${TMP_EXA_DIR} && unzip -o "exa-linux-x86_64.zip" || exit 1;
    if [[ "$EXA_BINARY_SHA1SUM" != "$(sha1sum exa-linux-x86_64 | cut -d' ' -f1)" ]]; then
        echo -e "${COLOR_RED}SHA1 checksum mismatch, aborting!${COLOR_NONE}"
        exit 1;
    fi
    sudo cp "exa-linux-x86_64" "/usr/local/bin/exa" || exit 1;
    echo -e "${COLOR_GREEN}Installation of exa successful!${COLOR_NONE}"
    echo "$(which exa) : $(exa --version)"
    rm -rf ${TMP_EXA_DIR}
}

install_all() {
    # TODO dependency management: duplicated 'apt-get update'?
    install_essential_packages
    install_python_packages
    install_node
    install_latest_tmux
    install_ppa_vim8
    install_neovim
    install_ppa_git
    install_ppa_nginx
    install_exa
    install_trodes_packages
}


## entrypoint script
#if [ `uname` != "Linux" ]; then
#    echo "Run on Linux (not on Mac OS X)"; exit 1
#fi
#if [ -n "$1" ]; then
#    $1
#else
#    echo "Usage: $0 [command], where command is one of the following:"
#    declare -F | cut -d" " -f3 | grep -v '^_'
#fi

# specify shortcut functions for basic tasks
#neofetch
echo "Personal functions in path!" 
alias ManageUpdate=manageupdate
function manageupdate()
{
        clearfunc=clear
        func() { figlet -w 300 -f univers $1}

        $clearfunc
        sys=$(uname)
        if [[ "$sys" == "Darwin" ]] ; then
              printf "u p d a t i n g \n  h o m e b r e w " | func | lolcat
              brew update; brew upgrade;
        else
          printf "u p d a t i n g \n  a p t " | func | lolcat
          sudo apt update && sudo apt upgrade -y
        fi
        printf  "u p d a t i n g  \n c o n d a " | func  | lolcat
        conda update --all
        $clearfunc
        printf "u p d a t i n g \n  b a s h i t " | func | lolcat
        bash-it update
        $clearfunc
        printf "u p d a t i n g \n  ruby " | func | lolcat
        sudo gem update 
        $clearfunc
        printf "u p d a t i n g \n  v i m " | func | lolcat
        vim -i NONE -c VundleUpdate 
        #mdp ~/.vim/bundle/Vundle.vim/changelog.md
        # printf "u p d a t i n g \n  b a s h i t " | func | lolcat
        # sudo apm update
        return 0
}
alias ManageUpdate=manageupdate

function gitpush
{
        git add -A; git commit -m "$1"; git push
}

function gitpull
{
        git pull;
}

function current_commit_hash()
{
    git --git-dir $(pwd)/.git --work-tree $(pwd) describe --always --abbrev=10
}

function cgitpush()
{
        pushd ~/Code; gitpush $1;
}

function cgitpull()
{
    pushd ~/Code; gitpull;
}

### Multi-Repo Commands ###
function codepush() 
{
    pushd ~/Code; gitpush $1;
    popd;
    pushd ~/Projects; gitpush $1;
    popd
}

function codepull() 
{
    pushd ~/Code; git pull;
    popd;
    pushd ~/Projects; git pull;
    popd
}

function codestatus()
{
    pushd ~/Code; git status;
    popd;
    pushd ~/Projects; git status;
    popd
}
function codediff()
{
    pushd ~/Code; git diff;
    popd;
    pushd ~/Projects; git diff;
    popd
}
###########################
# ALIASES
alias open="xdg-open"
function cl()
{
	cd $1;
	ls $2 $(pwd);
}

function gitprog()
{
	git status
	read -p "Press enter to continue ..."
	git diff --color=auto
}

function gcp()
{
	git add -A :/ && git commit -m "$1" && git push
}

function matlog()
{
	/usr/local/bin/matlab -logfile ~/LastMatlabInstance.log "$@"
}

function aptupdate()
{
	sudo apt-get update && sudo apt-get upgrade
	return 0
}
###########################

function cookiecutter_data()
{
	cookiecutter https://github.com/drivendata/cookiecutter-data-science
}


function cookiecutter_matlab()
{
	cookiecutter https://github.com/suever/matlab-plugin-cookiecutter.git
}

function lnswap()
{
	echo "copying $1 to $2"
	mv $1 $2/
	echo "linking $2/$1 to $1"
	ln -sf $2/$1 $1
}

function true_color_test()
{
    awk 'BEGIN{
    s="/\\/\\/\\/\\/\\"; s=s s s s s s s s;
    for (colnum = 0; colnum<77; colnum++) {
        r = 255-(colnum*255/76);
        g = (colnum*510/76);
        b = (colnum*255/76);
        if (g>255) g = 510-g;
            printf "\033[48;2;%d;%d;%dm", r,g,b;
            printf "\033[38;2;%d;%d;%dm", 255-r,255-g,255-b;
            printf "%s\033[0m", substr(s,colnum+1,1);
        }
        printf "\n";
    }'
}

## Uncomplicated FireWall (ufw) ##
function add_ufw_IP()
{
    # This function opens a port for sending and receiving from an
    # IP address through the firewall
    ufw allow from $1 to any port $2 
    ufw allow to $1 from any port $2 
}

## TMUX ##
function set_tmux_panetitle()
{
    printf '\033]2;' 
    printf "${1} "
    printf '\033\\'
    tmux set -g pane-border-format "#{$TMUX_PANE} #T"
     return 0
}

## RSYNC ##
# exfat-rsync : exfat is not aware of file permisions and so the -a tag that is the status quo for other file transfer types will break it.
function exfat-rsync {
    rsync -rltDv --progress --stats $1 $2
}

## SSH ##
# Fast ssh for x11 (like figures in matalb). Uses compression to send 
# packets and an encryption scheme that it doesn't require nearly as 
# much time to encode/decode as AES: blowfish, which is still pretty secure. There is one shema which is even faster but sacrifices some security.
function fastx11-ssh {
    ssh -XC -c blowfish-cbc,arcfour $1
}

## GIT RECOVERY
alias git-fsck="fsck --lost-found"
alias git-cat="git cat-file -p $1"

## VIM ##

alias vim=nvim


alias bfg="/usr/local/bin/bfg"

## VNC
#Start or stop the service with:
function startRealVNC
{
    sudo systemctl start vncserver-x11-serviced.service
}
function stopRealVNC
{
    sudo systemctl start vncserver-x11-serviced.service
}
#Mark or unmark the service to be started at boot time with:
#systemctl (enable|disable) vncserver-x11-serviced.service

#Installed systemd unit for VNC Server in Virtual Mode daemon
#Start or stop the service with:
#systemctl (start|stop) vncserver-virtuald.service
#Mark or unmark the service to be started at boot time with:
#systemctl (enable|disable) vncserver-virtuald.service

addPythonPath()
{
    P=${2:-$(pwd)} # The path
    N=$1 # The name
    echo "$N $P"
    # find directory
    PYTHON_SITE=$(python -m site --user-site)
    echo "Location = $PYTHON_SITE"
    ## create if it doesn't exist
    if [ -n $PYTHON_SITE ]
    then
        mkdir -p "$PYTHON_SITE"
        echo ${P} > "$PYTHON_SITE/${N}.pth"
        echo "Added ${P} to $PYTHON_SITE/${N}.pth"
    fi
    ## create new .pth file with our path
}


########### "FUCK" COMMAND TOOL #############
#eval $(thefuck --alias)

#alias imgoaljulia="cd ~/Projects/goal-code && julia --threads 16 --project=~/Projects/goal-code -e '@time using GoalFetchAnalysis'"
alias imgoaljulia="cd ~/Projects/goal-code && julia --threads 16 --project=~/Projects/goal-code -J ~/Projects/goal-code/GFA-dependencies-sysimage.so"
alias goaljulia="cd ~/Projects/goal-code && julia --threads 16 --project=~/Projects/goal-code -e 'using GoalFetchAnalysis'"
alias imjulia="cd ~/Projects/goal-code && julia --threads 16 -J ~/Projects/goal-code/GFA-dependencies-sysimage.so"
function plutogoaljulia()
{
    export USE_PLUTO=1 
    cd "/home/ryoung/Projects/goal-code" && \
    julia --threads 16 --project="/home/ryoung/Projects/goal-code" -J /home/ryoung/Projects/goal-code/GFA-dependencies-sysimage.so -e 'import Pluto; Pluto.run(sysimage=unsafe_string(Base.JLOptions().image_file))'
}
function compilejulia()
{
    #export USE_PLUTO=1 
    #cd "/home/ryoung/Projects/goal-code" && \
    julia --threads 16 -e 'quickactivate(expanduser("~/Projects/goal-code")); using GoalFetchAnalysis; import Precompile; Precompile.precompile_GFA_dependencies(); exit()'
}


alias wezterm='flatpak run org.wezfurlong.wezterm'
