#!/bin/bash

# do not run

basedir=`dirname $0`
basedir=`readlink -f $basedir/../`
cd $basedir || exit 1

version=`head -n1 VERSION`
name=gpxer
tar=$name-$version.tar.gz
tar_dst=$HOME/htdocs/src/$name
chroot_dir=/home/komar/chroot/wheezy-x86
chroot_dist_dir=/home/komar/
user=komar
static=$name-bin-static-x86-$version

function release_sources {
  git archive --format tar.gz --prefix $name-$version/ HEAD > $tar || exit 1
  cp $tar $tar_dst || exit 1
  make -s clean || exit 1
}

function release_build {
  cd $chroot_dist_dir/$name || exit 1
  make -s clean || exit 1

  make -s -j4 bin/${name}.static || exit 1
  mkdir -p $static
  mkdir -p $static/bin
  strip --strip-unneeded bin/${name}.static || exit 1
  mv bin/${name}.static $static/bin || exit 1
  cp -r share $static
  tar -cf $static.tar.gz $static || exit 1
  make -s clean || exit 1
  rm -r $static

  make -s clean || exit 1

  exit
}

case "$1" in
  sources) release_sources;;
  build-root) su -c "$0 build" $user;;
  build) release_build;;
  *) $0 sources || exit 1
    rm -rf $chroot_dir/$chroot_dist_dir/$name || exit 1
    cd $chroot_dir/$chroot_dist_dir || exit 1
    git clone $basedir || exit 1
    cd $basedir || exit 1
    chmod +x $chroot_dir/$chroot_dist_dir/$name/devel/release.bash || exit 1
    sudo chroot $chroot_dir $chroot_dist_dir/$name/devel/release.bash build-root;
    mkdir -p $tar_dst/../../builds/$name/;
    cp $chroot_dir/$chroot_dist_dir/$name/$static.tar.gz $tar_dst/../../builds/$name/;;
esac

