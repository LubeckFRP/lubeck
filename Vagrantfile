$script = <<SCRIPT

set -e

# Initial APT package installation

sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 575159689BEFB442

sed -i -e 's/us.archive.ubuntu.com/gb.archive.ubuntu.com/g' /etc/apt/sources.list
sed -i -e '/trusty multiverse/s/^# //' -e '/trusty-updates multiverse/s/^# //' /etc/apt/sources.list
echo 'deb http://download.fpcomplete.com/ubuntu trusty main'|sudo tee /etc/apt/sources.list.d/fpco.list

sudo apt-get install -y software-properties-common
sudo add-apt-repository -y ppa:hvr/ghc
wget -qO- https://deb.nodesource.com/gpgkey/nodesource.gpg.key | apt-key add -

echo 'deb https://deb.nodesource.com/node_0.10 trusty main' > /etc/apt/sources.list.d/nodesource.list
echo 'deb-src https://deb.nodesource.com/node_0.10 trusty main' >> /etc/apt/sources.list.d/nodesource.list

apt-get update

apt-get install -y cabal-install-1.22 ghc-7.10.2 stack zile libtinfo-dev nodejs alex-3.1.4 happy-1.19.5 dh-autoreconf zopfli

npm install closurecompiler -g

cat >> /home/vagrant/.bashrc <<EOF
export PATH="/home/vagrant/.cabal/bin:/home/vagrant/.local/bin:/opt/happy/1.19.5/bin:/opt/alex/3.1.4/bin:/opt/cabal/1.22/bin:/opt/ghc/7.10.2/bin:\$PATH"

EOF

su vagrant -c 'cd lubeck && bash setup_ghcjs.sh'

SCRIPT

Vagrant.configure("2") do |config|
  config.vm.box = "trusty-server-cloudimg-amd64-vagrant-disk1.box"
  config.vm.box_url = "https://cloud-images.ubuntu.com/vagrant/trusty/current/trusty-server-cloudimg-amd64-vagrant-disk1.box"

  config.vm.synced_folder ".", "/home/vagrant/lubeck"
  config.vm.synced_folder "../virtual-dom", "/home/vagrant/virtual-dom"
  config.vm.synced_folder "../ghcjs-ffiqq", "/home/vagrant/ghcjs-ffiqq"


  config.vm.provider :virtualbox do |vb|
    vb.gui = false
    vb.customize ["modifyvm", :id, "--memory", "4096"]
    vb.customize ["modifyvm", :id, "--cpus", "2"]
  end

  config.vm.provision "shell", inline: $script
end
