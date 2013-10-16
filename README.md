riak-c-client
=============


# Status

This is a **basho-labs** project, therefore it is NOT ready to use in a production environment. Once we feel the project has become stable, we'll move it to the [Basho](https://github.com/basho) Github org. The project will then be renamed to `riak-c-client`.


# Dependencies

* libevent
* protobuf
* protobuf-c
* pthreads
* log4c
* doxygen (if you are building docs)


# Building

### OSX Build

	brew install protobuf protobuf-c log4c scons libevent
	git clone git@github.com:/basho-labs/debris.git
	cd debris
	scons


### Ubuntu 13.04 build

```
sudo apt-get install scons libevent-dev protobuf-c-compiler dev-libprotobuf dev-libprotoc liblog4c-dev
 
wget https://protobuf-c.googlecode.com/files/protobuf-c-0.15.tar.gz
cd protobuf-c-0.15
./configure && make && sudo make install
cd ..
 
git clone git@github.com:/basho-labs/debris.git
cd debris
scons
```


### Building docs

To build documentation, you'll need `doxygen` installed. 

	scons docs

# Using

* TODO


# Contributing

1. Fork the debris repo
2. Clone the repo from your fork
3. Create a branch:
	* `git checkout -b my_branch`
4. git push origin my_branch
5. Submit a pull request

#License

http://www.apache.org/licenses/LICENSE-2.0.html

---

© 2013 Basho Technologies  
