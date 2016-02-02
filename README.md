exoweb_js
=========

Under development.
Might replace https://tonyrog/exoweb

### Dependencies

To build exoweb you will need a working installation of Erlang R15B (or
later).<br/>
Information on building and installing [Erlang/OTP](http://www.erlang.org)
can be found [here](https://github.com/erlang/otp/wiki/Installation)
([more info](https://github.com/erlang/otp/blob/master/INSTALL.md)).

exoweb is built using rebar that can be found [here](https://github.com/rebar/rebar), with building instructions [here](https://github.com/rebar/rebar/wiki/Building-rebar). rebar's dynamic configuration mechanism, described [here](https://github.com/rebar/rebar/wiki/Dynamic-configuration), is used so the environment variable `REBAR_DEPS` should be set to the directory where your erlang applications are located.

exoweb also requires the following erlang applications to be installed:
<ul>
<li>wse - https://github.com/tonyrog/wse</li>
<li>yaws - https://github.com/klacke/yaws</li>
<li>exodmapi - https://github.com/Feuerlabs/exodmapi</li>
<li>exo - https://github.com/Feuerlabs/exo</li>
<li>sync - https://github.com/rustyio/sync</li>
<li>gettext - https://github.com/Feuerlabs/gettext/li>
<li>gen_smtp - https://github.com/Vagabond/gen_smtp</li>
<li>lager - https://github.com/basho/lager</li>
</ul>

If you want to run in debug mode add:
<ul>
<li>ale - https://github.com/tonyrog/ale</li>
</ul>

exoweb is built on the [angularjs](https://angularjs.org/) frameworks, see Installation below.

You need a web browser to access the exoweb site.

### Download

Clone the repository in a suitable location:

```sh
$ git clone git://github.com/malotte/exoweb_js.git
```

### Installation
Apart from the above stated erlang applications you also need to install a number of angularjs components. This is done using [bower](http://bower.io/) which must be installed first:

```sh
$ cd exoweb/www
$ npm install bower
```

The needed components are specified in a bower.json-file and are installed by:

```sh
$ cd exoweb/www
$ bower install
```


### Configuration
#### Concepts

exoweb is a web service, using inets, connecting to exodm. It must thus be configured to find a suitable exodm server and a suitable http port to listen must be choosen.<br/>
exoweb uses gen_smtp for sending email and thus the smtp parameters must also be correctly set up.<br/>

#### Files

Arguments to all applicable erlang applications are specified in an erlang configuration file.<br/>
An example can be found in ["sys.config"](https://github.com/malotte/exoweb_js/raw/master/sys.config).<br/>
Good practise is to copy this to a local .config-file and use that file when starting the application.

### Known limitations

<ul>
<li>Only runs on Linux/Unix (uses os commands)</li>
<li>Max 100 devices.</li>
<li>Exactly one role/user.</li>
<li>Only predefined roles.</li>
<li>Unstable :-)</li>
</ul>

### Build

Rebar will compile all needed dependencies.<br/>
Compile:

```sh
$ cd exoweb
$ rebar compile
...
==> exoweb (compile)
```

### Run

There is a quick way to run the application for testing:

```sh
$ erl -config <your local config file> -pa <path>/exoweb/ebin
>exoweb:start().
```
Note: The config file is specified without its .config-extension.<br/>
Note: Full path is needed.<br/>
(Instead of specifing the path to the ebin directory you can set the environment variable `ERL_LIBS`.)

It is possible to run a debug version using:

```sh
>exoweb:start_dbg().
```
Note: Requires ale, see above.<br/>


Stop:

```sh
>halt().
```


### Documentation

exoweb is documented using edoc. To generate the documentation do:

```sh
$ cd exoweb
$ rebar doc
```
The result is a collection of html-documents under ```exoweb/doc```.


