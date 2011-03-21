dns_proxy
=========

dns_proxy is an Erlang DNS Proxy. At this point, it is functional ( by default running at port 6666 ), but there are many things to add/change ( see TODO )

Building
--------

To build dns_proxy rebar is required. If you don't already have it, you can get it from github.com/basho/rebar.git

### Downloading

Clone the git repository :
      
      $ git clone git://github.com/radubrumariu/dns_proxy.git

#### Building dns_proxy

     $ cd dns_proxy/
     $ rebar compile 
     Compiled src/dns_worker_srv.erl
     Compiled src/dns_worker_sup.erl
     ...
     $ rebar generate
     ==> rel (generate)
     
     You know have a self-contained dns_proxy in rel/dns_proxy

TODO
----

1. Allow to distribute the database
2. Keep records alive by looking up the expired ones
3. Allow configurable security zones
4. Profile the code
