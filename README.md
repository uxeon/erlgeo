erlgeo
===

An Erlang application/library for geocoding.

Warning
---
This is an early release, API may and will change until we reach 1.0.0.
For more information see Roadmap at the end of this document.

Features
---

* Partial support of [OpenCage Geocoder](https://geocoder.opencagedata.com/)

Quick start
----

```
# clone
$ git clone git://github.com/uxeon/erlgeo.git
$ cd erlgeo

# Configure
# Put your API key into config/sys.config
# (or check above file to add proper config to your app`s sys.config)

# Testing in Erlang shell
$ make start
1> application:ensure_all_started(erlgeo).
2> erlgeo:reverse(20.0,50.0).

```

Options
----

You can use ```reverse/3``` to provide options list. Options names are consistent with OpenCage API options:

* ```erlgeo:reverse(Lon,Lat,[no_annotations])```
* ```erlgeo:reverse(Lon,Lat,[no_dedupe])```
* ```erlgeo:reverse(Lon,Lat,[no_record])```
* ```erlgeo:reverse(Lon,Lat,[{language,"pl"}])``` or ```erlgeo:reverse(Lon,Lat,[{language,<<"pl">>}])```
* ```erlgeo:reverse(Lon,Lat,[{language,"pl"}])``` or ```erlgeo:reverse(Lon,Lat,[{language,<<"pl">>}])```
* ```erlgeo:reverse(Lon,Lat,[{min_confidence,10}])```
* ```erlgeo:reverse(Lon,Lat,[{limit,5}])```

See [OpenCage API documentation](https://geocoder.opencagedata.com/api) for more information.

Dependencies
---

This library requires **jsone** library as specified in ```rebar.config```. It could be independent by using **xmerl** instead. However UXEON already uses **jsone** in other applications so we do not consider switching to xmerl an important task. Current **jsone** dependent version is probably faster than theoretical **xmerl** version.

Roadmap
---

* Full OpenCage API support
* Caching support
