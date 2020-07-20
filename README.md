# weblarda3

Online interface for the larda library. 


## Production Build

```
lein clean
lein cljsbuild once min

lein cljsbuild once min_explorer
```

Deploy to `larda/http_server/public` directory of pyLARDA


### Development mode

```
lein clean
lein figwheel dev

lein figwheel dev_explorer
```

Figwheel will automatically push cljs changes to the browser.

Wait a bit, then browse to [http://localhost:3449](http://localhost:3449).



### License
Copyright 2020, Martin Radenz
[MIT License](http://www.opensource.org/licenses/mit-license.php)
For details see the LICENSE file.
