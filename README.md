# weblarda3

Online interface for the [larda data cube](https://github.com/lacros-tropos/larda). 






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

## query strings

```
/?params=CLOUDNET|CLASS,CLOUDNET|Z&camp=lacros_cycare

/explorer/lacros_cycare?interval=1480910294.999886-1480916594.999886%2C3667.0879999999997-6875.4814&params=CLOUDNET|CLASS,CLOUDNET|Z,CLOUDNET|beta

/explorer/lacros_cycare?interval=20161205_0358-20161205_0543,3667.0879999999997-6875.4814&params=CLOUDNET|beta,CLOUDNET|Z,CLOUDNET|CLASS
```


### License
Copyright 2021, Martin Radenz
[MIT License](http://www.opensource.org/licenses/mit-license.php)
For details see the LICENSE file.
