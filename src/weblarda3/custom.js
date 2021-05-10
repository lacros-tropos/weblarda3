// calculator.js

var custom = {
    add: function (a, b) {
        console.log('custom adding function')
        return a + b;
    },
    subtract: function (a, b) {
        return a - b;
    },
    makeempty: function (shape) {
        return arr = Array(shape[0]).fill(null).map(()=>Array(shape[1]).fill(null))
    },
    range: function (start, end) {
        var ans = [];
        for (let i = start; i < end; i++) {
            ans.push(i);
        }
        return ans;
    },
    forcexy: function(hierarched_data, d3) {
        //console.log('forcexyboilerplate')
        let force = d3.forceSimulation(hierarched_data.descendants())
            .force('collide', d3.forceCollide((d) => {return 2.6}).strength(1))
            //.force('x', d3.forceX().strength(0.1).x((d) => {return d.x}))
            .stop()
        //for (var i=0, n=Math.ceil(Math.log(force.alphaMin()) / Math.log(1- force.alphaDecay())); i<n, ++i;) {
        for (var i = 0; i < 1000; i++) {
            force.tick();
        }
        return hierarched_data
    },
    treeto2d: function(alltrees, selector, param) {
        // alltrees: dict with timestamps, ranges, trees
        // selector: dict with [node_no param]
        let xdim = alltrees.ts.length
        let ydim = alltrees.rg.length
        let arr = Array(xdim).fill(null).map(()=>Array(ydim).fill(null))
        for (let ix = 0; ix < xdim; ix++) {
            for (let iy = 0; iy < ydim; iy++) { 
                //let key = `${ix.toString().padStart(2, "0")}.${iy.toString().padStart(2, "0")}`
                let sel = selector(ix, iy)
                //if (ix < 10 && iy< 5) {
                //    console.log(sel)
                //}
                let val = 99999
                if (param != "no_nodes") {
                    if (alltrees["var"][ix][iy][sel] != undefined) {
                        val = alltrees["var"][ix][iy][sel][param]
                    }
                } else {
                    val = Object.keys(alltrees["var"][ix][iy]).length
                }
                //console.log(key, sel, val)
                arr[ix][iy] = val
            }
        }
        //console.log("resulting array ", arr)
        return {"var": arr, "ts": alltrees.ts, "rg": alltrees.rg}
    },
    pull_loc_in: function(alltrees) {
        // for performance reasons this has to be done directly on the js object
        //(defn pull-location-in "pipe the key from the outer map into every node within the list" 
        //   [trees]
        //   (reduce-kv (fn [m k v] (assoc m k (mapv #(assoc %1 "loc" k) v)))
        //            {} trees))
        let xdim = alltrees.ts.length
        let ydim = alltrees.rg.length
        for (let ix = 0; ix < xdim; ix++) {
            for (let iy = 0; iy < ydim; iy++) { 
                if (alltrees["var"][ix][iy].length != 0) {
                    Object.keys(alltrees["var"][ix][iy]).forEach(function(e) {alltrees["var"][ix][iy][e]["loc"] = [ix, iy]});
                    //alltrees["var"][ix][iy].forEach(function(e) {e["loc"]=key});
                }
            }
        }
        return alltrees["var"]
    },
    copy_string: function (str) {
        // Create new element
        var el = document.createElement('textarea');
        // Set value (string to be copied)
        el.value = str;
        // Set non-editable to avoid focus and move outside of view
        el.setAttribute('readonly', '');
        el.style = {position: 'absolute', left: '-9999px'};
        document.body.appendChild(el);
        // Select text inside element
        el.select();
        // Copy text to clipboard
        document.execCommand('copy');
        // Remove temporary element
        document.body.removeChild(el);
    },
    merge_mask_1d: function (x, y, ymask) {
        let arr = []
        
        for (let ix = 0; ix < x.length; ix++) {
            //console.log(x[ix], y[ix], ymask[ix])
            if (!ymask[ix] && !isNaN(y[ix])) {
                arr.push({'t': x[ix], 'v': y[ix]})
            }
        }
        return arr
    },
    paint_color_slow: function(context, data2d, plotProps, color, timeMapping, torgb) {
        let image = context.createImageData(plotProps.imageSize.w, plotProps.imageSize.h)

        torgb = function(s) {
            return s.slice(4,-1).split(",").map(Number)
        }

        for (let y = plotProps.imageSize.h, p = -1; y > 0; --y){
            for (x = 0; x < plotProps.imageSize.w; ++x) {
                let relx = x / plotProps.imageSize.w
                let rely = y / plotProps.imageSize.h
                //let iX = Math.round(relx * (data2d.timestamps.length - 1))
                let iX = timeMapping[x]
                let iY = Math.round((1 - rely) * (data2d.rg.length - 1))
                
                let value = data2d.data[iX][iY]
                let c = torgb('rgb(255, 255, 255)')
                if (value <= 99990) {
                    c = torgb(color(value))
                }
                // image.data[++p] = c.r
                // image.data[++p] = c.g
                // image.data[++p] = c.b
                image.data[++p] = c[0]
                image.data[++p] = c[1]
                image.data[++p] = c[2]
                image.data[++p] = 255
            }
        }
        context.putImageData(image, 50, 5)
    },
    paint_color: function(context, data2d, plotProps, color, timeMapping, torgb) {
        let image = context.createImageData(plotProps.imageSize.w, plotProps.imageSize.h)

        torgb = function(s) {
            return s.slice(4,-1).split(",").map(Number)
        }

        let expected_dt = 0
        for (let ix = 0; ix < 60; ix++) {
            let dt = data2d.ts[ix+1] - data2d.ts[ix]
            if (dt > expected_dt) {
                expected_dt = dt
            }
        }
        let jumps = []
        for (let ix = 0; ix < data2d.ts.length; ix++) {
            if (data2d.ts[ix+1] - data2d.ts[ix] > (1.5*expected_dt)) {
                jumps.push(ix)
                jumps.push(ix+1)
            }
        }
        console.log("jumps larger ", expected_dt, " sec ", jumps)

        values_arr = color.domain()
        color_arr = color.domain().map(color).map(torgb)

        find_closest = function(arr, target) {
            if (!(arr) || arr.length == 0)
                return null;
            if (arr.length == 1)
                return 0;
        
            for (var i=1; i<arr.length; i++) {
                // As soon as a number bigger than target is found, return the previous or current
                // number depending on which has smaller difference to the target.
                if (arr[i] > target) {
                    var p = arr[i-1];
                    var c = arr[i]
                    return Math.abs( p-target ) < Math.abs( c-target ) ? i-1 : i;
                }
            }
            // No number in array is bigger so return the last.
            return arr.length-1;
        }

        // console.log(torgb(color(-30)))
        // console.log(find_closest(values_arr, -30))
        // console.log(color_arr[find_closest(values_arr, -30)])
        // console.log(torgb(color(-20)))
        // console.log(find_closest(values_arr, -20))
        // console.log(color_arr[find_closest(values_arr, -20)])
        // console.log(torgb(color(-10)))
        // console.log(find_closest(values_arr, -10))
        // console.log(color_arr[find_closest(values_arr, -10)])
        //console.log("values", values_arr, color_arr)
        converter = function(x) {return x}
        if (data2d.plot_varconverter == "log") {
            console.log("js: set converter to log")
            converter = function(x) {return Math.log10(x)}
        } else if (data2d.plot_varconverter == "dB") {
            console.log("js: set converter to dB")
            converter = function(x) {return 10*Math.log10(x)}
        }

        //console.log("example", data2d.var[100][100], converter(data2d.var[100][100]))
        //console.log("time mapping ", timeMapping)

        for (let y = plotProps.imageSize.h, p = -1; y > 0; --y){
            for (x = 0; x < plotProps.imageSize.w; ++x) {
                //let relx = x / plotProps.imageSize.w
                let rely = y / plotProps.imageSize.h
                //let iX_true = Math.round(relx * (data2d.ts.length - 1))
                let iX = timeMapping[x]
                let iY = Math.round(rely * (data2d.rg.length - 1))
                
                let value = converter(data2d.var[iX][iY])
                let mask = data2d.mask[iX][iY]
                //let c = torgb('rgb(255, 255, 255)')
                let c = [255, 255, 255]
                if (jumps.includes(iX)) {
                    //console.log("gap detected", iX)
                    c = [192, 192, 192]
                } else {
                    //if (value > -999) {
                    if (!mask && !isNaN(value)) {
                        //c = torgb(color(value))
                        c = color_arr[find_closest(values_arr, value)]
                    }
                }
                // image.data[++p] = c.r
                // image.data[++p] = c.g
                // image.data[++p] = c.b
                image.data[++p] = c[0]
                image.data[++p] = c[1]
                image.data[++p] = c[2]
                image.data[++p] = 255
            }
        }
        context.putImageData(image, 50, 5)
    }
};

//module.exports = custom;