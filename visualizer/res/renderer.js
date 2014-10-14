function createRenderer(svg, popupRenderer) {

	var txt = svg.createText();
	
	function fromtable(item, k) {
		return ((item.table || []).filter(function(pair) {
			return (pair[0] == k);
		})[0] || [])[1];
	}

	function text(item) {
		return fromtable(item, "time to puck stat") ? (fromtable(item, "time to puck stat") + " " + fromtable(item, "time to puck")) : "";
	}

    function drawField() {
        svg.polygon([[65,150],[1135,150],[1135,360],[1200,360],[1200,560],[1135,560],[1135,770],[65,770],[65,560],[0,560],[0,360],[65,360]], {"class": "field"})
    }

    function renderItem(item) {

        switch (item.type) {
			
            case "circle":
                var c = svg.circle(item.x, item.y, item.radius, {"class": item.klass});
                if (item.vectors) {
                    item.vectors.forEach(function(vec) {
                        var vec = renderItem($.extend({}, item, vec, {"type": "vector", "klass": item.klass + " " + vec.klass + " " + vec.kls}));
                    })
                }
				var t = text(item);
				if (t && t.length) {
					svg.text(item.x + item.radius, item.y, t);
				}
                return c;
                break;
            case "vector":
				if (item.clicked) {
					item.dx*=50;
					item.dy*=50;
				}
                if (item.name == "vel") {
                    item.dx*=5;
                    item.dy*=5;
                }
                if (item.name == "move") {
                    item.dx*=500;
                    item.dy*=500;
                }
                var ms = 5;
                if (item.dx*item.dx + item.dy*item.dy < ms*ms) {
                    var mfx = ms / Math.abs(item.dx);
                    var mfy = ms / Math.abs(item.dy);
                    var mf = !isFinite(mfx) ? mfy : (!isFinite(mfy) ? mfx : null);
                    if (mf == null) mf = Math.min(mfx, mfy);
                    if (isFinite(mf)) {
                        //console.log(item.dx, item.dy, mf, item.dx * mf, item.dy * mf);
                        item.dx *= mf;
                        item.dy *= mf;
                    }
                }
                return svg.line(item.x, item.y, item.x+item.dx, item.y+item.dy, {"class": item.klass});
                break;
            case "area":
				var last;
				if (item.name == 'pass') {
					last = item.points[item.points.length-2];
					return svg.circle(last[0], last[1], 10, {"class": item.klass});
				}
                item.points.forEach(function(p) {
                    last = svg.circle(p[0], p[1], 2, {"class": item.klass});
                });
                return last;
            case "point":
                return svg.circle(item.x, item.y, 5, {"class": item.klass});
                break;
        }
    }

    function renderItemAndPopup(item) {
        var el = renderItem($.extend({},item));
        if (!el) return;
		$(el).click(function() {
			item.clicked = !item.clicked;
			$(el).remove();
			renderItemAndPopup(item);
		});
        $(el).mousemove(function(e) {
            popupRenderer.showAt(item, e.pageX, e.pageY);
        }).mouseleave(function(e) {
            popupRenderer.hide();
        });
        if (item.tag) {
            var tag = "tag_" + item.tag;
            $(el).addClass(tag);
            $(el).mouseover(function() {
                $("." + tag).addClass("highlighted");
            }).mouseout(function() {
                $("." + tag).removeClass("highlighted");

            })
        }
    }

    function isBg(item) {
        return item.type == 'area' && item.name != 'pass';
    }

    function isFg(item) {
        return !isBg(item);
    }

    return {
        render: function(items, bg) {
            svg.clear();
            drawField();
            if (bg) {
                bg.items.filter(isBg).forEach(renderItemAndPopup)
            }
            items.filter(isFg).forEach(renderItemAndPopup);
        }
    }
}

function createPopup(elem, itemToTable) {
    return {
        showAt: function(item, left, top) {
            elem.find(".content").html(itemToTable(item));
            elem.css({top: top+2, left: left+2}).show();
        },

        hide: function() {
            elem.hide();
        }
    }
}


function itemToPairs(item) {
    return [
        ["Name", item.name],
        ["X", item.x],
        ["Y", item.y],
        ["Velocity", item.vx ? Math.sqrt(item.vx*item.vx + item.vy*item.vy).toFixed(2) : undefined]
    ].concat(item.table || [])
}

function itemToTable(item) {
    var res = "<table>";
    function addProp(name, value) {
        if (value) {
            res += "<tr><th>" + name + "</th><td>" + value + "</td></tr>";
		}
    }

    itemToPairs(item).forEach(function(p) {
        addProp(p[0], p[1]);
    });




    return res + "</table>"
}