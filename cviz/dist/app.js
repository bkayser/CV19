
d3.csv("covid.csv").then(data => {
    
    let valueColumnName = "Cases";
    const width = 960;
    const height = width

    const hierarchy = d3.stratify()
          .id(d => d.id)
          .parentId(d => d.parent)(data);
    

    const pack = d3.pack()
          .size([width, height])
          .padding(4)
          .radius(d => Math.sqrt(d.value))

    const bgOpacity = 0.6   // Opacity of circles that are zoomed out
    
    format = d3.format(",d")
    const root = pack(hierarchy
                      .sum(d => +d[valueColumnName])
                      .sort((a, b) => +b[valueColumnName] - +a[valueColumnName]));

    const color = d3.scaleLinear()
          .domain([0, root.height])
          .range(["hsl(152,80%,80%)", "hsl(228,30%,70%)"])
          .interpolate(d3.interpolateHcl)

    const hotspotColor = d3.scaleLinear()
          .domain([0, 0.1])
          .range(["hsl(94,34%,40%)", "hsl(360,100%,40%)"])
          .interpolate(d3.interpolateHcl)


    let focus = root;
    let view;
    const fontSize = d => `${ (d.parent ? d.r * width / d.parent.r : d.r * width) / 6 }px`;

    const svg = d3.select("svg#top")
          .attr("viewBox", `-${width / 2} -${height / 2} ${width} ${height}`)
          .style("display", "block")
          .style("margin", "0 0px")
          .style("background", "white") //color(0))
          .style("cursor", "pointer")
          .on("click", () => zoom(root));

    const circlesContainer = svg.append("g");

    const circles = drawCircles();
    function drawCircles() {
        return circlesContainer
            .selectAll("circle")
            .data(root.descendants().slice(1))
            .join(enter => enter
                  .append("circle")
                  .attr("id", d => d.id)
                  .attr("class", d => d.children ? "parent" : "leaf")
                  .attr("fill-opacity", d => d.depth > 1 ? bgOpacity : 1)
                  .attr("pointer-events", d => d.depth > 1 ? "none" : null)
                  .on("mouseover", function(d){
                      // Inhibit info bar if focus has children and focus is the highlight request
                      // or if we are in the middle of zooming.
                      if (inZoom || (focus === d && d.children)) return;

                      d3.select(this).attr("stroke", "#000");
                      d3.select(`text#${this.id}`).classed("highlighted", true);
                      showInfo(d, this);
                  })
                  .on("mouseout", function(){
                      d3.select(this).attr("stroke", null);
                      hideInfo();
                      d3.select(`text#${this.id}`).classed("highlighted", false);
                  })
                  .on("click", d => {
                      hideInfo();
                      d3.event.stopPropagation(); focus === d ? zoom(d.parent) : zoom(d)
                  })
                  .on("dblclick", () => zoom(root)))
            .attr("fill", d => d.children ? color(d.depth) : hotspotColor(d.data.Growth))
        
    }

    const labelsContainer = 
        svg.append("g")
          .attr("class", "label")
          .attr("pointer-events", "none")

    const label = drawLabels();
    
    function drawLabels() {
        return labelsContainer
          .selectAll("text")
          .data(root.descendants())
            .join(enter => enter
                  .append("text")
                  .attr("id", d => d.id)
                  .attr("text-anchor", "middle")
                  .style("font-size", fontSize)
                  .style("fill-opacity", d => d.parent === root ? 1 : 0)
                  .style("display", d => d.parent === root ? "inline" : "none"))
            .text(d => d.data.name)
    }

    const infoBar = svg.append("text")
          .attr("id", "infobar")
          .attr("pointer-events", "none")
          .attr("text-anchor", "middle")

    // Sets the transforms for the initial setup.
    zoomTo([root.x, root.y, root.r * 2]);

    const parentOffset = svg.node().getBoundingClientRect();
    function showInfo(node, circle) {
        if (!circle) return;
        infoBar.node().textContent = `Cases: ${format(node.value)}`;
        const pos = circle.getBoundingClientRect();
        infoBar.attr("x", pos.x + pos.width/2 - width/2 - parentOffset.x) 
        infoBar.attr("y", pos.y + pos.height/2 + 30 - height/2 - parentOffset.y)
        infoBar.style("display", "inline");
    }
    function hideInfo() {
        infoBar.style("display", "none");
    }
    
    function zoomTo(v) {
        const k = width / v[2];

        view = v;
        label.attr("transform", d => `translate(${(d.x - v[0]) * k},${(d.y - v[1]) * k})`);
        circles.attr("transform", d => `translate(${(d.x - v[0]) * k},${(d.y - v[1]) * k})`);
        
        circles.attr("r", d => d.r * k);
    }

    // Track zooming state so we inhibit highlighting during zooms
    let inZoom = false;
    
    function zoom(d) {
        inZoom = true;
        const zoomIn = d.parent === focus;
        focus = d;
        const fadedDepth = Math.max(0, d.depth) + 1
        const transition = svg.transition()
              .duration(d3.event.altKey ? 7500 : 750)

        const opacityTransition = transition.ease(zoomIn ? d3.easeCubicOut : d3.easeCubicIn);
        
        const zoomTransition = transition
              .tween("zoom", d => {
                  const i = d3.interpolateZoom(view, [focus.x, focus.y, focus.r * 2]);
                  return t => zoomTo(i(t));
              });

        circles
            .attr("pointer-events", d => d.depth > fadedDepth ? "none" : null)
            .transition(opacityTransition)
            .attr("fill-opacity", d => d.depth > fadedDepth ? bgOpacity : 1)
        
        label
            .filter(function(d) { return d.parent === focus || this.style.display === "inline"; })
            .transition(zoomTransition)
            .style("fill-opacity", d => (d.parent !== focus && (d !== focus || d.children)) ? 0 : 1)
            .style("font-size", d => {
                if (focus === d && d.height == 0) {
                     // zoomed down to the count y
                    return `${Math.min(80, width/12)}px`;
                } else {
                    return fontSize(d);
                }
            })
            .on("start", function(d) {
                if (d.parent === focus ||
                    (d === focus && !d.children) ||
                    (!focus.children && focus.parent === d.parent)) this.style.display = "inline";
            })
            .on("end", function(d) {
                inZoom = false;
                if (d.parent !== focus &&
                    (d !== focus || d.children) &&
                    (focus.children || focus.parent !== d.parent)) this.style.display = "none";
                showInfo(focus, svg.select(`circle#${focus.id}`).node());
            });
    }


    const dateColFormat = d3.timeFormat("%Y-%m-%d");

    setupSlider(data, d => {
        console.log(`Slider: ${d}`);
        valueColumnName = `Cases_${dateColFormat(d)}`;
        pack(hierarchy
             .sum(d => +d[valueColumnName])
             .sort((a, b) => +b[valueColumnName] - +a[valueColumnName]));
        d3.select('#value-time')
            .text(+focus.value);
        zoom(focus)
        circles
            .attr("fill", d => d.children ? color(d.depth) : hotspotColor(d.data[`Growth_${dateColFormat(d)}`]))
    });
    
});


