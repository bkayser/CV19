
d3.json("sample.json").then(data => {
    const pack = data => d3.pack()
        .size([width, height])
        .padding(3)
    (d3.hierarchy(data)
     .sum(d => d.value)
     .sort((a, b) => b.value - a.value))

    const width = 932
    const height = width
    const bgOpacity = 0.6   // Opacity of circles that are zoomed out
    
    //format = d3.format(",d")
    const root = pack(data);
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
          .style("margin", "0 -14px")
          .style("background", "white") //color(0))
          .style("cursor", "pointer")
          .on("click", () => zoom(root));

    const nodeContainer = svg.append("g")
          .selectAll("g")
          .data(root.descendants().slice(1))
          .join("g")

    const node = nodeContainer
          .append("circle")
          .attr("fill", d => d.children ? color(d.depth) : hotspotColor(d.data.growth))
          .attr("class", d => d.children ? "parent" : "leaf")
          .attr("pointer-events", d => d.depth > 1 ? "none" : null)
          .attr("fill-opacity", d => d.depth > 1 ? bgOpacity : 1)
          .on("mouseover", function(d){
              d3.select(this).attr("stroke", "#000");
              infoBar.node().textContent = `Cases: ${d.value}`
              const pos = this.getBoundingClientRect()
              infoBar.attr("x", pos.x + (pos.width/2) - width/2)
              infoBar.attr("y", pos.y + Math.max(40, (pos.height*2/3)) - height/2)
          })
          .on("mouseout", function(){
              d3.select(this).attr("stroke", null);
              infoBar.node().textContent = "";
          })
          .on("click", d => { d3.event.stopPropagation(); focus === d ? zoom(d.parent) : zoom(d) })
          .on("dblclick", () => zoom(root))

    const labelContainer = svg.append("g")
          .attr("class", "label")
          .attr("pointer-events", "none")
          .attr("text-anchor", "middle")

    const label = labelContainer
          .selectAll("text")
          .data(root.descendants())
          .join("text")
          .style("font-size", fontSize)
          .style("fill-opacity", d => d.parent === root ? 1 : 0)
          .style("display", d => d.parent === root ? "inline" : "none")
          .text(d => d.data.name);

    const infoBar = svg.append("text")
          .attr("id", "infobar")
          .attr("pointer-events", "none")
              .attr("text-anchor", "middle")



    zoomTo([root.x, root.y, root.r * 2]);
         
    function zoomTo(v) {
        const k = width / v[2];

        view = v;

        label.attr("transform", d => `translate(${(d.x - v[0]) * k},${(d.y - v[1]) * k})`);
        node.attr("transform", d => `translate(${(d.x - v[0]) * k},${(d.y - v[1]) * k})`);
        node.attr("r", d => d.r * k);
    }

    function zoom(d) {
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

        node
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
            .on("start", function(d) { if (d.parent === focus ||
                                           (d === focus && !d.children) ||
                                           (!focus.children && focus.parent === d.parent)) this.style.display = "inline"; })
            .on("end", function(d) { if (d.parent !== focus &&
                                         (d !== focus || d.children) &&
                                         (focus.children || focus.parent !== d.parent)) this.style.display = "none"; });
//            .on("end", function(d) { if (d.parent !== focus) this.style.display = "none"; });
    }

});


