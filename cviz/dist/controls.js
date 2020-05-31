
const container = d3.select("div#date-slider");
const width = container.node().getBoundingClientRect().width - 60;


var gTime = container
    .append('svg')
    .attr('width',width)
    .attr('height', 100)
    .append('g')
    .attr('transform', 'translate(30,30)');

function setupSlider(data, onchange) {
    const dateParser = d3.timeParse("%Y-%m-%d");
    
    const dates = Object.keys(data[0])
          .filter( d => d.startsWith("Cases_") )
          .map( d => dateParser(d.match("202\\d-..-..")[0]) )
          .sort()

    const start = dates[0];
    const finish = dates[dates.length - 1];


    var sliderTime = d3
        .sliderBottom()
        .min(start) //d3.min(dayRange))
        .max(finish) //d3.max(dayRange))
        .step(1000 * 60 * 60 * 24)
        .width(width - 60)
        .tickFormat(d3.timeFormat('%b %e'))
        .default(finish)
        .on('onchange', val => {
            onchange(val);
           });
    gTime.call(sliderTime);
}

