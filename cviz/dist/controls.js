var dayRange = d3.range(0, 90, 7).map(function(d) {
    return new Date(2020, 3, 1+d);
});

const container = d3.select("div#date-slider");
const width = container.node().getBoundingClientRect().width - 60;
var sliderTime = d3
    .sliderBottom()
    .min(d3.min(dayRange))
    .max(d3.max(dayRange))
    .step(1000 * 60 * 60 * 24)
    .width(width - 60)
    .tickFormat(d3.timeFormat('%b %e'))
    .tickValues(dayRange)
    .default(d3.max(dayRange))
    .on('onchange', val => {
        d3.select('p#value-time').text(d3.timeFormat('%Y')(val));
    });


var gTime = container
    .append('svg')
    .attr('width',width)
    .attr('height', 100)
    .append('g')
    .attr('transform', 'translate(30,30)');

gTime.call(sliderTime);

