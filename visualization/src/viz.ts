import * as d3 from 'd3'

d3.select('body')
    .selectAll('p')
    .data([1,2,3,4,5,6])
    .enter()
    .append('p')
    .text(d => {
        return "Data element number: " + d;
    });
