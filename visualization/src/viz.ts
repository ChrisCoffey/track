import * as Reports from './viz_types.ts';
// Libraries
import * as d3 from 'd3';

let todReport: Reports.TimeOfDayReport;

function loadData(fileName: string): void {
    d3.json(fileName)
        .then( (rawJson: object) => {
            const items = rawJson['timeOfDayReport']
                .map(Reports.parseTODItem);
            todReport = new Reports.TimeOfDayReport(items);
        });
}

loadData('./junk.json');

d3.select('body')
    .selectAll('p')
    .data([1, 2, 3, 4, 5, 6])
    .enter()
    .append('p')
    .text((d) => {
        return 'Data element number: ' + d;
    });
