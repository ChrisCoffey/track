import * as Reports from './viz_types.ts'
// Libraries
import * as d3 from 'd3'
import * as d3Fetch from 'd3-fetch'

let todReport : Reports.TimeOfDayReport;

function loadData(fileName : string) : void {
    console.log("Extracting " + fileName);
    d3.json(fileName)
        .then( (rawJson: object) => {
            console.log(rawJson);
            const items = rawJson['timeOfDayReport'].map((rawItem: [string, number]) => {
                const res = Reports.parseTimeOfDay(rawItem[0]);
                if(<Reports.TimeOfDay>res){
                    return new Reports.TimeOfDayItem(<Reports.TimeOfDay>res,
                                            new Reports.Natural(rawItem[1]));
                } else {
                    throw new Error(res.toString());
                }
            })
            todReport = new Reports.TimeOfDayReport(items);
        })
}

loadData('./junk.json');

d3.select('body')
    .selectAll('p')
    .data([1,2,3,4,5,6])
    .enter()
    .append('p')
    .text(d => {
        return "Data element number: " + d;
    });
