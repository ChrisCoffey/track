import {TimeOfDay, ValidationFailure, parseTimeOfDay} from '../src/viz_types.ts';

import * as mocha from 'mocha';
import * as chai from 'chai';

const expect = chai.expect;
describe('TimeOfDay', () => {

    it("parses case insensitive", () => {
        expect(<TimeOfDay>parseTimeOfDay("latenight")).to.equal(LateNight);
        expect(<TimeOfDay>parseTimeOfDay("LateNight")).to.equal(LateNight);
        expect(<TimeOfDay>parseTimeOfDay("LaTENIght")).to.equal(LateNight);
    });

    it("parses the five terms", () => {
        expect(<TimeOfDay>parseTimeOfDay("latenight")).to.equal(LateNight);
        expect(<TimeOfDay>parseTimeOfDay("morning")).to.equal(Morning);
        expect(<TimeOfDay>parseTimeOfDay("midday")).to.equal(MidDay);
        expect(<TimeOfDay>parseTimeOfDay("afternoon")).to.equal(Afternoon);
        expect(<TimeOfDay>parseTimeOfDay("evening")).to.equal(Evening);
    })

    it("returns a validation error if it fails to parse", () => {
        expect(<ValidationFailure>parseTimeOfDay("foo").message).to.equal("foo is not a valid TimeOfDay"))
    });

    it("returns the term in the error ", () => {
    });
});
