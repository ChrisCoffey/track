import {TimeOfDay, ValidationFailure, parseTimeOfDay} from '../src/viz_types';

import {assert, expect} from 'chai';
import 'mocha';

describe('TimeOfDay', () => {
    it('parses case insensitive', () => {
        expect(<TimeOfDay>parseTimeOfDay('latenight')).to.equal(TimeOfDay.LateNight);
        expect(<TimeOfDay>parseTimeOfDay('LateNight')).to.equal(TimeOfDay.LateNight);
        expect(<TimeOfDay>parseTimeOfDay('LaTENIght')).to.equal(TimeOfDay.LateNight);
    });

    it('parses the five terms', () => {
        expect(<TimeOfDay>parseTimeOfDay('latenight')).to.equal(TimeOfDay.LateNight);
        expect(<TimeOfDay>parseTimeOfDay('morning')).to.equal(TimeOfDay.Morning);
        expect(<TimeOfDay>parseTimeOfDay('midday')).to.equal(TimeOfDay.MidDay);
        expect(<TimeOfDay>parseTimeOfDay('afternoon')).to.equal(TimeOfDay.Afternoon);
        expect(<TimeOfDay>parseTimeOfDay('evening')).to.equal(TimeOfDay.Evening);
    });

    it('returns a validation error if it fails to parse', () => {
        expect((<ValidationFailure>parseTimeOfDay('foo')).message).to.equal('foo is not a valid TimeOfDay.');
    });
});

describe('parseTODItem', () => {
    it('properly parses valid two element lists', () => {
        assert.fail();
    });

    it('can accept larger lists', () => {
        assert.fail();
    });

    it('returns a validation faliure if the tod item is invalid', () => {
        assert.fail();
    });

    it('returns a validation faliure if number is not a Natural', () => {
        assert.fail();
    });
});
