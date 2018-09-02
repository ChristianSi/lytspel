import assert from 'assert';
import {reverse} from '../src/lib/library';

describe("library", function () {

    it("reverse returns a string that appears to be red from behind", function () {
        let result = reverse('I am nuts');

        assert(result === 'stun ma I');
    });
});
