import { expect } from 'chai';

import { Point1 } from './nirum_fixture/fixture/foo';

class K {
    __nirum_serialize__(): any {
        return {};
    }
    static __nirum_deserialize__(value: any): K {
        return new K();
    }
}

describe('basic interfaces', function () {
    specify('', function () {
        const value = K.__nirum_deserialize__({});
        expect(value).instanceof(K);
        const serialized = value.__nirum_serialize__();
        expect(serialized).deep.eq({});
    });
});
