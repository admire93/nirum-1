import tape = require('tape-async');


class K {
    __nirum_serialize__(): any {
        return {};
    }
    static __nirum_deserialize__(value: any): K {
        return new K();
    }
}


tape('basic interfaces', async t => {
    const value = K.__nirum_deserialize__({});
    t.assert(value instanceof K);
    const serialized = value.__nirum_serialize__();
    t.deepEquals(serialized, {});
});
