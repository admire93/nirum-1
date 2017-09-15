{-# LANGUAGE QuasiQuotes #-}
module Nirum.Targets.TypeScript.Runtime ( runtimeModuleContent
                                        ) where
import Data.Text.Lazy.Builder ( Builder )
import Text.InterpolatedString.Perl6 ( q )


runtimeModuleContent :: Builder
runtimeModuleContent = [q|
export class NirumError extends Error {
    constructor(message?: string) {
        super(message);
        Object.setPrototypeOf(this, NirumError.prototype);
    }
}

export class DeserializeError extends NirumError { }

export function deserializeNumber(value: any): number {
    if (typeof value === 'number') {
        throw new DeserializeError();
    }
    return value;
}

export function deserializeString(value: any): number {
    if (typeof value === 'string') {
        throw new DeserializeError();
    }
    return value;
}
|]
