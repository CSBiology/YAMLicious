import { printf, toConsole } from "fable-library/String.js";
import { value } from "fable-library/Option.js";
import { disposeSafe, getEnumerator } from "fable-library/Util.js";
export class Main {
    constructor() {
    }
    static hello(name, from) {
        if (from == null) {
            toConsole(printf("Hello, %s!"))(name);
        }
        else {
            const f = value(from);
            toConsole(printf("Hello, %s! (from %s)"))(name)(f);
        }
    }
    static printTuples(tuples) {
        let enumerator;
        let copyOfStruct = tuples;
        enumerator = getEnumerator(copyOfStruct);
        try {
            while (enumerator["System.Collections.IEnumerator.MoveNext"]()) {
                const forLoopVar = enumerator["System.Collections.Generic.IEnumerator`1.get_Current"]();
                toConsole(printf("%s: %s"))(forLoopVar[0])(forLoopVar[1]);
            }
        }
        finally {
            disposeSafe(enumerator);
        }
    }
}
