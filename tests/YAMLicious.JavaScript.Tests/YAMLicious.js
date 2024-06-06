import * as assert from "assert"
// this import must be built from f# core project. For any changes to be recognized you must transpile the f# project first.
import { Main } from "./YAMLicious/index.js"

describe('YAMLicious', function(){
  it('test1', function(){
    Main.hello("CSBiology")
    assert.equal(1,1,"This should be equal")
  });
  it('test2', function(){
    Main.hello("CSBiology", "Kevin")
    assert.equal(1,1,"This should be equal")
  });
  it('test3', function(){
    Main.printTuples([["test1", "test2"], ["test3", "test4"]])
    assert.equal(1,1,"This should be equal")
  });
});