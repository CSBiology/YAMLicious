
# this import must be built from f# core project. For any changes to be recognized you must transpile the f# project first.
from YAMLicious.main import Main

class TestYAMLicious:

  def test_1(self):
    Main.hello("CSBiology")
    assert 1 == 1

  def test_2(self):
    Main.hello("CSBiology", "Kevin")
    assert 1 == 1

  def test_3(self):
    Main.print_tuples([("a", "b"), ("c", "d")])
    assert 1 == 1