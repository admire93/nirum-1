# -*- coding: utf-8 -*-
import enum

from pytest import raises
from nirum.rpc import Service
from six import PY3, text_type

from fixture.foo import (CultureAgnosticName, EastAsianName,
                         EvaChar, FloatUnbox, Gender, ImportedTypeUnbox, Irum,
                         Line, MixedName, NullService,
                         Point1, Point2, Point3d, Pop, PingService, Rnb,
                         Run, Stop, Way, WesternName)
from fixture.foo.bar import PathUnbox, IntUnbox, Point
from fixture.qux import Path, Name


def test_float_unbox():
    float_unbox = FloatUnbox(3.14)
    float1 = FloatUnbox(1.0)
    assert isinstance(FloatUnbox, type)
    assert float_unbox.value == 3.14
    assert float_unbox == FloatUnbox(3.14)
    assert float_unbox != float1
    assert {float_unbox, float_unbox, float1} == {float_unbox, float1}
    assert float_unbox.__nirum_serialize__() == 3.14
    assert FloatUnbox.__nirum_deserialize__(3.14) == float_unbox
    assert hash(float_unbox)
    assert hash(float_unbox) != 3.14
    with raises(ValueError):
        FloatUnbox.__nirum_deserialize__('a')
    with raises(TypeError):
        FloatUnbox('a')


def test_import_type_unbox():
    path = u'/path/string'
    path_unbox = PathUnbox(path)
    imported_unbox = ImportedTypeUnbox(path_unbox)
    assert isinstance(ImportedTypeUnbox, type)
    assert imported_unbox.value.value == u'/path/string'
    assert imported_unbox == ImportedTypeUnbox(PathUnbox(u'/path/string'))
    other_path = ImportedTypeUnbox(PathUnbox(u'/other/path'))
    assert imported_unbox != other_path
    expected = {imported_unbox, other_path}
    assert {imported_unbox, other_path, imported_unbox} == expected
    assert imported_unbox.__nirum_serialize__() == path
    assert ImportedTypeUnbox.__nirum_deserialize__(path) == imported_unbox
    with raises(ValueError):
        ImportedTypeUnbox.__nirum_deserialize__(123)
    with raises(TypeError):
        ImportedTypeUnbox(123)


def test_boxed_alias():
    w = Way(u'.')
    assert w.value == u'.'
    assert Way(Path(u'.')).value == u'.'
    assert Way.__nirum_deserialize__(u'.') == Way(u'.')
    assert Way(u'.').__nirum_serialize__() == u'.'
    assert Name(u'khj') == Irum(u'khj')
    assert Irum.__nirum_deserialize__(u'khj') == Irum(u'khj')
    assert Irum(u'khj').__nirum_serialize__() == u'khj'
    assert Irum.__nirum_deserialize__(u'khj') == Name(u'khj')
    assert Irum.__nirum_deserialize__(u'khj') == Irum(u'khj')


def test_enum():
    assert type(Gender) is enum.EnumMeta
    assert set(Gender) == {Gender.male, Gender.female}
    assert Gender.male.value == u'male'
    assert Gender.female.value == u'yeoseong'
    assert Gender.__nirum_deserialize__(u'male') == Gender.male
    assert Gender.__nirum_deserialize__(u'yeoseong') == Gender.female
    with raises(ValueError):
        Gender.__nirum_deserialize__(u'namja')
    assert Gender.male.__nirum_serialize__() == u'male'
    assert Gender.female.__nirum_serialize__() == u'yeoseong'
    assert type(EvaChar) is enum.EnumMeta
    assert set(EvaChar) == {EvaChar.soryu_asuka_langley,
                            EvaChar.ayanami_rei, EvaChar.ikari_shinji,
                            EvaChar.katsuragi_misato, EvaChar.nagisa_kaworu}
    soryu_asuka_langley = EvaChar.soryu_asuka_langley
    asuka = u'soryu_asuka_langley'
    assert soryu_asuka_langley.value == asuka
    assert soryu_asuka_langley.__nirum_serialize__() == asuka
    assert EvaChar.__nirum_deserialize__(asuka) == soryu_asuka_langley
    assert EvaChar.__nirum_deserialize__(asuka) == EvaChar.soryu_asuka_langley


def test_record():
    assert isinstance(Point1, type)
    point = Point1(left=3, top=14)
    assert point.left == 3
    assert point.top == 14
    assert point == Point1(left=3, top=14)
    assert point != Point1(left=3, top=15)
    assert point != Point1(left=4, top=14)
    assert point != Point1(left=4, top=15)
    assert point != 'foo'
    assert hash(Point1(left=3, top=14))
    point_serialized = {'_type': 'point1', 'x': 3, 'top': 14}
    assert point.__nirum_serialize__() == point_serialized
    assert Point1.__nirum_deserialize__(point_serialized) == point
    with raises(ValueError):
        Point1.__nirum_deserialize__({'x': 3, 'top': 14})
    with raises(ValueError):
        Point1.__nirum_deserialize__({'_type': 'foo'})
    with raises(TypeError):
        Point1(left=1, top='a')
    with raises(TypeError):
        Point1(left='a', top=1)
    with raises(TypeError):
        Point1(left='a', top='b')
    assert isinstance(Point2, type)
    int_three = IntUnbox(3)
    int_four_teen = IntUnbox(14)
    point2 = Point2(left=int_three, top=int_four_teen)
    assert point2.left == int_three
    assert point2.top == int_four_teen
    assert point2 == Point2(left=IntUnbox(3), top=IntUnbox(14))
    assert point2 != Point2(left=IntUnbox(3), top=IntUnbox(15))
    assert point2 != Point2(left=IntUnbox(4), top=IntUnbox(14))
    assert point2 != Point2(left=IntUnbox(4), top=IntUnbox(15))
    assert point2 != 'foo'
    point2_serialize = {'_type': 'point2', 'left': 3, 'top': 14}
    assert point2.__nirum_serialize__() == point2_serialize
    assert Point2.__nirum_deserialize__(point2_serialize) == point2
    with raises(ValueError):
        Point2.__nirum_deserialize__({'left': 3, 'top': 14})
    with raises(ValueError):
        Point2.__nirum_deserialize__({'_type': 'foo'})
    with raises(TypeError):
        Point2(left=IntUnbox(1), top='a')
    with raises(TypeError):
        Point2(left=IntUnbox(1), top=2)
    assert isinstance(Point3d, type)
    point3d = Point3d(xy=Point(x=1, y=2), z=3)
    assert point3d.xy == Point(x=1, y=2)
    assert point3d.xy.x == 1
    assert point3d.xy.y == 2
    assert point3d.z == 3
    point3d_serialize = {
        '_type': 'point3d',
        'xy': {
            '_type': 'point', 'x': 1, 'y': 2
        },
        'z': 3
    }
    assert point3d.__nirum_serialize__() == point3d_serialize
    assert Point3d.__nirum_deserialize__(point3d_serialize) == point3d


def test_record_with_one_field():
    assert isinstance(Line, type)
    line = Line(length=10)
    assert line.length == 10
    assert Line.__slots__ == ('length', )
    expected = {'_type': 'line', 'length': 3}
    assert Line(length=3).__nirum_serialize__() == expected


def test_union():
    assert isinstance(MixedName, type)
    assert MixedName.Tag.western_name.value == 'western_name'
    assert MixedName.Tag.east_asian_name.value == 'east_asian_name'
    assert MixedName.Tag.culture_agnostic_name.value == 'culture_agnostic_name'
    assert isinstance(WesternName, type)
    assert issubclass(WesternName, MixedName)
    with raises(NotImplementedError):
        MixedName()
    western_name = WesternName(first_name=u'foo', middle_name=u'bar',
                               last_name=u'baz')
    assert western_name.first_name == u'foo'
    assert western_name.middle_name == u'bar'
    assert western_name.last_name == u'baz'
    with raises(TypeError):
        WesternName(first_name=1, middle_name=u'bar', last_name=u'baz')
    with raises(TypeError):
        WesternName(first_name=u'foo', middle_name=1, last_name=u'baz')
    with raises(TypeError):
        WesternName(first_name=u'foo', middle_name=u'bar', last_name=1)
    assert western_name == WesternName(first_name=u'foo', middle_name=u'bar',
                                       last_name=u'baz')
    assert western_name != WesternName(first_name=u'wrong',
                                       middle_name=u'bar', last_name=u'baz')
    assert western_name != WesternName(first_name=u'foo', middle_name=u'wrong',
                                       last_name=u'baz')
    assert western_name != WesternName(first_name=u'foo', middle_name=u'bar',
                                       last_name=u'wrong')
    assert western_name != WesternName(first_name=u'wrong',
                                       middle_name=u'wrong',
                                       last_name=u'wrong')
    assert hash(WesternName(first_name=u'foo', middle_name=u'bar',
                            last_name=u'baz'))
    assert isinstance(EastAsianName, type)
    assert issubclass(EastAsianName, MixedName)
    east_asian_name = EastAsianName(family_name=u'foo', given_name=u'baz')
    assert east_asian_name.family_name == u'foo'
    assert east_asian_name.given_name == u'baz'
    assert east_asian_name == EastAsianName(family_name=u'foo',
                                            given_name=u'baz')
    assert east_asian_name != EastAsianName(family_name=u'foo',
                                            given_name=u'wrong')
    assert east_asian_name != EastAsianName(family_name=u'wrong',
                                            given_name=u'baz')
    assert east_asian_name != EastAsianName(family_name=u'wrong',
                                            given_name=u'wrong')
    with raises(TypeError):
        EastAsianName(family_name=1, given_name=u'baz')
    with raises(TypeError):
        EastAsianName(family_name=u'foo', given_name=2)
    assert isinstance(CultureAgnosticName, type)
    assert issubclass(CultureAgnosticName, MixedName)
    agnostic_name = CultureAgnosticName(fullname=u'foobar')
    assert agnostic_name.fullname == u'foobar'
    assert agnostic_name == CultureAgnosticName(fullname=u'foobar')
    assert agnostic_name != CultureAgnosticName(fullname=u'wrong')
    with raises(TypeError):
        CultureAgnosticName(fullname=1)


def test_union_with_special_case():
    kr_pop = Pop(country=u'KR')
    assert kr_pop.country == u'KR'
    assert kr_pop == Pop(country=u'KR')
    assert kr_pop != Pop(country=u'US')
    with raises(TypeError):
        assert Pop(country=1)
    assert Pop.__slots__ == ('country',)
    assert Rnb(country=u'KR').__nirum_tag__.value == 'rhythm_and_ballad'
    assert Run().__nirum_tag__.value == 'run'
    assert Stop().__nirum_tag__.value == 'stop'


def test_service():
    assert issubclass(NullService, Service)
    assert issubclass(PingService, Service)
    if PY3:
        assert set(PingService.ping.__annotations__) == {'nonce', 'return'}
        assert PingService.ping.__annotations__['nonce'] is text_type
        assert PingService.ping.__annotations__['return'] is bool
    with raises(NotImplementedError):
        PingService().ping(u'nonce')
    with raises(NotImplementedError):
        PingService().ping(nonce=u'nonce')
    with raises(TypeError):
        PingService().ping(wrongkwd=u'a')
