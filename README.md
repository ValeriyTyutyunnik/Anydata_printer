# Anydata_printer
Pl/SQL package to print the contents of any schema-level object/collection using anydata object

## Introduction

Sometimes, during debugging or for data analysis, it is necessary to display the values of all elements of the Pl/sql object collection. To do this, you often have to write a small script to display all the elements, but this script will only work for a collection with this type. For an other collection with a different type, you need to write a similar script, which periodically can take some time.

So I always wanted to write some framework to print PL/SQL collections automatically, but due to the limitations of the language, this seemed like an impossible task. The problem turned out to be solvable using ANYDATA type. You just need to convert your type to anydata and it will be possible to work with it through a single interface.

## Samples
In all sample you need to enable output and convert your collection or object to anydata.


### 1. Print simple array


This type is created in database by user "test":
```
create or replace type t_numbers_tbl is table of number
/
```
To print collection in output of this type you need to convert it in anydata type like this
```
declare
  t t_numbers_tbl;
begin
  dbms_output.put_line('-- 1. Print null collection');
  anydata_printer.prn_anydata( sys.anydata.convertCollection(t) );
  
  dbms_output.put_line(chr(10) || '-- 2. Print empty array');
  t := t_numbers_tbl();
  anydata_printer.prn_anydata( sys.anydata.convertCollection(t) );
  
  dbms_output.put_line(chr(10) || '-- 3. Print non empty array');
  select level 
    bulk collect into t
    from dual
   connect by level <= 100;

  anydata_printer.prn_anydata( sys.anydata.convertCollection(t) );
end;
```
The output will be:
```
-- 1. Print null collection
null

-- 2. Print empty array
test.t_numbers_tbl()

-- 3. Print non empty array
test.t_numbers_tbl(
  1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 
  34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 
  64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 
  94, 95, 96, 97, 98, 99, 100
)
```


### 2. Print object


This object is created in database by user "test":
```
create or replace type department is object (
  department_id   number,
  department_name varchar2(30)
)
```

Print object:
```
declare
  t department;
begin
  dbms_output.put_line('-- 1. Print null object');
  anydata_printer.prn_anydata( sys.anydata.convertObject(t) );
  
  dbms_output.put_line(chr(10) || '-- 2. Print empty object');
  t := department(null, null);
  anydata_printer.prn_anydata( sys.anydata.convertObject(t) );
  
  dbms_output.put_line(chr(10) || '-- 3. Print object');
  t.department_id := 1;
  t.department_name := 'Administration';

  anydata_printer.prn_anydata( sys.anydata.convertObject(t) );
end;
```

Output:
```
-- 1. Print null object
null

-- 2. Print empty object
test.department(
  DEPARTMENT_ID => null,
  DEPARTMENT_NAME => null
)

-- 3. Print object
test.department(
  DEPARTMENT_ID => 1,
  DEPARTMENT_NAME => Administration
)
```


### 3. Object collections


This object is created in database by user "test":
```
create or replace type departments is table of department
```

Print collection:
```
declare
  t departments;
begin
  dbms_output.put_line('-- 1. Print null collection');
  anydata_printer.prn_anydata( sys.anydata.convertCollection(t) );
  
  dbms_output.put_line(chr(10) || '-- 2. Print empty collection');
  t := departments();
  anydata_printer.prn_anydata( sys.anydata.convertCollection(t) );
  
  dbms_output.put_line(chr(10) || '-- 3. Print collection with 4 elements');
  t := departments(department(1, 'Administration'),
                   department(2, 'Marketing'),
                   null,
                   department(null, null)
                   );

  anydata_printer.prn_anydata( sys.anydata.convertCollection(t) );
end;
```

Output:
```
-- 1. Print null collection
null

-- 2. Print empty collection
test.departments()

-- 3. Print collection with 4 elements
test.departments(
  test.department(
    DEPARTMENT_ID => 1,
    DEPARTMENT_NAME => Administration
  ), 
  test.department(
    DEPARTMENT_ID => 2,
    DEPARTMENT_NAME => Marketing
  ), 
  null, 
  test.department(
    DEPARTMENT_ID => null,
    DEPARTMENT_NAME => null
  )
)
```


### 4. Anydata collection


This type is created in database by user "test":
```
create or replace type t_anydata_tbl is table of sys.anydata
```

Print collection:
```
declare
  t t_anydata_tbl;
begin

  t := t_anydata_tbl (
    sys.anydata.ConvertDate(sysdate),
    sys.anydata.ConvertIntervalDS(interval '1' day),
    sys.anydata.ConvertNumber(100500),
    sys.anydata.ConvertTimestamp(systimestamp),
    sys.anydata.ConvertVarchar2('Hello world!')
  );
  
  anydata_printer.prn_anydata( sys.anydata.convertCollection(t) );
end;
```

Output:
```
test.t_anydata_tbl(
  11.01.2021 19:26:23, 
  +01 00:00:00.000000, 
  100500, 
  11.01.2021 19:26:23.558000000, 
  Hello world!
)
```
