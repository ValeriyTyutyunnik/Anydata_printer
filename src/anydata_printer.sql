create or replace package anydata_printer
-- authid current_user
is

  /*
    MIT License
    Copyright (c) 2021 Valeriy Tyutyunnik
    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:
    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.
    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    SOFTWARE.
  */

  -- set true too get some debug output
  is_debug_on boolean := false;

  -- format masks for date types
  g_date_mask varchar2(100) := 'dd.mm.yyyy hh24:mi:ss';
  g_timestamp_mask varchar2(100) := 'dd.mm.yyyy hh24:mi:ss.ff';
  g_timestampltz_mask varchar2(100) := 'dd.mm.yyyy hh24:mi:ss.ff';
  g_timestamptz_mask varchar2(100) := 'dd.mm.yyyy hh24:mi:ss.ff tzh:tzm';

  procedure clear_cache;

  procedure fill_type_struct(p_type_fullname varchar2);

  procedure convert_anydata_to_clob(p_anydata in sys.anydata,
                                    p_clob    in out nocopy clob,
                                    p_offset  in integer default 0);

  procedure prn_anydata(p_anydata sys.anydata);

end anydata_printer;
/


create or replace package body anydata_printer is

  /*
    MIT License
    Copyright (c) 2021 Valeriy Tyutyunnik
    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:
    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.
    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    SOFTWARE.
  */

  type t_datatype_rec is record(type_name   varchar2(128 char),
                                type_owner  varchar2(128 char),
                                length      number,
                                precision   number,
                                scale       number,
                                char_used   varchar2(1)
                               );

  type t_object_attr_rec is record(attr_name  varchar2(128 char),
                                   attr_desc  t_datatype_rec
                                  );

  type t_obj_attr_tab is table of t_object_attr_rec;

  type t_type_struct is record(owner      varchar2(128 char),
                               name       varchar2(128 char),
                               typecode   varchar2(128 char),
                               attr_cnt   number,
                               elem_desc  t_datatype_rec,
                               attrs      t_obj_attr_tab
                              );

  type t_types is table of t_type_struct index by varchar2(128 char);

  g_type_tbl t_types;

  type t_sql is table of varchar2(32767) index by varchar2(128 char);

  g_sql_tbl t_sql;


  procedure debug(msg varchar2)
  is
  begin
    if is_debug_on then
      dbms_output.put_line(msg);
    end if;
  end debug;

  procedure clear_cache
  is
  begin
    g_type_tbl.delete;
    g_sql_tbl.delete;
  end clear_cache;

  procedure print_clob(p_clob clob)
  is
    l_offset  integer := 1;
    l_pos     integer;
    l_length  integer;
    l_ammount integer;
  begin
    l_length := dbms_lob.getlength(p_clob);
    loop
      exit when l_offset > l_length;
      l_pos := dbms_lob.instr(p_clob, chr(10), l_offset);
      if l_pos = 0 then
        l_ammount := l_length;
      else
        l_ammount := l_pos - l_offset;
      end if;
      dbms_output.put_line(dbms_lob.substr(p_clob, l_ammount, l_offset));
      l_offset := l_offset + l_ammount + 1;
    end loop;
  end print_clob;

  function get_attr_datatype(p_datatype in t_datatype_rec)
  return varchar2
  is
    l_res varchar2(100);
  begin
    if p_datatype.type_owner is not null then
      l_res := p_datatype.type_owner || '.' || p_datatype.type_name;
    else
      if p_datatype.type_name = 'TIMESTAMP WITH LOCAL TZ' then
        l_res := 'timestamp with local time zone';
      elsif p_datatype.type_name = 'TIMESTAMP WITH TZ' then
        l_res := 'timestamp with time zone';
      else
        l_res := p_datatype.type_name;
      end if;

      if p_datatype.length is not null then
        l_res := l_res || '(' || p_datatype.length;
        if p_datatype.char_used = 'C' then
          l_res := l_res || ' char';
        end if;
        l_res := l_res || ')';
      elsif p_datatype.precision is not null then
        l_res := l_res || '(' || p_datatype.precision;
        if p_datatype.scale is not null then
          l_res := l_res || ', ' || p_datatype.scale;
        end if;
        l_res := l_res || ')';
      end if;
    end if;

    return l_res;
  end get_attr_datatype;

  procedure execute_sql(p_sql     in varchar2,
                        p_anydata in sys.anydata,
                        p_offset  in integer,
                        p_clob    in out nocopy clob)
  is
  begin
    execute immediate p_sql using in p_anydata, in p_offset, in out p_clob;
  end execute_sql;

  procedure fill_type_struct(p_type_fullname varchar2)
  is
    l_point       number;
    l_type_struct t_type_struct;
  begin
    if p_type_fullname is null then
      raise_application_error(-20001, 'Param ''p_type_fullname'' can not be null');
    end if;
    l_point := instr(p_type_fullname, '.');
    if l_point <= 0 then
      raise_application_error(-20002, 'Type '||p_type_fullname||' is not collection or object type');
    end if;

    l_type_struct.owner := substr(p_type_fullname, 1, l_point-1);
    l_type_struct.name := substr(p_type_fullname, l_point+1);

    select at.typecode,
           at.attributes
      into l_type_struct.typecode,
           l_type_struct.attr_cnt
      from all_types at
     where at.owner = l_type_struct.owner
       and at.type_name = l_type_struct.name;

    if l_type_struct.typecode = 'OBJECT' then

      l_type_struct.attrs := t_obj_attr_tab();

      if l_type_struct.attr_cnt > 0 then

        for rec in (select ata.attr_name,
                           ata.attr_type_owner,
                           ata.attr_type_name,
                           ata.length,
                           ata.precision,
                           ata.scale,
                           ata.char_used
                      from all_type_attrs ata
                     where ata.owner = l_type_struct.owner
                       and ata.type_name = l_type_struct.name
                     order by ata.attr_no)
        loop
          l_type_struct.attrs.extend;
          if rec.attr_name = upper(rec.attr_name) then
            l_type_struct.attrs(l_type_struct.attrs.count).attr_name := rec.attr_name;
          else
            l_type_struct.attrs(l_type_struct.attrs.count).attr_name := '"'||rec.attr_name||'"';
          end if;
          l_type_struct.attrs(l_type_struct.attrs.count).attr_desc.type_owner := rec.attr_type_owner;
          l_type_struct.attrs(l_type_struct.attrs.count).attr_desc.type_name := rec.attr_type_name;
          l_type_struct.attrs(l_type_struct.attrs.count).attr_desc.length := rec.length;
          l_type_struct.attrs(l_type_struct.attrs.count).attr_desc.precision := rec.precision;
          l_type_struct.attrs(l_type_struct.attrs.count).attr_desc.scale := rec.scale;
          l_type_struct.attrs(l_type_struct.attrs.count).attr_desc.char_used := rec.char_used;
        end loop;
      end if;

    elsif l_type_struct.typecode = 'COLLECTION' then

      select act.elem_type_owner,
             act.elem_type_name,
             act.length,
             act.precision,
             act.scale,
             act.char_used
        into l_type_struct.elem_desc.type_owner,
             l_type_struct.elem_desc.type_name,
             l_type_struct.elem_desc.length,
             l_type_struct.elem_desc.precision,
             l_type_struct.elem_desc.scale,
             l_type_struct.elem_desc.char_used
        from all_coll_types act
       where act.owner = l_type_struct.owner
         and act.type_name = l_type_struct.name;

    end if;

    g_type_tbl(p_type_fullname) := l_type_struct;

    if is_debug_on then
       dbms_output.put_line('Describe type '||p_type_fullname||':');
       dbms_output.put_line('  owner='||l_type_struct.owner);
       dbms_output.put_line('  name='||l_type_struct.name);
       dbms_output.put_line('  typecode='||l_type_struct.typecode);
       dbms_output.put_line('  attr_cnt='||l_type_struct.attr_cnt);
       dbms_output.put_line('  element_datatype='||get_attr_datatype(l_type_struct.elem_desc));
       if l_type_struct.attrs is not null and l_type_struct.attrs.count > 0 then
        dbms_output.put_line('  attrs:');
         for i in 1 .. l_type_struct.attrs.count
         loop
           dbms_output.put_line('   '|| l_type_struct.attrs(i).attr_name || '  ' || get_attr_datatype(l_type_struct.attrs(i).attr_desc));
         end loop;
       end if;
    end if;

  end fill_type_struct;

  procedure prn_collection(p_anydata       in sys.anydata,
                           p_type_fullname in varchar2,
                           p_type_struct   in t_type_struct,
                           p_clob          in out nocopy clob,
                           p_offset        in integer) is
    l_sql              varchar2(32767);
    l_str              varchar2(32767);
    l_elem_type_struct t_type_struct;
  begin

    if g_sql_tbl.exists(p_type_fullname) then
      l_sql := g_sql_tbl(p_type_fullname);
    else
      -- declaration block
      l_sql := 'declare
  procedure generated(p_data   in sys.anydata,
                      p_offset in integer,
                      p_result in out nocopy clob)
  is
    l_collection '|| p_type_fullname ||';
    l_index number' || /* assotiative array by varchar2 can not be object collection */ ';
    l_value_char varchar2(32767);
    l_value ' || get_attr_datatype(p_type_struct.elem_desc) || ';
    l_attr_value_length number := 0;
    l_str varchar2(32767);
  begin

    if p_data.getCollection(l_collection) <> dbms_types.success then
      l_str := ''Geting collection ' || p_type_fullname || ' from anydata was unsuccessfull''||chr(10);
      dbms_lob.writeappend(p_result, length(l_str), l_str);
      return;
    end if;

    if l_collection is null then
      l_str := ''null'' || chr(10);
      dbms_lob.writeappend(p_result, length(l_str), l_str);
      return;
    elsif l_collection.count = 0 then
      l_str := ''' || p_type_fullname || '()'';
      dbms_lob.writeappend(p_result, length(l_str), l_str);
      return;
    end if;

    l_index := l_collection.first;

    l_str := ''' || p_type_fullname || '('' || chr(10) || lpad('' '', p_offset + 2);
    dbms_lob.writeappend(p_result, length(l_str), l_str);

    loop
      exit when l_index is null;
      l_value := l_collection(l_index);
      l_value_char := null;

      if l_value is not null then'||chr(10);

      -- anydata type
      if p_type_struct.elem_desc.type_owner in ('SYS', 'PUBLIC') and p_type_struct.elem_desc.type_name = 'ANYDATA' then

        l_sql := l_sql || '        anydata_printer.convert_anydata_to_clob(p_anydata => l_value,
                                   p_clob    => p_result,
                                   p_offset  => p_offset + 2);' || chr(10);

      -- object type
      elsif p_type_struct.elem_desc.type_owner is not null then

        if not g_type_tbl.exists(p_type_struct.elem_desc.type_owner || '.' || p_type_struct.elem_desc.type_name) then
          begin
            fill_type_struct(p_type_struct.elem_desc.type_owner || '.' || p_type_struct.elem_desc.type_name);
          exception
            when others then
              l_str := 'Error get type '||p_type_struct.elem_desc.type_owner || '.' || p_type_struct.elem_desc.type_name ||
                       ':' || sqlerrm || chr(10);
              dbms_lob.writeappend(p_clob, length(l_str), l_str);
              return;
          end;
        end if;

        begin
          l_elem_type_struct := g_type_tbl(p_type_struct.elem_desc.type_owner || '.' || p_type_struct.elem_desc.type_name);
        exception
          when no_data_found then
            l_str := 'Type ' || p_type_struct.elem_desc.type_owner || '.' || p_type_struct.elem_desc.type_name || ' not exist' || chr(10);
            dbms_lob.writeappend(p_clob, length(l_str), l_str);
            return;
        end;

        if l_elem_type_struct.typecode = 'COLLECTION' then
          l_sql := l_sql || '        anydata_printer.convert_anydata_to_clob(p_anydata => sys.anydata.convertCollection(l_value),
                                                  p_clob   => p_result,
                                                  p_offset => p_offset + 2);' || chr(10);
        elsif l_elem_type_struct.typecode = 'OBJECT' then
          l_sql := l_sql || '        anydata_printer.convert_anydata_to_clob(p_anydata => sys.anydata.convertObject(l_value),
                                                  p_clob   => p_result,
                                                  p_offset => p_offset + 2);' || chr(10);
        else
          l_str := 'Typecode ' || l_elem_type_struct.typecode || ' not implemented' || chr(10);
          dbms_lob.writeappend(p_clob, length(l_str), l_str);
          return;
        end if;

      -- base types
      else
        if p_type_struct.elem_desc.type_name = 'VARCHAR2' then
          l_sql := l_sql || '        l_value_char := l_value;' || chr(10);
        elsif p_type_struct.elem_desc.type_name = 'DATE' then
          l_sql := l_sql || '        l_value_char := to_char(l_value, '''||g_date_mask||''');' || chr(10);
        elsif p_type_struct.elem_desc.type_name = 'TIMESTAMP' then
          l_sql := l_sql || '        l_value_char := to_char(l_value, '''||g_timestamp_mask||''');' || chr(10);
        elsif p_type_struct.elem_desc.type_name = 'TIMESTAMP WITH LOCAL TZ' then
          l_sql := l_sql || '        l_value_char := to_char(l_value, '''||g_timestampltz_mask||''');' || chr(10);
        elsif p_type_struct.elem_desc.type_name = 'TIMESTAMP WITH TZ' then
          l_sql := l_sql || '        l_value_char := to_char(l_value, '''||g_timestamptz_mask||''');' || chr(10);
        else
          l_sql := l_sql || '        l_value_char := to_char(l_value);' || chr(10);
        end if;
      end if;

      -- if l_value is not null END
      l_sql := l_sql || '      else
        l_value_char := ''null'';
      end if;

      l_index := l_collection.next(l_index);

      if l_index is not null then
        l_value_char := l_value_char || '', ''';
      if p_type_struct.elem_desc.type_owner is not null then
        l_sql := l_sql || ' || chr(10) || lpad('' '', p_offset + 2)';
      end if;
      l_sql := l_sql || ';
      end if;

      if l_value_char is not null then
        if l_index is not null and length(l_value_char) + l_attr_value_length >= 120 then
          l_value_char := l_value_char || chr(10) || lpad('' '', p_offset+2);
          l_attr_value_length := 0;
        else
          l_attr_value_length := l_attr_value_length + length(l_value_char);
        end if;
        dbms_lob.writeappend(p_result, length(l_value_char), l_value_char);
      end if;

    end loop;

    l_str := chr(10) || lpad('' '', p_offset) || '')'';
    dbms_lob.writeappend(p_result, length(l_str), l_str);

  exception
    when others then
      l_str := ''Error while printing collection with type=' || p_type_fullname || '. Err='' || dbms_utility.format_error_stack || chr(10);
      dbms_lob.writeappend(p_result, length(l_str), l_str);
  end generated;

begin
  generated(:anydata, :offset, :clob);
end;';

      g_sql_tbl(p_type_fullname) := l_sql;
      debug('SQL to print collection type ' || p_type_fullname || ':');
      debug(l_sql);
    end if;

    execute_sql(l_sql, p_anydata, p_offset, p_clob);

  end prn_collection;

  procedure prn_object(p_anydata       in sys.anydata,
                       p_type_fullname in varchar2,
                       p_type_struct   in t_type_struct,
                       p_clob          in out nocopy clob,
                       p_offset        in integer)
  is
    l_sql              varchar2(32767);
    l_str              varchar2(32767);
    l_type_name        varchar2(300);
    l_point            number;
    l_elem_type_struct t_type_struct;
  begin
    l_point := instr(p_type_fullname, '.');
    l_type_name := substr(p_type_fullname, l_point+1);
    if l_type_name <> upper(l_type_name) then
      l_type_name := '"' || l_type_name || '"';
    end if;
    l_type_name := substr(p_type_fullname, 1, l_point-1) || '.' || l_type_name;
    debug('prn_object.l_type_name='||l_type_name);
    if p_type_struct.attrs is null then
      l_str := 'null';
      dbms_lob.writeappend(p_clob, length(l_str), l_str);
      return;
    elsif p_type_struct.attrs.count = 0 then
      l_str := l_type_name || '()';
      dbms_lob.writeappend(p_clob, length(l_str), l_str);
      return;
    end if;

    if g_sql_tbl.exists(p_type_fullname) then
      l_sql := g_sql_tbl(p_type_fullname);
    else

      l_sql := 'declare
procedure generated(p_data   in sys.anydata,
                    p_offset in integer,
                    p_result in out nocopy clob)
is
  l_object ' || l_type_name || ';
  l_str varchar2(32767);
  l_value_char varchar2(32767);' || chr(10);
    for i in 1 .. p_type_struct.attrs.count loop
      if p_type_struct.attrs(i).attr_desc.type_owner is null then
        l_sql := l_sql || '  l_value_' || i ||' ' || get_attr_datatype(p_type_struct.attrs(i).attr_desc) || ';' || chr(10);
      end if;
    end loop;
    l_sql := l_sql || 'begin
  if p_data.getObject(l_object) <> dbms_types.success then
    l_str := ''Geting object ' || l_type_name || ' from anydata was unsuccessfull'' || chr(10);
    dbms_lob.writeappend(p_result, length(l_str), l_str);
    return;
  end if;

  if l_object is null then
    l_str := ''null'' || chr(10);
    dbms_lob.writeappend(p_result, length(l_str), l_str);
    return;
  end if;

  l_str := ''' || l_type_name || '('' || chr(10);
  dbms_lob.writeappend(p_result, length(l_str), l_str);' || chr(10) || chr(10);

      for i in 1 .. p_type_struct.attrs.count loop
        -- anydata type
        if p_type_struct.attrs(i).attr_desc.type_owner in ('SYS', 'PUBLIC') and p_type_struct.attrs(i).attr_desc.type_name = 'ANYDATA' then

          l_sql := l_sql || '  l_str := lpad('' '', p_offset+2) || ''' || p_type_struct.attrs(i).attr_name || ' => '';
  if l_object.' || p_type_struct.attrs(i).attr_name || ' is not null then
    dbms_lob.writeappend(p_result, length(l_str), l_str);
    anydata_printer.convert_anydata_to_clob(p_anydata => l_object.' || p_type_struct.attrs(i).attr_name || ',
                                            p_clob    => p_result,
                                            p_offset  => p_offset + 2);
  else
    l_str := l_str || ''null'';
    dbms_lob.writeappend(p_result, length(l_str), l_str);
  end if;' || chr(10) || chr(10);

        if i < p_type_struct.attrs.count then
          l_sql := l_sql || '  l_str := '','' || chr(10);' || chr(10);
        else
          l_sql := l_sql || '  l_str := chr(10);' || chr(10);
        end if;
        l_sql := l_sql || '  dbms_lob.writeappend(p_result, length(l_str), l_str);' || chr(10);

        -- object type
        elsif p_type_struct.attrs(i).attr_desc.type_owner is not null then

          if not g_type_tbl.exists(p_type_struct.attrs(i).attr_desc.type_owner || '.' || p_type_struct.attrs(i).attr_desc.type_name) then
            begin
              fill_type_struct(p_type_struct.attrs(i).attr_desc.type_owner || '.' || p_type_struct.attrs(i).attr_desc.type_name);
            exception
              when others then
                l_sql := l_sql || '  l_str := ''Error get type ' || p_type_struct.attrs(i).attr_desc.type_owner || '.' || p_type_struct.attrs(i).attr_desc.type_name ||
                         ': ' || sqlerrm || ');' || chr(10) || chr(10);
                continue;
            end;
          end if;

          begin
            l_elem_type_struct := g_type_tbl(p_type_struct.attrs(i).attr_desc.type_owner || '.' || p_type_struct.attrs(i).attr_desc.type_name);
          exception
            when no_data_found then
              l_sql := l_sql || '  l_str := ''Type ' || p_type_struct.attrs(i).attr_desc.type_owner || '.' || p_type_struct.attrs(i).attr_desc.type_name ||
                       ' not exists'';' || chr(10) || chr(10);
              continue;
          end;

            l_sql := l_sql || '  l_str := lpad('' '', p_offset+2) || ''' || p_type_struct.attrs(i).attr_name || ' => '';
  if l_object.' || p_type_struct.attrs(i).attr_name || ' is not null then
    dbms_lob.writeappend(p_result, length(l_str), l_str);
    anydata_printer.convert_anydata_to_clob(p_anydata => sys.anydata.convert' || l_elem_type_struct.typecode || '(l_object.' || p_type_struct.attrs(i).attr_name || '),
                                            p_clob    => p_result,
                                            p_offset  => p_offset + 2);
  else
    l_str := l_str || ''null'';
    dbms_lob.writeappend(p_result, length(l_str), l_str);
  end if;' || chr(10) || chr(10);

          if i < p_type_struct.attrs.count then
            l_sql := l_sql || '  l_str := '','' || chr(10);' || chr(10);
          else
            l_sql := l_sql || '  l_str := chr(10);' || chr(10);
          end if;
          l_sql := l_sql || '  dbms_lob.writeappend(p_result, length(l_str), l_str);' || chr(10);

        --base types
        else
          l_sql := l_sql || '  l_value_' || i || ' := l_object.' || p_type_struct.attrs(i).attr_name || ';
  if l_value_' || i || ' is not null then' || chr(10);

        if p_type_struct.attrs(i).attr_desc.type_name = 'VARCHAR2' then
          l_sql := l_sql || '    l_value_char := l_value_' || i || ';' || chr(10);
        elsif p_type_struct.attrs(i).attr_desc.type_name = 'DATE' then
          l_sql := l_sql || '    l_value_char := to_char(l_value_' || i || ', '''||g_date_mask||''');' || chr(10);
        elsif p_type_struct.attrs(i).attr_desc.type_name = 'TIMESTAMP' then
          l_sql := l_sql || '    l_value_char := to_char(l_value_' || i || ', '''||g_timestamp_mask||''');' || chr(10);
        elsif p_type_struct.attrs(i).attr_desc.type_name = 'TIMESTAMP WITH LOCAL TZ' then
          l_sql := l_sql || '    l_value_char := to_char(l_value_' || i || ', '''||g_timestampltz_mask||''');' || chr(10);
        elsif p_type_struct.attrs(i).attr_desc.type_name = 'TIMESTAMP WITH TZ' then
          l_sql := l_sql || '    l_value_char := to_char(l_value_' || i || ', '''||g_timestamptz_mask||''');' || chr(10);
        else
          l_sql := l_sql || '    l_value_char := to_char(l_value_' || i || ');' || chr(10);
        end if;

        l_sql := l_sql || '  else
    l_value_char := ''null'';
  end if;
  l_str := lpad('' '', p_offset+2) || '''|| p_type_struct.attrs(i).attr_name || ' => '' || l_value_char;' || chr(10);

        if i < p_type_struct.attrs.count then
          l_sql := l_sql || '  l_str := l_str || '','' || chr(10);' || chr(10);
        else
          l_sql := l_sql || '  l_str := l_str || chr(10);' || chr(10);
        end if;

        l_sql := l_sql || '  dbms_lob.writeappend(p_result, length(l_str), l_str);' || chr(10) || chr(10);

        end if;
      end loop;

      l_sql := l_sql || '  l_str := lpad('' '', p_offset) || '')'';
  dbms_lob.writeappend(p_result, length(l_str), l_str);
exception
  when others then
    l_str := ''Error while printing object with type=' || l_type_name || '. Err='' || dbms_utility.format_error_stack || chr(10);
    dbms_lob.writeappend(p_result, length(l_str), l_str);
end generated;

begin
  generated(:anydata, :offset, :clob);
end;';

      g_sql_tbl(p_type_fullname) := l_sql;
      debug('SQL to print collection type ' || p_type_fullname || ':');
      debug(l_sql);
    end if;

    execute_sql(l_sql, p_anydata, p_offset, p_clob);

  end prn_object;

  procedure prn_type(p_anydata in sys.anydata,
                     p_clob    in out nocopy clob,
                     p_offset  in integer)
  is
    l_type_fullname    varchar2(300 char);
    l_type_struct      t_type_struct;
    l_str              varchar2(1000);
  begin
    if p_anydata is null then
      dbms_lob.writeappend(p_clob, 4, 'null');
      return;
    end if;

    l_type_fullname := p_anydata.getTypeName();

    if not g_type_tbl.exists(l_type_fullname) then
      fill_type_struct(l_type_fullname);
    end if;

    -- check
    if not g_type_tbl.exists(l_type_fullname) then
      l_str := 'Type '||l_type_fullname||' not exist';
      dbms_lob.writeappend(p_clob, length(l_str), l_str);
      return;
    end if;

    l_type_struct := g_type_tbl(l_type_fullname);

    if l_type_struct.typecode = 'COLLECTION' then

      prn_collection(p_anydata       => p_anydata,
                     p_type_fullname => l_type_fullname,
                     p_type_struct   => l_type_struct,
                     p_clob          => p_clob,
                     p_offset        => p_offset);

    elsif     l_type_struct.typecode = 'OBJECT'
          and l_type_struct.attr_cnt > 0
          and l_type_struct.attrs is not null then

      prn_object(p_anydata       => p_anydata,
                 p_type_fullname => l_type_fullname,
                 p_type_struct   => l_type_struct,
                 p_clob          => p_clob,
                 p_offset        => p_offset);
    else
      dbms_lob.writeappend(p_clob, 4, 'null');
    end if;

  exception
    when others then
      l_str := 'Type '||l_type_fullname||' can not be printed. Err='||dbms_utility.format_error_stack||';'||dbms_utility.format_error_backtrace;
      dbms_lob.writeappend(p_clob, length(l_str), l_str);
  end prn_type;

  procedure convert_anydata_to_clob (p_anydata in sys.anydata,
                                     p_clob    in out nocopy clob,
                                     p_offset  in integer default 0)
  is
    l_type_name varchar2(300 char);
    l_offset integer;

    --- datatypes
    l_num          number;
    l_bduble       binary_double;
    l_bfloat       binary_float;
    l_varchar      varchar2(32767);
    l_clob         clob;
    l_date         date;
    l_intervalym   interval year to month;
    l_intervalds   interval day to second;
    l_timestamp    timestamp;
    l_timestamplzt timestamp with local time zone;
    l_timestamptz  timestamp with time zone;
    l_urowid       urowid;
  begin
    if p_clob is null then
      dbms_lob.createtemporary(p_clob, true);
    end if;

    if p_anydata is null then
      dbms_lob.writeappend(p_clob, 4, 'null');
      return;
    end if;

    l_offset := greatest(0, nvl(p_offset, 0));
    l_type_name := p_anydata.getTypeName();

    if l_type_name not like 'SYS.%' then
      prn_type(p_anydata, p_clob, l_offset);
      return;
    elsif l_type_name = 'SYS.NUMBER' then
      l_num := p_anydata.AccessNumber;
      l_varchar := to_char(l_num);
    elsif l_type_name = 'SYS.BINARY_DOUBLE' then
      l_bduble := p_anydata.AccessBDouble;
      l_varchar := to_char(l_bduble);
    elsif l_type_name = 'SYS.BINARY_FLOAT' then
      l_bfloat := p_anydata.AccessBFloat;
      l_varchar := to_char(l_bfloat);
    elsif l_type_name = 'SYS.CHAR' then
      l_varchar := p_anydata.AccessChar;
    elsif l_type_name = 'SYS.CLOB' then
      l_clob := p_anydata.AccessClob;
      if l_clob is not null and length(l_clob) > 0 then
        dbms_lob.writeappend(p_clob, length(l_clob), l_clob);
        return;
      end if;
    elsif l_type_name = 'SYS.DATE' then
      l_date := p_anydata.AccessDate;
      l_varchar := to_char(l_date, g_date_mask);
    elsif l_type_name = 'SYS.INTERVAL_YEAR_MONTH' then
      l_intervalym := p_anydata.AccessIntervalYM;
      l_varchar := to_char(l_intervalym);
    elsif l_type_name = 'SYS.INTERVAL_DAY_SECOND' then
      l_intervalds := p_anydata.AccessIntervalDS;
      l_varchar := to_char(l_intervalds);
    elsif l_type_name = 'SYS.NCHAR' then
      l_varchar := p_anydata.AccessNchar;
    elsif l_type_name = 'SYS.NVARCHAR2' then
      l_varchar := p_anydata.AccessNVarchar2;
    elsif l_type_name = 'SYS.TIMESTAMP' then
      l_timestamp := p_anydata.AccessTimestamp;
      l_varchar := to_char(l_timestamp, g_timestamp_mask);
    elsif l_type_name = 'SYS.TIMESTAMP_WITH_LTZ' then
      l_timestamplzt := p_anydata.AccessTimestampLTZ;
      l_varchar := to_char(l_timestamplzt, g_timestampltz_mask);
    elsif l_type_name = 'SYS.TIMESTAMP_WITH_TIMEZONE' then
      l_timestamptz := p_anydata.AccessTimestampTZ;
      l_varchar := to_char(l_timestamptz, g_timestamptz_mask);
    elsif l_type_name = 'SYS.UROWID' then
      l_urowid := p_anydata.AccessURowid;
      l_varchar := to_char(l_urowid);
    elsif l_type_name = 'SYS.VARCHAR' then
      l_varchar := p_anydata.AccessVarchar;
    elsif l_type_name = 'SYS.VARCHAR2' then
      l_varchar := p_anydata.AccessVarchar2;
    elsif l_type_name in ('SYS.BFILE',
                          'SYS.BLOB',
                          'SYS.RAW' ) then
      l_varchar := 'Anydata value''s type is '||l_type_name||'. Content will not be printed';
    else
      l_varchar := 'Unimplemented type '||l_type_name;
    end if;

    l_varchar := nvl(l_varchar, 'null');
    dbms_lob.writeappend(p_clob, length(l_varchar), l_varchar);

  end convert_anydata_to_clob;

  procedure prn_anydata (p_anydata sys.anydata)
  is
    l_clob clob;
  begin
    dbms_lob.createtemporary(l_clob, true);
    convert_anydata_to_clob(p_anydata, l_clob);
    print_clob(l_clob);
    dbms_lob.freetemporary(l_clob);
  end prn_anydata;

end anydata_printer;
/
