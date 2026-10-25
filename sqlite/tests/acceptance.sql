create table t(a integer primary key, b text, c real);
insert into t values(1,'x',1.5),(2,'y',2.25),(3,'zz',-3.75);
select sum(a), count(*), avg(c), max(b) from t;
.mode box
select * from t where a > 1 order by c desc;
select a, b || '-' || b, round(c*2,1), typeof(c) from t;
select datetime(0, 'unixepoch'), typeof(1.5), sqlite_version(), hex('AB'), length('hello');
create index i on t(b);
.schema
.tables
explain query plan select * from t where b = 'y';
select printf('%5.2f|%-4d|%x', c, a, a*255) from t;
with recursive r(n) as (select 1 union all select n+1 from r where n<10) select group_concat(n) from r;
select a, c, c*c, c/3, cast(c as integer), abs(c) from t;
select typeof(a*255), typeof(5*7919), 5*7919, (5*7919)%97, typeof(-7/2), -7/2, -7%3, typeof(a*a) from t;
select typeof(9223372036854775807*2), typeof(-9223372036854775807*-1), 4611686018427387904*2;
create table p(id integer primary key, shade integer, mass real);
with recursive seq(n) as (select 1 union all select n+1 from seq where n<300) insert into p select n, (n*7919)%97, (n%100)/7.0 from seq;
create index p_shade on p(shade);
select shade, typeof(shade), count(*), sum(mass) from p group by shade order by shade limit 6;
select count(*) from p a join p b on a.shade = b.shade and a.id < b.id and a.id % 50 = 0;
update t set c = c + 100 where a = 2;
delete from t where a = 3;
select * from t;
pragma integrity_check;
