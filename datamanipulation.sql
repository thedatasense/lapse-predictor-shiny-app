update CPRDATA
set label = null;


/*Status Code = 0 è inforce
Status Code = 1 è terminated due to lapse (target)
Status Code = 2 è terminated due to death
Status Code = 3 è terminated for other reasons (conversion, maturity, etc.)
Status Code = 4 è inforce following death of one of the insured lives (joint policies only)*/

update cpdata
set XLABEL = 1
where POLICYNO in (select distinct POLICYNO from cpdata where cpdata.XSTATUSCODE = 1);
update cpdata
set XLABEL = 0
where POLICYNO in (select distinct POLICYNO from cpdata where cpdata.XSTATUSCODE != 1);

update cprdata
set label='4'
where POLICYNO in (select policyno from cprdata where STATUSCODE = 4)
  and label is null;

update cprdata
set label='3'
where POLICYNO in (select policyno from cprdata where STATUSCODE = 3)
  and label is null;

update cprdata
set label='2'
where POLICYNO in (select policyno from cprdata where STATUSCODE = 2)
  and label is null;

update cprdata
set label='1'
where POLICYNO in (select policyno from cprdata where STATUSCODE = 1)
  and label is null;

update cprdata
set label='0'
where POLICYNO in (select policyno from cprdata where STATUSCODE = 0)
  and label is null;

create table cpdata as
select *
from CPRDATA;

select *
from POLICYTERMDATE;

create table policytermdate as
select POLICYNO,
       OBSYR,
       to_date(ISSUEDATE, 'mm/dd/yyyy')                                                        as issuedate,
       to_date(TERMDATE, 'mm/dd/yyyy')                                                         as TERMDATE,
       to_date(TERMDATE, 'mm/dd/yyyy') - to_date(ISSUEDATE, 'mm/dd/yyyy')                      as DaysActive,
       round((to_date(TERMDATE, 'mm/dd/yyyy') - to_date(ISSUEDATE, 'mm/dd/yyyy')) / 30)        as MonthsActive,
       round((to_date(TERMDATE, 'mm/dd/yyyy') - to_date(ISSUEDATE, 'mm/dd/yyyy')) / (30 * 12)) as YearsActive
from cpdata
where termdate is not null;



alter table cpdata
    add (XSECPOLICYHOLDER NUMBER GENERATED ALWAYS AS (case when (secgender is null or secdob is null) then 0 else 1 end ));


select SECDOB, SECGENDER, XSECPOLICYHOLDER
from CPDATA
where SECDOB is null
   or SECDOB is null;


select SECDOB, SECGENDER, XSECPOLICYHOLDER
from CPDATA
where (SECDOB is not null and SECDOB is not null);

alter table CPDATA
    add XPRIMRISKSTDCLASS varchar2(100);
/
update cpdata a
set XPRIMRISKSTDCLASS = (select STANDARDRC from STANDARDRC b where a.PRIMRISKCLASS = b.PRIMRISKCLASS);
-- SecRiskClass

alter table CPDATA
    add XSECRISKSTDCLASS varchar2(100)
/

alter table CPDATA
    add XSTATUSCODE number;
/

alter table CPDATA
    drop column
        XSTATUSCODE;
/



update cpdata a
set XSECRISKSTDCLASS = (select STANDARDRC from STANDARDRC b where a.SECRISKCLASS = b.PRIMRISKCLASS);
ALTER TABLE cpdata
    DROP COLUMN XSTATUSCODE;
alter table cpdata
    add (XSTATUSCODE NUMBER GENERATED ALWAYS AS (case when STATUSCODE = '1' then 1 else 0 end ));


alter table cpdata
    add (XAVFMTMISS number generated always as (case when (AVBOY is null and FAMTBOY is null) then 1 else 0 end));


ALTER TABLE cpdata
    DROP COLUMN XLOANFLAG;
ALTER TABLE cpdata
    DROP COLUMN XSECGIND;


select distinct LOANBOY
from cpdata;


ALTER TABLE cpdata
    DROP COLUMN XLOANFLAG;
alter table cpdata
    add (XLOANFLAG number generated always as (case
                                                   when (LOANBOY is null)
                                                       then 0
                                                   when LOANBOY = '0' then 0
                                                   when LOANBOY = '*' then 0
                                                   else 1 end) );

alter table cpdata
    add (XSECGIND number generated always as (case when (SECGUAR is null OR to_number(SECGUAR) = 0) then 0 else 1 end));

alter table cpdata
    add (XSECGPRD number generated always as (case when (SECGUAR is null OR to_number(SECGUAR) = 0) then 0 else SEC end));

select *
select *
from cpdata;

alter table cpdata
    add XPRIMAGEINYEARS number;

alter table cpdata
    drop column XPRIMAGEINYEARS;

alter table cpdata
    add XSECAGEINYEARS number;

select *
from cprdata
where PRIMDOB is null;

update cpdata
set XPRIMAGEINYEARS = case
                          when PRIMDOB is null then 0
                          else round((to_date(OBSYR, 'YYYY') - to_date(PRIMDOB, 'mm/dd/yyyy')) / (30 * 12)) end;


alter table CPDATA
    drop column XPRIMAGEINYEARS;
/

alter table CPDATA
    drop column XSECAGEINYEARS
/


select to_date(PRIMDOB, 'mm/dd/yyyy'),
       to_date(OBSYR, 'YYYY'),
       round((to_date(OBSYR, 'YYYY') - to_date(PRIMDOB, 'mm/dd/yyyy')) / (30 * 12))
from CPDATA;


alter table cpdata
    add (XPRIMAGEINYEARS number generated always as (round(
            (to_date(OBSYR, 'YYYY') - to_date(PRIMDOB, 'mm/dd/yyyy')) / (30 * 12))));


alter table cpdata
    add (XSECAGEINYEARS number generated always as (round(
            (to_date(OBSYR, 'YYYY') - to_date(SECDOB, 'mm/dd/yyyy')) / (30 * 12))));


alter table cpdata
    add XPREMPERYEAR number generated always as
        (CUMPREM - LAG(CUMPREM, 1, 0) OVER (PARTITION BY POLICYNO ORDER BY POLICYNO,OBSYR) );

drop view vwcpdata;
create view vwcpdata as
select a.*,
       b.TERMDATE     as XTERMDATE,
       b.DAYSACTIVE   as XdAYSACTIVE,
       b.MONTHSACTIVE as XMONTHSACTIVE,
       b.YEARSACTIVE  as XYEARSACTIVE,
       case
           when a.CUMPREM is not null then (a.CUMPREM - LAG(CUMPREM, 1, 0)
                                                            OVER (PARTITION BY a.POLICYNO ORDER BY a.POLICYNO,a.OBSYR))
           else 0 end as XPREMPERYEAR
from CPDATA a
         left join POLICYTERMDATE b on a.POLICYNO = b.POLICYNO
where a.POLICYNO = '10283'
order by a.POLICYNO, a.OBSYR;

select *
from vwcpdata;

select count(distinct POLICYNO)
from cpdata;
-- 2106359

select count(*)
from (select distinct POLICYNO, FAMTBOY
      from cpdata
      where FAMTBOY is not null);
-- 2214910

select a.POLICYNO, a.FAMTBOY, b.FAMTBOY
from cpdata a
         join cpdata b
              on a.POLICYNO = b.POLICYNO and a.FAMTBOY != b.FAMTBOY and (a.FAMTBOY is not null or b.FAMTBOY is null);


select *
from cpdata
where DURATION = 1
order by POLICYNO, OBSYR;



select a.*, CUMPREM - LAG(CUMPREM, 1, 0) OVER (PARTITION BY POLICYNO ORDER BY POLICYNO,OBSYR) AS PREM_YEAR
from cpdata a
where (POLICYNO = '3050227710' OR POLICYNO = '1022900400')
order by POLICYNO, OBSYR;


select count(distinct POLICYNO)
from vwcpdata
where XPREMPERYEAR < 0
  and XSTATUSCODE = 1;


select *
from vwcpdata
where POLICYNO = '1021166470'
  and LOANBOY = '*';

-- alter table cpdata
--     add (XSECGIND number generated always as (case when (SECGUAR is null OR to_number(SECGUAR)=0 ) then 0 else 1 end));
-- SELECT deptno, sal, REGR_SLOPE(deptno, sal) OVER (partition by deptno) "REGR_SLOPE"
-- FROM employee


select POLICYNO, CUMPREM, REGR_SLOPE(DURATION, CUMPREM) over (partition by POLICYNO order by OBSYR)
from vwcpdata
where POLICYNO = '10284';


select distinct --POLICYNO,
                SECGUAR,
                SECGUARIND,
                XSECGIND,
                SGUARPERIOD
from cpdata
where cpdata.XSECGIND = '1';


select POLICYNO,
       SECGUAR,
       SECGUARIND,
       XSECGIND,
       SGUARPERIOD,
       STATUSCODE,
       cpdata.XSTATUSCODE,
       DURATION
from cpdata
where cpdata.XSTATUSCODE = 1
  and cpdata.XSECGIND = '1';


select *
from cpdata
where CURRENTPREM is null;

create table firstcumprem as
select a.POLICYNO, a.CUMPREM, ROUND((a.CUMPREM) / a.DURATION) as DCUMPREM, b.fobsyr, a.OBSYR
from cpdata a
         join (select policyno, min(OBSYR) as fobsyr, min(DURATION) as minduration from cpdata group by policyno) b
              on a.POLICYNO = b.POLICYNO and a.OBSYR = b.fobsyr;

select *
from cpdata
where DURATION = 0;


select distinct PRODTYPE, PRODFORM, COCODE
from cpdata
where CREDITEDRATEBOY is not null;

select distinct PRODTYPE,
                PRODFORM,
                COCODE,
                count(distinct POLICYNO),
                SUM(case when cpdata.STATUSCODE = 1 then 1 else 0 end),
                MEDIAN() over ()
from cpdata
group by PRODTYPE, PRODFORM, COCODE
order by count(distinct POLICYNO) desc;

select *
from cpdata
where FAMTBOY is null;


-- average of avboy and max of famtboy.
--


select *
from vwcpdata
where XLOANFLAG = 1
  and XSTATUSCODE = 1;

select *
from vwcpdata
where POLICYNO = '151224205';


select a.POLICYNO,
       a.CUMPREM,
       case
           when a.CUMPREM is not null then (a.CUMPREM - LAG(CUMPREM, 1, 0)
                                                            OVER (PARTITION BY a.POLICYNO ORDER BY a.POLICYNO,a.OBSYR))
           else 0 end as XPREMPERYEAR
from CPDATA a
where POLICYNO = '151224205';

select a.POLICYNO,
       a.OBSYR,
       a.CUMPREM,
       (a.CUMPREM - LAG(CUMPREM, 1, 0) OVER (PARTITION BY a.POLICYNO ORDER BY a.POLICYNO,a.OBSYR)) / a.CUMPREM
from cpdata a
where POLICYNO = '10284'
order by a.POLICYNO, a.OBSYR;



b.TERMDATE     as XTERMDATE,
       b.DAYSACTIVE   as XdAYSACTIVE,
       b.MONTHSACTIVE as XMONTHSACTIVE,
       b.YEARSACTIVE  as XYEARSACTIVE,

create table policyaggregate as
select a.POLICYNO,
       min(OBSYR)            as XFIRSTOBSYEAR,
       max(OBSYR)            as XLASTOBSYEAR,
       max(DURATION)         as XDURATION,
       max(CUMPREM)          as XCUMPREM,
       max(FAMTBOY)          as XFAMTBOY,
       max(LOANBOY)          as XLOANBOY,
       AVG(CREDITEDRATEBOY)  as XCREDITEDRATEBOY,
       AVG(GUARCREDITEDRATE) as XGUARCREDITEDRATE
from cpdata a
-- where POLICYNO = '10284'
group by a.POLICYNO
order by a.POLICYNO;

update cpdata a
set XLOANFLAG = 1
where POLICYNO in (select distinct POLICYNO from cpdata where a.LOANBOY >= 1);
update cpdata
set LOANBOY='0'
where LOANBOY = '*';

select distinct LOANBOY
from cpdata;

drop materialized view vmonecpdata;
create materialized view vmonecpdata as
select distinct a.PRODFORM,
                a.POLICYNO,
                a.XLABEL,
                a.COCODE,
                a.ISSUEDATE,
                a.ISSUESTATE,
                a.XPRIMRISKSTDCLASS,
                a.PRIMDOB,
                a.XSECPOLICYHOLDER,
                a.PRIMGENDER,
                a.DISTCHANNEL,
                a.XSTATUSCODE,
                a.PRODTYPE,
                MAX(a.XPRIMAGEINYEARS)    as MXPRIMAGEINYEARS,
                a.XSECGIND,
                a.XLOANFLAG,
                c.XFIRSTOBSYEAR,
                c.XLASTOBSYEAR,
                c.XDURATION,
                c.XCUMPREM,
                c.XFAMTBOY,
                c.XLOANBOY,
                ROUND(c.XCREDITEDRATEBOY) as XCREDITEDRATEBOY,
                c.XGUARCREDITEDRATE,
                b.TERMDATE                as XTERMDATE,
                b.DAYSACTIVE              as XdAYSACTIVE,
                b.MONTHSACTIVE            as XMONTHSACTIVE,
                b.YEARSACTIVE             as XYEARSACTIVE
from cpdata a
         left join policytermdate b on a.POLICYNO = b.POLICYNO
         left join policyaggregate c on
    a.POLICYNO = c.POLICYNO
group by a.PRODFORM,
         a.POLICYNO,
         a.COCODE,
         a.ISSUEDATE,
         a.ISSUESTATE,
         a.XPRIMRISKSTDCLASS,
         a.PRIMDOB,
         a.XSECPOLICYHOLDER,
         a.PRIMGENDER,
         a.DISTCHANNEL,
         a.XSTATUSCODE,
         a.PRODTYPE,
         a.XSECGIND,
         a.XLOANFLAG,
         c.XFIRSTOBSYEAR,
         c.XLASTOBSYEAR,
         c.XDURATION,
         a.XLABEL,
         c.XCUMPREM,
         c.XFAMTBOY,
         c.XLOANBOY,
         ROUND(c.XCREDITEDRATEBOY),
         c.XGUARCREDITEDRATE,
         b.TERMDATE,
         b.DAYSACTIVE,
         b.MONTHSACTIVE,
         b.YEARSACTIVE
order by a.POLICYNO;


select OBSYR, count(distinct POLICYNO)
from cpdata
where cpdata.XPRIMAGEINYEARS > 20
  and cpdata.XPRIMAGEINYEARS < 40 group by cpdata.OBSYR;




