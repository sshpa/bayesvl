select Id, Title, ViewCount, (select  STRING_AGG(ISNULL(TagName, 'N/A'), '; ') from BlogPostTag as bb
where PostId=aa.Id) as Tags,
(select STRING_AGG(ISNULL(Title, 'N/A'), '; ') from BlogTopic as cc inner join BlogTopicPost as dd on cc.Id=dd.TopicId where dd.PostId=aa.Id) as Topics,
   LastModified from BestView25 as aa


select count(*) as ViewCount, CtryCode, PostId, convert(date, ViewDate) as ViewDate from BlogView as aa where exists (select 1 from BestView25 as bb where aa.PostId=bb.Id)
group by CtryCode, PostId, convert(date, ViewDate)

select count(*) as ViewCount, PostId, convert(date, ViewDate) as ViewDate from BlogView as aa where exists (select 1 from BestView25 as bb where aa.PostId=bb.Id)
group by PostId, convert(date, ViewDate)

select count(*) as ViewCount, CtryCode, PostId from BlogView as aa where exists (select 1 from BestView25 as bb where aa.PostId=bb.Id)
group by CtryCode, PostId


select count(*) as ViewCount, RefererUrl, PostId from BlogView as aa where exists (select 1 from BestView25 as bb where aa.PostId=bb.Id)
group by RefererUrl, PostId

select count(*) as ViewCount, Referer, PostId from
(
select Id, 
case 
when RefererUrl like '%facebook%' or  RefererUrl like '%twitter%' or RefererUrl like '%linkedin%' then 1 
when RefererUrl like '%google%' or RefererUrl like '%coccoc%' or RefererUrl like '%bing.com%'  or RefererUrl like '%search.yahoo.com%' then 2
when RefererUrl like '%sc.sshpa.com%' then 3
when RefererUrl like '%www.sshpa.com%' or RefererUrl like '%data.sshpa.com%' then 4
else 5 end as Referer, 
PostId from BlogView as aa
 where exists (select 1 from BestView25 as bb where aa.PostId=bb.Id)
) as cc
group by Referer, PostId
order by PostId, Referer


select * from (

select count(*) as ViewCount,ViewDate,Referer,PostId,ROW_NUMBER()  
          over (Partition BY Referer,PostId
                ORDER BY ViewDate, Referer,PostId DESC ) AS RankId from
(
select 
case 
when RefererUrl like '%facebook%' or  RefererUrl like '%twitter%' or RefererUrl like '%linkedin%' then 1 
when RefererUrl like '%google%' or RefererUrl like '%coccoc%' or RefererUrl like '%bing.com%'  or RefererUrl like '%search.yahoo.com%' then 2
when RefererUrl like '%sc.sshpa.com%' then 3
when RefererUrl like '%www.sshpa.com%' or RefererUrl like '%data.sshpa.com%' then 4
else 5 end as Referer, 
convert(date,ViewDate) as ViewDate,
PostId from BlogView as aa
 where exists (select 1 from BestView25 as bb where aa.PostId=bb.Id)
) as eee
 group by PostId, Referer,ViewDate

 ) as fff
 where  RankId <= 7

select sum(ViewCount) as ViewCount,Referer,PostId from (

select count(*) as ViewCount,ViewDate,Referer,PostId,ROW_NUMBER()  
          over (Partition BY Referer,PostId
                ORDER BY ViewDate, Referer,PostId DESC ) AS RankId from
(
select 
case 
when RefererUrl like '%facebook%' or  RefererUrl like '%twitter%' or RefererUrl like '%linkedin%' then 1 
when RefererUrl like '%google%' or RefererUrl like '%coccoc%' or RefererUrl like '%bing.com%'  or RefererUrl like '%search.yahoo.com%' then 2
when RefererUrl like '%sc.sshpa.com%' then 3
when RefererUrl like '%www.sshpa.com%' or RefererUrl like '%data.sshpa.com%' then 4
else 5 end as Referer, 
convert(date,ViewDate) as ViewDate,
PostId from BlogView as aa
 where exists (select 1 from BestView25 as bb where aa.PostId=bb.Id)
) as eee
 group by PostId, Referer,ViewDate

 ) as fff
 where  RankId <= 100
  group by PostId, Referer
  order by PostId, Referer

