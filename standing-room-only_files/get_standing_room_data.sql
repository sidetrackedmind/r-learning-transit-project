WITH ALL_ROUTE_72_DIR_0_STOP_DATA AS (
SELECT 
se.SERVICE_DATE, se.SERVICE_KEY , se.TRIP_NUMBER
,t.TRIP_BEGIN_TIME 
,CASE WHEN t.TRIP_BEGIN_TIME > 86399 THEN TO_CHAR(TO_DATE(t.TRIP_BEGIN_TIME-86400,'sssss'),'hh:mi AM')
ELSE TO_CHAR(TO_DATE(t.TRIP_BEGIN_TIME,'sssss'),'hh:mi AM') END AS TRIP_BEGIN_TIME_STR
, rd.ROUTE_NUMBER, r.PUBLIC_ROUTE_DESCRIPTION, rd.DIRECTION
,rd.PUBLIC_DIRECTION_DESCRIPTION
,rs.STOP_SEQUENCE_NUMBER 
,se.LOCATION_ID, loc.PUBLIC_LOCATION_DESCRIPTION 
,se.ARRIVE_TIME, se.LEAVE_TIME, se.ESTIMATED_LOAD 
,CASE WHEN se.ESTIMATED_LOAD >= 39 THEN 1 ELSE 0 END AS IS_STANDING_ROOM
,ROW_NUMBER() OVER (PARTITION BY se.SERVICE_DATE, se.SERVICE_KEY , se.TRIP_NUMBER, rd.ROUTE_NUMBER, rd.DIRECTION,se.LOCATION_ID ORDER BY se.LEAVE_TIME DESC) AS STOP_RECORD_PRIORITY
FROM 
TRANS.STOP_EVENT se
JOIN
TRANS.LOCATION loc
ON
se.LOCATION_ID = loc.LOCATION_ID 
JOIN
TRANS.ROUTE r 
ON
se.ROUTE_NUMBER = r.ROUTE_NUMBER 
AND se.SERVICE_DATE BETWEEN r.ROUTE_BEGIN_DATE AND r.ROUTE_END_DATE 
JOIN
TRANS.ROUTE_DIRECTION rd 
ON
r.ROUTE_NUMBER = rd.ROUTE_NUMBER 
AND r.ROUTE_BEGIN_DATE = rd.ROUTE_BEGIN_DATE 
AND rd.DIRECTION = se.DIRECTION 
JOIN 
TRANS.ROUTE_STOP rs 
ON
rs.ROUTE_NUMBER = rd.ROUTE_NUMBER 
AND rs.DIRECTION = rd.DIRECTION 
AND rs.LOCATION_ID = se.LOCATION_ID 
AND rs.ROUTE_BEGIN_DATE = rd.ROUTE_BEGIN_DATE 
JOIN 
TRANS.TRIP t 
ON
se.SERVICE_DATE BETWEEN t.TRIP_BEGIN_DATE AND t.TRIP_END_DATE 
AND rd.ROUTE_NUMBER = t.ROUTE_NUMBER AND rd.DIRECTION = t.DIRECTION 
AND se.TRIP_NUMBER = t.TRIP_NUMBER AND se.SERVICE_KEY = t.SERVICE_KEY 
WHERE 
SERVICE_DATE BETWEEN '27-AUG-23' AND '30-SEP-23'
AND se.ROUTE_NUMBER = 72 AND se.DIRECTION = 0
--ORDER BY se.SERVICE_DATE,se.TRIP_NUMBER, rd.ROUTE_NUMBER, rd.DIRECTION,se.ARRIVE_TIME
)
SELECT 
TRIP_BEGIN_TIME_STR
, ROUTE_NUMBER, PUBLIC_ROUTE_DESCRIPTION
, DIRECTION,PUBLIC_DIRECTION_DESCRIPTION
,STOP_SEQUENCE_NUMBER , PUBLIC_LOCATION_DESCRIPTION 
,SUM(IS_STANDING_ROOM) AS STANDING_ROOM_COUNT
,COUNT(*) AS NUM_TRIPS
FROM 
ALL_ROUTE_72_DIR_0_STOP_DATA 
WHERE STOP_RECORD_PRIORITY = 1 AND SERVICE_KEY = 'W'
GROUP BY 
TRIP_BEGIN_TIME_STR
, ROUTE_NUMBER, PUBLIC_ROUTE_DESCRIPTION
, DIRECTION,PUBLIC_DIRECTION_DESCRIPTION
,STOP_SEQUENCE_NUMBER , PUBLIC_LOCATION_DESCRIPTION
ORDER BY TRIP_BEGIN_TIME_STR, ROUTE_NUMBER, STOP_SEQUENCE_NUMBER
;