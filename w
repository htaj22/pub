CREATE VOLATILE TABLE V_RollingWeeks AS (
  WITH DateSequence AS (
    SELECT
      TD.Monday,
      TD.Tuesday,
      CASE
        WHEN HD.MondayDate IS NOT NULL THEN TD.Monday
        ELSE TD.Tuesday
      END AS ValidDate
    FROM
      (SELECT
        CAST(ADD_MONTHS(CURRENT_DATE, -1) + ((2 - EXTRACT(DAYOFWEEK FROM ADD_MONTHS(CURRENT_DATE, -1))) MOD 7) - (7 * s.Seq) AS DATE) AS Monday,
        CAST(ADD_MONTHS(CURRENT_DATE, -1) + ((3 - EXTRACT(DAYOFWEEK FROM ADD_MONTHS(CURRENT_DATE, -1))) MOD 7) - (7 * s.Seq) AS DATE) AS Tuesday
       FROM
         (SELECT ROW_NUMBER() OVER (ORDER BY Calendar_Date) - 1 AS Seq
          FROM Sys_Calendar.CALENDAR
          WHERE Calendar_Date BETWEEN CURRENT_DATE - INTERVAL '50' DAY AND CURRENT_DATE) s
       WHERE s.Seq < 7) TD
    LEFT JOIN
      (SELECT DISTINCT
        CAST(LOAD_TS AS DATE) AS MondayDate
       FROM HRI
       WHERE EXTRACT(DAYOFWEEK FROM LOAD_TS) = 2) HD
    ON TD.Monday = HD.MondayDate
  )
  SELECT
    ValidDate AS MondayDate,
    ROW_NUMBER() OVER (ORDER BY ValidDate DESC) AS row_index
  FROM DateSequence
  WHERE
    ValidDate <= CURRENT_DATE
  QUALIFY
    row_index <= 7
) WITH DATA
ON COMMIT PRESERVE ROWS;
