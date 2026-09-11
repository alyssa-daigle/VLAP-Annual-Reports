SELECT DISTINCT
    wqd_report_view.rellake_wbid,
    wqd_report_view.huccode,
    wqd_report_view.waterbodyname,
    wqd_report_view.rellake,
    wqd_report_view.town,
    wqd_report_view.statname,
    wqd_report_view.stationid,
    wqd_report_view.startdate,
    wqd_report_view.activityid,
    wqd_report_view.depthzone,
    wqd_report_view.wshedparmname,
    wqd_report_view.numresult,
    wqd_report_view.resultunits,
    wqd_report_view.qualifier,
    wqd_report_view.textresult,
    wqd_report_view.analyticalmethod,
    wqd_report_view.detlim,
    wqd_report_view.resultstatus,
    wqd_report_view."DEPTH",
    wqd_report_view.depthunits,
    wqd_report_view.actcmts,
    wqd_report_view.resultcmt,
    wqd_report_view.labqval,
    wqd_report_view.labid,
    wqd_report_view.detlimu,
    wqd_report_view.detcmt,
    wqd_report_view.fractiontype,
    wqd_report_view.projid,
    wqd_report_view.acttype,
    wqd_report_view.valid,
    dbawqd.wqd_waterbody.best_trophic_class,
    dbawqd.wqd_waterbody.current_trophic_status,
    wqd_report_view.stattype1
FROM
         wqd_report_view
    INNER JOIN dbawqd.wqd_waterbody ON wqd_report_view.rellake_wbid = dbawqd.wqd_waterbody.waterbodyid
WHERE
        wqd_report_view.startdate > TO_DATE('1984-12-31', 'YYYY-MM-DD')
    AND wqd_report_view.projid = 'VLAP'
    AND wqd_report_view.acttype = 'SAMPLE - ROUTINE'
    AND dbawqd.wqd_waterbody.best_trophic_class IS NOT NULL
ORDER BY
    wqd_report_view.rellake,
    wqd_report_view.town,
    wqd_report_view.stationid,
    EXTRACT(YEAR FROM wqd_report_view.startdate) DESC,
    wqd_report_view.startdate DESC