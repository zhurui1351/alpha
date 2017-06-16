# -*- coding: utf-8 -*-
from flask import render_template
from flask import request
from flask import jsonify
from .echarts_option_template  import *
from app import app

import rpy2.robjects as robjects
from rpy2.robjects import pandas2ri
from rpy2.robjects.packages import importr
import pymysql
import pandas.io.sql as sql
import numpy as np
import datetime
pandas2ri.activate()
quantmod =  importr("quantmod")
path = 'D:/tradingSystem/alpha/src/visualization/webframe/R/data.R'
robjects.r.source(path)

dbname = 'china_future_ods_m'
tbname = 'dlcmi'
host = '127.0.0.1'
username="root"
password = '123456'

conn = pymysql.connect(host=host,port=3306,user=username,password=password, db=dbname,charset='utf8')
select_sql = 'select * from ' + tbname
data =  sql.read_sql(select_sql,conn)
conn.close()

dates = data['datetime'].tolist()
dates = [ datetime.datetime.strptime(d, "%Y-%m-%d %H:%M:%S") for d in dates]
data.index =  dates


@app.route('/')
@app.route('/index', methods = ['GET', 'POST'])
def index():
    return render_template('index.html')


@app.route('/getdata', methods = ['GET', 'POST'])
def getdata():
    date = request.form['date']
    date = '2015'
    print('ok')
    print(date)

    datar = data[date]
    datar = robjects.DataFrame(datar)

    freq = 5
    datar = robjects.r['getdata'](datar,freq)
    datar = pandas2ri.ri2py(datar)

    dates = datar['Date']
    dates = dates.tolist()
    bar = datar[['Open','Close','Low','High']]
    bar = np.array(bar)
    bar = bar.tolist()
    cci = datar['cci']
    cci = cci.tolist()
    cci = [round(c,2) for c in cci]
    sma = datar['sma']
    sma = sma.tolist()


    option = {'bar':bar,'sma':sma,'cci':cci,'dates':dates}
    return jsonify(option)
