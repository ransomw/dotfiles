""""""
from typing import Dict
import openpyxl
import openpyxl.utils.dataframe
import pandas as pd


def xlst_to_dataframes(filepath):
    wb = openpyxl.load_workbook(filepath)
    return {sn: pd.DataFrame(wb[sn].values) for sn in wb.sheetnames}


def dataframes_to_xlst(dfs: Dict[str, pd.DataFrame], filepath):
    wb = openpyxl.Workbook()
    wb.active
    for (nm, df) in dfs.items():
        ws = wb.create_sheet(nm)
        ws.title
        for row in openpyxl.utils.dataframe.dataframe_to_rows(
            df, index=True, header=True
        ):
            ws.append(row)
    wb.save(filepath)
