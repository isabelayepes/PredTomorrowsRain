#%%
# ISABELA: Since i had anaconda installed, I installed the CDS API by:
#    running a cmd.exe prompt terminal with my current environment from navigator activated
#    And then I used: conda config --add channels conda-forge AND conda install cdsapi
# https://cds.climate.copernicus.eu/api-how-to
# 1) Install api via pip: pip3 install cdsapi # for Python 3

import cdsapi
import zipfile
import os
import time


c = cdsapi.Client()

#%%
def download_cds(variable, year, month):
    """
    Download ERA5 Land zipped netcdf using CDS API
    Parameters
    ----------
    variable : str
        Climate variable to download
    year : int
        Year to download
    month : int
        Month to download
    Returns
    -------
        Downloads data file as zipped netcdf named "download.netcdf.zip"
        and contents "data.nc"
    """
    year = str(year)
    month = f'{month:02d}'

    c.retrieve(
        'reanalysis-era5-land',
        {
            'variable': [
                variable,
            ],
            'year': year,
            'month': month,
            'day': [
                '01', '02', '03',
                '04', '05', '06',
                '07', '08', '09',
                '10', '11', '12',
                '13', '14', '15',
                '16', '17', '18',
                '19', '20', '21',
                '22', '23', '24',
                '25', '26', '27',
                '28', '29', '30',
                '31',
            ],
            'time': [
                '00:00', '01:00', '02:00',
                '03:00', '04:00', '05:00',
                '06:00', '07:00', '08:00',
                '09:00', '10:00', '11:00',
                '12:00', '13:00', '14:00',
                '15:00', '16:00', '17:00',
                '18:00', '19:00', '20:00',
                '21:00', '22:00', '23:00',
            ],


            'area': [
                27, #north
                -82, #west
                25, #south
                -79, #east
            ],
            'format': 'netcdf.zip',
        },
        'download.netcdf.zip')

def extract_and_rename(variable, year, month):
    """
    Download ERA5 zipped netcdf using CDS API

    Parameters
    ----------
    variable: str
        Climate variable downloaded
    year : int
        Year downloaded
    month : int
        Month downloaded
    Returns
    -------
        Unzips and renames netcdf file to appropriate parameters
    """
    year = str(year)
    month = f'{month:02d}'
    with zipfile.ZipFile('download.netcdf.zip', 'r') as zip_ref:
        zip_ref.extractall('')
    try:
        os.remove(f'ERA5_{variable}_{year}_{month}.nc')
    except OSError:
        pass
    os.rename('data.nc', f'ERA5_{variable}_{year}_{month}.nc', )

# variable = '2m_temperature' # '2m_temperature', 'total_precipitation',
# year = 2022
# month = 12
# download_cds(variable,year,month)
# extract_and_rename(variable,year,month)

years = range(2015,2016,1)
months = range(1,13,1)
variables = ['2m_temperature', 'total_precipitation','10m_u_component_of_wind','10m_v_component_of_wind','lake_shape_factor','leaf_area_index_low_vegetation','leaf_area_index_high_vegetation','surface_pressure','surface_sensible_heat_flux']

for year in years:
    for month in months:
        for variable in variables:
            download_cds(variable,year,month)
            extract_and_rename(variable,year,month)
            time.sleep(15)
# %%

