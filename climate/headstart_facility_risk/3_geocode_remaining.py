############################################################################### 
# PURPOSE: finish geocoding addresses Open Street Maps couldn't find
# LAST UPDATED: 16 mar 2023
############################################################################### 

import pandas as pd
from geopy.geocoders import get_geocoder_for_service, SERVICE_TO_GEOCODER

df = pd.read_csv('~/data/py_addresses_to_geocode.csv')
items = df.to_dict(orient = 'records')
items[0]

def get_lat_long(addr:str, skip_serv:list = [], use_servs:list = []) -> tuple[float, float]:
    if not use_servs:
        use_servs = SERVICE_TO_GEOCODER.keys()
    for serv in use_servs:
        if serv in skip_serv:
            continue
        try:
            coder = get_geocoder_for_service(serv)
            locator = coder(user_agent = "hs_facilities")
            loc = locator.geocode(addr)
            print(serv)
            return loc.latitude, loc.longitude
        except:
            continue
    return None, None


used = set()
found = []

for item in items:
    addr = item['query']
    if addr in used:
        continue
    used.add(addr)

    print(addr)
    lat, lon = get_lat_long(addr)
    if not lat:
        print('missed')
    item['lat'] = lat
    item['lon'] = lon
    found.append(item)

df_found = pd.DataFrame(found)
df_found.to_csv('~/data/py_addresses_geocoded.csv')
# arcgis service ended up doing all of the geocodes