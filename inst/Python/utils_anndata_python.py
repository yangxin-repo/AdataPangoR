
# Python/utils_anndata_python.py

import pandas as pd
import tempfile

def get_dataframe_as_parquet(dataframe):
  
  # copy the dataframe
  df = dataframe.copy()
  
  # verify the index
  if not isinstance(df.index, pd.RangeIndex):
    df = df.reset_index(names="index")
      
  # create the tmp file
  with tempfile.NamedTemporaryFile(suffix=".parquet",delete=False) as tmp:
    tmp_path = tmp.name
  
  # write the dataframe to parquet
  df.to_parquet(tmp_path,
                engine="pyarrow",
                index=False)
  
  return tmp_path

def get_index_as_parquet(index):
  
  # copy the index
  id = index.copy()
  
  # create the tmp file
  with tempfile.NamedTemporaryFile(suffix=".parquet",delete=False) as tmp:
    tmp_path = tmp.name
  
  # build the dataframe
  df = pd.DataFrame(index=id).reset_index(names="index")

  # write the dataframe to parquet
  df.to_parquet(tmp_path,
                engine="pyarrow",
                index=False)
  
  return tmp_path
