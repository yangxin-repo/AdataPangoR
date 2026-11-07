
# Python/utils_anndata_python.py

import tempfile

import fitz
import numpy as np
import pandas as pd
from PIL import Image


def get_dataframe_as_parquet(dataframe:pd.DataFrame) -> str:
  
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

def get_index_as_parquet(index) -> str:
  
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

def sampling_pdf_to_png(pdf_path:str,
                        png_path:str,
                        dpi:int = 300) -> str:
  
  zoom = dpi / 72
  
  tile = 2000
  
  pdf_doc = fitz.open(pdf_path)
  plt_rect = pdf_doc[0].rect

  plt_W = int(plt_rect.width * zoom)
  plt_H = int(plt_rect.height * zoom)
  
  canvas = Image.new("RGB", (plt_W, plt_H), "white")

  for y in range(0, plt_H, tile):
    for x in range(0, plt_W, tile):
      
        clip = fitz.Rect(
            (x / zoom), 
            (y / zoom),
            (min(x + tile, plt_W) / zoom), 
            (min(y + tile, plt_H) / zoom)
        )
        
        pix = pdf_doc[0].get_pixmap(
            matrix=fitz.Matrix(zoom, zoom),
            clip=clip,
            alpha=False
        )
        arr =(np.frombuffer(pix.samples, 
                            dtype=np.uint8)
              .reshape(pix.height, 
                       pix.width, 
                       3))
        canvas.paste(Image.fromarray(arr), 
                     (x, y))

  canvas.save(png_path, "PNG")

  return png_path
