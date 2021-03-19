import ants
import argparse
import numpy as np

def create_feature_input(args):
  im = ants.image_read( args.segmentation_image )
  im_view = im.view()
  out_im = im.copy()
  out_im_view = out_im.view()
  out_im_view.fill(0)
  for seg_id in args.segmentation_id:
    out_im_view[ im_view == int(seg_id) ] = 1
  if args.intensity_image is not None:
     i_im = ants.image_read( args.intensity_image )
     i_im_view = i_im.view()
     out_im_view *= i_im_view

  #downsample for faster processing
  if args.downsample > 1:
    shape = np.round( np.asarray(out_im.shape) / args.downsample )
    out_im = ants.resample_image(out_im, shape, True)

  ants.image_write( out_im, args.out_image )

def main():
  parser = argparse.ArgumentParser(description='Create feature input from segmentation image')
  parser.add_argument('--segmentation_image', required=True, help='image filename of segementation')
  parser.add_argument('--segmentation_id', required=True, nargs="+", help='segmentation id to extract')
  parser.add_argument('--intensity_image', required=False, help='image to extract intensity from, if not given fill with 1')
  parser.add_argument('--out_image', required=True, help='output image filename of registered image')
  parser.add_argument('--downsample', required=False, help='downsample output image', default=1)
  args = parser.parse_args()
  create_feature_input(args)

if __name__ == '__main__':
  main()
