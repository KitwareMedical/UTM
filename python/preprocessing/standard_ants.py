import ants
import argparse
import numpy as np

def standard_ants_preprocessing(args):

  im = ants.image_read( args.image )
  atlas = ants.image_read( args.atlas )

  print("Runnning N4 Bias Correction")
  im_n4 = ants.n4_bias_field_correction( im )

  print("Runnning Registration")

  reg = ants.registration(atlas, im_n4, type_of_transform="SyN", outprefix=args.out_tx)
  im_warped = reg['warpedmovout']
  ants.image_write( im_warped, args.out_image )

  jac = ants.create_jacobian_determinant_image(atlas, reg['fwdtransforms'][0],False,True)
  ants.image_write( jac.apply(np.abs), args.out_jac_image )

  print("Creating Mask")
  priors = [
             ants.image_read( args.atlas_csf ),
             ants.image_read( args.atlas_grey ),
             ants.image_read( args.atlas_white )
           ]
  mask = priors[0].copy()
  mask_view = mask.view()
  for i in range(1, len(priors)):
    mask_view[ priors[i].numpy() > 0 ] = 1
  mask_view[mask_view > 0] = 1
  ants.image_write( mask, "mask.nii")

  print("Runnning Segmentation")
  seg = ants.prior_based_segmentation( im_warped, priors, mask )

  ants.image_write( seg['segmentation'], args.out_segmentation )

  return reg

def main():
  parser = argparse.ArgumentParser(description='Perform registration to atlas')
  parser.add_argument('--image', required=True,
          help='image filename to register to atalas')
  parser.add_argument('--atlas', required=True,
          help='atlas image filename')
  parser.add_argument('--atlas_csf', required=True,
          help='atlas probability csf image filenames')
  parser.add_argument('--atlas_grey', required=True,
          help='atlas probability grey image filenames')
  parser.add_argument('--atlas_white', required=True,
          help='atlas probability white image filenames')
  parser.add_argument('--out_image', required=True,
          help='output filename of registered image')
  parser.add_argument('--out_tx', required=True,
          help='output prefix for transforms', default="")
  parser.add_argument('--out_jac_image', required=True,
          help='output filename of jacobian image')
  parser.add_argument('--out_segmentation', required=True,
          help='output filename of segmentation')
  args = parser.parse_args()

  standard_ants_preprocessing(args)


if __name__ == '__main__':
  main()
