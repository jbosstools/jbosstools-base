/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.model.util;

import java.awt.Point;
import java.awt.image.*;

import javax.swing.*;

import org.eclipse.core.runtime.Status;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.PaletteData;
import org.eclipse.swt.graphics.RGB;
import org.jboss.tools.common.model.plugin.ModelPlugin;

public class IconUtil {
	public static final int PALETTE_GROUP_IMAGE_TYPE = 1; 
	public static final int PALETTE_ELEMENT_AS_GROUP_IMAGE_TYPE = 2; 
	public static final int PALETTE_ELEMENT_IMAGE_TYPE = 3; 
	public static final int PALETTE_IMAGE_WIDTH = 50; 
	private static final int PALETTE_ARROW_TOP = 7; 
	private static final int PALETTE_ARROW_RIGHT_SHIFT = 8; 

    public IconUtil() {}

/*
    public static ImageIcon getImage(String imageName) {
        try {
            return new ImageIcon(ModelImages.getImage(imageName));
        } catch (Exception e) {
			ModelPlugin.getDefault().getLog().log(new Status(Status.ERROR, ModelPlugin.PLUGIN_ID, Status.OK, "Image " + imageName + " not found", e));
        }
        return null;
    }
*/

/*
  private static JPanel dummy = new JPanel();

  public static ImageIcon getRowImage(ImageIcon[] icons) {
      Point[] ps = new Point[icons.length];
      int width = 0;
      for (int i = 0; i < icons.length; i++) {
          ps[i] = new Point(width, 0);
          width = width + icons[i].getIconWidth();
      }
      Point size = new Point(width, 16);
      return IconUtil.placeImages(size, icons, ps, false);
  }

  public static ImageIcon placeImages(Point size,
                                      ImageIcon[] icons, Point[] ps,
                                      boolean transparent) {
      int[] px = new int[size.x * size.y];
      for (int i = 0; i < px.length; i++) px[i] = 0xffffff;
      if(icons.length > 0) {
          addImage(size, px, icons[0], ps[0], false);
          for (int i = 1; i < icons.length; i++)
            addImage(size, px, icons[i], ps[i], transparent);
      }
      ImageProducer ip = new MemoryImageSource(size.x, size.y, px, 0, size.x);
      return new ImageIcon(dummy.createImage(ip));
  }


  public static ImageIcon placeImages(Point size, ImageIcon i1, Point p1,
                                                  ImageIcon i2, Point p2,
                                                  boolean transparent) {
      if(i1 == null) return (i2 == null) ? getEmptyIcon(size) : i2;
      if(i2 == null) return i1;
      int[] px = new int[size.x * size.y];
      for (int i = 0; i < px.length; i++) px[i] = 0xffffff;
      addImage(size, px, i1, p1, false);
      addImage(size, px, i2, p2, transparent);
      ImageProducer ip = new MemoryImageSource(size.x, size.y, px, 0, size.x);
      return new ImageIcon(dummy.createImage(ip));
  }

  private static void addImage(Point size, int[] base, ImageIcon im, Point p,
                               boolean transparent) {
      int w = im.getIconWidth(), h = im.getIconHeight();
      int[] img = grabPixels(im);
      int t = img[0];
      for (int i = 0; i < w; i++) for (int j = 0; j < h; j++) {
          int x = p.x + i, y = p.y + j;
          if(x < size.x && y < size.y) {
              int k = img[j*w + i];
              if(k != t || !transparent) base[y*size.x + x] = k;
          }
      }
  }

  public static ImageIcon getEmptyIcon(Point size) {
      int[] px = new int[size.x * size.y];
      for (int i = 0; i < px.length; i++) px[i] = 0xffffff;
      ImageProducer ip = new MemoryImageSource(size.x, size.y, px, 0, size.x);
      return new ImageIcon(dummy.createImage(ip));
  }
*/
  public static int[] grabPixels(ImageIcon im) {
      int w = im.getIconWidth(), h = im.getIconHeight();
      int[] px = new int[h * w];
      PixelGrabber pg = new PixelGrabber(im.getImage(), 0, 0, w, h, px, 0, w);
      try {
    	  pg.grabPixels();
      } catch (Exception e) {
    	  //ignore
      }
      return px;
  }

  public static ImageData toEclipseImageData(ImageIcon icon) {
	  int[] pixels = IconUtil.grabPixels(icon);
	  int w = icon.getIconWidth(), h = icon.getIconHeight();
	  ImageData imgData = new ImageData(w, h, 32, new PaletteData(0xff0000, 0xff00, 0xff));
	  int maskWidth = (((w + 7) / 8) + 1) / 2 * 2;
	  byte[] maskDate = new byte[maskWidth * h];
	  ImageData mask = new ImageData(w, h, 1, new PaletteData(new RGB[] {new RGB(0, 0, 0), new RGB(255, 255, 255)}), 2, maskDate);
	  
	  for (int x = 0; x < w; x++) for (int y = 0; y < h; y++) {
		  int pixelValue = pixels[y * w + x]; 
		  if ((pixelValue >>> 24) < 0x80)  { // trasparant
			  imgData.setPixel(x, y, 0);
			  mask.setPixel(x, y, 0);
		  } else {
			  imgData.setPixel(x, y, pixelValue);
			  mask.setPixel(x, y, 1);
		  }
	  }		 
	  imgData.maskPad = mask.scanlinePad;  
	  imgData.maskData = mask.data;
	  return imgData;
  }

  public static Image toEclipseImage(ImageIcon icon) {
	  return new Image(null, toEclipseImageData(icon));
  }

/*
  public static Image toEclipsePaletteImage(ImageIcon icon, int type) {
	  int w = icon.getIconWidth(), h = icon.getIconHeight();
	  int left = 0, right = 0;
	  if (type == PALETTE_GROUP_IMAGE_TYPE || type == PALETTE_ELEMENT_AS_GROUP_IMAGE_TYPE) {
		  if (w < PALETTE_IMAGE_WIDTH) {
			  left = (PALETTE_IMAGE_WIDTH - w) / 2;
			  right = PALETTE_IMAGE_WIDTH - w - left;
		  }
	  }
	  int width = left + w + right;
	  int[] pixels = IconUtil.grabPixels(icon);
	  int[] tmp = new int[256];
	  int rgbCount = 0;		
	  int transparentIndex = -1;
	  for (int i = 0; i < pixels.length; i++) {
		  int pixelIndex = -1; 
		  int pixelValue = pixels[i]; 
		  if ((pixelValue >>> 24) < 0x80) {
			  if (transparentIndex < 0) {
				  pixelIndex = rgbCount++;
				  tmp[pixelIndex] = pixelValue;  
				  transparentIndex = pixelIndex;
			  } else {
				  pixelIndex = transparentIndex;
			  }
		  } else {
			  for (int j = 0; j < rgbCount; j++) {
					if (pixelValue == tmp[j] && j != transparentIndex) {
						pixelIndex = j;
						break; 
				  }
			  }
			  if (pixelIndex < 0) {
				  pixelIndex = rgbCount++;
				  tmp[pixelIndex] = pixelValue;  
			  }
		  }
		  pixels[i] = pixelIndex;
	  }
	  
	  RGB[] colors = new RGB[256];
	  for (int i = 0; i < 256; i++) {
		  if (i < rgbCount) {
			  int pixelValue = tmp[i];
			  colors[i] = new RGB((pixelValue >> 16) & 0xFF, (pixelValue >> 8) & 0xFF, pixelValue & 0xFF);
		  } else {
			  colors[i] = new RGB(0, 0, 0);
		  }
	  }
	  PaletteData p = new PaletteData(colors);
	  ImageData idata = new ImageData(width, h, 8, p);
	  idata.transparentPixel = transparentIndex;
	  for (int y = 0; y < h; y++) {
		  idata.setPixels(left, y, w, pixels, y * w);
	  }
	  if (transparentIndex != -1 && (left > 0 || right > 0)) {
		  pixels = new int[left > right ? left : right];
		  for (int x = 0; x < pixels.length; x++) {
			  pixels[x] = transparentIndex;
		  }
		  if (left > 0) {
			  int shift = left + w; 
			  for (int y = 0; y < h; y++) {
				  idata.setPixels(0, y, left, pixels, 0);
				  idata.setPixels(shift, y, right, pixels, 0);
		  	  }
		  }
	  }

	  if (type == PALETTE_GROUP_IMAGE_TYPE) {
		int shiftX = width - PALETTE_ARROW_RIGHT_SHIFT;
		for (int y = 0; y < 3; y++) {
			for (int x = y; x < 5 - y; x++) {
				idata.setPixel(shiftX + x, PALETTE_ARROW_TOP + y, 200);
			}
		}
	  }

	  return new Image(null, idata);
  }
*/
  
  public static Image getEclipseImage(String imageName) {
	  return ModelImages.getImage(imageName);
  }

}

