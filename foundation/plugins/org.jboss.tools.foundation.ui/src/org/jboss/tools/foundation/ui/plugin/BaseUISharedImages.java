/******************************************************************************* 
 * Copyright (c) 2013 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.foundation.ui.plugin;

import java.util.Hashtable;
import java.util.Iterator;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Image;
import org.osgi.framework.Bundle;

/**
 * A helper class for managing shared images
 * Clients are expected to subclass this method. 
 * In their constructor, they should perform something like:
 * 
 * 		super(bundle);
 * 		addImage(ERROR_IMG, "images/error.gif");
 * 		addImage(ERROR_HOVER_IMG, "images/errorHover.gif");
 * @Since 1.1
 */
public class BaseUISharedImages {

	private Hashtable<String, Image> images;
	private Hashtable<String, ImageDescriptor> descriptors;
	private Bundle pluginBundle;
	
	/**
	 * Instantiate your shared images with your bundle
	 * @param pluginBundle
	 */
	public BaseUISharedImages(Bundle pluginBundle) {
		this.pluginBundle = pluginBundle;
		this.images = new Hashtable<String, Image>();
		this.descriptors = new Hashtable<String, ImageDescriptor>();
	}
	
	/**
	 * Allow subclasses to add a key / path combination to the shared images
	 * @param key
	 * @param path
	 */
	protected void addImage(String key, String path) {
		ImageDescriptor id = createImageDescriptor(path);
		if( id != null ) {
			addDescriptor(descriptors, key, id);
		}
	}
	
	protected ImageDescriptor createImageDescriptor(String path) {
		return createImageDescriptor(pluginBundle, path);
	}
	
	protected void addDescriptor(Hashtable<String, ImageDescriptor> map, String key, ImageDescriptor val) {
		map.put(key, val);
	}
	
	/* Internal method to create an Image descriptor */
	private ImageDescriptor createImageDescriptor (Bundle pluginBundle, String relativePath) {
		return ImageDescriptor.createFromURL(pluginBundle.getEntry(relativePath));
	}
	
	/**
	 * Fetch an image if one is available 
	 * Otherwise, if the image is either not yet created, or is disposed, 
	 * create the image. 
	 * 
	 * @param key
	 * @return
	 */
	public Image image(String key) {
		Image image = images.get(key);
		if( image == null || image.isDisposed()) {
			ImageDescriptor desc = descriptors.get(key);
			if( desc == null )
				return null;
			images.put(key, desc.createImage());
		}
		return images.get(key);
	}
	
	/**
	 * Get an image descriptor for the given key
	 * @param key
	 * @return
	 */
	public ImageDescriptor descriptor(String key) {
		return (ImageDescriptor) descriptors.get(key);
	}
	
	
	/**
	 * Dispose of all items 
	 */
	public void dispose() {
		Iterator<String> iter = images.keySet().iterator();
		while (iter.hasNext()) {
			Image image = (Image) images.get(iter.next());
			image.dispose();
		}
		images.clear();
		descriptors.clear();
		images = null;
		descriptors = null;
	}
	
	/**
	 * Make sure we dispose all of our images
	 */
	protected void finalize() throws Throwable {
		try {
			dispose();
		} finally {
			super.finalize();
		}
	}
}
