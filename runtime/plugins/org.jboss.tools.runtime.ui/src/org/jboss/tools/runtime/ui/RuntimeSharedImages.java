/*************************************************************************************
 * Copyright (c) 2012 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.runtime.ui;

import java.util.Hashtable;
import java.util.Iterator;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Image;
import org.osgi.framework.Bundle;

public class RuntimeSharedImages {
	public static final String CHECKBOX_ON_KEY = "checkbox_on";//$NON-NLS-1$
	public static final String CHECKBOX_OFF_KEY = "checkbox_off";//$NON-NLS-1$
	public static final String ERROR_KEY = "error_image";//$NON-NLS-1$

	private static final String CHECKBOX_ON_PATH = "/icons/xpl/complete_tsk.gif";//$NON-NLS-1$
	private static final String CHECKBOX_OFF_PATH = "/icons/xpl/incomplete_tsk.gif";//$NON-NLS-1$
	private static final String ERROR_PATH = "/icons/xpl/error_tsk.gif";//$NON-NLS-1$



	private static RuntimeSharedImages INSTANCE;
	public static RuntimeSharedImages getDefault() {
		if( INSTANCE == null ) {
			INSTANCE = new RuntimeSharedImages();
		}
		return INSTANCE;
	}

	private Hashtable<String, Image> images;
	private Hashtable<String, ImageDescriptor> descriptors;

	private RuntimeSharedImages() {
		images = new Hashtable<String, Image>();
		descriptors = new Hashtable<String, ImageDescriptor>();
		Bundle pluginBundle = RuntimeUIActivator.getDefault().getBundle();		


		descriptors.put(CHECKBOX_ON_KEY, createImageDescriptor(pluginBundle, CHECKBOX_ON_PATH)); //$NON-NLS-1$
		descriptors.put(CHECKBOX_OFF_KEY, createImageDescriptor(pluginBundle, CHECKBOX_OFF_PATH)); //$NON-NLS-1$
		descriptors.put(ERROR_KEY, createImageDescriptor(pluginBundle, ERROR_PATH)); //$NON-NLS-1$
		Iterator<String> iter = descriptors.keySet().iterator();
		while (iter.hasNext()) {
			String key = iter.next();
			ImageDescriptor descriptor = descriptor(key);
			images.put(key,  descriptor.createImage());
		}

	}
    private ImageDescriptor createImageDescriptor (Bundle pluginBundle, String relativePath) {
            return ImageDescriptor.createFromURL(pluginBundle.getEntry(relativePath));
    }

	public static Image getImage(String key) {
		return getDefault().image(key);
	}

	public static ImageDescriptor getImageDescriptor(String key) {
		return getDefault().descriptor(key);
	}

	public Image image(String key) {
		return (Image) images.get(key);
	}

	public ImageDescriptor descriptor(String key) {
		return (ImageDescriptor) descriptors.get(key);
	}

	public void cleanup() {
		Iterator<String> iter = images.keySet().iterator();
		while (iter.hasNext()) {
			Image image = (Image) images.get(iter.next());
			image.dispose();
		}
		images = null;
		INSTANCE = null;
	}

	protected void finalize() throws Throwable {
		cleanup();
		super.finalize();
	}


}
