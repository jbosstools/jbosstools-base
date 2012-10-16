/******************************************************************************* 
 * Copyright (c) 2011 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.ui;

import java.net.MalformedURLException;
import java.net.URL;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Image;
import org.jboss.tools.common.ui.CommonUIPlugin;

public class ModelUIImages {

	private static ModelUIImages INSTANCE;

	static {
		try {
			INSTANCE = new ModelUIImages(new URL(CommonUIPlugin.getDefault().getBundle().getEntry("/"), "icons/")); //$NON-NLS-1$ //$NON-NLS-2$
		} catch (MalformedURLException e) {
			CommonUIPlugin.getDefault().logError(e);
		}
	}

	public static final String JAVA_SERVICE_PROVIDER_IMAGE = "wizard/JavaServiceProviderWizBan.png"; //$NON-NLS-1$
	public static final String NEW_JAVA_SERVICE_PROVIDER_IMAGE = "wizard/NewJavaServiceProviderWizBan.png"; //$NON-NLS-1$

	public static Image getImage(ImageDescriptor descriptor) {
		return CommonUIPlugin.getImageDescriptorRegistry().get(descriptor);
	}

	public static Image getImage(String key) {
		return INSTANCE.createImageDescriptor(key).createImage();
	}

	public static ImageDescriptor getImageDescriptor(String key) {
		return INSTANCE.createImageDescriptor(key);
	}

	public static void setImageDescriptors(IAction action, String iconName)	{
		action.setImageDescriptor(INSTANCE.createImageDescriptor(iconName));
	}

	public static ModelUIImages getInstance() {
		return INSTANCE;
	}

	private URL baseUrl;
	private ModelUIImages parentRegistry;

	protected ModelUIImages(URL registryUrl, ModelUIImages parent){
		if(registryUrl == null) throw new IllegalArgumentException(CommonUIMessages.IMAGESBASE_URL_FOR_IMAGE_REGISTRY_CANNOT_BE_NULL);
		baseUrl = registryUrl;
		parentRegistry = parent;
	}
	
	protected ModelUIImages(URL url){
		this(url,null);		
	}

	public Image getImageByFileName(String key) {
		return createImageDescriptor(key).createImage();
	}

	public ImageDescriptor createImageDescriptor(String key) {
		try {
			return ImageDescriptor.createFromURL(makeIconFileURL(key));
		} catch (MalformedURLException e) {
			if(parentRegistry == null) {
				return ImageDescriptor.getMissingImageDescriptor();
			} else {
				return parentRegistry.createImageDescriptor(key);
			}
		}		
	}

	private URL makeIconFileURL(String name) throws MalformedURLException {
		if (name == null) throw new MalformedURLException(CommonUIMessages.IMAGESIMAGE_NAME_CANNOT_BE_NULL);
		return new URL(baseUrl, name);
	}

}